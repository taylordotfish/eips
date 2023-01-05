/*
 * Copyright (C) [unpublished] taylor.fish <contact@taylor.fish>
 *
 * This file is part of Eips.
 *
 * Eips is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published
 * by the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Eips is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Eips. If not, see <https://www.gnu.org/licenses/>.
 */

#![deny(unsafe_op_in_unsafe_fn)]

use eips::{Direction, Eips, LocalChange, RemoteChange, Visibility};
use serde::{Deserialize, Serialize};
use std::collections::btree_map::{BTreeMap, Entry};
use std::env;
use std::error::Error;
use std::fmt::{self, Debug, Display};
use std::io::{self, ErrorKind};
use std::mem::{self, ManuallyDrop, MaybeUninit};
use std::net::{Shutdown, SocketAddr, TcpListener, TcpStream};
use std::num::{NonZeroU32, ParseIntError};
use std::ops::Range;
use std::process::exit;
use std::ptr;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, RwLock};
use std::thread::{self, JoinHandle};

mod condvar;
mod queue;
#[macro_use]
mod readline;

use condvar::RwCondvar;
use queue::{Receiver, Sender};
use readline::readline;

macro_rules! ignore_error {
    ($expr:expr) => {
        if let Err(ref e) = $expr {
            if cfg!(debug_assertions) {
                rl_eprintln!("{}:{}: {:?}", file!(), line!(), e);
            }
        }
    };
}

fn bincode_io_kind(e: &bincode::Error) -> Option<ErrorKind> {
    if let bincode::ErrorKind::Io(e) = &**e {
        Some(e.kind())
    } else {
        None
    }
}

type Node = NonZeroU32;
type Port = u16;

#[derive(
    Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize,
)]
struct Id {
    pub node: Node,
    pub seq: u64,
}

impl Display for Id {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(fmt, "({}, {})", self.node, self.seq)
    }
}

impl Id {
    pub fn increment(&mut self) -> Self {
        let old = *self;
        self.seq += 1;
        old
    }
}

type EipsOptions = eips::Options<
    /* LIST_FANOUT */ 8,
    /* CHUNK_SIZE */ 16,
    /* RESUMABLE_ITER */ true,
>;

#[derive(Default)]
struct Document {
    pub eips: Eips<Id, EipsOptions>,
    pub text: Vec<char>,
}

struct Outgoing {
    pub shared: Arc<OutgoingShared>,
    pub node: Option<Node>,
    pub thread: JoinHandle<()>,
}

struct OutgoingShared {
    pub stream: TcpStream,
    pub cond: RwCondvar,
    pub shutdown: AtomicBool,
}

struct Incoming {
    pub stream: Arc<TcpStream>,
    pub node: Option<Node>,
    pub thread: JoinHandle<()>,
}

#[derive(Clone, Copy, Serialize, Deserialize)]
struct Update {
    pub change: RemoteChange<Id>,
    pub character: Option<char>,
}

#[derive(Clone, Copy)]
struct ReceivedUpdate {
    pub node: Node,
    pub update: Update,
}

#[derive(Clone, Copy, Serialize, Deserialize)]
enum Message {
    Update(Update),
}

type IncomingMap = BTreeMap<SocketAddr, Incoming>;
type OutgoingMap = BTreeMap<SocketAddr, Outgoing>;
type AddrMap = BTreeMap<Node, SocketAddr>;

struct ShutdownOnDrop;

impl Drop for ShutdownOnDrop {
    fn drop(&mut self) {
        readline::close();
    }
}

const UPDATE_BUFFER_LEN: usize = 4096;

struct OutgoingThread {
    pub addr: SocketAddr,
    pub local_node: Node,
    pub document: Arc<RwLock<Document>>,
    pub updates: Receiver<ReceivedUpdate>,
    pub outgoing: Arc<RwLock<OutgoingMap>>,
    pub addrs: Arc<RwLock<AddrMap>>,
}

#[derive(Debug)]
enum OutgoingError {
    WriteFailed(bincode::Error),
    ReadFailed(bincode::Error),
    AlreadyConnected(Node),
}

impl Display for OutgoingError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::WriteFailed(e) => write!(f, "could not write to node: {e}"),
            Self::ReadFailed(e) => write!(f, "could not read from node: {e}"),
            Self::AlreadyConnected(node) => {
                write!(f, "already connected to node {node}")
            }
        }
    }
}

impl OutgoingThread {
    pub fn run(self) -> Result<(), OutgoingError> {
        let shutdown = ShutdownOnDrop;
        let shared = self.outgoing.read().unwrap()[&self.addr].shared.clone();
        let buffer = Vec::with_capacity(UPDATE_BUFFER_LEN);
        let result = RunningOutgoingThread {
            addr: self.addr,
            local_node: self.local_node,
            stream: &shared.stream,
            cond: &shared.cond,
            shutdown: &shared.shutdown,
            document: &self.document,
            updates: self.updates,
            outgoing: &self.outgoing,
            addrs: &self.addrs,
            buffer,
        }
        .run();
        ignore_error!(shared.stream.shutdown(Shutdown::Both));
        self.outgoing.write().unwrap().remove(&self.addr);
        mem::forget(shutdown);
        result
    }
}

struct RunningOutgoingThread<'a> {
    pub addr: SocketAddr,
    pub local_node: Node,
    pub stream: &'a TcpStream,
    pub cond: &'a RwCondvar,
    pub shutdown: &'a AtomicBool,
    pub document: &'a RwLock<Document>,
    pub updates: Receiver<ReceivedUpdate>,
    pub outgoing: &'a RwLock<OutgoingMap>,
    pub addrs: &'a RwLock<AddrMap>,
    pub buffer: Vec<Update>,
}

impl RunningOutgoingThread<'_> {
    fn send_buffer(&mut self) -> Result<(), OutgoingError> {
        for update in self.buffer.drain(..) {
            let msg = Message::Update(update);
            if let Err(e) = bincode::serialize_into(self.stream, &msg) {
                return Err(OutgoingError::WriteFailed(e));
            }
        }
        Ok(())
    }

    fn send_initial(&mut self) -> Result<(), OutgoingError> {
        self.buffer.clear();
        let mut guard = self.document.read().unwrap();
        let mut paused = guard.eips.changes().pause();
        loop {
            if self.shutdown.load(Ordering::Acquire) {
                return Ok(());
            }

            let mut iter = paused.resume(&guard.eips);
            self.buffer.extend(iter.by_ref().take(UPDATE_BUFFER_LEN).map(
                |(change, i)| Update {
                    change,
                    character: i.map(|i| guard.text[i]),
                },
            ));
            paused = iter.pause();
            drop(guard);

            if self.buffer.is_empty() {
                return Ok(());
            }

            self.send_buffer()?;
            guard = self.document.read().unwrap();
        }
    }

    fn send_updates(&mut self, node: Node) -> Result<(), OutgoingError> {
        self.buffer.clear();
        // Using `ManuallyDrop` due to
        // https://github.com/rust-lang/rust/issues/70919
        let mut iter = ManuallyDrop::new(self.updates.recv());
        loop {
            let mut inner = ManuallyDrop::into_inner(iter);
            if self.shutdown.load(Ordering::Acquire) {
                return Ok(());
            }

            let updates = inner.by_ref().filter_map(|update| {
                (update.node != node).then_some(update.update)
            });
            self.buffer.extend(updates.take(UPDATE_BUFFER_LEN));

            if self.buffer.is_empty() {
                iter = ManuallyDrop::new(inner.wait(self.cond));
                continue;
            }

            drop(inner);
            self.send_buffer()?;
            iter = ManuallyDrop::new(self.updates.recv());
        }
    }

    pub fn run(mut self) -> Result<(), OutgoingError> {
        let local = self.local_node;
        if let Err(e) = bincode::serialize_into(self.stream, &local) {
            return Err(OutgoingError::WriteFailed(e));
        }

        let node: Node = match bincode::deserialize_from(self.stream) {
            Ok(node) => node,
            Err(e) => return Err(OutgoingError::ReadFailed(e)),
        };

        match self.addrs.write().unwrap().entry(node) {
            Entry::Vacant(entry) => {
                entry.insert(self.addr);
            }
            Entry::Occupied(_) => {
                return Err(OutgoingError::AlreadyConnected(node));
            }
        }

        self.outgoing.write().unwrap().get_mut(&self.addr).unwrap().node =
            Some(node);
        self.send_initial()?;
        self.send_updates(node)
    }
}

struct IncomingThread {
    pub addr: SocketAddr,
    pub local_node: Node,
    pub document: Arc<RwLock<Document>>,
    pub updates: Sender<ReceivedUpdate>,
    pub outgoing: Arc<RwLock<OutgoingMap>>,
    pub incoming: Arc<RwLock<IncomingMap>>,
}

#[derive(Debug)]
enum IncomingError {
    ReadFailed(bincode::Error),
    WriteFailed(bincode::Error),
}

impl Display for IncomingError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::ReadFailed(e) => write!(f, "could not read from node: {e}"),
            Self::WriteFailed(e) => write!(f, "could not write to node: {e}"),
        }
    }
}

impl IncomingThread {
    pub fn run(self) -> Result<(), IncomingError> {
        let shutdown = ShutdownOnDrop;
        let stream = self.incoming.read().unwrap()[&self.addr].stream.clone();
        let result = RunningIncomingThread {
            addr: self.addr,
            local_node: self.local_node,
            stream: &stream,
            document: &self.document,
            updates: self.updates,
            outgoing: &self.outgoing,
            incoming: &self.incoming,
        }
        .run();
        ignore_error!(stream.shutdown(Shutdown::Both));
        self.incoming.write().unwrap().remove(&self.addr);
        mem::forget(shutdown);
        result
    }
}

struct RunningIncomingThread<'a> {
    addr: SocketAddr,
    local_node: Node,
    stream: &'a TcpStream,
    document: &'a RwLock<Document>,
    updates: Sender<ReceivedUpdate>,
    outgoing: &'a RwLock<OutgoingMap>,
    incoming: &'a RwLock<IncomingMap>,
}

impl RunningIncomingThread<'_> {
    fn run(mut self) -> Result<(), IncomingError> {
        let node: Node = match bincode::deserialize_from(self.stream) {
            Ok(node) => node,
            Err(e) => return Err(IncomingError::ReadFailed(e)),
        };

        let local = self.local_node;
        if let Err(e) = bincode::serialize_into(self.stream, &local) {
            return Err(IncomingError::WriteFailed(e));
        }
        self.incoming.write().unwrap().get_mut(&self.addr).unwrap().node =
            Some(node);

        let addr = self.addr;
        rl_println!("node {node} ({addr}) connected (incoming)");
        loop {
            let msg = match bincode::deserialize_from(self.stream) {
                Ok(msg) => msg,
                Err(e) => {
                    if bincode_io_kind(&e) == Some(ErrorKind::UnexpectedEof) {
                        return Ok(());
                    }
                    return Err(IncomingError::ReadFailed(e));
                }
            };
            HandleMessage {
                node,
                document: self.document,
                updates: &mut self.updates,
                outgoing: self.outgoing,
            }
            .handle(msg);
        }
    }
}

struct HandleMessage<'a> {
    pub node: Node,
    pub document: &'a RwLock<Document>,
    pub updates: &'a mut Sender<ReceivedUpdate>,
    pub outgoing: &'a RwLock<OutgoingMap>,
}

impl HandleMessage<'_> {
    pub fn handle(&mut self, msg: Message) {
        let Message::Update(Update {
            change,
            character: c,
        }) = msg;
        let mut guard = self.document.write().unwrap();
        match match guard.eips.apply_change(change) {
            Ok(local) => local,
            Err(e) => {
                rl_eprintln!("Error: remote change: {e}");
                return;
            }
        } {
            LocalChange::AlreadyApplied => return,
            LocalChange::None => {}
            LocalChange::Insert(i) => {
                let Some(c) = c else {
                    drop(guard);
                    rl_eprintln!("Error: expected character for insertion");
                    return;
                };
                guard.text.insert(i, c);
            }
            LocalChange::Remove(i) => {
                guard.text.remove(i);
            }
            LocalChange::Move {
                old,
                new,
            } => {
                let c = guard.text.remove(old);
                guard.text.insert(new, c);
            }
        }

        drop(guard);
        self.updates.send(ReceivedUpdate {
            node: self.node,
            update: Update {
                change,
                character: c,
            },
        });
        for outgoing in self.outgoing.read().unwrap().values() {
            outgoing.shared.cond.notify_all();
        }
    }
}

struct Server {
    pub node: Node,
    pub listener: TcpListener,
    pub stop_server: Arc<AtomicBool>,
    pub document: Arc<RwLock<Document>>,
    pub updates: Sender<ReceivedUpdate>,
    pub outgoing: Arc<RwLock<OutgoingMap>>,
    pub incoming: Arc<RwLock<IncomingMap>>,
}

#[derive(Debug)]
enum ServerError {
    ListenFailed(io::Error),
}

impl Display for ServerError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::ListenFailed(e) => {
                write!(f, "could not listen on server socket: {e}")
            }
        }
    }
}

impl Server {
    pub fn run(self) -> Result<(), ServerError> {
        struct DropActions<'a> {
            stop_server: &'a AtomicBool,
            _shutdown: ShutdownOnDrop,
        }

        impl Drop for DropActions<'_> {
            fn drop(&mut self) {
                self.stop_server.store(true, Ordering::Relaxed);
            }
        }

        let drop_actions = DropActions {
            stop_server: &self.stop_server,
            _shutdown: ShutdownOnDrop,
        };

        let result = RunningServer {
            node: self.node,
            listener: &self.listener,
            stop_server: &self.stop_server,
            document: &self.document,
            updates: self.updates,
            outgoing: &self.outgoing,
            incoming: &self.incoming,
        }
        .run();
        if result.is_ok() {
            mem::forget(drop_actions);
        }
        result
    }
}

struct RunningServer<'a> {
    pub node: Node,
    pub listener: &'a TcpListener,
    pub stop_server: &'a AtomicBool,
    pub document: &'a Arc<RwLock<Document>>,
    pub updates: Sender<ReceivedUpdate>,
    pub outgoing: &'a Arc<RwLock<OutgoingMap>>,
    pub incoming: &'a Arc<RwLock<IncomingMap>>,
}

impl RunningServer<'_> {
    pub fn run(self) -> Result<(), ServerError> {
        loop {
            let (stream, addr) = match self.listener.accept() {
                Ok(client) => client,
                Err(e) => return Err(ServerError::ListenFailed(e)),
            };
            if self.stop_server.load(Ordering::Relaxed) {
                return Ok(());
            }

            let thread = IncomingThread {
                addr,
                local_node: self.node,
                document: self.document.clone(),
                updates: self.updates.clone(),
                outgoing: self.outgoing.clone(),
                incoming: self.incoming.clone(),
            };

            let mut guard = self.incoming.write().unwrap();
            let handle = thread::spawn(move || {
                if let Err(e) = thread.run() {
                    rl_eprintln!("Error: {e}");
                }
                rl_println!("{} disconnected (incoming)", addr);
            });
            let old = guard.insert(
                addr,
                Incoming {
                    stream: Arc::new(stream),
                    node: None,
                    thread: handle,
                },
            );
            debug_assert!(old.is_none());
            drop(guard);
        }
    }
}

#[derive(Debug)]
enum StartError {
    BindError {
        port: Port,
        io: io::Error,
    },
}

impl Display for StartError {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::BindError {
                port,
                io,
            } => write!(fmt, "could not bind to port {port}: {io}"),
        }
    }
}

impl Error for StartError {}

struct BadCommand;

impl From<()> for BadCommand {
    fn from(_: ()) -> Self {
        Self
    }
}

impl From<ParseIntError> for BadCommand {
    fn from(_: ParseIntError) -> Self {
        Self
    }
}

struct Cli {
    id: Id,
    document: Arc<RwLock<Document>>,
    #[cfg(eips_debug)]
    debug: eips::debug::State<Id, EipsOptions>,
    updates: Sender<ReceivedUpdate>,
    port: Port,
    server: Option<JoinHandle<()>>,
    stop_server: Arc<AtomicBool>,
    outgoing: Arc<RwLock<OutgoingMap>>,
    incoming: Arc<RwLock<IncomingMap>>,
    addrs: Arc<RwLock<AddrMap>>,
}

impl Cli {
    pub fn new(node: Node, port: Port) -> Self {
        Self {
            id: Id {
                node,
                seq: 0,
            },
            document: Arc::default(),
            #[cfg(eips_debug)]
            debug: Default::default(),
            updates: Sender::new(),
            port,
            server: None,
            stop_server: Arc::default(),
            outgoing: Arc::default(),
            incoming: Arc::default(),
            addrs: Arc::default(),
        }
    }

    fn connect(&mut self, port: Port) {
        let stream = match TcpStream::connect(("127.0.0.1", port)) {
            Ok(stream) => stream,
            Err(e) => {
                println!("Could not connect to node: {e}");
                return;
            }
        };

        let addr = stream.peer_addr().unwrap();
        let thread = OutgoingThread {
            addr,
            local_node: self.id.node,
            document: self.document.clone(),
            updates: self.updates.new_receiver(),
            outgoing: self.outgoing.clone(),
            addrs: self.addrs.clone(),
        };

        let mut guard = self.outgoing.write().unwrap();
        let handle = thread::spawn(move || {
            if let Err(e) = thread.run() {
                rl_eprintln!("Error: {e}");
            }
        });
        let old = guard.insert(
            addr,
            Outgoing {
                shared: Arc::new(OutgoingShared {
                    stream,
                    cond: RwCondvar::new(),
                    shutdown: AtomicBool::new(false),
                }),
                node: None,
                thread: handle,
            },
        );
        debug_assert!(old.is_none());
        drop(guard);
    }

    fn disconnect(&mut self, node: Node) {
        let Some(&addr) = self.addrs.read().unwrap().get(&node) else {
            println!("Error: Not connected to node {node}");
            return;
        };
        let outgoing = self.outgoing.write().unwrap().remove(&addr).unwrap();
        outgoing.shared.shutdown.store(true, Ordering::Relaxed);
        outgoing.thread.join().expect("error joining outgoing thread");
    }

    fn broadcast(&mut self, msg: Message) {
        HandleMessage {
            node: self.id.node,
            document: &self.document,
            updates: &mut self.updates,
            outgoing: &self.outgoing,
        }
        .handle(msg);
    }

    fn insert(&mut self, index: usize, text: &str) {
        for (i, c) in text.chars().enumerate() {
            let id = self.id.increment();
            let mut guard = self.document.write().unwrap();
            let Ok(change) = guard.eips.insert(index + i, id) else {
                println!("Error: Bad index");
                return;
            };
            drop(guard);
            self.broadcast(Message::Update(Update {
                change,
                character: Some(c),
            }));
        }
    }

    fn remove(&mut self, range: Range<usize>) {
        for i in range.rev() {
            let mut guard = self.document.write().unwrap();
            let Ok(change) = guard.eips.remove(i) else {
                println!("Error: Bad index");
                return;
            };
            drop(guard);
            self.broadcast(Message::Update(Update {
                change,
                character: None,
            }));
        }
    }

    fn do_move(&mut self, range: Range<usize>, dest: usize) {
        let start = range.start;
        for i in range {
            let id = self.id.increment();
            let mut guard = self.document.write().unwrap();
            let Ok(change) = (if dest < start {
                // Moving backwards
                guard.eips.mv(i, dest + i - start, id)
            } else {
                // Moving forwards
                guard.eips.mv(start, dest, id)
            }) else {
                println!("Error: Bad index");
                return;
            };
            drop(guard);
            self.broadcast(Message::Update(Update {
                change,
                character: None,
            }));
        }
    }

    fn print_node(addr: SocketAddr, node: Option<Node>) {
        let port = addr.port();
        let print = |node| {
            println!("{node} [:{port}]");
        };
        match node {
            Some(node) => print(format_args!("{node}")),
            None => print(format_args!("({})", addr.ip())),
        }
    }

    fn handle_line(&mut self, line: String) -> Result<(), BadCommand> {
        let (start, rest) = line.split_once(' ').unwrap_or((&*line, ""));
        match start {
            "" => {}
            "help" => {
                print!("{}", include_str!("interactive-help"));
                if cfg!(eips_debug) {
                    println!("  debug");
                }
            }
            "connect" => {
                let mut iter = rest.split(' ');
                let port = iter.next().ok_or(())?.parse()?;
                self.connect(port);
            }
            "disconnect" => {
                let mut iter = rest.split(' ');
                let node = iter.next().ok_or(())?.parse()?;
                self.disconnect(node);
            }
            "insert" => {
                let mut iter = rest.splitn(2, ' ');
                let index = iter.next().ok_or(())?.parse()?;
                let text = iter.next().ok_or(())?;
                self.insert(index, text);
            }
            "remove" => {
                let mut iter = rest.split(' ');
                let src = iter.next().ok_or(())?.parse()?;
                let len: usize = iter.next().ok_or(())?.parse()?;
                self.remove(src..(src + len));
            }
            "move" => {
                let mut iter = rest.split(' ');
                let src: usize = iter.next().ok_or(())?.parse()?;
                let len: usize = iter.next().ok_or(())?.parse()?;
                let dest = iter.next().ok_or(())?.parse()?;
                if (src..src + len).contains(&dest) {
                    return Err(BadCommand);
                }
                self.do_move(src..(src + len), dest);
            }
            "show" => {
                let guard = self.document.read().unwrap();
                for c in &guard.text {
                    print!("{c}");
                }
                println!();
            }
            "show-changes" => {
                let guard = self.document.read().unwrap();
                for (change, i) in guard.eips.changes() {
                    if change.visibility == Visibility::Hidden {
                        print!("[\u{d7}] ");
                    }
                    print!(
                        "{} {}",
                        change.id,
                        match change.direction {
                            Direction::Before => "\u{2190}",
                            Direction::After => "\u{2192}",
                        },
                    );
                    match &change.parent {
                        Some(parent) => print!(" {parent}"),
                        None => print!(" (root)"),
                    }
                    print!(" [{}", change.move_timestamp);
                    if let Some(id) = &change.old_location {
                        print!(" \u{2192} {id}");
                    }
                    print!("]");
                    if let Some(i) = i {
                        print!(" {:?}", guard.text[i]);
                    }
                    println!();
                }
            }
            "outgoing" => {
                for (&addr, outgoing) in &*self.outgoing.read().unwrap() {
                    Self::print_node(addr, outgoing.node);
                }
            }
            "incoming" => {
                for (&addr, incoming) in &*self.incoming.read().unwrap() {
                    Self::print_node(addr, incoming.node);
                }
            }
            #[cfg(eips_debug)]
            "debug" => {
                let guard = self.document.read().unwrap();
                let eips = &guard.eips;
                if let Err(e) = eips.debug(&mut self.debug, &guard.text) {
                    println!("Error writing debug files: {e}");
                }
            }
            _ => return Err(BadCommand),
        }
        Ok(())
    }

    fn start_server(&mut self) -> Result<(), StartError> {
        let listener = TcpListener::bind(("127.0.0.1", self.port));
        let listener = listener.map_err(|e| StartError::BindError {
            port: self.port,
            io: e,
        })?;
        let server = Server {
            node: self.id.node,
            listener,
            stop_server: self.stop_server.clone(),
            document: self.document.clone(),
            updates: self.updates.clone(),
            outgoing: self.outgoing.clone(),
            incoming: self.incoming.clone(),
        };
        self.server = Some(thread::spawn(move || {
            if let Err(e) = server.run() {
                rl_eprintln!("Server error: {e}");
            }
        }));
        Ok(())
    }

    pub fn run(mut self) {
        if let Err(e) = self.start_server() {
            eprintln!("Could not start server: {e}");
            exit(1);
        }
        let prompt = format!("[{}:{}] ", self.id.node, self.port);
        while let Some(line) = readline(prompt.clone()).unwrap() {
            if self.handle_line(line).is_err() {
                eprintln!("Error: Bad command or arguments");
            }
        }
    }
}

impl Drop for Cli {
    fn drop(&mut self) {
        let Some(server) = self.server.take() else {
            return;
        };
        if !self.stop_server.swap(true, Ordering::Relaxed) {
            let stream = TcpStream::connect(("127.0.0.1", self.port));
            ignore_error!(stream);
            if let Ok(stream) = stream {
                ignore_error!(stream.shutdown(Shutdown::Both));
            }
        }
        server.join().unwrap();

        let outgoing = mem::take(&mut *self.outgoing.write().unwrap());
        let incoming = mem::take(&mut *self.incoming.write().unwrap());
        for outgoing in outgoing.values() {
            outgoing.shared.shutdown.store(true, Ordering::Relaxed);
            outgoing.shared.cond.notify_all();
        }
        for incoming in incoming.values() {
            ignore_error!(incoming.stream.shutdown(Shutdown::Both));
        }
        for outgoing in outgoing.into_values() {
            outgoing.thread.join().expect("error joining outgoing thread");
        }
        for incoming in incoming.into_values() {
            incoming.thread.join().expect("error joining incoming thread");
        }
    }
}

fn show_usage() -> ! {
    print!("{}", include_str!("usage"));
    exit(0);
}

fn show_version() -> ! {
    println!("{}", env!("CARGO_PKG_VERSION"));
    exit(0);
}

macro_rules! args_error {
    ($($args:tt)*) => {{
        eprintln!("Error: {}", format_args!($($args)*));
        eprintln!("See test-cli --help for usage information.");
        exit(1);
    }};
}

struct Args {
    pub node: Node,
    pub port: Port,
}

impl Args {
    pub fn from_env() -> Self {
        let process_arg = |arg: &str| match arg {
            "--help" => show_usage(),
            "--version" => show_version(),
            s if s.starts_with("--") => {
                args_error!("unrecognized option: {s}");
            }
            s if s.starts_with('-') => s
                .chars()
                .skip(1)
                .map(|c| match c {
                    'h' => show_usage(),
                    'v' => show_version(),
                    c => args_error!("unrecognized option: -{c}"),
                })
                .fold(true, |_, _| false),
            _ => true,
        };

        let mut iter =
            env::args_os().skip(1).filter_map(|a| match a.into_string() {
                Ok(s) => process_arg(&s).then_some(s),
                Err(os) => args_error!("invalid argument: {os:?}"),
            });

        let node: Node = iter
            .next()
            .and_then(|s| s.parse().ok())
            .unwrap_or_else(|| args_error!("missing or invalid <node>"));

        let port = iter.next().map_or(0, |s| {
            s.parse().unwrap_or_else(|_| args_error!("invalid <port>"))
        });

        if let Some(s) = iter.next() {
            args_error!("unexpected argument: {s}");
        }
        Self {
            node,
            port,
        }
    }
}

/// Note: only async-signal-safe functions may be called.
extern "C" fn handle_sigint(_: libc::c_int) {
    readline::close();
}

fn main() {
    let action = libc::sigaction {
        sa_sigaction: handle_sigint as usize,
        sa_mask: {
            let mut mask = MaybeUninit::uninit();
            unsafe {
                libc::sigemptyset(mask.as_mut_ptr());
                mask.assume_init()
            }
        },
        sa_flags: 0,
        sa_restorer: None,
    };

    unsafe {
        libc::sigaction(libc::SIGINT, &action as _, ptr::null_mut());
    }

    let args = Args::from_env();
    let cli = Cli::new(args.node, args.port);
    cli.run();
}
