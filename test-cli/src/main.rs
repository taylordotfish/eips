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

use eips::changes::Direction;
use eips::{Eips, LocalChange, RemoteChange};
use serde::{Deserialize, Serialize};
use std::borrow::BorrowMut;
use std::collections::btree_map::{BTreeMap, Entry};
use std::env;
use std::error::Error;
use std::fmt::{self, Debug, Display};
use std::fs::File;
use std::io::{self, BufRead, BufReader, ErrorKind, Write};
use std::mem::{self, ManuallyDrop, MaybeUninit};
use std::net::{Shutdown, SocketAddr, TcpListener, TcpStream};
use std::num::{NonZeroU32, ParseIntError};
use std::ops::{ControlFlow, Range};
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

macro_rules! ignore_error {
    ($expr:expr) => {
        if let Err(ref e) = $expr {
            if cfg!(debug_assertions) {
                rl_eprintln!("{}:{}: {e:?}", file!(), line!());
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

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
#[derive(Serialize, Deserialize)]
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
    8,    /* LIST_FANOUT */
    16,   /* CHUNK_SIZE */
    true, /* RESUMABLE_ITER */
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
    pub blocked: AtomicBool,
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
            blocked: &shared.blocked,
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
    pub blocked: &'a AtomicBool,
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
        let mut document = self.document.read().unwrap();
        let mut paused = document.eips.changes().pause();
        loop {
            if self.shutdown.load(Ordering::Acquire) {
                return Ok(());
            }

            let mut iter = paused.resume(&document.eips);
            self.buffer.extend(iter.by_ref().take(UPDATE_BUFFER_LEN).map(
                |(change, i)| Update {
                    change,
                    character: i.map(|i| document.text[i]),
                },
            ));

            if self.buffer.is_empty() {
                return Ok(());
            }
            paused = iter.pause();
            drop(document);
            self.send_buffer()?;
            document = self.document.read().unwrap();
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
            if self.blocked.load(Ordering::Acquire) {
                iter = ManuallyDrop::new(inner.wait(self.cond));
                continue;
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
            handle_message(msg, HandleMessage {
                node,
                document: self.document,
                updates: &mut self.updates,
                outgoing: self.outgoing,
            });
        }
    }
}

fn handle_message<'a, C>(msg: Message, mut context: C)
where
    C: BorrowMut<HandleMessage<'a>>,
{
    context.borrow_mut().handle_message(msg);
}

struct HandleMessage<'a> {
    pub node: Node,
    pub document: &'a RwLock<Document>,
    pub updates: &'a mut Sender<ReceivedUpdate>,
    pub outgoing: &'a RwLock<OutgoingMap>,
}

impl HandleMessage<'_> {
    pub fn handle_message(&mut self, msg: Message) {
        match msg {
            Message::Update(update) => self.handle_update(update),
        }
    }

    pub fn handle_update(&mut self, update: Update) {
        let Update {
            change,
            character: c,
        } = update;

        let mut document = self.document.write().unwrap();
        match match document.eips.apply_change(change) {
            Ok(local) => local,
            Err(e) => {
                rl_eprintln!("Error: Remote change: {e}");
                return;
            }
        } {
            LocalChange::AlreadyApplied => return,
            LocalChange::None => {}
            LocalChange::Insert(i) => {
                let Some(c) = c else {
                    drop(document);
                    rl_eprintln!("Error: Expected character for insertion");
                    return;
                };
                document.text.insert(i, c);
            }
            LocalChange::Remove(i) => {
                document.text.remove(i);
            }
            LocalChange::Move {
                old,
                new,
            } => {
                let c = document.text.remove(old);
                document.text.insert(new, c);
            }
        }

        drop(document);
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

            let mut incoming = self.incoming.write().unwrap();
            let handle = thread::spawn(move || {
                if let Err(e) = thread.run() {
                    rl_eprintln!("Error: {e}");
                }
                rl_println!("{addr} disconnected (incoming)");
            });
            let old = incoming.insert(addr, Incoming {
                stream: Arc::new(stream),
                node: None,
                thread: handle,
            });
            debug_assert!(old.is_none());
            drop(incoming);
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

const PRINT_BUFFER_LEN: usize = 16384;

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

        let mut outgoing = self.outgoing.write().unwrap();
        let handle = thread::spawn(move || {
            if let Err(e) = thread.run() {
                rl_eprintln!("Error: {e}");
            }
        });
        let old = outgoing.insert(addr, Outgoing {
            shared: Arc::new(OutgoingShared {
                stream,
                cond: RwCondvar::new(),
                shutdown: AtomicBool::new(false),
                blocked: AtomicBool::new(false),
            }),
            node: None,
            thread: handle,
        });
        debug_assert!(old.is_none());
        drop(outgoing);
    }

    fn disconnect(&mut self, node: Node) {
        let Some(&addr) = self.addrs.read().unwrap().get(&node) else {
            println!("Error: Not connected to node {node}.");
            return;
        };
        let outgoing = self.outgoing.write().unwrap().remove(&addr).unwrap();
        outgoing.shared.shutdown.store(true, Ordering::Relaxed);
        outgoing.thread.join().expect("error joining outgoing thread");
    }

    fn broadcast(&mut self, msg: Message) {
        handle_message(msg, HandleMessage {
            node: self.id.node,
            document: &self.document,
            updates: &mut self.updates,
            outgoing: &self.outgoing,
        });
    }

    fn insert(&mut self, index: usize, text: &str) {
        for (i, c) in text.chars().enumerate() {
            let id = self.id.increment();
            let mut document = self.document.write().unwrap();
            let Ok(change) = document.eips.insert(index + i, id) else {
                drop(document);
                println!("Error: Index out of range.");
                return;
            };
            drop(document);
            self.broadcast(Message::Update(Update {
                change,
                character: Some(c),
            }));
        }
    }

    fn remove(&mut self, range: Range<usize>) {
        for i in range.rev() {
            let mut document = self.document.write().unwrap();
            let Ok(change) = document.eips.remove(i) else {
                drop(document);
                println!("Error: Index out of range.");
                return;
            };
            drop(document);
            self.broadcast(Message::Update(Update {
                change,
                character: None,
            }));
        }
    }

    fn do_move(&mut self, range: Range<usize>, dest: usize) {
        let start = range.start;
        if dest == start || range.is_empty() {
            return;
        }

        let mut mv = |old, new| -> ControlFlow<()> {
            let id = self.id.increment();
            let mut document = self.document.write().unwrap();
            let Ok(change) = document.eips.mv(old, new, id) else {
                drop(document);
                println!("Error: Index out of range.");
                return ControlFlow::Break(());
            };
            drop(document);
            self.broadcast(Message::Update(Update {
                change,
                character: None,
            }));
            ControlFlow::Continue(())
        };

        if dest > start {
            // Moving forwards
            for i in range.rev() {
                if mv(i, dest + (i - start)) == ControlFlow::Break(()) {
                    return;
                }
            }
        } else {
            // Moving backwards
            for i in range {
                if mv(i, dest + (i - start)) == ControlFlow::Break(()) {
                    return;
                }
            }
        }
    }

    fn show_text(&mut self) {
        let mut buf = Vec::new();
        let mut index = 0;
        let mut document = self.document.read().unwrap();
        buf.reserve_exact(PRINT_BUFFER_LEN.min(document.text.len()));
        loop {
            let iter = document.text.iter().copied().skip(index);
            buf.extend(iter.take(PRINT_BUFFER_LEN));
            drop(document);
            if buf.is_empty() {
                break;
            }
            index += buf.len();
            let mut lock = io::stdout().lock();
            for c in buf.drain(..) {
                write!(lock, "{c}").unwrap();
            }
            drop(lock);
            document = self.document.read().unwrap();
        }
        println!();
    }

    fn write_change(update: Update, mut stream: impl Write) -> io::Result<()> {
        write!(
            stream,
            "{} {}",
            update.change.id,
            match update.change.direction {
                Direction::Before => "←",
                Direction::After => "→",
            },
        )?;
        match &update.change.parent {
            Some(parent) => write!(stream, " {parent}"),
            None => write!(stream, " (root)"),
        }?;
        write!(stream, " [{}", update.change.move_timestamp)?;
        if let Some(id) = &update.change.old_location {
            write!(stream, " → {id}")?;
        }
        write!(stream, "]")?;
        if let Some(c) = update.character {
            write!(stream, " {c:?}")
        } else {
            write!(stream, " ∅")
        }?;
        writeln!(stream)?;
        Ok(())
    }

    fn show_changes(&mut self) {
        let mut buf = Vec::new();
        let mut document = self.document.read().unwrap();
        buf.reserve_exact(UPDATE_BUFFER_LEN.min(document.text.len()));
        let mut paused = document.eips.changes().pause();
        loop {
            let mut iter = paused.resume(&document.eips);
            buf.extend(iter.by_ref().take(UPDATE_BUFFER_LEN).map(
                |(change, i)| Update {
                    change,
                    character: i.map(|i| document.text[i]),
                },
            ));

            paused = iter.pause();
            drop(document);
            if buf.is_empty() {
                break;
            }

            let mut lock = io::stdout().lock();
            for update in buf.drain(..) {
                Self::write_change(update, &mut lock).unwrap();
            }
            drop(lock);
            document = self.document.read().unwrap();
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

    fn source(&mut self, path: &str) -> Result<(), BadCommand> {
        let mut reader = match File::open(path) {
            Ok(file) => BufReader::new(file),
            Err(e) => {
                println!("Error: Could not open file: {e}");
                return Ok(());
            }
        };
        let mut line = String::new();
        loop {
            line.clear();
            match reader.read_line(&mut line) {
                Ok(0) => break,
                Ok(_) => {}
                Err(e) => {
                    println!("Error reading from file: {e}");
                    break;
                }
            }
            let cmd = line.trim_end_matches(['\r', '\n']);
            println!("> {cmd}");
            self.handle_line(cmd)?;
        }
        Ok(())
    }

    fn set_blocked(&mut self, node: Option<Node>, blocked: bool) {
        let Some(node) = node else {
            for outgoing in self.outgoing.read().unwrap().values() {
                outgoing.shared.blocked.store(blocked, Ordering::Release);
            }
            return;
        };
        let Some(&addr) = self.addrs.read().unwrap().get(&node) else {
            println!("Error: Not connected to node {node}.");
            return;
        };
        let shared = self.outgoing.read().unwrap()[&addr].shared.clone();
        if shared.blocked.load(Ordering::Relaxed) == blocked {
            if blocked {
                println!("Node {node} is already blocked.");
            } else {
                println!("Node {node} is already unblocked.");
            }
        }
        shared.blocked.store(blocked, Ordering::Release);
    }

    fn handle_line(&mut self, line: &str) -> Result<(), BadCommand> {
        let (start, rest) = line.split_once(' ').unwrap_or((line, ""));
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
            "show" => self.show_text(),
            "show-changes" => {
                self.show_changes();
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
            "block" => {
                let mut iter = rest.split(' ');
                let node = iter.next().map(|a| a.parse()).transpose()?;
                self.set_blocked(node, true);
            }
            "unblock" => {
                let mut iter = rest.split(' ');
                let node = iter.next().map(|a| a.parse()).transpose()?;
                self.set_blocked(node, false);
            }
            "source" => {
                if rest.is_empty() {
                    return Err(BadCommand);
                }
                self.source(rest)?;
            }
            #[cfg(eips_debug)]
            "debug" => {
                let document = self.document.read().unwrap();
                let Document {
                    eips,
                    text,
                    ..
                } = &*document;
                if let Err(e) = eips.save_debug(&mut self.debug, text) {
                    drop(document);
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
        self.port = listener.local_addr().unwrap().port();
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
        let mut prompt_buf = prompt.clone();
        while let Some(line) = readline::readline(prompt_buf) {
            let mut line = String::from_utf8(line).unwrap();
            if self.handle_line(&line).is_err() {
                eprintln!("Error: Bad command or arguments");
            }
            line.clear();
            line.push_str(&prompt);
            prompt_buf = line;
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
