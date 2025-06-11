/*
 * Copyright (C) 2025 taylor.fish <contact@taylor.fish>
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

use eips::change::Direction;
use eips::{Eips, LocalChange, RemoteChange};
use serde::{Deserialize, Serialize};
use std::borrow::BorrowMut;
use std::collections::BTreeMap;
use std::env;
use std::fmt::{self, Debug, Display};
use std::fs::File;
use std::io::{self, BufRead, BufReader, Write};
use std::mem::{self, MaybeUninit};
use std::net::{Shutdown, SocketAddr, TcpListener, TcpStream};
use std::num::ParseIntError;
use std::ops::{ControlFlow, Range};
use std::process::exit;
use std::ptr;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, RwLock};
use std::thread::{self, JoinHandle};

#[macro_use]
mod readline;

macro_rules! ignore_error {
    ($expr:expr) => {
        if let Err(ref e) = $expr {
            if cfg!(debug_assertions) {
                rl_eprintln!("{}:{}: {e:?}", file!(), line!());
            }
        }
    };
}

mod bincode;
mod condvar;
mod help;
mod incoming;
mod outgoing;
mod queue;

use condvar::RwCondvar;
use incoming::{Incoming, IncomingThread};
use outgoing::{Outgoing, OutgoingShared, OutgoingThread};
use queue::Sender;

type Node = u32;
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
    /* SUPPORTS_MOVE */ { cfg!(feature = "move") },
    /* LIST_FANOUT */ 12,
    /* CHUNK_SIZE */ 32,
>;

#[cfg(not(feature = "btree-vec"))]
type TextBuffer = Vec<char>;
#[cfg(feature = "btree-vec")]
type TextBuffer = btree_vec::BTreeVec<char>;

#[derive(Default)]
struct Document {
    pub eips: Eips<Id, EipsOptions>,
    pub text: TextBuffer,
}

#[derive(Clone, Copy, Debug, Serialize, Deserialize)]
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

const UPDATE_BUFFER_LEN: usize = 1024;

struct ShutdownOnDrop;

impl Drop for ShutdownOnDrop {
    fn drop(&mut self) {
        readline::close();
    }
}

#[derive(Copy, Clone, Debug)]
struct NodeAddr {
    pub addr: SocketAddr,
    pub node: Option<Node>,
}

impl NodeAddr {
    pub fn new(addr: SocketAddr, node: Option<Node>) -> Self {
        Self {
            addr,
            node,
        }
    }
}

impl Display for NodeAddr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(node) = self.node {
            write!(
                f,
                "{} {node}",
                if f.alternate() {
                    "Node"
                } else {
                    "node"
                }
            )
        } else {
            write!(f, "{}", self.addr)
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
                rl_eprintln!("error: remote change: {e}");
                return;
            }
        } {
            LocalChange::AlreadyApplied => return,
            LocalChange::None => {}
            LocalChange::Insert(i) => {
                let Some(c) = c else {
                    drop(document);
                    rl_eprintln!("error: expected character for insertion");
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

        let stop_server = self.stop_server.clone();
        let drop_actions = DropActions {
            stop_server: &self.stop_server,
            _shutdown: ShutdownOnDrop,
        };

        let result = RunningServer {
            node: self.node,
            listener: &self.listener,
            stop_server,
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
    pub stop_server: Arc<AtomicBool>,
    pub document: &'a Arc<RwLock<Document>>,
    pub updates: Sender<ReceivedUpdate>,
    pub outgoing: &'a Arc<RwLock<OutgoingMap>>,
    pub incoming: &'a Arc<RwLock<IncomingMap>>,
}

impl RunningServer<'_> {
    fn accept_one(&mut self) -> Result<ControlFlow<()>, ServerError> {
        let (stream, addr) = match self.listener.accept() {
            Ok(client) => client,
            Err(e) => return Err(ServerError::ListenFailed(e)),
        };
        if self.stop_server.load(Ordering::Relaxed) {
            return Ok(ControlFlow::Break(()));
        }

        let thread = IncomingThread {
            addr,
            local_node: self.node,
            document: self.document.clone(),
            updates: self.updates.clone(),
            outgoing: self.outgoing.clone(),
            incoming: self.incoming.clone(),
        };

        let stop_server = self.stop_server.clone();
        let mut incoming = self.incoming.write().unwrap();
        let handle = thread::spawn(move || {
            let (node, result) = thread.run();
            if let Err(e) = result {
                rl_eprintln!("error: {e}");
            }
            if !stop_server.load(Ordering::Relaxed) {
                let node_addr = NodeAddr::new(addr, node);
                rl_println!("{node_addr:#} disconnected (incoming)");
            }
        });
        let old = incoming.insert(addr, Incoming {
            stream: Arc::new(stream),
            node: None,
            thread: handle,
        });
        debug_assert!(old.is_none());
        Ok(ControlFlow::Continue(()))
    }

    pub fn run(mut self) -> Result<(), ServerError> {
        while self.accept_one()?.is_continue() {}
        Ok(())
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

#[derive(Clone, Copy)]
enum CommandError {
    MissingArg,
    InvalidArg,
    UnknownCommand,
}

impl Display for CommandError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::MissingArg => write!(f, "missing argument"),
            Self::InvalidArg => write!(f, "invalid argument"),
            Self::UnknownCommand => write!(f, "unknown command"),
        }
    }
}

impl From<ParseIntError> for CommandError {
    fn from(_: ParseIntError) -> Self {
        Self::InvalidArg
    }
}

struct InputError<'a> {
    pub command: &'a str,
    pub inner: CommandError,
}

impl Display for InputError<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let cmd = self.command;
        writeln!(f, "{}", self.inner)?;
        match self.inner {
            CommandError::UnknownCommand => {
                write!(f, "See `help` for a list of commands.")
            }
            _ => write!(f, "See `help {cmd}` for usage information."),
        }
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
                println!("error: could not connect to node: {e}");
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

        let stop_server = self.stop_server.clone();
        let mut outgoing = self.outgoing.write().unwrap();
        let handle = thread::spawn(move || {
            let (node, result) = thread.run();
            if let Err(e) = result {
                rl_eprintln!("error: {e}");
            }
            if !stop_server.load(Ordering::Relaxed) {
                let node_addr = NodeAddr::new(addr, node);
                rl_println!("{node_addr:#} disconnected (outgoing)");
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
        let Some(outgoing) = (|| {
            let addr = *self.addrs.read().unwrap().get(&node)?;
            self.outgoing.write().unwrap().remove(&addr)
        })() else {
            println!("error: not connected to node {node}");
            return;
        };
        outgoing.shared.shutdown.store(true, Ordering::Relaxed);
        outgoing.shared.cond.notify_all();
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

    fn insert_char(&mut self, index: usize, c: char) -> Result<(), ()> {
        let id = self.id.increment();
        let document = self.document.read().unwrap();
        let Ok(change) = document.eips.insert(index, id) else {
            drop(document);
            println!("error: index out of range");
            return Err(());
        };
        drop(document);
        self.broadcast(Message::Update(Update {
            change,
            character: Some(c),
        }));
        Ok(())
    }

    fn insert(&mut self, index: usize, text: &str) {
        let _ = text
            .chars()
            .enumerate()
            .try_for_each(|(i, c)| self.insert_char(index + i, c));
    }

    fn insert_reverse(&mut self, index: usize, text: &str) {
        let _ =
            text.chars().rev().try_for_each(|c| self.insert_char(index, c));
    }

    fn remove(&mut self, range: Range<usize>) {
        for i in range.rev() {
            let document = self.document.read().unwrap();
            let Ok(change) = document.eips.remove(i) else {
                drop(document);
                println!("error: index out of range");
                return;
            };
            drop(document);
            self.broadcast(Message::Update(Update {
                change,
                character: None,
            }));
        }
    }

    #[cfg(feature = "move")]
    fn do_move(&mut self, range: Range<usize>, dest: usize) {
        let start = range.start;
        if dest == start || range.is_empty() {
            return;
        }

        let mut mv = |old, new| -> ControlFlow<()> {
            let id = self.id.increment();
            let document = self.document.read().unwrap();
            let Ok(change) = document.eips.mv(old, new, id) else {
                drop(document);
                println!("error: index out of range");
                return ControlFlow::Break(());
            };
            drop(document);
            self.broadcast(Message::Update(Update {
                change,
                character: None,
            }));
            ControlFlow::Continue(())
        };

        let _ = if dest > start {
            // Moving forwards
            range.rev().try_for_each(|i| mv(i, dest + (i - start)))
        } else {
            // Moving backwards
            let mut range = range;
            range.try_for_each(|i| mv(i, dest + (i - start)))
        };
    }

    fn show_text(&self, start: usize, len: Option<usize>) {
        if len == Some(0) {
            return;
        }
        let mut buf = Vec::new();
        let mut index = start;
        let mut document = self.document.read().unwrap();
        let doc_len = document.text.len();
        let end = (|| Some(start.checked_add(len?)?.min(doc_len)))()
            .unwrap_or(doc_len);
        if start >= end {
            return;
        }
        buf.reserve_exact(PRINT_BUFFER_LEN.min(end - start));
        while index < end {
            let iter = document.text.iter().copied().skip(index);
            let amount = PRINT_BUFFER_LEN.min(end - index);
            buf.extend(iter.take(amount));
            debug_assert_eq!(buf.len(), amount);
            drop(document);
            index += buf.len();

            let mut lock = io::stdout().lock();
            for c in buf.drain(..) {
                write!(lock, "{c}").unwrap();
            }
            drop(lock);
            document = self.document.read().unwrap();
        }
        if start < end {
            println!();
        }
    }

    fn search(&self, needle: &str) -> Option<usize> {
        let needle_len = needle.chars().count();
        let document = self.document.read().unwrap();
        let mut iter = document.text.iter().copied();
        let mut pos = 0;
        loop {
            if iter.clone().take(needle_len).eq(needle.chars()) {
                return Some(pos);
            }
            iter.next()?;
            pos += 1;
        }
    }

    fn write_change<W>(update: Update, stream: &mut W) -> io::Result<()>
    where
        W: Write,
    {
        let change = &update.change;
        write!(stream, "{} {}", change.id, match change.direction {
            Direction::Before => "←",
            Direction::After => "→",
        },)?;
        match change.parent() {
            Some(parent) => write!(stream, " {parent}"),
            None => write!(stream, " (root)"),
        }?;
        if let Some(mv) = change.move_info {
            write!(stream, " [{} → {}]", mv.timestamp, mv.old_location)
        } else if let Some(c) = update.character {
            write!(stream, " {c:?}")
        } else {
            write!(stream, " ∅")
        }?;
        writeln!(stream)?;
        Ok(())
    }

    fn show_changes(&self) {
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

    fn write_node<W>(node: NodeAddr, stream: &mut W) -> io::Result<()>
    where
        W: Write,
    {
        let addr = node.addr;
        if let Some(node) = node.node {
            write!(stream, "{node} [:{}]", addr.port())
        } else {
            write!(stream, "({addr})")
        }
    }

    fn write_outgoing<W>(
        addr: SocketAddr,
        outgoing: &Outgoing,
        stream: &mut W,
    ) -> io::Result<()>
    where
        W: Write,
    {
        Self::write_node(NodeAddr::new(addr, outgoing.node), stream)?;
        if outgoing.shared.blocked.load(Ordering::Relaxed) {
            write!(stream, " (blocked)")?;
        }
        writeln!(stream)
    }

    fn write_incoming<W>(
        addr: SocketAddr,
        outgoing: &Incoming,
        stream: &mut W,
    ) -> io::Result<()>
    where
        W: Write,
    {
        Self::write_node(NodeAddr::new(addr, outgoing.node), stream)?;
        writeln!(stream)
    }

    fn source(&mut self, path: &str) -> Result<(), CommandError> {
        let mut reader = match File::open(path) {
            Ok(file) => BufReader::new(file),
            Err(e) if e.kind() == io::ErrorKind::NotFound => {
                println!("error: no such file: ‘{path}’");
                return Ok(());
            }
            Err(e) => {
                println!("error: could not open file: {e}");
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
                    println!("error reading from file: {e}");
                    break;
                }
            }
            let cmd = line.trim_end_matches(['\r', '\n']);
            println!("> {cmd}");
            if let Err(e) = self.handle_line(cmd) {
                println!("error: {}", e.inner);
                break;
            }
        }
        Ok(())
    }

    fn set_blocked(&mut self, node: Option<Node>, blocked: bool) {
        let Some(node) = node else {
            for outgoing in self.outgoing.read().unwrap().values() {
                outgoing.shared.set_blocked(blocked);
            }
            return;
        };
        let Some(&addr) = self.addrs.read().unwrap().get(&node) else {
            println!("error: not connected to node {node}");
            return;
        };
        let shared = self.outgoing.read().unwrap()[&addr].shared.clone();
        if !shared.set_blocked(blocked).changed() {
            if blocked {
                println!("Node {node} is already blocked.");
            } else {
                println!("Node {node} is already unblocked.");
            }
        }
    }

    fn help<W>(&self, cmd: Option<&str>, stream: &mut W) -> io::Result<()>
    where
        W: Write,
    {
        let Some(name) = cmd else {
            writeln!(stream, "Commands:")?;
            return help::COMMANDS.iter().try_for_each(|cmd| {
                writeln!(stream, "  {}", cmd.short_display())
            });
        };
        if let Some(cmd) = help::COMMANDS.iter().find(|c| c.name == name) {
            writeln!(stream, "{cmd}")
        } else {
            writeln!(stream, "error: no such command")
        }
    }

    fn handle_command(
        &mut self,
        command: &str,
        rest: Option<&str>,
    ) -> Result<(), CommandError> {
        use CommandError::MissingArg as Missing;
        let rest = rest.ok_or(Missing);
        match command {
            "" => {}
            "help" | "?" => {
                let mut iter = rest.into_iter().flat_map(|s| s.split(' '));
                let cmd = iter.next();
                self.help(cmd, &mut io::stdout().lock())
                    .expect("error writing to stdout");
            }
            "connect" => {
                let mut iter = rest?.split(' ');
                let port = iter.next().ok_or(Missing)?.parse()?;
                self.connect(port);
            }
            "disconnect" => {
                let mut iter = rest?.split(' ');
                let node = iter.next().ok_or(Missing)?.parse()?;
                self.disconnect(node);
            }
            "insert" => {
                let mut iter = rest?.splitn(2, ' ');
                let index = iter.next().ok_or(Missing)?.parse()?;
                let text = iter.next().ok_or(Missing)?;
                self.insert(index, text);
            }
            "insert-reverse" => {
                let mut iter = rest?.splitn(2, ' ');
                let index = iter.next().ok_or(Missing)?.parse()?;
                let text = iter.next().ok_or(Missing)?;
                self.insert_reverse(index, text);
            }
            "remove" => {
                let mut iter = rest?.split(' ');
                let src = iter.next().ok_or(Missing)?.parse()?;
                let len: usize = iter.next().ok_or(Missing)?.parse()?;
                self.remove(src..(src + len));
            }
            #[cfg(feature = "move")]
            "move" => {
                let mut iter = rest?.split(' ');
                let src: usize = iter.next().ok_or(Missing)?.parse()?;
                let len: usize = iter.next().ok_or(Missing)?.parse()?;
                let dest = iter.next().ok_or(Missing)?.parse()?;
                self.do_move(src..(src + len), dest);
            }
            "show" => {
                let mut iter = rest.into_iter().flat_map(|s| s.split(' '));
                let start = iter.next().map_or(Ok(0), |a| a.parse())?;
                let len = iter.next().map(|a| a.parse()).transpose()?;
                self.show_text(start, len);
            }
            "search" => {
                if let Some(pos) = self.search(rest?) {
                    println!("{pos}");
                }
            }
            "changes" => {
                self.show_changes();
            }
            "outgoing" => {
                let mut stdout = io::stdout().lock();
                self.outgoing
                    .read()
                    .unwrap()
                    .iter()
                    .try_for_each(|(&addr, outgoing)| {
                        Self::write_outgoing(addr, outgoing, &mut stdout)
                    })
                    .expect("error writing to stdout");
            }
            "incoming" => {
                let mut stdout = io::stdout().lock();
                self.incoming
                    .read()
                    .unwrap()
                    .iter()
                    .try_for_each(|(&addr, incoming)| {
                        Self::write_incoming(addr, incoming, &mut stdout)
                    })
                    .expect("error writing to stdout");
            }
            "block" => {
                let mut iter = rest.into_iter().flat_map(|s| s.split(' '));
                let node = iter.next().map(|a| a.parse()).transpose()?;
                self.set_blocked(node, true);
            }
            "unblock" => {
                let mut iter = rest.into_iter().flat_map(|s| s.split(' '));
                let node = iter.next().map(|a| a.parse()).transpose()?;
                self.set_blocked(node, false);
            }
            "source" => {
                self.source(rest?)?;
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
                    println!("error writing debug files: {e}");
                }
            }
            _ => return Err(CommandError::UnknownCommand),
        }
        Ok(())
    }

    fn handle_line<'s>(
        &mut self,
        line: &'s str,
    ) -> Result<(), InputError<'s>> {
        let (cmd, rest) = match line.split_once(' ') {
            Some((a, b)) => (a, Some(b)),
            None => (line, None),
        };
        self.handle_command(cmd, rest).map_err(|e| InputError {
            command: cmd,
            inner: e,
        })
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
                rl_eprintln!("server error: {e}");
            }
        }));
        Ok(())
    }

    pub fn run(mut self) {
        if let Err(e) = self.start_server() {
            eprintln!("error: could not start server: {e}");
            exit(1);
        }
        let prompt = format!("[{}:{}] ", self.id.node, self.port);
        let mut prompt_buf = prompt.clone().into_bytes();
        while let Some(line) = readline::readline(prompt_buf) {
            let mut line = match String::from_utf8(line) {
                Ok(line) => line,
                Err(e) => {
                    eprintln!("error: non-utf-8 bytes in command");
                    prompt_buf = e.into_bytes();
                    continue;
                }
            };
            if let Err(e) = self.handle_line(&line) {
                println!("error: {e}");
            }
            line.clear();
            line.push_str(&prompt);
            prompt_buf = line.into_bytes();
        }
        println!();
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
        eprintln!("error: {}", format_args!($($args)*));
        eprintln!("See `test-cli --help` for usage information.");
        exit(1);
    }};
}

struct Args {
    pub node: Node,
    pub port: Port,
}

impl Args {
    pub fn from_env() -> Self {
        let mut node = None;
        let mut port = None;
        let mut options_done = false;

        for arg in env::args().skip(1) {
            if options_done {
            } else if arg == "--" {
                options_done = true;
                continue;
            } else if arg == "--help" || arg.starts_with("-h") {
                show_usage();
            } else if arg == "--version" || arg.starts_with("-v") {
                show_version();
            } else if arg.starts_with("-") {
                args_error!("unrecognized option: {arg}");
            }
            if node.is_none() {
                node = Some(arg);
            } else if port.is_none() {
                port = Some(arg);
            } else {
                args_error!("unexpected argument: {arg}");
            }
        }

        let Some(node) = node else {
            args_error!("missing argument");
        };
        let node: Node = node
            .parse()
            .ok()
            .unwrap_or_else(|| args_error!("invalid node id"));
        let port = port.map_or(0, |s| {
            s.parse().unwrap_or_else(|_| args_error!("invalid port"))
        });
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
