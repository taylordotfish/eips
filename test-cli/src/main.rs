#![deny(unsafe_op_in_unsafe_fn)]

use eips::{Direction, Eips, Visibility};
use serde::{Deserialize, Serialize};
use std::collections::btree_map::{BTreeMap, Entry};
use std::env;
use std::fmt::{self, Display};
use std::io::{self, ErrorKind};
use std::mem;
use std::mem::MaybeUninit;
use std::net::{Shutdown, SocketAddr, TcpListener, TcpStream};
use std::num::NonZeroU64;
use std::num::ParseIntError;
use std::ops::Range;
use std::process::exit;
use std::ptr;
use std::sync::{Arc, Mutex};
use std::thread::{self, JoinHandle};

#[macro_use]
mod readline;
use readline::readline;

macro_rules! ignore_error {
    ($expr:expr) => {
        if let Err(e) = $expr {
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

type Node = u32;

#[derive(
    Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize,
)]
struct Id {
    pub node: Node,
    pub seq: NonZeroU64,
}

impl eips::Id for Id {}

impl Default for Id {
    fn default() -> Self {
        Self {
            node: 0,
            seq: NonZeroU64::new(1).unwrap(),
        }
    }
}

impl Display for Id {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(fmt, "({}, {})", self.node, self.seq)
    }
}

impl Id {
    pub fn increment(&mut self) -> Self {
        let old = *self;
        self.seq = NonZeroU64::new(self.seq.get() + 1).unwrap();
        old
    }
}

#[derive(Default)]
struct State {
    eips: Eips<Id>,
    buffer: Vec<char>,
    slot_state: eips::ApplySlotState<Id, char>,
    #[cfg(eips_debug)]
    debug: eips::debug::State<Id>,
}

#[derive(Clone, Copy, Serialize, Deserialize)]
enum Command {
    Insert(eips::Insertion<Id>, char),
    Remove(Id),
    Move(eips::Move<Id>),
    ApplySlot(eips::Slot<Id>, Option<char>),
}

fn handle_remote(cmd: Command, state: &mut State) {
    match cmd {
        Command::Insert(ins, c) => match state.eips.remote_insert(ins) {
            Ok(Some(i)) => {
                state.buffer.insert(i, c);
            }
            Ok(None) => {}
            Err(e) => {
                rl_eprintln!("ERROR: Remote insert: {e}");
            }
        },
        Command::Remove(id) => match state.eips.remote_remove(id) {
            Ok(Some(i)) => {
                state.buffer.remove(i);
            }
            Ok(None) => {}
            Err(e) => {
                rl_eprintln!("ERROR: Remote remove: {e}");
            }
        },
        Command::Move(mv) => match state.eips.remote_move(mv) {
            Ok(Some((old, new))) => {
                dbg!((old, new));
                let c = state.buffer.remove(old);
                state.buffer.insert(new, c);
            }
            Ok(None) => {}
            Err(e) => {
                rl_eprintln!("ERROR: Remote move: {e}");
            }
        },
        Command::ApplySlot(slot, c) => {
            let applied =
                state.eips.apply_slot(slot, c, &mut state.slot_state);
            for result in applied {
                match result {
                    Ok((i, c)) => {
                        state.buffer.insert(i, c);
                    }
                    Err(e) => {
                        rl_eprintln!("ERROR: Apply slot: {e}");
                    }
                }
            }
        }
    }
}

fn send_slots(stream: &TcpStream, state: &mut State) -> Result<(), ()> {
    let addr = stream.peer_addr().unwrap();
    for (slot, c) in state.eips.slots(state.buffer.iter().copied()) {
        let cmd = Command::ApplySlot(slot, c);
        if let Err(e) = bincode::serialize_into(stream, &cmd) {
            println!("Error writing to {addr}: {e}");
            return Err(());
        }
    }
    Ok(())
}

fn start_incoming_thread<F, R>(
    stream: Arc<TcpStream>,
    state: Arc<Mutex<State>>,
    done: F,
) -> JoinHandle<R>
where
    F: 'static + Send + FnOnce() -> R,
    R: 'static + Send,
{
    thread::spawn(move || {
        let addr = stream.peer_addr().unwrap();
        rl_println!("{addr} connected (incoming)");
        if send_slots(&stream, &mut state.lock().unwrap()).is_err() {
            return done();
        }
        loop {
            match bincode::deserialize_from(&*stream) {
                Ok(cmd) => {
                    handle_remote(cmd, &mut state.lock().unwrap());
                }
                Err(e) => {
                    if bincode_io_kind(&e) != Some(ErrorKind::UnexpectedEof) {
                        rl_println!("Error reading from {addr}: {e}");
                    }
                    break;
                }
            };
        }
        done()
    })
}

#[derive(Debug)]
enum ServerError {
    BindError {
        port: u16,
        io: io::Error,
    },
}

impl Display for ServerError {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::BindError {
                port,
                io,
            } => write!(fmt, "could not bind to port {port}: {io}"),
        }
    }
}

impl std::error::Error for ServerError {}

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

type IncomingMap = BTreeMap<SocketAddr, (Arc<TcpStream>, JoinHandle<()>)>;

struct Cli {
    id: Id,
    state: Arc<Mutex<State>>,
    port: u16,
    server: Option<JoinHandle<()>>,
    stop_server: Arc<Mutex<bool>>,
    outgoing: BTreeMap<Node, TcpStream>,
    incoming: Arc<Mutex<IncomingMap>>,
}

impl Cli {
    pub fn new(node: u32, port: u16) -> Self {
        Self {
            id: Id {
                node,
                seq: NonZeroU64::new(1).unwrap(),
            },
            state: Arc::default(),
            port,
            server: None,
            stop_server: Arc::default(),
            outgoing: BTreeMap::new(),
            incoming: Arc::new(Mutex::new(BTreeMap::new())),
        }
    }

    fn start_server(&mut self) -> Result<(), ServerError> {
        assert!(self.server.is_none(), "server is already running");
        let listener =
            TcpListener::bind(("127.0.0.1", self.port)).map_err(|e| {
                ServerError::BindError {
                    port: self.port,
                    io: e,
                }
            })?;

        self.port = listener.local_addr().unwrap().port();
        let stop_server = self.stop_server.clone();
        let state = self.state.clone();
        let streams = self.incoming.clone();

        self.server = Some(thread::spawn(move || {
            loop {
                let (stream, addr) = listener.accept().unwrap();
                if mem::take(&mut *stop_server.lock().unwrap()) {
                    break;
                }

                let stream = Arc::new(stream);
                let streams_clone = streams.clone();
                let mut streams_lock = streams.lock().unwrap();
                let handle = start_incoming_thread(
                    stream.clone(),
                    state.clone(),
                    move || {
                        streams_clone.lock().unwrap().remove(&addr);
                        rl_println!("{addr} disconnected (incoming)");
                    },
                );

                let old = streams_lock.insert(addr, (stream, handle));
                assert!(old.is_none());
                drop(streams_lock);
            }

            let mut streams_lock = streams.lock().unwrap();
            for (stream, _) in streams_lock.values() {
                ignore_error!(stream.shutdown(Shutdown::Both));
            }

            let streams = mem::take(&mut *streams_lock);
            drop(streams_lock);
            for (_, handle) in streams.into_values() {
                handle.join().expect("error joining client thread");
            }
        }));
        Ok(())
    }

    fn stop_server(&mut self) {
        let handle = self.server.take().expect("server is not running");
        let mut stop = self.stop_server.lock().unwrap();
        let stream = TcpStream::connect(("127.0.0.1", self.port)).unwrap();
        *stop = true;
        drop(stop);
        stream.shutdown(Shutdown::Both).expect("error closing server socket");
        handle.join().unwrap();
        self.port = 0;
    }

    fn broadcast(&mut self, cmd: Command) {
        handle_remote(cmd, &mut self.state.lock().unwrap());
        self.outgoing.retain(|node, stream| {
            let port = stream.peer_addr().unwrap().port();
            if let Err(e) = bincode::serialize_into(stream, &cmd) {
                if bincode_io_kind(&e) != Some(ErrorKind::BrokenPipe) {
                    println!("Error writing to [{node}:{port}]: {e}");
                }
                println!("Disconnecting from [{node}:{port}] (outgoing)");
                false
            } else {
                true
            }
        });
    }

    fn connect(&mut self, node: Node, port: u16) {
        let entry = match self.outgoing.entry(node) {
            Entry::Vacant(entry) => entry,
            Entry::Occupied(_) => {
                println!("Already connected to node {}", node);
                return;
            }
        };

        let stream = match TcpStream::connect(("127.0.0.1", port)) {
            Ok(stream) => stream,
            Err(e) => {
                println!("Could not connect to node: {e}");
                return;
            }
        };

        if send_slots(&stream, &mut self.state.lock().unwrap()).is_ok() {
            entry.insert(stream);
        } else {
            ignore_error!(stream.shutdown(Shutdown::Both));
        }
    }

    fn disconnect(&mut self, node: Node) {
        let stream = if let Some(stream) = self.outgoing.remove(&node) {
            stream
        } else {
            println!("Error: Not connected to node {}", node);
            return;
        };
        ignore_error!(stream.shutdown(Shutdown::Both));
    }

    fn insert(&mut self, index: usize, text: &str) {
        for (i, c) in text.chars().enumerate() {
            let id = self.id.increment();
            let mut state = self.state.lock().unwrap();
            let insertion = if let Ok(insertion) =
                state.eips.local_insert(index + i, id)
            {
                insertion
            } else {
                println!("Error: Bad index");
                return;
            };
            drop(state);
            self.broadcast(Command::Insert(insertion, c));
        }
    }

    fn remove(&mut self, range: Range<usize>) {
        for i in range.rev() {
            let mut state = self.state.lock().unwrap();
            let id = if let Ok(id) = state.eips.local_remove(i) {
                id
            } else {
                println!("Error: Bad index");
                return;
            };
            drop(state);
            self.broadcast(Command::Remove(id));
        }
    }

    fn do_move(&mut self, range: Range<usize>, dest: usize) {
        let start = range.start;
        for i in range {
            let id = self.id.increment();
            let mut state = self.state.lock().unwrap();
            let mv = if dest < start {
                // Moving backwards
                state.eips.local_move(i, dest + i - start, id)
            } else {
                // Moving forwards
                state.eips.local_move(start, dest, id)
            };

            drop(state);
            let mv = if let Ok(mv) = mv {
                mv
            } else {
                println!("Error: Bad index");
                return;
            };
            self.broadcast(Command::Move(mv));
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
                let node = iter.next().ok_or(())?.parse()?;
                let port = iter.next().ok_or(())?.parse()?;
                self.connect(node, port);
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
                let state = self.state.lock().unwrap();
                for c in &state.buffer {
                    print!("{c}");
                }
                println!();
            }
            "show-slots" => {
                let state = self.state.lock().unwrap();
                for (slot, item) in state.eips.slots(&state.buffer) {
                    if slot.visibility == Visibility::Hidden {
                        print!("[\u{d7}] ");
                    }
                    print!(
                        "{} {}",
                        slot.id,
                        match slot.direction {
                            Direction::Before => "\u{2190}",
                            Direction::After => "\u{2192}",
                        },
                    );
                    match &slot.parent {
                        Some(parent) => print!(" {parent}"),
                        None => print!(" (root)"),
                    }
                    print!(" [{}", slot.move_timestamp);
                    if let Some(id) = &slot.other_location {
                        print!(" \u{2192} {id}");
                    }
                    print!("]");
                    if let Some(item) = item {
                        print!(" {item:?}");
                    }
                    println!();
                }
            }
            "outgoing" => {
                for (node, stream) in &self.outgoing {
                    let port = stream.peer_addr().unwrap().port();
                    println!("{node} (port {port})");
                }
            }
            "incoming" => {
                for addr in self.incoming.lock().unwrap().keys() {
                    println!("{addr}");
                }
            }
            #[cfg(eips_debug)]
            "debug" => {
                let mut state = self.state.lock().unwrap();
                let state = &mut *state;
                if let Err(e) =
                    state.eips.debug(&mut state.debug, &state.buffer)
                {
                    println!("Error writing debug files: {e}");
                }
            }
            _ => return Err(BadCommand),
        }
        Ok(())
    }

    pub fn run(&mut self) {
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
        for stream in mem::take(&mut self.outgoing).into_values() {
            ignore_error!(stream.shutdown(Shutdown::Both));
        }
        self.stop_server();
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
    pub port: u16,
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
    let mut cli = Cli::new(args.node, args.port);
    cli.run();
}
