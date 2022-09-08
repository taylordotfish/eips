#![deny(unsafe_op_in_unsafe_fn)]

use eips::Eips;
use serde::{Deserialize, Serialize};
use std::collections::btree_map::{BTreeMap, Entry};
use std::fmt::{self, Display};
use std::mem;
use std::mem::MaybeUninit;
use std::net::{Shutdown, TcpListener, TcpStream};
use std::num::NonZeroU64;
use std::num::ParseIntError;
use std::ops::Range;
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
    use ron::ser::{self, PrettyConfig};
    let config = PrettyConfig::new().struct_names(true);
    let addr = stream.peer_addr().unwrap();

    for (slot, c) in state.eips.slots(state.buffer.iter().copied()) {
        let cmd = Command::ApplySlot(slot, c);
        if let Err(e) = ser::to_writer_pretty(stream, &cmd, config.clone()) {
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
        rl_println!("{addr} connected");
        if send_slots(&stream, &mut state.lock().unwrap()).is_err() {
            return done();
        }
        loop {
            match ron::de::from_reader(&*stream) {
                Ok(cmd) => {
                    handle_remote(cmd, &mut state.lock().unwrap());
                }
                Err(e) => {
                    rl_println!("Error reading from {addr}: {e}");
                    break;
                }
            };
        }
        done()
    })
}

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
    state: Arc<Mutex<State>>,
    port: u16,
    server: Option<JoinHandle<()>>,
    stop_server: Arc<Mutex<bool>>,
    outgoing: BTreeMap<Node, TcpStream>,
}

impl Cli {
    pub fn new(node: u32) -> Self {
        Self {
            id: Id {
                node,
                seq: NonZeroU64::new(1).unwrap(),
            },
            state: Arc::default(),
            port: 0,
            server: None,
            stop_server: Arc::default(),
            outgoing: BTreeMap::new(),
        }
    }

    fn start_server(&mut self) {
        assert!(self.server.is_none(), "server is already running");
        let listener = TcpListener::bind(("127.0.0.1", 0)).unwrap();
        self.port = listener.local_addr().unwrap().port();
        let stop_server = self.stop_server.clone();
        let state = self.state.clone();

        self.server = Some(thread::spawn(move || {
            let streams = Arc::new(Mutex::new(BTreeMap::new()));
            loop {
                let (stream, addr) = listener.accept().unwrap();
                if mem::take(&mut *stop_server.lock().unwrap()) {
                    break;
                }

                let stream = Arc::new(stream);
                let streams_clone = streams.clone();
                let handle = start_incoming_thread(
                    stream.clone(),
                    state.clone(),
                    move || {
                        streams_clone.lock().unwrap().remove(&addr);
                        rl_println!("{addr} disconnected");
                    },
                );

                let old =
                    streams.lock().unwrap().insert(addr, (stream, handle));
                assert!(old.is_none());
            }

            let mut streams = streams.lock().unwrap();
            for (stream, _) in streams.values() {
                ignore_error!(stream.shutdown(Shutdown::Both));
            }

            let streams = mem::take(&mut *streams);
            for (_, handle) in streams.into_values() {
                handle.join().expect("error joining client thread");
            }
        }));
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
        use ron::ser::{self, PrettyConfig};
        let config = PrettyConfig::new().struct_names(true);
        self.outgoing.retain(|_, stream| {
            let addr = stream.peer_addr().unwrap();
            match ser::to_writer_pretty(stream, &cmd, config.clone()) {
                Ok(_) => true,
                Err(e) => {
                    println!("Error writing to {addr}: {e}");
                    false
                }
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
            _ => return Err(BadCommand),
        }
        Ok(())
    }

    pub fn run(&mut self) {
        self.start_server();
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

    let mut cli = Cli::new(1);
    cli.run();
}
