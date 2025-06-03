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

use atomic_int::AtomicCInt;
use gnu_readline_sys as rl;
use libc::{c_char, c_int};
use std::cell::Cell;
use std::ffi::CStr;
use std::io::{self, Write};
use std::mem::{self, MaybeUninit};
use std::ptr::{self, NonNull};
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Mutex, MutexGuard};

/// Like [`println`], but prints above any current Readline prompt.
macro_rules! rl_println {
    ($($args:tt)*) => {{
        let mut stdout = $crate::readline::lock_stdout();
        writeln!(stdout, $($args)*).expect("error writing to stdout");
    }};
}

/// Like [`eprintln`], but prints above any current Readline prompt.
macro_rules! rl_eprintln {
    ($($args:tt)*) => {{
        let mut stderr = $crate::readline::lock_stdout();
        writeln!(stderr, $($args)*).expect("error writing to stderr");
    }};
}

/// Obtain a Readline-compatible lock on standard output.
///
/// Text written to the lock will appear above the current prompt.
///
/// To prevent deadlocks, while the lock is held, do not call other functions
/// in this module, and do not attempt to write to standard output through
/// other means.
pub fn lock_stdout() -> StdoutLock {
    StdoutLock(StreamLock::new(io::stdout().lock()))
}

/// Obtain a Readline-compatible lock on standard error.
///
/// Text written to the lock will appear above the current prompt.
///
/// To prevent deadlocks, while the lock is held, do not call other functions
/// in this module, and do not attempt to write to standard error through other
/// means.
pub fn lock_stderr() -> StderrLock {
    StderrLock(StreamLock::new(io::stderr().lock()))
}

/// A Readline-compatible lock on standard output.
///
/// This will redraw the prompt as necessary when dropped. The last character
/// written to the stream before being dropped (if any) should be a newline.
pub struct StdoutLock(StreamLock<io::StdoutLock<'static>>);

/// A Readline-compatible lock on standard error.
///
/// This will redraw the prompt as necessary when dropped. The last character
/// written to the stream before being dropped (if any) should be a newline.
pub struct StderrLock(StreamLock<io::StderrLock<'static>>);

impl Write for StdoutLock {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        self.0.write(buf)
    }

    fn flush(&mut self) -> io::Result<()> {
        self.0.flush()
    }
}

impl Write for StderrLock {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        self.0.write(buf)
    }

    fn flush(&mut self) -> io::Result<()> {
        self.0.flush()
    }
}

trait Stream: Write {
    fn with_stderr_lock<F, R>(&mut self, f: F) -> R
    where
        F: FnOnce(&mut io::StderrLock<'static>) -> R;
}

impl Stream for io::StdoutLock<'static> {
    fn with_stderr_lock<F, R>(&mut self, f: F) -> R
    where
        F: FnOnce(&mut io::StderrLock<'static>) -> R,
    {
        f(&mut io::stderr().lock())
    }
}

impl Stream for io::StderrLock<'static> {
    fn with_stderr_lock<F, R>(&mut self, f: F) -> R
    where
        F: FnOnce(&mut io::StderrLock<'static>) -> R,
    {
        f(self)
    }
}

struct StreamLock<S> {
    stream: S,
    _guard: MutexGuard<'static, Initialized>,
    old_point: Option<c_int>,
}

fn ansi_clear_line<W: Write>(stream: &mut W) -> io::Result<()> {
    write!(stream, "\x1b[2K\r").and_then(|_| stream.flush())
}

impl<S: Stream> StreamLock<S> {
    pub fn new(mut stream: S) -> Self {
        let guard = lock();
        let rl_active = CALLBACK_INSTALLED.load(Ordering::Relaxed);
        let old_point = rl_active.then(|| {
            let point = unsafe { rl::rl_point };
            unsafe {
                rl::rl_point = 0;
                rl::rl_redisplay();
            }
            stream
                .with_stderr_lock(ansi_clear_line)
                .expect("error writing to stderr");
            point
        });
        Self {
            stream,
            _guard: guard,
            old_point,
        }
    }
}

impl<W> Drop for StreamLock<W> {
    fn drop(&mut self) {
        if let Some(point) = self.old_point {
            unsafe {
                rl::rl_point = point;
                rl::rl_forced_update_display();
            }
        }
    }
}

impl<W: Write> Write for StreamLock<W> {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        self.stream.write(buf)
    }

    fn flush(&mut self) -> io::Result<()> {
        self.stream.flush()
    }
}

struct Events {
    pub closed: AtomicBool,
    pub sigwinch: AtomicBool,
    /// Read half of the signal handler pipe.
    read_fd: AtomicCInt,
    /// Write half of the signal handler pipe.
    write_fd: AtomicCInt,
}

static EVENTS: Events = Events {
    closed: AtomicBool::new(false),
    sigwinch: AtomicBool::new(false),
    read_fd: AtomicCInt::new(0),
    write_fd: AtomicCInt::new(0),
};

/// Signal handler passed to `libc::sigaction`. Only async-signal-safe
/// functions may be called from this handler.
extern "C" fn handle_signal(signal: c_int) {
    match signal {
        libc::SIGWINCH => &EVENTS.sigwinch,
        _ => return,
    }
    .store(true, Ordering::Relaxed);
    let fd = EVENTS.write_fd.load(Ordering::Relaxed);
    unsafe {
        libc::write(fd, [signal as u8].as_ptr().cast(), 1);
    }
}

/// Installs `action`, re-raises `signal`, and restores the previous
/// [`sigaction`].
///
/// # Safety
///
/// It must be safe to call [`libc::sigaction`](fn@libc::sigaction) with
/// `action`.
///
/// [`sigaction`]: struct@libc::sigaction
unsafe fn forward_signal(signal: c_int, action: &libc::sigaction) {
    let mut current = MaybeUninit::uninit();
    assert_eq!(
        unsafe { libc::sigaction(signal, action as _, current.as_mut_ptr()) },
        0,
    );
    unsafe {
        libc::raise(signal);
    }
    assert_eq!(
        unsafe { libc::sigaction(signal, current.as_ptr(), ptr::null_mut()) },
        0,
    );
}

struct Sigactions {
    pub sigwinch: libc::sigaction,
}

/// Installs the signal handlers and returns the old [`sigaction`]s.
///
/// [`sigaction`]: struct@libc::sigaction
fn install_signal_handlers() -> Sigactions {
    let action = libc::sigaction {
        sa_sigaction: handle_signal as usize,
        sa_mask: {
            let mut mask = MaybeUninit::uninit();
            unsafe {
                libc::sigemptyset(mask.as_mut_ptr());
            }
            // SAFETY: `libc::sigemptyset` initializes `mask`.
            unsafe { mask.assume_init() }
        },
        sa_flags: 0,
        sa_restorer: None,
    };

    let mut sigwinch = MaybeUninit::uninit();
    let status = unsafe {
        libc::sigaction(libc::SIGWINCH, &action as _, sigwinch.as_mut_ptr())
    };
    assert_eq!(status, 0);
    Sigactions {
        // SAFETY: Initialized by `sigaction`.
        sigwinch: unsafe { sigwinch.assume_init() },
    }
}

/// Uninstalls the signal handlers and restores the old [`sigaction`]s.
///
/// # Safety
///
/// It must be safe to call [`libc::sigaction`](fn@libc::sigaction) with
/// the [`sigaction`]s in `old_actions`.
///
/// [`sigaction`]: struct@libc::sigaction
unsafe fn uninstall_signal_handlers(old_actions: &Sigactions) {
    let status = unsafe {
        libc::sigaction(libc::SIGWINCH, &old_actions.sigwinch, ptr::null_mut())
    };
    assert_eq!(status, 0);
    if EVENTS.sigwinch.swap(false, Ordering::Relaxed) {
        unsafe {
            libc::raise(libc::SIGWINCH);
        }
    }
}

thread_local! {
    /// The completed line obtained from a call to [`readline`].
    static LINE: Cell<Option<*mut c_char>> = const { Cell::new(None) };
}

/// Set to true as soon as the Readline callback handler is installed.
static CALLBACK_INSTALLED: AtomicBool = AtomicBool::new(false);

struct Initialized(bool);

/// Obtains the Readline lock and initializes global state if not done yet.
fn lock() -> MutexGuard<'static, Initialized> {
    static MUTEX: Mutex<Initialized> = Mutex::new(Initialized(false));

    let mut guard = MUTEX.lock().unwrap();
    if mem::replace(&mut guard.0, true) {
        return guard;
    }

    let mut event_fds = [0; 2];
    assert_eq!(unsafe { libc::pipe(event_fds.as_mut_ptr()) }, 0);

    for fd in event_fds {
        let flags =
            unsafe { libc::fcntl(fd, libc::F_GETFL) } | libc::O_NONBLOCK;
        assert_ne!(flags, -1);
        assert_ne!(unsafe { libc::fcntl(fd, libc::F_SETFL, flags) }, -1);
    }

    EVENTS.read_fd.store(event_fds[0], Ordering::Relaxed);
    EVENTS.write_fd.store(event_fds[1], Ordering::Relaxed);
    unsafe {
        rl::using_history();
    }
    guard
}

/// # Safety
///
/// The Readline lock ([`lock`]) must be held.
unsafe extern "C" fn line_handler(line: *mut c_char) {
    CALLBACK_INSTALLED.store(false, Ordering::Relaxed);
    unsafe {
        rl::rl_callback_handler_remove();
    }
    LINE.with(|cell| cell.set(Some(line)));
}

/// Obtains a line of text using GNU Readline.
///
/// # Panics
///
/// This function will panic if called from two threads concurrently.
pub fn readline<P>(prompt: P) -> Option<Vec<u8>>
where
    P: Into<Vec<u8>>,
{
    let mut prompt = prompt.into();
    static RUNNING: AtomicBool = AtomicBool::new(false);
    if RUNNING.swap(true, Ordering::Acquire) {
        panic!("`readline` is already running on another thread");
    }
    prompt.push(0);

    // SAFETY: We just pushed a null byte onto the end of `prompt`.
    let cprompt = unsafe { CStr::from_bytes_with_nul_unchecked(&prompt) };
    let guard = lock();
    unsafe {
        rl::rl_callback_handler_install(cprompt.as_ptr(), Some(line_handler));
    }
    CALLBACK_INSTALLED.store(true, Ordering::Relaxed);
    drop(guard);

    let events_fd = EVENTS.read_fd.load(Ordering::Relaxed);
    let old_sigactions = install_signal_handlers();
    let mut poll_fds = [0, events_fd].map(|fd| libc::pollfd {
        fd,
        events: libc::POLLIN,
        revents: 0,
    });

    let ptr = loop {
        unsafe {
            libc::poll(poll_fds.as_mut_ptr(), poll_fds.len() as _, -1);
        }

        if poll_fds[0].revents != 0 {
            let _guard = lock();
            unsafe {
                rl::rl_callback_read_char();
            }
            match LINE.with(|line| line.take()) {
                Some(ptr) => break ptr,
                None => continue,
            }
        }

        if poll_fds[1].revents & libc::POLLIN == 0 {
            continue;
        }

        let mut buf = [0; 16];
        while unsafe {
            libc::read(events_fd, buf.as_mut_ptr().cast(), buf.len())
        } > 0
        {}

        if EVENTS.sigwinch.swap(false, Ordering::Relaxed) {
            let _guard = lock();
            unsafe {
                rl::rl_resize_terminal();
                forward_signal(libc::SIGWINCH, &old_sigactions.sigwinch);
            }
        }

        if EVENTS.closed.swap(false, Ordering::Acquire) {
            let _guard = lock();
            unsafe {
                line_handler(ptr::null_mut());
            }
            break LINE.with(|line| line.take()).unwrap();
        }
    };

    unsafe {
        uninstall_signal_handlers(&old_sigactions);
    }

    let line = NonNull::new(ptr).map(|ptr| {
        // Reuse `prompt`'s buffer.
        prompt.clear();
        prompt.extend(
            unsafe { CStr::from_ptr(ptr.as_ptr()) }.to_bytes_with_nul(),
        );
        unsafe {
            libc::free(ptr.as_ptr().cast());
        }
        let _guard = lock();
        unsafe {
            rl::add_history(prompt.as_ptr());
        }
        prompt.pop(); // null byte
        prompt
    });

    RUNNING.store(false, Ordering::Release);
    line
}

/// Causes the ongoing call to [`readline`] to stop and return [`None`]. If
/// there is no ongoing call, the effects will apply to the next call to
/// [`readline`].
///
/// This function is async-signal-safe. A call to this function
/// synchronizes-with the call to [`readline`] that returns [`None`].
pub fn close() {
    EVENTS.closed.store(true, Ordering::Release);
    let fd = EVENTS.write_fd.load(Ordering::Relaxed);
    unsafe {
        libc::write(fd, [0_u8].as_ptr().cast(), 1);
    }
}

#[allow(dead_code)]
fn allow_unused() {
    let _ = lock_stderr;
}
