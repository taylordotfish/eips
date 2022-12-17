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

use libc::{c_char, c_int};
use std::any::Any;
use std::cell::Cell;
use std::ffi::CStr;
use std::io::{self, Write};
use std::mem::{self, MaybeUninit};
use std::ptr;
use std::ptr::NonNull;
use std::string::FromUtf8Error;
use std::sync::atomic::{AtomicBool, AtomicPtr, Ordering};
use std::sync::{Mutex, MutexGuard};

pub mod raw {
    use libc::{c_char, c_int};

    extern "C" {
        pub static mut rl_point: c_int;
        pub fn add_history(string: *const c_char);
        pub fn rl_callback_handler_install(
            prompt: *const c_char,
            lhandler: unsafe extern "C" fn(*mut c_char),
        );

        pub fn rl_callback_read_char();
        pub fn rl_callback_handler_remove();
        pub fn rl_crlf() -> c_int;
        pub fn rl_forced_update_display();
        pub fn rl_redisplay();
        pub fn rl_resize_terminal();
        pub fn using_history();
    }
}

/// Like `println`, but prints above the current Readline prompt (if Readline
/// is active).
#[allow(unused_macros)]
macro_rules! rl_println {
    ($($args:tt)*) => {
        match format_args!($($args)*) {
            args => $crate::readline::wrap_print(|| println!("{args}")),
        }
    };
}

/// Like `eprintln`, but prints above the current Readline prompt (if Readline
/// is active).
#[allow(unused_macros)]
macro_rules! rl_eprintln {
    ($($args:tt)*) => {
        match format_args!($($args)*) {
            args => $crate::readline::wrap_print(|| eprintln!("{args}")),
        }
    };
}

/// Used by `rl_println` and `rl_eprintln`.
///
/// Note: `print` is called while the Readline lock is held, so it should be
/// made as minimal as possible.
#[doc(hidden)]
pub fn wrap_print<F, R>(print: F) -> R
where
    F: FnOnce() -> R,
{
    let _guard = lock();
    if !WAITING_FOR_LINE.load(Ordering::Relaxed) {
        return print();
    }
    let old_point = unsafe { raw::rl_point };
    unsafe {
        raw::rl_point = 0;
        raw::rl_redisplay();
    }
    eprint!("\x1b[2K\r");
    io::stderr().flush().unwrap();
    let result = print();
    unsafe {
        raw::rl_point = old_point;
        raw::rl_forced_update_display();
    }
    result
}

/// The signals handled by [`handle_signal`].
const SIGNALS: [c_int; 1] = [libc::SIGWINCH];
const SIGWINCH_INDEX: usize = 0;

/// Read half of the pipe used to communicate between the signal handler and
/// main program.
///
/// This must always contain either null or a valid, aligned pointer to an
/// initialized `c_int`.
static EVENTS_READ: AtomicPtr<c_int> = AtomicPtr::new(ptr::null_mut());

/// Write half of the pipe used to communicate between the signal handler and
/// main program.
///
/// This must always contain either null or a valid, aligned pointer to an
/// initialized `c_int`.
static EVENTS_WRITE: AtomicPtr<c_int> = AtomicPtr::new(ptr::null_mut());

/// Set to true when `close` is called.
static EVENT_CLOSED: AtomicBool = AtomicBool::new(false);

/// Element at index `i` set to true when `SIGNALS[i]` is received.
static EVENT_SIGNAL: [AtomicBool; 1] = [AtomicBool::new(false)];

/// Gets the file descriptor stored in [`EVENTS_WRITE`].
fn events_write_fd() -> c_int {
    NonNull::new(EVENTS_WRITE.load(Ordering::Relaxed)).map_or_else(
        || unsafe {
            libc::abort();
        },
        |p| {
            // SAFETY: Safe due to invariants of `EVENTS_WRITE`.
            unsafe { *p.as_ref() }
        },
    )
}

/// Signal handler passed to `libc::sigaction`. Only async-signal-safe
/// functions may be called from this handler.
extern "C" fn handle_signal(signal: c_int) {
    if let Some(i) = SIGNALS.iter().position(|s| signal == *s as _) {
        EVENT_SIGNAL[i].store(true, Ordering::Relaxed);
        unsafe {
            libc::write(events_write_fd(), [signal as u8].as_ptr().cast(), 1);
        }
    }
}

/// Installs the old [`sigaction`](struct@libc::sigaction) at index `index` in
/// `old_actions` and re-raises the signal at index `index` in [`SIGNALS`].
///
/// # Safety
///
/// It must be safe to call [`libc::sigaction`](fn@libc::sigaction) with the
/// provided actions in `old_actions`.
unsafe fn forward_signal(index: usize, old_actions: &[libc::sigaction]) {
    let mut current_action = MaybeUninit::uninit();
    unsafe {
        assert_eq!(
            libc::sigaction(
                SIGNALS[index],
                &old_actions[index] as _,
                current_action.as_mut_ptr(),
            ),
            0,
        );
        libc::raise(SIGNALS[index]);
        assert_eq!(
            libc::sigaction(
                SIGNALS[index],
                current_action.as_ptr(),
                ptr::null_mut(),
            ),
            0,
        );
    }
}

thread_local! {
    /// The completed line obtained from a call to [`readline`].
    static LINE: Cell<Option<*mut c_char>> = Cell::new(None);
}

/// Set to true as soon as the Readline callback handler is installed.
static WAITING_FOR_LINE: AtomicBool = AtomicBool::new(false);

/// # Safety
///
/// The Readline lock ([`lock`]) must be held.
unsafe extern "C" fn line_handler(line: *mut c_char) {
    WAITING_FOR_LINE.store(false, Ordering::Relaxed);
    unsafe {
        raw::rl_callback_handler_remove();
    }
    LINE.with(|cell| cell.set(Some(line)));
}

/// Obtains the Readline lock and initializes global state if not done yet.
fn lock() -> MutexGuard<'static, impl Any> {
    struct Initialized(bool);
    static MUTEX: Mutex<Initialized> = Mutex::new(Initialized(false));

    let mut guard = MUTEX.lock().unwrap();
    if mem::replace(&mut guard.0, true) {
        return guard;
    }

    let mut event_fds = [0; 2];
    assert!(unsafe { libc::pipe(event_fds.as_mut_ptr()) } == 0);

    for fd in event_fds {
        let flags =
            unsafe { libc::fcntl(fd, libc::F_GETFL) } | libc::O_NONBLOCK;
        assert!(flags != -1);
        assert!(unsafe { libc::fcntl(fd, libc::F_SETFL, flags) } != -1);
    }

    // Although extremely unlikely, we store pointers to the descriptors
    // instead of the descriptors themselves to avoid the case of some esoteric
    // platform where `c_int` is larger than all of the available atomics.
    let event_fds = event_fds.map(|fd| Box::into_raw(Box::new(fd)));
    EVENTS_READ.store(event_fds[0], Ordering::Relaxed);
    EVENTS_WRITE.store(event_fds[1], Ordering::Relaxed);

    unsafe {
        raw::using_history();
    }
    guard
}

/// Installs the signal handler and returns the old [`sigaction`]s.
///
/// [`sigaction`]: struct@libc::sigaction
fn install_signal_handler() -> [libc::sigaction; SIGNALS.len()] {
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

    let mut old_actions = [MaybeUninit::uninit(); SIGNALS.len()];
    for (signal, old) in SIGNALS.into_iter().zip(&mut old_actions) {
        unsafe {
            assert_eq!(
                libc::sigaction(signal, &action as _, old.as_mut_ptr()),
                0,
            );
        }
    }
    // SAFETY: We initialized every element in `old_actions`.
    unsafe { mem::transmute(old_actions) }
}

/// Obtains a line of text using GNU Readline.
pub fn readline<P>(prompt: P) -> Result<Option<String>, FromUtf8Error>
where
    P: Into<String>,
{
    static RUNNING: AtomicBool = AtomicBool::new(false);
    if RUNNING.swap(true, Ordering::Acquire) {
        panic!("`readline` is already running on another thread");
    }

    let mut prompt = prompt.into().into_bytes();
    let prompt_empty = prompt.is_empty();
    prompt.push(0);

    // SAFETY: We just pushed a null byte onto the end of `prompt`.
    let prompt_cstr = unsafe { CStr::from_bytes_with_nul_unchecked(&prompt) };
    let guard = lock();
    unsafe {
        raw::rl_callback_handler_install(prompt_cstr.as_ptr(), line_handler);
    }
    WAITING_FOR_LINE.store(true, Ordering::Relaxed);
    drop(guard);

    // SAFETY: Safe due to the invariants of `EVENTS_READ`.
    let signal_fd = *unsafe {
        NonNull::new(EVENTS_READ.load(Ordering::Relaxed))
            .expect("`EVENTS_READ` is null")
            .as_ref()
    };

    let old_sigactions = install_signal_handler();
    let mut poll_fds = [0, signal_fd].map(|fd| libc::pollfd {
        fd,
        events: libc::POLLIN,
        revents: 0,
    });

    let mut closed = false;
    let ptr = loop {
        unsafe {
            libc::poll(poll_fds.as_mut_ptr(), poll_fds.len() as _, -1);
        }

        if poll_fds[0].revents != 0 {
            let _guard = lock();
            unsafe {
                raw::rl_callback_read_char();
            }
            match LINE.with(|line| line.take()) {
                Some(ptr) => break ptr,
                None => continue,
            }
        }

        if poll_fds[1].revents & libc::POLLIN == 0 {
            continue;
        }

        let mut buf = [0; 64];
        while unsafe {
            libc::read(signal_fd, buf.as_mut_ptr().cast(), buf.len())
        } > 1
        {}

        if EVENT_SIGNAL[SIGWINCH_INDEX].swap(false, Ordering::Relaxed) {
            let _guard = lock();
            unsafe {
                raw::rl_resize_terminal();
                forward_signal(SIGWINCH_INDEX, &old_sigactions);
            }
        }

        if EVENT_CLOSED.swap(false, Ordering::Acquire) {
            closed = true;
            let _guard = lock();
            unsafe {
                line_handler(ptr::null_mut());
            }
            break LINE.with(|line| line.take()).unwrap();
        }
    };

    for (signal, old) in SIGNALS.into_iter().zip(&old_sigactions) {
        unsafe {
            assert_eq!(libc::sigaction(signal, old as _, ptr::null_mut()), 0);
        }
    }

    let line = (!ptr.is_null()).then(|| {
        // Reuse `prompt`'s buffer.
        prompt.clear();
        prompt.extend(unsafe { CStr::from_ptr(ptr) }.to_bytes());
        String::from_utf8(prompt)
    });

    if closed && !prompt_empty {
        unsafe {
            raw::rl_crlf();
        }
    }

    if let Some(Ok(_)) = line {
        let _guard = lock();
        unsafe {
            raw::add_history(ptr);
        }
    }

    unsafe {
        libc::free(ptr.cast());
    }

    RUNNING.store(false, Ordering::Release);
    line.transpose()
}

/// Causes the ongoing call to [`readline`] to stop and return [`None`]. If
/// there is no ongoing call, the effects will apply to the next call to
/// [`readline`].
///
/// This function is async-signal-safe. A call to this function
/// synchronizes-with the call to [`readline`] that returns [`None`].
pub fn close() {
    EVENT_CLOSED.store(true, Ordering::Release);
    unsafe {
        libc::write(events_write_fd(), [0_u8].as_ptr().cast(), 1);
    }
}
