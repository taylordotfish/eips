use libc::{c_char, c_int};
use std::cell::Cell;
use std::ffi::CStr;
use std::io::{self, Write};
use std::mem::{ManuallyDrop, MaybeUninit};
use std::ptr;
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

/// Like `erintln`, but prints above the current Readline prompt (if Readline
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
    let _lock = lock();
    if !WAITING_FOR_LINE.load(Ordering::Relaxed) {
        return print();
    }
    let old_point = unsafe { raw::rl_point };
    unsafe {
        raw::rl_point = 0;
        raw::rl_redisplay();
    }
    eprint!("\x1b[2J\r");
    io::stderr().flush().unwrap();
    let result = print();
    unsafe {
        raw::rl_point = old_point;
        raw::rl_forced_update_display();
    }
    result
}

/// Read half of the pipe used to communicate between the signal handler and
/// main program.
///
/// This must always contain either null, or a valid, aligned pointer to an
/// initialized `c_int`.
static EVENTS_READ: AtomicPtr<c_int> = AtomicPtr::new(ptr::null_mut());

/// Write half of the pipe used to communicate between the signal handler and
/// main program.
///
/// This must always contain either null, or a valid, aligned pointer to an
/// initialized `c_int`.
static EVENTS_WRITE: AtomicPtr<c_int> = AtomicPtr::new(ptr::null_mut());

/// Set to true when `close` is called.
static EVENT_CLOSED: AtomicBool = AtomicBool::new(false);

/// Set to true when `SIGWINCH` is received.
static EVENT_SIGWINCH: AtomicBool = AtomicBool::new(false);

/// Gets the file descriptor stored in [`EVENTS_WRITE`].
fn events_write_fd() -> c_int {
    match EVENTS_WRITE.load(Ordering::Relaxed) {
        p if p.is_null() => unsafe {
            libc::abort();
        },
        // SAFETY: Safe due to invariants of `EVENTS_WRITE`.
        p => unsafe { *p },
    }
}

/// Signal handler passed to `libc::sigaction`. Only async-signal-safe
/// functions may be called from this handler.
extern "C" fn handle_signal(signal: c_int) {
    match signal {
        libc::SIGWINCH => &EVENT_SIGWINCH,
        _ => return,
    }
    .store(true, Ordering::Relaxed);
    unsafe {
        libc::write(events_write_fd(), [signal as u8].as_ptr().cast(), 1);
    }
}

/// The signals handled by [`handle_signal`].
const SIGNALS: [c_int; 1] = [libc::SIGWINCH];
const SIGWINCH_INDEX: usize = 0;

/// Installs the old `sigaction` struct in `old_actions` and re-raises the
/// signal at index `index` in [`SIGNALS`].
unsafe fn forward_signal(
    index: usize,
    action: &libc::sigaction,
    old_actions: &mut [MaybeUninit<libc::sigaction>],
) {
    unsafe {
        libc::sigaction(
            SIGNALS[index],
            old_actions[index].as_ptr(),
            ptr::null_mut(),
        );
        libc::raise(SIGNALS[index]);
        libc::sigaction(SIGNALS[index], action as _, ptr::null_mut());
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
/// The Readline lock (`lock`) must be held.
unsafe extern "C" fn line_handler(line: *mut c_char) {
    WAITING_FOR_LINE.store(false, Ordering::Relaxed);
    unsafe {
        raw::rl_callback_handler_remove();
    }
    LINE.with(|cell| cell.set(Some(line)));
}

/// Obtains the Readline lock and initializes global state if not done yet.
fn lock() -> MutexGuard<'static, ()> {
    static MUTEX: AtomicPtr<Mutex<()>> = AtomicPtr::new(ptr::null_mut());

    // Fast path -- check if global state is already initialized.
    let mutex = MUTEX.load(Ordering::Relaxed);
    if !mutex.is_null() {
        return unsafe { &*mutex }.lock().unwrap();
    }

    let mut mutex = ManuallyDrop::new(Box::new(Mutex::new(())));
    let mutex_ptr = &mut **mutex as *mut Mutex<_>;
    let lock = unsafe { &*mutex_ptr }.lock().unwrap();

    if let Err(m) = MUTEX.compare_exchange(
        ptr::null_mut(),
        mutex_ptr,
        Ordering::Relaxed,
        Ordering::Relaxed,
    ) {
        ManuallyDrop::into_inner(mutex);
        return unsafe { &*m }.lock().unwrap();
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
    lock
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

    {
        let _lock = lock();
        unsafe {
            raw::rl_callback_handler_install(
                CStr::from_bytes_with_nul_unchecked(&prompt).as_ptr(),
                line_handler,
            );
        }
        WAITING_FOR_LINE.store(true, Ordering::Relaxed);
    }

    let signal_fd = match EVENTS_READ.load(Ordering::Relaxed) {
        p if p.is_null() => panic!("EVENTS_READ is null"),
        // SAFETY: Safe due to invariants of `EVENTS_READ`.
        p => unsafe { *p },
    };

    let sigaction = libc::sigaction {
        sa_sigaction: handle_signal as usize,
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

    let mut old_sigactions = [MaybeUninit::uninit(); SIGNALS.len()];
    for (signal, old) in SIGNALS.into_iter().zip(&mut old_sigactions) {
        unsafe {
            libc::sigaction(signal, &sigaction as _, old.as_mut_ptr());
        }
    }

    let mut poll_fds = [0, signal_fd].map(|fd| libc::pollfd {
        fd,
        events: libc::POLLIN,
        revents: 0,
    });

    let ptr = loop {
        unsafe {
            libc::poll(poll_fds.as_mut_ptr(), poll_fds.len() as _, -1);
        }

        if poll_fds[0].revents != 0 {
            let _lock = lock();
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

        if EVENT_SIGWINCH.swap(false, Ordering::Relaxed) {
            let _lock = lock();
            unsafe {
                raw::rl_resize_terminal();
                forward_signal(
                    SIGWINCH_INDEX,
                    &sigaction,
                    &mut old_sigactions,
                );
            }
        }

        if EVENT_CLOSED.swap(false, Ordering::Acquire) {
            let _lock = lock();
            unsafe {
                line_handler(ptr::null_mut());
            }
            break LINE.with(|line| line.take()).unwrap();
        }
    };

    for (signal, old) in SIGNALS.into_iter().zip(&mut old_sigactions) {
        unsafe {
            libc::sigaction(signal, old.as_ptr(), ptr::null_mut());
        }
    }

    let line = (!ptr.is_null()).then(|| {
        // Reuse `prompt`'s buffer.
        prompt.clear();
        prompt.extend(unsafe { CStr::from_ptr(ptr) }.to_bytes());
        String::from_utf8(prompt)
    });

    if line.is_none() && !prompt_empty {
        unsafe {
            raw::rl_crlf();
        }
    }

    if let Some(Ok(_)) = line {
        let _lock = lock();
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

/// Causes the ongoing call to [`readline`], if any, to stop and return
/// [`None`]. This function is async-signal-safe.
pub fn close() {
    EVENT_CLOSED.store(true, Ordering::Release);
    unsafe {
        libc::write(events_write_fd(), [0_u8].as_ptr().cast(), 1);
    }
}
