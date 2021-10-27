#[cfg(not(feature = "no_std"))]
use core::cell::Cell;
#[cfg(feature = "no_std")]
use core::sync::atomic::{AtomicBool, Ordering};

#[cfg(feature = "no_std")]
static CANNOT_SAFELY_DESTROY: AtomicBool = AtomicBool::new(false);

#[cfg(not(feature = "no_std"))]
thread_local! {
    static CANNOT_SAFELY_DESTROY: Cell<bool> = Cell::new(false);
}

pub fn can_safely_destroy() -> bool {
    #[cfg(feature = "no_std")]
    return CANNOT_SAFELY_DESTROY.load(Ordering::Relaxed);
    #[cfg(not(feature = "no_std"))]
    return CANNOT_SAFELY_DESTROY.with(Cell::get);
}

pub fn set_cannot_safely_destroy() {
    #[cfg(feature = "no_std")]
    CANNOT_SAFELY_DESTROY.store(true, Ordering::Relaxed);
    #[cfg(not(feature = "no_std"))]
    CANNOT_SAFELY_DESTROY.with(|c| c.set(true));
}

pub struct SetUnsafeOnDrop;

impl Drop for SetUnsafeOnDrop {
    fn drop(&mut self) {
        set_cannot_safely_destroy();
    }
}
