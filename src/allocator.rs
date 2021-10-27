use alloc::boxed::Box;
use core::ptr::NonNull;

/// # Safety
///
/// * [`Self::allocate`] must return a pointer to valid memory initialized with
///   `value`. The memory must remain valid for the life of this allocator and
///   its clones.
/// * Clones of this allocator must behave like the original allocator.
pub unsafe trait Allocator {
    fn allocate<T>(&self, value: T) -> NonNull<T>;

    /// # Safety
    ///
    /// * `ptr` must have been allocated by this allocator.
    /// * `ptr` must not have been deallocated already.
    unsafe fn deallocate<T>(&self, ptr: NonNull<T>);
}

#[derive(Clone, Copy, Default)]
pub struct Global;

unsafe impl Allocator for Global {
    fn allocate<T>(&self, value: T) -> NonNull<T> {
        unsafe { NonNull::new_unchecked(Box::into_raw(Box::new(value))) }
    }

    unsafe fn deallocate<T>(&self, ptr: NonNull<T>) {
        unsafe { Box::from_raw(ptr.as_ptr()) };
    }
}

#[cfg(feature = "allocator_api")]
use alloc::vec::Vec;

#[cfg(feature = "allocator_api")]
unsafe impl<A> Allocator for A
where
    A: alloc::alloc::Allocator,
{
    fn allocate<T>(&self, value: T) -> NonNull<T> {
        // Workaround for https://github.com/rust-lang/rust/issues/78459
        let mut vec = Vec::with_capacity_in(1, self);
        vec.push(value);
        unsafe { NonNull::new_unchecked(vec.into_raw_parts_with_alloc().0) }
    }

    unsafe fn deallocate<T>(&self, ptr: NonNull<T>) {
        unsafe { Vec::from_raw_parts_in(ptr.as_ptr(), 1, 1, self) };
    }
}
