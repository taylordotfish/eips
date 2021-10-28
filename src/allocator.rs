use ::alloc::alloc::{self, Layout};
use core::ptr::{self, NonNull};
#[cfg(not(feature = "allocator_api"))]
use fixed_bump::Bump;

/// # Safety
///
/// Same requirements as [`alloc::Allocator`].
pub unsafe trait Allocator {
    fn allocate(&self, layout: Layout) -> Option<NonNull<[u8]>>;

    /// # Safety
    ///
    /// Same requirements as [`alloc::Allocator::deallocate`].
    unsafe fn deallocate(&self, ptr: NonNull<u8>, layout: Layout);
}

pub(crate) fn alloc_value<T, A>(value: T, alloc: &A) -> NonNull<T>
where
    A: Allocator,
{
    let ptr = alloc
        .allocate(Layout::new::<T>())
        .expect("memory allocation failed")
        .cast();
    // SAFETY: `ptr` is guaranteed to point to valid memory, so we can
    // initialize it with `value`.
    unsafe { ptr::write(ptr.as_ptr(), value) };
    ptr
}

/// # Safety
///
/// `ptr` must have been allocated by [`alloc_value::<T>`][alloc_value] with
/// the same allocator as `alloc`, and it must not have been deallocated
/// already.
pub(crate) unsafe fn dealloc_value<T, A>(ptr: NonNull<T>, alloc: &A)
where
    A: Allocator,
{
    // SAFETY: Ensured by caller.
    unsafe { alloc.deallocate(ptr.cast(), Layout::new::<T>()) };
}

#[cfg(feature = "allocator_api")]
// SAFETY: This trait has the same requirements as `alloc::Allocator`.
unsafe impl<A> Allocator for A
where
    A: alloc::Allocator,
{
    fn allocate(&self, layout: Layout) -> Option<NonNull<[u8]>> {
        self.allocate(layout).ok()
    }

    unsafe fn deallocate(&self, ptr: NonNull<u8>, layout: Layout) {
        // SAFETY: Ensured by caller.
        unsafe { self.deallocate(ptr, layout) };
    }
}

#[derive(Clone, Copy, Default)]
pub struct Global;

// SAFETY: The `alloc` and `dealloc` functions in the standard library behave
// as required. Clones of this allocator will necessarily behave the same, as
// they forward to the global allocator.
unsafe impl Allocator for Global {
    fn allocate(&self, layout: Layout) -> Option<NonNull<[u8]>> {
        assert!(layout.size() != 0);
        NonNull::new(ptr::slice_from_raw_parts_mut(
            // SAFETY: We ensured that the size of the layout is not 0.
            unsafe { alloc::alloc(layout) },
            layout.size(),
        ))
    }

    unsafe fn deallocate(&self, ptr: NonNull<u8>, layout: Layout) {
        // SAFETY: Ensured by caller.
        unsafe { alloc::dealloc(ptr.as_ptr(), layout) }
    }
}

#[cfg(not(feature = "allocator_api"))]
// SAFETY: `Bump` behaves as required and is designed to be used as an
// allocator, as it implements `alloc::Allocator` when available.
unsafe impl<Size, Align> Allocator for Bump<Size, Align> {
    fn allocate(&self, layout: Layout) -> Option<NonNull<[u8]>> {
        self.allocate(layout)
    }

    unsafe fn deallocate(&self, _ptr: NonNull<u8>, _layout: Layout) {
        // Bump allocators don't free memory until they're dropped.
    }
}
