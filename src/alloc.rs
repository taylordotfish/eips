use super::node::Node;
use super::pos_map::PosMapNode;
use super::sibling_set::SiblingSetNode;
use super::Id;
use skip_list::AllocItem;

#[allow(unused_imports)]
use core::marker::PhantomData;
#[cfg(feature = "fixed-bump")]
use fixed_bump::Bump;

#[cfg(feature = "allocator_api")]
pub use core_alloc::alloc::{Allocator, Global};

#[cfg(not(feature = "allocator_api"))]
pub use allocator_fallback::{Allocator, Global};

#[repr(transparent)]
pub struct AllocItem1<I: Id>(Node<I>);
#[repr(transparent)]
pub struct AllocItem2<I: Id>(AllocItem<PosMapNode<I>>);
#[repr(transparent)]
pub struct AllocItem3<I: Id>(AllocItem<SiblingSetNode<I>>);

pub trait Allocators {
    type Alloc1: Allocator;
    type Alloc2: Allocator;
    type Alloc3: Allocator;
    fn into_allocators(self) -> (Self::Alloc1, Self::Alloc2, Self::Alloc3);
}

#[derive(Clone, Copy, Default)]
pub struct GlobalAllocators;

impl Allocators for GlobalAllocators {
    type Alloc1 = Global;
    type Alloc2 = Global;
    type Alloc3 = Global;

    fn into_allocators(self) -> (Global, Global, Global) {
        (Global, Global, Global)
    }
}

#[cfg(feature = "fixed-bump")]
#[derive(Clone, Copy, Default)]
pub struct BumpAllocators<I, const N: usize>(PhantomData<fn() -> I>);

#[cfg(feature = "fixed-bump")]
impl<I, const N: usize> BumpAllocators<I, N> {
    pub fn new() -> Self {
        Self(PhantomData)
    }
}

#[cfg(feature = "fixed-bump")]
impl<I: Id, const N: usize> Allocators for BumpAllocators<I, N> {
    type Alloc1 = Bump<[AllocItem1<I>; N]>;
    type Alloc2 = Bump<[AllocItem2<I>; N]>;
    type Alloc3 = Bump<[AllocItem3<I>; N]>;

    fn into_allocators(self) -> (Self::Alloc1, Self::Alloc2, Self::Alloc3) {
        (Bump::new(), Bump::new(), Bump::new())
    }
}
