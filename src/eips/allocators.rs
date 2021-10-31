use super::node::Node;
use super::pos_map::PosMapNode;
use super::sibling_set::SiblingSetNode;
use super::Id;
use crate::allocator::{Allocator, Global};
use crate::skip_list::AllocItem;
use core::marker::PhantomData;
use fixed_bump::Bump;

#[repr(transparent)]
pub struct AllocItem0<I: Id>(Node<I>);
#[repr(transparent)]
pub struct AllocItem1<I: Id>(AllocItem<PosMapNode<I>>);
#[repr(transparent)]
pub struct AllocItem2<I: Id>(AllocItem<SiblingSetNode<I>>);

pub trait Allocators {
    type Alloc0: Allocator;
    type Alloc1: Allocator;
    type Alloc2: Allocator;
    fn into_allocators(self) -> (Self::Alloc0, Self::Alloc1, Self::Alloc2);
}

#[derive(Clone, Copy, Default)]
pub struct GlobalAllocators;

impl Allocators for GlobalAllocators {
    type Alloc0 = Global;
    type Alloc1 = Global;
    type Alloc2 = Global;

    fn into_allocators(self) -> (Global, Global, Global) {
        (Global, Global, Global)
    }
}

pub struct BumpAllocators<I, const N: usize>(PhantomData<fn() -> I>);

impl<I, const N: usize> BumpAllocators<I, N> {
    pub fn new() -> Self {
        Self(PhantomData)
    }
}

impl<I, const N: usize> Default for BumpAllocators<I, N> {
    fn default() -> Self {
        Self::new()
    }
}

impl<I: Id, const N: usize> Allocators for BumpAllocators<I, N> {
    type Alloc0 = Bump<[AllocItem0<I>; N]>;
    type Alloc1 = Bump<[AllocItem1<I>; N]>;
    type Alloc2 = Bump<[AllocItem2<I>; N]>;

    fn into_allocators(self) -> (Self::Alloc0, Self::Alloc1, Self::Alloc2) {
        (Bump::new(), Bump::new(), Bump::new())
    }
}
