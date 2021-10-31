use super::align::{Align2, Align4};
use super::node::{Node, Visibility};
use super::Id;
use crate::cell::CellDefaultExt;
use crate::skip_list::{LeafNext, LeafRef, OpaqueData};
use core::marker::PhantomData;
use core::num::Wrapping;
use core::ptr::NonNull;
use tagged_pointer::TaggedPtr;

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum PosMapNodeKind {
    Normal = 0,
    Marker = 1,
}

impl From<usize> for PosMapNodeKind {
    fn from(n: usize) -> Self {
        match n {
            0 => Self::Normal,
            1 => Self::Marker,
            n => panic!("bad enum value: {}", n),
        }
    }
}

pub struct PosMapNext<I: Id>(
    TaggedPtr<Align4, 2>,
    PhantomData<&'static Node<I>>,
);

impl<I: Id> PosMapNext<I> {
    pub fn new() -> Self {
        Self(TaggedPtr::new(Align4::sentinel(), 0), PhantomData)
    }

    pub fn is_opaque_data(&self) -> bool {
        (self.0.tag() & 0b10) != 0
    }

    pub fn kind(&self) -> PosMapNodeKind {
        (self.0.tag() & 0b01).into()
    }

    pub fn set_kind(&mut self, kind: PosMapNodeKind) {
        let (ptr, tag) = self.0.get();
        self.0 = TaggedPtr::new(ptr, (tag & 0b10) | kind as usize);
    }
}

impl<I: Id> PosMapNext<I> {
    pub fn get(&self) -> Option<LeafNext<PosMapNode<I>>> {
        Some(self.0.ptr()).filter(|p| *p != Align4::sentinel()).map(|p| {
            if self.is_opaque_data() {
                LeafNext::Data(OpaqueData::new(p.cast()))
            } else {
                LeafNext::Leaf(PosMapNode::new(
                    unsafe { p.cast().as_ref() },
                    self.kind(),
                ))
            }
        })
    }

    pub fn set(&mut self, next: Option<LeafNext<PosMapNode<I>>>) {
        let (ptr, tag) = next.map_or_else(
            || (Align4::sentinel(), 0),
            |n| match n {
                LeafNext::Data(data) => (data.ptr.cast(), 0b10),
                LeafNext::Leaf(leaf) => {
                    (NonNull::from(leaf.node()).cast(), leaf.kind() as usize)
                }
            },
        );
        self.0 = TaggedPtr::new(ptr, tag);
    }
}

impl<I: Id> Clone for PosMapNext<I> {
    fn clone(&self) -> Self {
        Self(self.0, self.1)
    }
}

impl<I: Id> Copy for PosMapNext<I> {}

impl<I: Id> Default for PosMapNext<I> {
    fn default() -> Self {
        Self::new()
    }
}

pub struct PosMapNode<I: Id>(
    TaggedPtr<Node<I>, 1>,
    PhantomData<&'static Node<I>>,
);

impl<I: Id> PosMapNode<I> {
    pub fn new(node: &'static Node<I>, kind: PosMapNodeKind) -> Self {
        Self(TaggedPtr::new(NonNull::from(node), kind as usize), PhantomData)
    }

    pub fn get(&self) -> &'static Node<I> {
        unsafe { self.0.ptr().as_ref() }
    }

    pub fn kind(&self) -> PosMapNodeKind {
        self.0.tag().into()
    }

    pub fn node(&self) -> &'static Node<I> {
        unsafe { self.0.ptr().as_ref() }
    }
}

impl<I: Id> Clone for PosMapNode<I> {
    fn clone(&self) -> Self {
        Self(self.0, self.1)
    }
}

impl<I: Id> Copy for PosMapNode<I> {}

unsafe impl<I: Id> LeafRef for PosMapNode<I> {
    type Size = Wrapping<usize>;
    type KeyRef = ();
    type Align = Align2;

    fn next(&self) -> Option<LeafNext<Self>> {
        self.node().pos_map_next[self.kind() as usize].get().get()
    }

    fn set_next(&self, next: Option<LeafNext<Self>>) {
        self.node().pos_map_next[self.kind() as usize]
            .with_mut(|n| n.set(next));
    }

    fn key(&self) {}

    fn size(&self) -> Self::Size {
        Wrapping(
            (self.kind() == PosMapNodeKind::Normal
                && self.node().visibility() == Visibility::Visible)
                as usize,
        )
    }
}
