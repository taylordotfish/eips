use super::align::{Align2, Align4};
use super::node::{Direction, Node};
use super::Id;
use crate::cell::CellDefaultExt;
use crate::skip_list::{LeafNext, LeafRef, NoSize, OpaqueData};
use core::cmp::Ordering;
use core::marker::PhantomData;
use core::ptr::NonNull;
use tagged_pointer::TaggedPtr;

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum SiblingSetNodeKind {
    Normal = 0,
    Childless = 1,
}

impl From<usize> for SiblingSetNodeKind {
    fn from(n: usize) -> Self {
        match n {
            0 => Self::Normal,
            1 => Self::Childless,
            n => panic!("bad enum value: {}", n),
        }
    }
}

pub struct SiblingSetNext<I: Id>(
    TaggedPtr<Align4, 2>,
    PhantomData<&'static Node<I>>,
);

impl<I: Id> SiblingSetNext<I> {
    pub fn new() -> Self {
        Self(TaggedPtr::new(Align4::sentinel(), 0), PhantomData)
    }

    pub fn is_opaque_data(&self) -> bool {
        (self.0.tag() & 0b10) != 0
    }

    pub fn kind(&self) -> SiblingSetNodeKind {
        (self.0.tag() & 0b01).into()
    }

    pub fn set_kind(&mut self, kind: SiblingSetNodeKind) {
        let (ptr, tag) = self.0.get();
        self.0 = TaggedPtr::new(ptr, (tag & 0b10) | kind as usize);
    }
}

impl<I: Id> SiblingSetNext<I> {
    pub fn get(&self) -> Option<LeafNext<SiblingSetNode<I>>> {
        Some(self.0.ptr()).filter(|p| *p != Align4::sentinel()).map(|p| {
            if self.is_opaque_data() {
                LeafNext::Data(OpaqueData::new(p.cast()))
            } else {
                LeafNext::Leaf(SiblingSetNode::new(
                    unsafe { p.cast().as_ref() },
                    self.kind(),
                ))
            }
        })
    }

    pub fn set(&mut self, next: Option<LeafNext<SiblingSetNode<I>>>) {
        let (ptr, tag) = next.map_or_else(
            || (Align4::sentinel(), 0),
            |n| match n {
                LeafNext::Data(data) => (data.ptr.cast(), 1),
                LeafNext::Leaf(leaf) => {
                    (NonNull::from(leaf.get()).cast(), leaf.kind() as usize)
                }
            },
        );
        self.0 = TaggedPtr::new(ptr, tag);
    }
}

impl<I: Id> Clone for SiblingSetNext<I> {
    fn clone(&self) -> Self {
        Self(self.0, self.1)
    }
}

impl<I: Id> Copy for SiblingSetNext<I> {}

impl<I: Id> Default for SiblingSetNext<I> {
    fn default() -> Self {
        Self::new()
    }
}

pub struct SiblingSetNode<I: Id>(
    TaggedPtr<Node<I>, 1>,
    PhantomData<&'static Node<I>>,
);

impl<I: Id> SiblingSetNode<I> {
    pub fn new(node: &'static Node<I>, kind: SiblingSetNodeKind) -> Self {
        Self(TaggedPtr::new(NonNull::from(node), kind as usize), PhantomData)
    }

    pub fn get(&self) -> &'static Node<I> {
        unsafe { self.0.ptr().as_ref() }
    }

    pub fn kind(&self) -> SiblingSetNodeKind {
        self.0.tag().into()
    }

    pub fn node(&self) -> &'static Node<I> {
        unsafe { self.0.ptr().as_ref() }
    }

    pub fn parent_id(&self) -> Option<I> {
        match self.kind() {
            SiblingSetNodeKind::Normal => self.node().parent.clone(),
            SiblingSetNodeKind::Childless => Some(self.node().id.clone()),
        }
    }

    pub fn child_id(&self) -> Option<I> {
        match self.kind() {
            SiblingSetNodeKind::Normal => Some(self.node().id.clone()),
            SiblingSetNodeKind::Childless => None,
        }
    }

    pub fn direction(&self) -> Option<Direction> {
        match self.kind() {
            SiblingSetNodeKind::Normal => Some(self.node().direction()),
            SiblingSetNodeKind::Childless => None,
        }
    }
}

impl<I: Id> Clone for SiblingSetNode<I> {
    fn clone(&self) -> Self {
        Self(self.0, self.1)
    }
}

impl<I: Id> Copy for SiblingSetNode<I> {}

unsafe impl<I: Id> LeafRef for SiblingSetNode<I> {
    type Size = NoSize;
    type KeyRef = Self;
    type Align = Align2;

    fn next(&self) -> Option<LeafNext<Self>> {
        self.node().sibling_set_next[self.kind() as usize].get().get()
    }

    fn set_next(&self, next: Option<LeafNext<Self>>) {
        self.node().sibling_set_next[self.kind() as usize]
            .with_mut(|n| n.set(next))
    }

    fn key(&self) -> Self::KeyRef {
        *self
    }
}

pub struct FindChildless<I: Id>(pub I);

impl<I: Id> PartialEq<SiblingSetNode<I>> for FindChildless<I> {
    fn eq(&self, other: &SiblingSetNode<I>) -> bool {
        matches!(self.partial_cmp(other), Some(Ordering::Equal))
    }
}

impl<I: Id> PartialOrd<SiblingSetNode<I>> for FindChildless<I> {
    fn partial_cmp(&self, other: &SiblingSetNode<I>) -> Option<Ordering> {
        match Some(self.0.clone()).cmp(&other.parent_id()) {
            Ordering::Equal => {}
            ordering => return Some(ordering),
        }
        Some(other.child_id().map_or(Ordering::Equal, |_| Ordering::Greater))
    }
}
