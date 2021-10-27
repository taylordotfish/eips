use crate::cell::{Cell, CellDefaultExt};
use crate::skip_list::{AllocSize, SkipList};
use crate::skip_list::{LeafNext, LeafRef, NoSize, OpaqueData};
use core::cmp::Ordering;
use core::marker::PhantomData;
use core::mem::ManuallyDrop;
use core::num::NonZeroU64;
use core::ops::Deref;
use core::ptr::NonNull;
use fixed_bump::Bump;
use tagged_pointer::TaggedPtr;

trait Ids: 'static {
    type Id: Sized + Ord;
    type PerClientId: Sized + Ord;
}

struct BasicIds<Id, PerClientId = Id>(PhantomData<(Id, PerClientId)>);

impl<I, P> Ids for BasicIds<I, P>
where
    I: 'static + Sized + Ord,
    P: 'static + Sized + Ord,
{
    type Id = I;
    type PerClientId = P;
}

#[repr(align(2))]
struct Align2(u16);

impl Align2 {
    pub fn sentinel() -> NonNull<Self> {
        Align4::sentinel().cast()
    }
}

#[repr(align(4))]
struct Align4(u32);

impl Align4 {
    pub fn sentinel() -> NonNull<Self> {
        static SENTINEL: Align4 = Self(0);
        NonNull::from(&SENTINEL)
    }
}

struct PosMapNext<I: Ids>(
    TaggedPtr<Align4, 2>,
    PhantomData<&'static Node<I>>,
);

impl<I: Ids> PosMapNext<I> {
    pub fn new() -> Self {
        Self(TaggedPtr::new(Align4::sentinel(), 0), PhantomData)
    }

    pub fn is_opaque_data(&self) -> bool {
        (self.0.tag() & 0b01) != 0
    }

    pub fn is_marker(&self) -> bool {
        (self.0.tag() & 0b10) != 0
    }

    pub fn set_is_marker(&mut self, value: bool) {
        let (ptr, tag) = self.0.get();
        self.0 = TaggedPtr::new(ptr, (tag & 0b01) | ((value as usize) << 1));
    }
}

impl<I: Ids> PosMapNext<I> {
    pub fn get(&self) -> Option<LeafNext<PosMapNode<I>>> {
        Some(self.0.ptr()).filter(|p| *p != Align4::sentinel()).map(|p| {
            if self.is_opaque_data() {
                LeafNext::Data(OpaqueData::new(p.cast()))
            } else {
                LeafNext::Leaf(PosMapNode::new(
                    unsafe { p.cast().as_ref() },
                    self.is_marker(),
                ))
            }
        })
    }

    pub fn set(&mut self, next: Option<LeafNext<PosMapNode<I>>>) {
        let (ptr, tag) = next.map_or_else(
            || (Align4::sentinel(), 0),
            |n| match n {
                LeafNext::Data(data) => (data.ptr.cast(), 0b01),
                LeafNext::Leaf(leaf) => (
                    NonNull::from(&*leaf).cast(),
                    (leaf.is_marker() as usize) << 1,
                ),
            },
        );
        self.0 = TaggedPtr::new(ptr, tag);
    }
}

impl<I: Ids> Clone for PosMapNext<I> {
    fn clone(&self) -> Self {
        Self(self.0, self.1)
    }
}

impl<I: Ids> Copy for PosMapNext<I> {}

impl<I: Ids> Default for PosMapNext<I> {
    fn default() -> Self {
        Self::new()
    }
}

struct SiblingSetNext<I: Ids>(
    TaggedPtr<Align2, 1>,
    PhantomData<&'static Node<I>>,
);

impl<I: Ids> SiblingSetNext<I> {
    pub fn new() -> Self {
        Self(TaggedPtr::new(Align2::sentinel(), 0), PhantomData)
    }

    pub fn is_opaque_data(&self) -> bool {
        self.0.tag() != 0
    }
}

impl<I: Ids> SiblingSetNext<I> {
    pub fn get(&self) -> Option<LeafNext<SiblingSetNode<I>>> {
        Some(self.0.ptr()).filter(|p| *p != Align2::sentinel()).map(|p| {
            if self.is_opaque_data() {
                LeafNext::Data(OpaqueData::new(p.cast()))
            } else {
                LeafNext::Leaf(SiblingSetNode(unsafe { p.cast().as_ref() }))
            }
        })
    }

    pub fn set(&mut self, next: Option<LeafNext<SiblingSetNode<I>>>) {
        let (ptr, tag) = next.map_or_else(
            || (Align2::sentinel(), 0),
            |n| match n {
                LeafNext::Data(data) => (data.ptr.cast(), 1),
                LeafNext::Leaf(leaf) => (NonNull::from(leaf.0).cast(), 0),
            },
        );
        self.0 = TaggedPtr::new(ptr, tag);
    }
}

impl<I: Ids> Clone for SiblingSetNext<I> {
    fn clone(&self) -> Self {
        Self(self.0, self.1)
    }
}

impl<I: Ids> Copy for SiblingSetNext<I> {}

impl<I: Ids> Default for SiblingSetNext<I> {
    fn default() -> Self {
        Self::new()
    }
}

struct NewLocation<I: Ids>(
    TaggedPtr<Align4, 2>,
    PhantomData<&'static Node<I>>,
);

impl<I: Ids> NewLocation<I> {
    pub fn new() -> Self {
        Self(TaggedPtr::new(Align4::sentinel(), 0), PhantomData)
    }

    pub fn get(&self) -> Option<&'static Node<I>> {
        Some(self.0.ptr())
            .filter(|p| *p != Align4::sentinel())
            .map(|p| unsafe { p.cast().as_ref() })
    }

    pub fn set(&mut self, node: Option<&'static Node<I>>) {
        self.0 = TaggedPtr::new(
            node.map_or_else(Align4::sentinel, |r| NonNull::from(r).cast()),
            self.0.tag(),
        );
    }

    pub fn is_left(&self) -> bool {
        (self.0.tag() & 0b01) != 0
    }

    pub fn set_is_left(&mut self, value: bool) {
        let (ptr, tag) = self.0.get();
        self.0 = TaggedPtr::new(ptr, (tag & 0b10) | value as usize);
    }

    pub fn is_visible(&self) -> bool {
        (self.0.tag() & 0b10) != 0
    }

    pub fn set_is_visible(&mut self, value: bool) {
        let (ptr, tag) = self.0.get();
        self.0 = TaggedPtr::new(ptr, (tag & 0b01) | ((value as usize) << 1));
    }
}

impl<I: Ids> Clone for NewLocation<I> {
    fn clone(&self) -> Self {
        Self(self.0, self.1)
    }
}

impl<I: Ids> Copy for NewLocation<I> {}

impl<I: Ids> Default for NewLocation<I> {
    fn default() -> Self {
        Self::new()
    }
}

#[repr(align(4))]
struct Node<I: Ids> {
    id: I::Id,
    parent: I::Id,
    pos_map_next: Cell<PosMapNext<I>>,
    pos_map_marker_next: Cell<PosMapNext<I>>,
    sibling_set_next: Cell<SiblingSetNext<I>>,
    move_timestamp: Cell<Option<(NonZeroU64, I::PerClientId)>>,
    new_location: Cell<NewLocation<I>>,
}

impl<I: Ids> Node<I> {
    pub fn is_visible(&self) -> bool {
        self.new_location.get().is_visible()
    }

    pub fn is_left(&self) -> bool {
        self.new_location.get().is_left()
    }
}

struct PosMapNode<I: Ids>(
    TaggedPtr<Node<I>, 1>,
    PhantomData<&'static Node<I>>,
);

impl<I: Ids> PosMapNode<I> {
    pub fn new(node: &'static Node<I>, is_marker: bool) -> Self {
        Self(
            TaggedPtr::new(NonNull::from(node), is_marker as usize),
            PhantomData,
        )
    }

    pub fn get(&self) -> &'static Node<I> {
        unsafe { self.0.ptr().as_ref() }
    }

    pub fn is_marker(&self) -> bool {
        self.0.tag() != 0
    }
}

impl<I: Ids> Clone for PosMapNode<I> {
    fn clone(&self) -> Self {
        Self(self.0, self.1)
    }
}

impl<I: Ids> Copy for PosMapNode<I> {}

impl<I: Ids> Deref for PosMapNode<I> {
    type Target = Node<I>;

    fn deref(&self) -> &Self::Target {
        self.get()
    }
}

unsafe impl<I: Ids> LeafRef for PosMapNode<I> {
    type Size = usize;
    type KeyRef = ();
    type Align = Align4;

    fn next(&self) -> Option<LeafNext<Self>> {
        if self.is_marker() {
            self.pos_map_marker_next.get().get()
        } else {
            self.pos_map_next.get().get()
        }
    }

    fn set_next(&self, next: Option<LeafNext<Self>>) {
        if self.is_marker() {
            self.pos_map_marker_next.with_mut(|n| n.set(next));
        } else {
            self.pos_map_next.with_mut(|n| n.set(next));
        }
    }

    fn key(&self) {}
    fn size(&self) -> Self::Size {
        self.is_visible() as usize
    }
}

struct SiblingSetNode<I: Ids>(pub &'static Node<I>);

impl<I: Ids> Clone for SiblingSetNode<I> {
    fn clone(&self) -> Self {
        Self(self.0)
    }
}

impl<I: Ids> Copy for SiblingSetNode<I> {}

impl<I: Ids> Deref for SiblingSetNode<I> {
    type Target = Node<I>;

    fn deref(&self) -> &Self::Target {
        self.0
    }
}

impl<I: Ids> PartialEq for SiblingSetNode<I> {
    fn eq(&self, other: &Self) -> bool {
        (&self.parent, self.is_left(), &self.id)
            == (&other.parent, other.is_left(), &other.id)
    }
}

impl<I: Ids> Eq for SiblingSetNode<I> {}

impl<I: Ids> PartialOrd for SiblingSetNode<I> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl<I: Ids> Ord for SiblingSetNode<I> {
    fn cmp(&self, other: &Self) -> Ordering {
        (&self.parent, self.is_left(), &self.id).cmp(&(
            &other.parent,
            other.is_left(),
            &other.id,
        ))
    }
}

unsafe impl<I: Ids> LeafRef for SiblingSetNode<I> {
    type Size = NoSize;
    type KeyRef = Self;
    type Align = Align2;

    fn next(&self) -> Option<LeafNext<Self>> {
        self.sibling_set_next.get().get()
    }

    fn set_next(&self, next: Option<LeafNext<Self>>) {
        self.sibling_set_next.with_mut(|n| n.set(next))
    }

    fn key(&self) -> Self::KeyRef {
        todo!()
    }
}

type PosMapAlloc<I, const N: usize> =
    Bump<[AllocSize<PosMapNode<I>>; N]>;

type SiblingSetAlloc<I, const N: usize> =
    Bump<[AllocSize<SiblingSetNode<I>>; N]>;

unsafe impl<I: Ids, const N: usize> Send for Eips<I, N> {}

struct Eips<I: Ids, const N: usize> {
    node_alloc: ManuallyDrop<Bump<[Node<I>; N]>>,
    pos_map_alloc: ManuallyDrop<PosMapAlloc<I, N>>,
    sibling_set_alloc: ManuallyDrop<SiblingSetAlloc<I, N>>,
    pos_map: SkipList<PosMapNode<I>>,
    sibling_set: SkipList<SiblingSetNode<I>>,
}
