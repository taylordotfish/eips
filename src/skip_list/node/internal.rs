use super::{Down, LeafRef, Next, NodeRef};
use crate::allocator::{alloc_value, dealloc_value, Allocator};
use crate::cell::{Cell, CellDefaultExt};
use core::marker::PhantomData;
use core::mem::{ManuallyDrop, MaybeUninit};
use core::ops::Deref;
use core::ptr::NonNull;
use tagged_pointer::TaggedPtr;

union DownUnion<L: LeafRef> {
    leaf: ManuallyDrop<L>,
    internal: Option<InternalNodeRef<L>>,
}

impl<L: LeafRef> Default for DownUnion<L> {
    fn default() -> Self {
        Self {
            internal: None,
        }
    }
}

#[repr(transparent)]
pub struct AllocItem<L: LeafRef>(MaybeUninit<InternalNode<L>>);

#[repr(align(4))]
pub struct InternalNode<L: LeafRef> {
    _align: [L::Align; 0],
    next: Cell<InternalNext<L>>,
    down: Cell<DownUnion<L>>,
    pub size: Cell<L::Size>,
    pub len: Cell<usize>,
    pub key: Cell<Option<L::KeyRef>>,
}

impl<L: LeafRef> Default for InternalNode<L> {
    fn default() -> Self {
        Self {
            _align: [],
            next: Cell::default(),
            down: Cell::default(),
            size: Cell::default(),
            len: Cell::default(),
            key: Cell::default(),
        }
    }
}

impl<L: LeafRef> InternalNode<L> {
    pub fn next(&self) -> Option<Next<InternalNodeRef<L>>> {
        let next = self.next.get();
        next.get().map(|n| {
            if next.next_is_parent() {
                Next::Parent(n)
            } else {
                Next::Next(n)
            }
        })
    }

    pub fn set_next(&self, next: Option<Next<InternalNodeRef<L>>>) {
        self.next.with_mut(|sn| {
            sn.set_next_is_parent(matches!(next, Some(Next::Parent(_))));
            sn.set(next.map(|n| match n {
                Next::Next(n) => n,
                Next::Parent(n) => n,
            }));
        });
    }

    fn drop_down(&self) {
        let is_leaf = self.next.get().down_is_leaf();
        self.next.with_mut(|n| n.set_down_is_leaf(false));
        if is_leaf {
            // SAFETY: Safe due to this type's invariants (`down` and
            // `down_is_leaf` are always in sync).
            ManuallyDrop::into_inner(unsafe { self.down.take().leaf });
        }
    }

    pub fn down(&self) -> Option<Down<L>> {
        let next = self.next.take();
        let down = self.down.take();
        let result = if next.down_is_leaf() {
            // SAFETY: Safe due to this type's invariants (`down` and
            // `down_is_leaf` are always in sync).
            Some(Down::Leaf(L::clone(unsafe { &down.leaf })))
        } else {
            // SAFETY: Safe due to this type's invariants (`down` and
            // `down_is_leaf` are always in sync).
            unsafe { down.internal }.map(Down::Internal)
        };
        self.down.set(down);
        self.next.set(next);
        result
    }

    pub fn down_as<N: NodeRef<Leaf = L>>(&self) -> Option<N> {
        self.down().and_then(|d| d.into_node())
    }

    pub fn set_down(&self, down: Option<Down<L>>) {
        self.drop_down();
        self.next.with_mut(|n| {
            n.set_down_is_leaf(matches!(down, Some(Down::Leaf(_))));
        });
        self.down.set(match down {
            Some(Down::Leaf(node)) => DownUnion {
                leaf: ManuallyDrop::new(node),
            },
            Some(Down::Internal(node)) => DownUnion {
                internal: Some(node),
            },
            None => DownUnion {
                internal: None,
            },
        });
    }

    pub fn size(&self) -> L::Size {
        self.size.get()
    }
}

#[repr(align(4))]
struct Align4(u32);

struct InternalNext<L: LeafRef>(
    TaggedPtr<Align4, 2>,
    PhantomData<NonNull<InternalNode<L>>>,
);

impl<L: LeafRef> Default for InternalNext<L> {
    fn default() -> Self {
        Self(TaggedPtr::new(Self::sentinel(), 0), PhantomData)
    }
}

impl<L: LeafRef> Clone for InternalNext<L> {
    fn clone(&self) -> Self {
        Self(self.0, self.1)
    }
}

impl<L: LeafRef> Copy for InternalNext<L> {}

impl<L: LeafRef> InternalNext<L> {
    fn sentinel() -> NonNull<Align4> {
        static SENTINEL: Align4 = Align4(0);
        NonNull::from(&SENTINEL)
    }

    pub fn get(&self) -> Option<InternalNodeRef<L>> {
        let ptr = self.0.ptr();
        if ptr == Self::sentinel() {
            None
        } else {
            Some(InternalNodeRef(ptr.cast()))
        }
    }

    pub fn set(&mut self, node: Option<InternalNodeRef<L>>) {
        self.0 = TaggedPtr::new(
            node.map_or_else(Self::sentinel, |n| n.0.cast()),
            self.0.tag(),
        );
    }

    pub fn down_is_leaf(&self) -> bool {
        self.0.tag() & 0b1 != 0
    }

    pub fn next_is_parent(&self) -> bool {
        self.0.tag() & 0b10 != 0
    }

    pub fn set_down_is_leaf(&mut self, value: bool) {
        let (ptr, tag) = self.0.get();
        self.0 = TaggedPtr::new(ptr, (tag & !0b1) | (value as usize));
    }

    pub fn set_next_is_parent(&mut self, value: bool) {
        let (ptr, tag) = self.0.get();
        self.0 = TaggedPtr::new(ptr, (tag & !0b10) | ((value as usize) << 1));
    }
}

pub struct InternalNodeRef<L: LeafRef>(NonNull<InternalNode<L>>);

impl<L: LeafRef> InternalNodeRef<L> {
    pub fn alloc<A: Allocator>(alloc: &A) -> Self {
        Self(alloc_value(InternalNode::default(), alloc))
    }

    /// # Safety
    ///
    /// This node must have been allocator by `alloc`.
    pub unsafe fn dealloc<A: Allocator>(self, alloc: &A) {
        // SAFETY: Checked by caller.
        unsafe { dealloc_value(self.0, alloc) };
    }

    /// # Safety
    ///
    /// `ptr` must have come from a previous call to [`Self::as_ptr`].
    pub unsafe fn from_ptr(ptr: NonNull<u8>) -> Self {
        Self(ptr.cast())
    }

    pub fn as_ptr(&self) -> NonNull<u8> {
        NonNull::<InternalNode<L>>::from(&**self).cast()
    }
}

impl<L: LeafRef> NodeRef for InternalNodeRef<L> {
    type Leaf = L;

    fn next(&self) -> Option<Next<Self>> {
        (**self).next()
    }

    fn set_next(&self, next: Option<Next<Self>>) {
        (**self).set_next(next);
    }

    fn size(&self) -> L::Size {
        (**self).size()
    }

    fn as_down(&self) -> Down<L> {
        Down::Internal(*self)
    }

    fn from_down(down: Down<Self::Leaf>) -> Option<Self> {
        match down {
            Down::Internal(node) => Some(node),
            _ => None,
        }
    }

    fn key(&self) -> Option<L::KeyRef> {
        self.key.get()
    }
}

impl<L: LeafRef> Deref for InternalNodeRef<L> {
    type Target = InternalNode<L>;

    fn deref(&self) -> &Self::Target {
        // SAFETY: Guaranteed by this type's invariants -- this type
        // conceptually represents a static reference.
        unsafe { self.0.as_ref() }
    }
}

impl<L: LeafRef> Clone for InternalNodeRef<L> {
    fn clone(&self) -> Self {
        Self(self.0)
    }
}

impl<L: LeafRef> Copy for InternalNodeRef<L> {}

impl<L: LeafRef> PartialEq for InternalNodeRef<L> {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

impl<L: LeafRef> Eq for InternalNodeRef<L> {}

impl<L: LeafRef> Drop for InternalNode<L> {
    fn drop(&mut self) {
        self.drop_down();
    }
}
