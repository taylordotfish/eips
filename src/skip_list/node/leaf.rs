use super::{Down, InternalNodeRef, Next, NodeRef};
use core::ops::{AddAssign, SubAssign};
use core::ptr::NonNull;

#[derive(Clone, Default, PartialEq, Eq, PartialOrd, Ord)]
pub struct NoSize;

impl AddAssign for NoSize {
    fn add_assign(&mut self, _rhs: Self) {}
}

impl SubAssign for NoSize {
    fn sub_assign(&mut self, _rhs: Self) {}
}

/// # Safety
///
/// * `Self` must not be [`Send`] or [`Sync`].
///
/// * [`Self::next`] must initially return [`None`] and must not change until
///   [`Self::set_next`] is called.
///
/// * After [`Self::set_next`] is called (with `n` as the value of the `next`
///   parameter), [`Self::next`] must return an object identical to `n` (until
///   the next call to [`Self::set_next`]).
///
/// * Clones produced through [`Clone::clone`] must behave identically to the
///   original object. In particular, if an operation is performed on an object
///   `s` of type `Self`, all clones of `s` (transitively and symmetrically)
///   must behave as if that same operation were performed on them.
pub unsafe trait LeafRef: Clone {
    const FANOUT: usize = 8;
    type Size: Clone + Default + Ord + AddAssign + SubAssign;
    type KeyRef: Clone;
    type Align;

    fn next(&self) -> Option<LeafNext<Self>>;
    fn set_next(&self, next: Option<LeafNext<Self>>);
    fn key(&self) -> Self::KeyRef;
    fn size(&self) -> Self::Size {
        Self::Size::default()
    }
}

pub enum LeafNext<L: LeafRef> {
    Leaf(L),
    Data(OpaqueData<L>),
}

pub struct OpaqueData<L: LeafRef> {
    _align: [L::Align; 0],
    pub ptr: NonNull<u8>,
}

impl<L: LeafRef> OpaqueData<L> {
    pub fn new(ptr: NonNull<u8>) -> Self {
        Self {
            _align: [],
            ptr,
        }
    }
}

impl<L: LeafRef> NodeRef for L {
    type Leaf = L;

    fn next(&self) -> Option<Next<Self>> {
        LeafRef::next(self).map(|next| match next {
            LeafNext::Leaf(node) => Next::Next(node),
            LeafNext::Data(data) => {
                Next::Parent(unsafe { InternalNodeRef::from_ptr(data.ptr) })
            }
        })
    }

    fn set_next(&self, next: Option<Next<Self>>) {
        LeafRef::set_next(
            self,
            next.map(|next| match next {
                Next::Next(node) => LeafNext::Leaf(node),
                Next::Parent(node) => {
                    LeafNext::Data(OpaqueData::new(node.as_ptr()))
                }
            }),
        );
    }

    fn size(&self) -> L::Size {
        LeafRef::size(self)
    }

    fn as_down(&self) -> Down<L> {
        Down::Leaf(self.clone())
    }

    fn from_down(down: Down<Self::Leaf>) -> Option<Self> {
        match down {
            Down::Leaf(node) => Some(node),
            _ => None,
        }
    }

    fn key(&self) -> Option<L::KeyRef> {
        Some(self.key())
    }
}
