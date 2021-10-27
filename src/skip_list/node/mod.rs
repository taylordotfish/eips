use core::convert::TryFrom;

pub mod internal;
pub mod leaf;

pub use internal::{AllocItem, InternalNodeRef};
pub use leaf::{LeafNext, LeafRef, NoSize, OpaqueData};

pub trait NodeRef: Clone {
    type Leaf: LeafRef;
    fn next(&self) -> Option<Next<Self>>;
    fn set_next(&self, next: Option<Next<Self>>);
    fn size(&self) -> <Self::Leaf as LeafRef>::Size;
    fn as_down(&self) -> Down<Self::Leaf>;
    fn from_down(down: Down<Self::Leaf>) -> Option<Self>;
    fn key(&self) -> Option<<Self::Leaf as LeafRef>::KeyRef>;
    fn next_sibling(&self) -> Option<Self> {
        self.next().and_then(|n| n.into_next())
    }
}

#[derive(Clone)]
pub enum Next<N: NodeRef> {
    Next(N),
    Parent(InternalNodeRef<N::Leaf>),
}

impl<N: NodeRef> Next<N> {
    pub fn into_next(self) -> Option<N> {
        match self {
            Self::Next(node) => Some(node),
            _ => None,
        }
    }

    pub fn into_parent(self) -> Option<InternalNodeRef<N::Leaf>> {
        match self {
            Self::Parent(node) => Some(node),
            _ => None,
        }
    }
}

#[derive(Clone)]
pub enum Down<L: LeafRef> {
    Leaf(L),
    Internal(InternalNodeRef<L>),
}

impl<L: LeafRef> Down<L> {
    pub fn size(&self) -> L::Size {
        match self {
            Self::Leaf(node) => node.size(),
            Self::Internal(node) => node.size(),
        }
    }

    pub fn key(&self) -> Option<L::KeyRef> {
        match self {
            Self::Leaf(node) => NodeRef::key(node),
            Self::Internal(node) => node.key(),
        }
    }

    pub fn into_node<N: NodeRef<Leaf = L>>(self) -> Option<N> {
        N::from_down(self)
    }
}

impl<'a, L: LeafRef> TryFrom<&'a Down<L>> for &'a InternalNodeRef<L> {
    type Error = ();

    fn try_from(down: &'a Down<L>) -> Result<Self, ()> {
        match down {
            Down::Leaf(_) => Err(()),
            Down::Internal(node) => Ok(node),
        }
    }
}
