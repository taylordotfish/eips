use super::align::{Align2, Align4};
use super::node::{Node, StaticNode, Visibility};
use super::Id;
use skip_list::{LeafNext, LeafRef, SetNextParams};
use core::fmt;
use core::marker::PhantomData;
use core::num::Wrapping;
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

pub struct PosMapNext<I: Id>(TaggedPtr<Align4, 2>, PhantomData<StaticNode<I>>);

impl<I: Id> PosMapNext<I> {
    pub fn new() -> Self {
        Self(TaggedPtr::new(Align4::sentinel(), 0), PhantomData)
    }

    fn is_opaque_data(&self) -> bool {
        (self.0.tag() & 0b10) != 0
    }

    pub fn kind(&self) -> PosMapNodeKind {
        (self.0.tag() & 0b01).into()
    }

    pub fn set_kind(&mut self, kind: PosMapNodeKind) {
        let (ptr, tag) = self.0.get();
        self.0 = TaggedPtr::new(ptr, (tag & 0b10) | kind as usize);
    }

    pub fn get(&self) -> Option<LeafNext<PosMapNode<I>>> {
        Some(self.0.ptr()).filter(|p| *p != Align4::sentinel()).map(|p| {
            if self.is_opaque_data() {
                LeafNext::Data(p.cast())
            } else {
                LeafNext::Leaf(PosMapNode::new(
                    unsafe { StaticNode::from_ptr(p.cast()) },
                    self.kind(),
                ))
            }
        })
    }

    pub fn set(&mut self, next: Option<LeafNext<PosMapNode<I>>>) {
        let (ptr, tag) = next.map_or_else(
            || (Align4::sentinel(), 0),
            |n| match n {
                LeafNext::Data(data) => (data.cast(), 0b10),
                LeafNext::Leaf(leaf) => {
                    (leaf.node().ptr().cast(), leaf.kind() as usize)
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

impl<I> fmt::Debug for PosMapNext<I>
where
    I: Id + fmt::Debug,
{
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        fmt.debug_struct("PosMapNext")
            .field("kind", &self.kind())
            .field("opaque", &self.is_opaque_data())
            .field(
                "ptr",
                &Some(self.0.ptr()).filter(|p| *p != Align4::sentinel()),
            )
            .finish()
    }
}

pub struct PosMapNode<I: Id>(
    TaggedPtr<Node<I>, 1>,
    PhantomData<StaticNode<I>>,
);

impl<I: Id> PosMapNode<I> {
    pub fn new(node: StaticNode<I>, kind: PosMapNodeKind) -> Self {
        Self(TaggedPtr::new(node.ptr(), kind as usize), PhantomData)
    }

    pub fn kind(&self) -> PosMapNodeKind {
        self.0.tag().into()
    }

    pub fn node(&self) -> StaticNode<I> {
        unsafe { StaticNode::from_ptr(self.0.ptr()) }
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
    type StoreKeys = ();
    type Align = Align2;
    const FANOUT: usize = I::FANOUT;

    fn next(&self) -> Option<LeafNext<Self>> {
        self.node().pos_map_next[self.kind() as usize].get().get()
    }

    fn set_next(params: SetNextParams<'_, Self>) {
        let (this, next) = params.get();
        this.node().pos_map_next[this.kind() as usize]
            .with_mut(|n| n.set(next));
    }

    fn size(&self) -> Self::Size {
        Wrapping(
            (self.kind() == PosMapNodeKind::Normal
                && self.node().visibility() == Visibility::Visible)
                as usize,
        )
    }
}

impl<I> fmt::Debug for PosMapNode<I>
where
    I: Id + fmt::Debug,
{
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt.debug_struct("PosMapNode")
            .field("kind", &self.kind())
            .field("node", &self.node())
            .finish()
    }
}
