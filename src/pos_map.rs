use super::align::{Align2, Align4};
use super::node::{Node, StaticNode, Visibility};
use super::Id;
use core::fmt;
use core::marker::PhantomData;
use skip_list::{LeafNext, LeafRef, SetNextParams};
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

#[derive(Clone, Debug)]
pub struct PosMapNext<I: Id>(TaggedPtr<Align4, 2>, PhantomData<StaticNode<I>>);

impl<I: Id> PosMapNext<I> {
    pub fn new(next: LeafNext<PosMapNode<I>>) -> Self {
        let (ptr, tag) = match next {
            LeafNext::Data(data) => (data.cast(), 0b10),
            LeafNext::Leaf(leaf) => {
                (leaf.node().ptr().cast(), leaf.kind() as usize)
            }
        };
        Self(TaggedPtr::new(ptr, tag), PhantomData)
    }

    pub fn get(&self) -> LeafNext<PosMapNode<I>> {
        let (ptr, tag) = self.0.get();
        if (tag & 0b10) != 0 {
            LeafNext::Data(ptr.cast())
        } else {
            LeafNext::Leaf(PosMapNode::new(
                unsafe { StaticNode::from_ptr(ptr.cast()) },
                (tag & 0b01).into(),
            ))
        }
    }
}

impl<I: Id> Copy for PosMapNext<I> {}

#[derive(Clone)]
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

impl<I: Id> Copy for PosMapNode<I> {}

// Required by `Node::pos_map_next` for safety
pub struct Token(());

unsafe impl<I: Id> LeafRef for PosMapNode<I> {
    type Size = usize;
    type StoreKeys = ();
    type Align = Align2;
    const FANOUT: usize = I::FANOUT;

    fn next(&self) -> Option<LeafNext<Self>> {
        self.node().pos_map_next(Token(()))[self.kind() as usize]
            .get()
            .map(|n| n.get())
    }

    fn set_next(params: SetNextParams<'_, Self>) {
        let (this, next) = params.get();
        this.node().pos_map_next(Token(()))[this.kind() as usize]
            .set(next.map(PosMapNext::new));
    }

    fn size(&self) -> Self::Size {
        (self.kind() == PosMapNodeKind::Normal
            && self.node().visibility() == Visibility::Visible)
            as usize
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
