use super::min_node_length;
use super::node::{InternalNodeRef, LeafRef, Next, NodeRef};
use crate::allocator::Allocator;
use core::iter::FusedIterator;

pub struct Split<N: NodeRef> {
    start: N,
    node: Option<N>,
    chunk_len: usize,
    extra: usize,
}

pub struct InternalNodeSetup<N: NodeRef> {
    start: N,
    end: N,
    len: usize,
    size: <N::Leaf as LeafRef>::Size,
}

impl<N: NodeRef> InternalNodeSetup<N> {
    pub fn apply(self, node: InternalNodeRef<N::Leaf>) {
        node.len.set(self.len);
        node.size.set(self.size);
        node.set_down(Some(self.start.as_down()));
        node.key.set(self.start.key());
        self.end.set_next(Some(Next::Parent(node)));
    }

    pub fn into_new<A>(self, alloc: &A) -> InternalNodeRef<N::Leaf>
    where
        A: Allocator,
    {
        let node = InternalNodeRef::alloc(alloc);
        self.apply(node);
        node
    }
}

impl<N: NodeRef> Iterator for Split<N> {
    type Item = InternalNodeSetup<N>;

    fn next(&mut self) -> Option<Self::Item> {
        let start = self.start.clone();
        let len = self.chunk_len + (self.extra > 0) as usize;
        self.extra = self.extra.saturating_sub(1);

        let mut node = self.node.take()?;
        let mut size = node.size();
        for _ in 1..len {
            node = match node.next() {
                Some(Next::Next(next)) => next,
                _ => panic!("expected next node"),
            };
            size += node.size();
        }

        self.node = match node.next() {
            Some(Next::Next(next)) => {
                self.start = next.clone();
                Some(next)
            }
            _ => None,
        };

        Some(InternalNodeSetup {
            start,
            end: node,
            len,
            size,
        })
    }
}

impl<N: NodeRef> FusedIterator for Split<N> {}

pub fn split<N: NodeRef>(node: N, len: usize) -> Split<N> {
    let num_chunks = (len / min_node_length::<N::Leaf>()).max(1);
    Split {
        start: node.clone(),
        node: Some(node),
        chunk_len: len / num_chunks,
        extra: len % num_chunks,
    }
}
