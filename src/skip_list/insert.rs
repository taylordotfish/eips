use super::max_node_length;
use super::node::{Down, InternalNodeRef, LeafRef, Next, NodeRef};
use super::split::split;
use super::traverse::get_parent;
use crate::allocator::Allocator;
use crate::cell::CellDefaultExt;

struct Insertion<N: NodeRef> {
    pub count: usize,
    pub child: N,
    pub diff: <N::Leaf as LeafRef>::Size,
    pub root: Option<Down<N::Leaf>>,
}

enum InsertionResult<L: LeafRef> {
    Insertion(Insertion<InternalNodeRef<L>>),
    Done(FinishedInsertion<L>),
}

fn handle_insertion<N, A>(
    mut insertion: Insertion<N>,
    alloc: &A,
) -> InsertionResult<N::Leaf>
where
    N: NodeRef,
    A: Allocator,
{
    let child = insertion.child;
    let mut parent = if let Some(parent) = get_parent(child.clone()) {
        parent
    } else {
        let root = insertion.root.get_or_insert_with(|| child.as_down());
        if insertion.count == 0 {
            return InsertionResult::Done(FinishedInsertion {
                old_root: root.clone(),
                new_root: child.as_down(),
            });
        }
        let root = InternalNodeRef::alloc(alloc);
        root.set_down(Some(child.as_down()));
        root
    };

    let new_len = parent.len.get() + insertion.count;
    let use_fast_insertion =
        new_len < max_node_length::<N::Leaf>() && insertion.root.is_none();

    let count = if use_fast_insertion {
        let diff = insertion.diff.clone();
        parent.len.set(new_len);
        parent.size.with_mut(|s| *s += diff);
        0
    } else {
        let first: N = parent.down_as().unwrap();
        let mut iter = split(first, new_len);
        let end = parent.next();
        iter.next().unwrap().apply(parent);
        let count = iter
            .map(|setup| {
                let node = setup.into_new(alloc);
                parent.set_next(Some(Next::Next(node)));
                parent = node;
            })
            .count();
        parent.set_next(end);
        count
    };

    InsertionResult::Insertion(Insertion {
        count,
        child: parent,
        diff: insertion.diff,
        root: insertion.root,
    })
}

pub struct FinishedInsertion<L: LeafRef> {
    pub old_root: Down<L>,
    pub new_root: Down<L>,
}

pub fn insert_after<L, I, A>(
    mut pos: L,
    items: I,
    alloc: &A,
) -> FinishedInsertion<L>
where
    L: LeafRef,
    I: Iterator<Item = L>,
    A: Allocator,
{
    let end = pos.next();
    let mut size = L::Size::default();
    let count = items
        .map(|item| {
            size += item.size();
            NodeRef::set_next(&pos, Some(Next::Next(item.clone())));
            pos = item;
        })
        .count();
    pos.set_next(end);
    let insertion = Insertion {
        count,
        child: pos,
        diff: size,
        root: None,
    };
    let mut result = handle_insertion(insertion, alloc);
    loop {
        match result {
            InsertionResult::Done(done) => return done,
            InsertionResult::Insertion(insertion) => {
                result = handle_insertion(insertion, alloc);
            }
        }
    }
}
