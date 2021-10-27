use super::min_node_length;
use super::node::{Down, InternalNodeRef, LeafRef, Next, NodeRef};
use super::traverse::{get_last_sibling, get_nth_sibling};
use super::traverse::{get_parent, get_previous_node};
use crate::cell::CellDefaultExt;

struct Removal<N: NodeRef> {
    pub child: N,
    pub size_only: bool,
    pub diff: <N::Leaf as LeafRef>::Size,
}

enum RemovalResult<N: NodeRef> {
    Removal(Removal<InternalNodeRef<N::Leaf>>),
    Done(N),
}

fn handle_removal<N: NodeRef>(removal: Removal<N>) -> RemovalResult<N> {
    let child = removal.child;
    let diff = removal.diff;

    let last = get_last_sibling(child.clone());
    let parent = if let Some(parent) = get_parent(last.clone()) {
        parent
    } else {
        return RemovalResult::Done(child);
    };

    parent.size.with_mut(|s| *s -= diff.clone());
    if removal.size_only {
        return RemovalResult::Removal(Removal {
            child: parent,
            size_only: true,
            diff,
        });
    }

    match get_previous_node(child.clone()).unwrap() {
        Next::Parent(parent) => {
            parent.set_down(Some(child.next_sibling().unwrap().as_down()));
        }
        Next::Next(prev) => {
            prev.set_next(child.next());
        }
    }

    child.set_next(None);
    parent.len.with_mut(|n| *n -= 1);
    if parent.len.get() >= min_node_length::<N::Leaf>() {
        return RemovalResult::Removal(Removal {
            child: parent,
            size_only: true,
            diff,
        });
    }

    let first: N = parent.down_as().unwrap();
    let (neighbor, right) = match parent.next().unwrap() {
        Next::Next(right) => (right, true),
        Next::Parent(grandparent) => (
            get_nth_sibling(
                grandparent.down_as().unwrap(),
                grandparent.len.get() - 2,
            )
            .unwrap(),
            false,
        ),
    };

    if right {
        let right = neighbor;
        let right_first: N = right.down_as().unwrap();
        if right.len.get() > min_node_length::<N::Leaf>() {
            let right_second = right_first.next_sibling().unwrap();
            right.set_down(Some(right_second.as_down()));
            right.key.set(right_second.key());
            right_first.set_next(last.next());
            right.len.with_mut(|n| *n -= 1);
            parent.len.with_mut(|n| *n += 1);
            right.size.with_mut(|s| *s -= right_first.size());
            parent.size.with_mut(|s| *s += right_first.size());
            last.set_next(Some(Next::Next(right_first)));
            return RemovalResult::Removal(Removal {
                child: parent,
                size_only: true,
                diff,
            });
        }

        last.set_next(Some(Next::Next(right_first)));
        right.set_down(None);
        parent.size.set(right.size.take());
        parent.len.with_mut(|n| *n += right.len.take());
        return RemovalResult::Removal(Removal {
            child: right,
            size_only: false,
            diff,
        });
    }

    let left = neighbor;
    let left_len = left.len.get();
    let left_first: N = left.down_as().unwrap();
    let left_penultimate = get_nth_sibling(left_first, left_len - 2).unwrap();
    let left_last = left_penultimate.next_sibling().unwrap();
    if left_len > min_node_length::<N::Leaf>() {
        left_penultimate.set_next(left_last.next());
        left_last.set_next(Some(Next::Next(first)));
        parent.set_down(Some(left_last.as_down()));
        parent.key.set(left_last.key());
        left.len.with_mut(|n| *n -= 1);
        parent.len.with_mut(|n| *n += 1);
        left.size.with_mut(|s| *s -= left_last.size());
        parent.size.with_mut(|s| *s += left_last.size());
        return RemovalResult::Removal(Removal {
            child: parent,
            size_only: true,
            diff,
        });
    }

    last.set_next(left_last.next());
    left_last.set_next(Some(Next::Next(first)));
    left.size.set(parent.size.take());
    left.len.with_mut(|n| *n += parent.len.take());
    RemovalResult::Removal(Removal {
        child: parent,
        size_only: false,
        diff,
    })
}

pub struct FinishedRemoval<L: LeafRef> {
    pub old_root: Down<L>,
    pub new_root: Option<Down<L>>,
    pub removed: Option<InternalNodeRef<L>>,
}

pub fn remove<L: LeafRef>(item: L) -> FinishedRemoval<L> {
    let result = handle_removal(Removal {
        diff: item.size(),
        child: item,
        size_only: false,
    });

    let mut removed = None;
    let mut removal = match result {
        RemovalResult::Removal(removal) => removal,
        RemovalResult::Done(root) => {
            return FinishedRemoval {
                old_root: root.as_down(),
                new_root: None,
                removed,
            };
        }
    };

    loop {
        let result = handle_removal(removal);
        removal = match result {
            RemovalResult::Removal(removal) => {
                if let Some(node) = &mut removed {
                    node.set_next(Some(Next::Next(removal.child)));
                } else {
                    removed = Some(removal.child);
                }
                removal
            }
            RemovalResult::Done(root) => {
                let old = root.as_down();
                let new = if root.len.get() < min_node_length::<L>() {
                    let down = root.down().unwrap();
                    root.set_down(None);
                    down
                } else {
                    old.clone()
                };
                return FinishedRemoval {
                    old_root: old,
                    new_root: Some(new),
                    removed,
                };
            }
        }
    }
}
