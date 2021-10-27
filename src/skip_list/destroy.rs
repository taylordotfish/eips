use super::destroy_safety::can_safely_destroy;
use super::node::{Down, InternalNodeRef, LeafRef, Next, NodeRef};
use crate::allocator::Allocator;

pub fn deconstruct<L: LeafRef>(node: Down<L>) -> Option<InternalNodeRef<L>> {
    let mut nodes = None;
    match node {
        Down::Leaf(mut node) => loop {
            let next = node.next_sibling();
            NodeRef::set_next(&node, None);
            node = if let Some(next) = next {
                next
            } else {
                break;
            }
        },
        Down::Internal(mut node) => loop {
            if let Some(down) = node.down() {
                if let Some(node) = deconstruct(down) {
                    node.set_next(nodes.map(Next::Next));
                    nodes = Some(node);
                }
            }
            let next = node.next_sibling();
            node = if let Some(next) = next {
                next
            } else {
                break;
            }
        },
    }
    nodes
}

/// # Safety
///
/// Every node in the list must have been allocafed by `alloc`.
pub unsafe fn destroy_node_list<L: LeafRef, A: Allocator>(
    mut head: Option<InternalNodeRef<L>>,
    alloc: &A,
) {
    if !can_safely_destroy() {
        return;
    }
    while let Some(node) = head {
        let next = node.next_sibling();
        unsafe { node.dealloc(alloc) };
        head = next;
    }
}
