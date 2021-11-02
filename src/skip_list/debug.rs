use super::node::{Down, LeafRef, NodeRef};
use super::SkipList;
use crate::allocator::Allocator;
use core::fmt::Debug;

fn print_indent(indent: usize) {
    for _ in 0..indent {
        eprint!("  ");
    }
}

macro_rules! debug_print {
    ($indent:expr, $($args:tt)*) => {
        print_indent($indent);
        eprintln!($($args)*);
    };
}

impl<L, A> SkipList<L, A>
where
    L: LeafRef + Debug,
    L::Size: Debug,
    A: Allocator,
{
    pub(crate) fn debug(&self) {
        eprintln!("list size: {:?}", self.size());
        if let Some(root) = self.root.clone() {
            debug(root, 0, 0);
        }
    }
}

pub fn debug<L>(node: Down<L>, depth: usize, indent: usize)
where
    L: LeafRef + Debug,
    L::Size: Debug,
{
    match node {
        Down::Internal(mut node) => loop {
            eprintln!();
            debug_print!(indent, "depth {}: internal", depth);
            debug_print!(indent, "length: {}", node.len.get());
            debug_print!(indent, "size: {:?}", node.size());
            if let Some(down) = node.down() {
                debug(down, depth + 1, indent + 1);
            }
            node = if let Some(next) = node.next_sibling() {
                next
            } else {
                break;
            }
        },
        Down::Leaf(mut node) => loop {
            eprintln!();
            debug_print!(indent, "leaf: {:?}", node);
            debug_print!(indent, "size: {:?}", node.size());
            node = if let Some(next) = node.next_sibling() {
                next
            } else {
                break;
            }
        },
    }
}
