use crate::allocator::{Allocator, Global};
use crate::cell::CellDefaultExt;
use core::cmp::Ordering;
use core::convert::TryFrom;
use core::iter::{self, FusedIterator};
use core::mem;

mod destroy;
mod destroy_safety;
mod insert;
mod node;
mod remove;
mod split;
mod traverse;

use destroy::{deconstruct, destroy_node_list};
use destroy_safety::SetUnsafeOnDrop;
use insert::insert_after;
pub use node::{AllocItem, LeafNext, LeafRef, NoSize, OpaqueData};
use node::{Down, InternalNodeRef, Next, NodeRef};
use remove::remove;
use traverse::{get_last_sibling, get_previous_node};

fn min_node_length<L: LeafRef>() -> usize {
    max_node_length::<L>() / 2
}

fn max_node_length<L: LeafRef>() -> usize {
    L::FANOUT.max(2)
}

fn roots_match<L: LeafRef>(a: &Down<L>, b: &Down<L>) -> bool {
    type Internal<'a, L> = &'a InternalNodeRef<L>;
    Internal::try_from(a) == Internal::try_from(b)
}

pub struct SkipList<L, A = Global>
where
    L: LeafRef,
    A: Allocator,
{
    alloc: A,
    root: Option<Down<L>>,
}

impl<L: LeafRef> SkipList<L> {
    pub fn new() -> Self {
        Self::new_in(Global)
    }
}

impl<L, A> SkipList<L, A>
where
    L: LeafRef,
    A: Allocator,
{
    pub fn new_in(alloc: A) -> Self {
        Self {
            alloc,
            root: None,
        }
    }

    pub fn insert_after(&mut self, pos: L, item: L) {
        self.insert_after_from(pos, iter::once(item));
    }

    pub fn insert_after_from<I>(&mut self, pos: L, items: I)
    where
        I: IntoIterator<Item = L>,
    {
        let root = self.root.as_ref().expect("`pos` is not from this list");
        let set_unsafe_on_drop = SetUnsafeOnDrop;
        let result = insert_after(pos, items.into_iter(), &self.alloc);
        assert!(
            roots_match(root, &result.old_root),
            "`pos` is not from this list"
        );
        mem::forget(set_unsafe_on_drop);
        self.root = Some(result.new_root);
    }

    pub fn insert_at_start(&mut self, item: L) {
        self.insert_at_start_from(iter::once(item));
    }

    pub fn insert_at_start_from<I>(&mut self, items: I)
    where
        I: IntoIterator<Item = L>,
    {
        let mut iter = items.into_iter();
        let first = match iter.next() {
            Some(item) => item,
            None => return,
        };

        let size = first.size();
        let mut down = self.root.clone();
        let mut parent = None;
        let next = loop {
            match down {
                None => break None,
                Some(Down::Leaf(node)) => break Some(node),
                Some(Down::Internal(node)) => {
                    node.size.with_mut(|s| *s += size.clone());
                    down = node.down();
                    parent = Some(node);
                }
            }
        };

        if let Some(parent) = parent {
            // The order of operations here is important for exception safety:
            // we don't want `node` to be accessible from `first` if this list
            // gets dropped due to a panic in `NodeRef::set_next`.
            parent.set_down(Some(Down::Leaf(first.clone())));
        } else {
            self.root = Some(Down::Leaf(first.clone()));
        }

        NodeRef::set_next(&first, next.map(Next::Next));
        self.insert_after_from(first, iter);
    }

    pub fn insert_before(&mut self, pos: L, item: L) {
        self.insert_before_from(pos, iter::once(item));
    }

    pub fn insert_before_from<I>(&mut self, pos: L, items: I)
    where
        I: IntoIterator<Item = L>,
    {
        if let Some(prev) = self.previous(pos) {
            self.insert_after_from(prev, items);
        } else {
            self.insert_at_start_from(items);
        }
    }

    pub fn remove(&mut self, item: L) {
        let root = self.root.as_ref().expect("`item` is not from this list");
        let result = remove(item);
        assert!(
            roots_match(root, &result.old_root),
            "`pos` is not from this list"
        );
        unsafe { destroy_node_list(result.removed, &self.alloc) };
        self.root = result.new_root;
    }

    pub fn first(&self) -> Option<L> {
        let mut node = self.root.clone()?;
        loop {
            node = match node {
                Down::Leaf(node) => return Some(node),
                Down::Internal(node) => node.down().unwrap(),
            }
        }
    }

    pub fn last(&self) -> Option<L> {
        let mut node = self.root.clone()?;
        loop {
            node = match node {
                Down::Leaf(node) => return Some(get_last_sibling(node)),
                Down::Internal(node) => get_last_sibling(node).down().unwrap(),
            }
        }
    }

    pub fn size(&self) -> L::Size {
        self.root.as_ref().map_or_else(L::Size::default, |r| r.size())
    }

    pub fn get(&self, pos: L::Size) -> Option<L> {
        match pos.cmp(&self.size()) {
            Ordering::Less => {}
            Ordering::Equal => return self.last(),
            Ordering::Greater => return None,
        }

        let mut node = self.root.clone()?;
        let mut size = L::Size::default();
        loop {
            node = match node {
                Down::Leaf(mut node) => loop {
                    size += node.size();
                    if size > pos {
                        return Some(node);
                    }
                    node = node.next_sibling().unwrap();
                },
                Down::Internal(mut node) => loop {
                    let mut new_size = size.clone();
                    new_size += node.size();
                    if new_size > pos {
                        break node.down().unwrap();
                    }
                    node = node.next_sibling().unwrap();
                },
            }
        }
    }

    pub fn position(&self, item: L) -> L::Size {
        self.root.as_ref().expect("`item` is not from this list");
        let mut size = self.size();
        let mut pos = item.size();

        fn add_siblings<N: NodeRef>(
            pos: &mut <N::Leaf as LeafRef>::Size,
            mut node: N,
        ) -> Option<InternalNodeRef<N::Leaf>> {
            loop {
                node = match node.next()? {
                    Next::Parent(parent) => return Some(parent),
                    Next::Next(node) => {
                        *pos += node.size();
                        node
                    }
                }
            }
        }

        let mut node = if let Some(parent) = add_siblings(&mut pos, item) {
            parent
        } else {
            size -= pos;
            return size;
        };
        loop {
            node = if let Some(parent) = add_siblings(&mut pos, node) {
                parent
            } else {
                size -= pos;
                return size;
            };
        }
    }

    pub fn find<K>(&self, key: &K) -> Option<L::KeyRef>
    where
        K: PartialOrd<L::KeyRef>,
    {
        self.find_closest(key).filter(|k| key == k)
    }

    pub fn find_closest<K>(&self, key: &K) -> Option<L::KeyRef>
    where
        K: PartialOrd<L::KeyRef>,
    {
        let mut node = self.root.clone()?;
        if key < &node.key().unwrap() {
            return None;
        }
        loop {
            node = match node {
                Down::Leaf(mut node) => loop {
                    debug_assert!(key <= &node.key());
                    let next = node.next_sibling();
                    node = match next {
                        None => return Some(node.key()),
                        Some(n) if key < &n.key() => return Some(node.key()),
                        Some(n) => n,
                    };
                },
                Down::Internal(mut node) => loop {
                    let node_key = node.key().unwrap();
                    debug_assert!(key <= &node_key);
                    if key == &node_key {
                        return Some(node_key);
                    }
                    let next = node.next_sibling();
                    node = match next {
                        None => break node.down().unwrap(),
                        Some(n) if key < &n.key().unwrap() => {
                            break node.down().unwrap();
                        }
                        Some(n) => n,
                    };
                },
            }
        }
    }

    fn next(&self, item: L) -> Option<L> {
        let mut node = match NodeRef::next(&item)? {
            Next::Next(node) => return Some(node),
            Next::Parent(mut node) => loop {
                node = match node.next()? {
                    Next::Next(node) => break node,
                    Next::Parent(node) => node,
                }
            },
        };
        loop {
            node = match node.down().unwrap() {
                Down::Leaf(node) => return Some(node),
                Down::Internal(node) => node,
            };
        }
    }

    fn previous(&self, item: L) -> Option<L> {
        let mut node = match get_previous_node(item)? {
            Next::Next(node) => return Some(node),
            Next::Parent(mut node) => loop {
                node = match get_previous_node(node)? {
                    Next::Next(node) => break node,
                    Next::Parent(node) => node,
                }
            },
        };
        loop {
            node = match node.down().unwrap() {
                Down::Leaf(node) => return Some(get_last_sibling(node)),
                Down::Internal(node) => get_last_sibling(node),
            };
        }
    }

    pub fn iter(&self) -> Iter<'_, L, A> {
        Iter {
            leaf: self.first(),
            list: self,
        }
    }
}

impl<L: LeafRef> Default for SkipList<L> {
    fn default() -> Self {
        Self::new()
    }
}

impl<L, A> Drop for SkipList<L, A>
where
    L: LeafRef,
    A: Allocator,
{
    fn drop(&mut self) {
        let root = match self.root.take() {
            Some(root) => root,
            None => return,
        };
        let nodes = deconstruct(root);
        unsafe { destroy_node_list(nodes, &self.alloc) };
    }
}

pub struct Iter<'a, L, A>
where
    L: LeafRef,
    A: Allocator,
{
    leaf: Option<L>,
    list: &'a SkipList<L, A>,
}

impl<'a, L, A> Iterator for Iter<'a, L, A>
where
    L: LeafRef,
    A: Allocator,
{
    type Item = L;

    fn next(&mut self) -> Option<L> {
        self.leaf = self.leaf.take().and_then(|n| self.list.next(n));
        self.leaf.clone()
    }
}

impl<'a, L, A> FusedIterator for Iter<'a, L, A>
where
    L: LeafRef,
    A: Allocator,
{
}

impl<'a, L, A> IntoIterator for &'a SkipList<L, A>
where
    L: LeafRef,
    A: Allocator,
{
    type Item = L;
    type IntoIter = Iter<'a, L, A>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

pub struct IntoIter<L, A>
where
    L: LeafRef,
    A: Allocator,
{
    leaf: Option<L>,
    list: SkipList<L, A>,
}

impl<L, A> Iterator for IntoIter<L, A>
where
    L: LeafRef,
    A: Allocator,
{
    type Item = L;

    fn next(&mut self) -> Option<L> {
        self.leaf = self.leaf.take().and_then(|n| self.list.next(n));
        self.leaf.clone()
    }
}

impl<L, A> FusedIterator for IntoIter<L, A>
where
    L: LeafRef,
    A: Allocator,
{
}

impl<L, A> IntoIterator for SkipList<L, A>
where
    L: LeafRef,
    A: Allocator,
{
    type Item = L;
    type IntoIter = IntoIter<L, A>;

    fn into_iter(self) -> Self::IntoIter {
        IntoIter {
            leaf: self.first(),
            list: self,
        }
    }
}
