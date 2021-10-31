use crate::allocator::{alloc_value, Allocator, Global};
use crate::cell::{Cell, CellDefaultExt};
use crate::skip_list::{AllocItem, SkipList};
use crate::skip_list::{LeafNext, LeafRef, NoSize, OpaqueData};
use core::cmp::Ordering;
use core::marker::PhantomData;
use core::num::Wrapping;
use core::ptr::NonNull;
use fixed_bump::Bump;
use tagged_pointer::TaggedPtr;

trait Id: 'static + Sized + Clone + Ord {}

impl<I> Id for I where I: 'static + Sized + Clone + Ord {}

#[repr(align(2))]
struct Align2(u16);

impl Align2 {
    pub fn sentinel() -> NonNull<Self> {
        Align4::sentinel().cast()
    }
}

#[repr(align(4))]
struct Align4(u32);

impl Align4 {
    pub fn sentinel() -> NonNull<Self> {
        static SENTINEL: Align4 = Self(0);
        NonNull::from(&SENTINEL)
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
enum PosMapNodeKind {
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

struct PosMapNext<I: Id>(TaggedPtr<Align4, 2>, PhantomData<&'static Node<I>>);

impl<I: Id> PosMapNext<I> {
    pub fn new() -> Self {
        Self(TaggedPtr::new(Align4::sentinel(), 0), PhantomData)
    }

    pub fn is_opaque_data(&self) -> bool {
        (self.0.tag() & 0b10) != 0
    }

    pub fn kind(&self) -> PosMapNodeKind {
        (self.0.tag() & 0b01).into()
    }

    pub fn set_kind(&mut self, kind: PosMapNodeKind) {
        let (ptr, tag) = self.0.get();
        self.0 = TaggedPtr::new(ptr, (tag & 0b10) | kind as usize);
    }
}

impl<I: Id> PosMapNext<I> {
    pub fn get(&self) -> Option<LeafNext<PosMapNode<I>>> {
        Some(self.0.ptr()).filter(|p| *p != Align4::sentinel()).map(|p| {
            if self.is_opaque_data() {
                LeafNext::Data(OpaqueData::new(p.cast()))
            } else {
                LeafNext::Leaf(PosMapNode::new(
                    unsafe { p.cast().as_ref() },
                    self.kind(),
                ))
            }
        })
    }

    pub fn set(&mut self, next: Option<LeafNext<PosMapNode<I>>>) {
        let (ptr, tag) = next.map_or_else(
            || (Align4::sentinel(), 0),
            |n| match n {
                LeafNext::Data(data) => (data.ptr.cast(), 0b10),
                LeafNext::Leaf(leaf) => {
                    (NonNull::from(leaf.node()).cast(), leaf.kind() as usize)
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

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
enum SiblingSetNodeKind {
    Normal = 0,
    Childless = 1,
}

impl From<usize> for SiblingSetNodeKind {
    fn from(n: usize) -> Self {
        match n {
            0 => Self::Normal,
            1 => Self::Childless,
            n => panic!("bad enum value: {}", n),
        }
    }
}

struct SiblingSetNext<I: Id>(
    TaggedPtr<Align4, 2>,
    PhantomData<&'static Node<I>>,
);

impl<I: Id> SiblingSetNext<I> {
    pub fn new() -> Self {
        Self(TaggedPtr::new(Align4::sentinel(), 0), PhantomData)
    }

    pub fn is_opaque_data(&self) -> bool {
        (self.0.tag() & 0b10) != 0
    }

    pub fn kind(&self) -> SiblingSetNodeKind {
        (self.0.tag() & 0b01).into()
    }

    pub fn set_kind(&mut self, kind: SiblingSetNodeKind) {
        let (ptr, tag) = self.0.get();
        self.0 = TaggedPtr::new(ptr, (tag & 0b10) | kind as usize);
    }
}

impl<I: Id> SiblingSetNext<I> {
    pub fn get(&self) -> Option<LeafNext<SiblingSetNode<I>>> {
        Some(self.0.ptr()).filter(|p| *p != Align4::sentinel()).map(|p| {
            if self.is_opaque_data() {
                LeafNext::Data(OpaqueData::new(p.cast()))
            } else {
                LeafNext::Leaf(SiblingSetNode::new(
                    unsafe { p.cast().as_ref() },
                    self.kind(),
                ))
            }
        })
    }

    pub fn set(&mut self, next: Option<LeafNext<SiblingSetNode<I>>>) {
        let (ptr, tag) = next.map_or_else(
            || (Align4::sentinel(), 0),
            |n| match n {
                LeafNext::Data(data) => (data.ptr.cast(), 1),
                LeafNext::Leaf(leaf) => {
                    (NonNull::from(leaf.get()).cast(), leaf.kind() as usize)
                }
            },
        );
        self.0 = TaggedPtr::new(ptr, tag);
    }
}

impl<I: Id> Clone for SiblingSetNext<I> {
    fn clone(&self) -> Self {
        Self(self.0, self.1)
    }
}

impl<I: Id> Copy for SiblingSetNext<I> {}

impl<I: Id> Default for SiblingSetNext<I> {
    fn default() -> Self {
        Self::new()
    }
}

struct NewLocation<I: Id>(TaggedPtr<Align4, 2>, PhantomData<&'static Node<I>>);

impl<I: Id> NewLocation<I> {
    pub fn new() -> Self {
        Self(TaggedPtr::new(Align4::sentinel(), 0), PhantomData)
    }

    pub fn get(&self) -> Option<&'static Node<I>> {
        Some(self.0.ptr())
            .filter(|p| *p != Align4::sentinel())
            .map(|p| unsafe { p.cast().as_ref() })
    }

    pub fn set(&mut self, node: Option<&'static Node<I>>) {
        self.0 = TaggedPtr::new(
            node.map_or_else(Align4::sentinel, |r| NonNull::from(r).cast()),
            self.0.tag(),
        );
    }

    pub fn direction(&self) -> Direction {
        (self.0.tag() & 0b01).into()
    }

    pub fn set_direction(&mut self, direction: Direction) {
        let (ptr, tag) = self.0.get();
        self.0 = TaggedPtr::new(ptr, (tag & 0b10) | direction as usize);
    }

    pub fn visibility(&self) -> Visibility {
        ((self.0.tag() & 0b10) >> 1).into()
    }

    pub fn set_visibility(&mut self, vis: Visibility) {
        let (ptr, tag) = self.0.get();
        self.0 = TaggedPtr::new(ptr, (tag & 0b01) | ((vis as usize) << 1));
    }
}

impl<I: Id> Clone for NewLocation<I> {
    fn clone(&self) -> Self {
        Self(self.0, self.1)
    }
}

impl<I: Id> Copy for NewLocation<I> {}

impl<I: Id> Default for NewLocation<I> {
    fn default() -> Self {
        Self::new()
    }
}

#[repr(align(4))]
struct Node<I: Id> {
    id: I,
    parent: Option<I>,
    pos_map_next: [Cell<PosMapNext<I>>; 2],
    sibling_set_next: [Cell<SiblingSetNext<I>>; 2],
    move_timestamp: Cell<usize>,
    old_location: Cell<Option<&'static Self>>,
    new_location: Cell<NewLocation<I>>,
}

impl<I: Id> Node<I> {
    pub fn from_insertion(insertion: &Insertion<I>) -> Self {
        let mut new_location = NewLocation::new();
        new_location.set_direction(insertion.direction);
        Self {
            id: insertion.id.clone(),
            parent: insertion.parent.clone(),
            pos_map_next: Default::default(),
            sibling_set_next: Default::default(),
            move_timestamp: Cell::default(),
            old_location: Cell::default(),
            new_location: Cell::new(new_location),
        }
    }

    pub fn visibility(&self) -> Visibility {
        self.new_location.get().visibility()
    }

    pub fn set_visibility(&self, vis: Visibility) {
        self.new_location.with_mut(|n| n.set_visibility(vis));
    }

    pub fn direction(&self) -> Direction {
        self.new_location.get().direction()
    }

    pub fn new_location_or_self(&'static self) -> &'static Self {
        if let Some(node) = self.new_location.get().get() {
            node
        } else {
            self
        }
    }
}

struct PosMapNode<I: Id>(TaggedPtr<Node<I>, 1>, PhantomData<&'static Node<I>>);

impl<I: Id> PosMapNode<I> {
    pub fn new(node: &'static Node<I>, kind: PosMapNodeKind) -> Self {
        Self(TaggedPtr::new(NonNull::from(node), kind as usize), PhantomData)
    }

    pub fn get(&self) -> &'static Node<I> {
        unsafe { self.0.ptr().as_ref() }
    }

    pub fn kind(&self) -> PosMapNodeKind {
        self.0.tag().into()
    }

    pub fn node(&self) -> &'static Node<I> {
        unsafe { self.0.ptr().as_ref() }
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
    type KeyRef = ();
    type Align = Align4;

    fn next(&self) -> Option<LeafNext<Self>> {
        self.node().pos_map_next[self.kind() as usize].get().get()
    }

    fn set_next(&self, next: Option<LeafNext<Self>>) {
        self.node().pos_map_next[self.kind() as usize]
            .with_mut(|n| n.set(next));
    }

    fn key(&self) {}

    fn size(&self) -> Self::Size {
        Wrapping(
            (self.kind() == PosMapNodeKind::Normal
                && self.node().visibility() == Visibility::Visible)
                as usize,
        )
    }
}

struct SiblingSetNode<I: Id>(
    TaggedPtr<Node<I>, 1>,
    PhantomData<&'static Node<I>>,
);

impl<I: Id> SiblingSetNode<I> {
    pub fn new(node: &'static Node<I>, kind: SiblingSetNodeKind) -> Self {
        Self(TaggedPtr::new(NonNull::from(node), kind as usize), PhantomData)
    }

    pub fn get(&self) -> &'static Node<I> {
        unsafe { self.0.ptr().as_ref() }
    }

    pub fn kind(&self) -> SiblingSetNodeKind {
        self.0.tag().into()
    }

    pub fn node(&self) -> &'static Node<I> {
        unsafe { self.0.ptr().as_ref() }
    }

    pub fn parent_id(&self) -> Option<I> {
        match self.kind() {
            SiblingSetNodeKind::Normal => self.node().parent.clone(),
            SiblingSetNodeKind::Childless => Some(self.node().id.clone()),
        }
    }

    pub fn child_id(&self) -> Option<I> {
        match self.kind() {
            SiblingSetNodeKind::Normal => Some(self.node().id.clone()),
            SiblingSetNodeKind::Childless => None,
        }
    }

    pub fn direction(&self) -> Option<Direction> {
        match self.kind() {
            SiblingSetNodeKind::Normal => Some(self.node().direction()),
            SiblingSetNodeKind::Childless => None,
        }
    }
}

impl<I: Id> Clone for SiblingSetNode<I> {
    fn clone(&self) -> Self {
        Self(self.0, self.1)
    }
}

impl<I: Id> Copy for SiblingSetNode<I> {}

unsafe impl<I: Id> LeafRef for SiblingSetNode<I> {
    type Size = NoSize;
    type KeyRef = Self;
    type Align = Align2;

    fn next(&self) -> Option<LeafNext<Self>> {
        self.node().sibling_set_next[self.kind() as usize].get().get()
    }

    fn set_next(&self, next: Option<LeafNext<Self>>) {
        self.node().sibling_set_next[self.kind() as usize]
            .with_mut(|n| n.set(next))
    }

    fn key(&self) -> Self::KeyRef {
        *self
    }
}

#[repr(transparent)]
struct AllocItem1<I: Id>(Node<I>);
#[repr(transparent)]
struct AllocItem2<I: Id>(AllocItem<PosMapNode<I>>);
#[repr(transparent)]
struct AllocItem3<I: Id>(AllocItem<SiblingSetNode<I>>);

trait Allocators {
    type Alloc1: Allocator + Default;
    type Alloc2: Allocator + Default;
    type Alloc3: Allocator + Default;
}

struct GlobalAllocators<I>(PhantomData<fn() -> I>);

impl<I: Id> Allocators for GlobalAllocators<I> {
    type Alloc1 = Global;
    type Alloc2 = Global;
    type Alloc3 = Global;
}

struct BumpAllocators<I, const N: usize>(PhantomData<fn() -> I>);

impl<I: Id, const N: usize> Allocators for BumpAllocators<I, N> {
    type Alloc1 = Bump<[AllocItem1<I>; N]>;
    type Alloc2 = Bump<[AllocItem2<I>; N]>;
    type Alloc3 = Bump<[AllocItem3<I>; N]>;
}

struct Eips<I, A = BumpAllocators<I, 32>>
where
    I: Id,
    A: Allocators,
{
    alloc: A::Alloc1,
    pos_map: SkipList<PosMapNode<I>, A::Alloc2>,
    sibling_set: SkipList<SiblingSetNode<I>, A::Alloc3>,
}

unsafe impl<I, A> Send for Eips<I, A>
where
    I: Id + Send,
    A: Allocators + Send,
{
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
enum Visibility {
    Visible = 0,
    Hidden = 1,
}

impl From<usize> for Visibility {
    fn from(n: usize) -> Self {
        match n {
            0 => Self::Visible,
            1 => Self::Hidden,
            n => panic!("bad enum value: {}", n),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
enum Direction {
    After = 0,
    Before = 1,
}

impl From<usize> for Direction {
    fn from(n: usize) -> Self {
        match n {
            0 => Self::After,
            1 => Self::Before,
            n => panic!("bad enum value: {}", n),
        }
    }
}

struct Insertion<I: Id> {
    pub id: I,
    pub parent: Option<I>,
    pub direction: Direction,
}

impl<I: Id> PartialEq<SiblingSetNode<I>> for Insertion<I> {
    fn eq(&self, other: &SiblingSetNode<I>) -> bool {
        matches!(self.partial_cmp(other), Some(Ordering::Equal))
    }
}

impl<I: Id> PartialOrd<SiblingSetNode<I>> for Insertion<I> {
    fn partial_cmp(&self, other: &SiblingSetNode<I>) -> Option<Ordering> {
        match self.parent.cmp(&other.parent_id()) {
            Ordering::Equal => {}
            ordering => return Some(ordering),
        }
        Some((self.direction, self.id.clone()).cmp(&(
            match other.direction() {
                Some(direction) => direction,
                None => return Some(Ordering::Greater),
            },
            match other.child_id() {
                Some(child) => child,
                None => return Some(Ordering::Greater),
            },
        )))
    }
}

struct Move<I: Id> {
    pub insertion: Insertion<I>,
    pub old: I,
    pub timestamp: usize,
}

struct FindChildless<I: Id>(pub I);

impl<I: Id> PartialEq<SiblingSetNode<I>> for FindChildless<I> {
    fn eq(&self, other: &SiblingSetNode<I>) -> bool {
        matches!(self.partial_cmp(other), Some(Ordering::Equal))
    }
}

impl<I: Id> PartialOrd<SiblingSetNode<I>> for FindChildless<I> {
    fn partial_cmp(&self, other: &SiblingSetNode<I>) -> Option<Ordering> {
        match Some(self.0.clone()).cmp(&other.parent_id()) {
            Ordering::Equal => {}
            ordering => return Some(ordering),
        }
        Some(other.child_id().map_or(Ordering::Equal, |_| Ordering::Greater))
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
struct BadIndex;

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
struct BadId;

impl<I, A> Eips<I, A>
where
    I: Id,
    A: Allocators,
{
    pub fn new() -> Self {
        Self {
            alloc: A::Alloc1::default(),
            pos_map: SkipList::new_in(A::Alloc2::default()),
            sibling_set: SkipList::new_in(A::Alloc3::default()),
        }
    }

    fn alloc_node(&self, node: Node<I>) -> &'static mut Node<I> {
        unsafe { alloc_value(node, &self.alloc).as_mut() }
    }

    pub fn local_get(&self, index: usize) -> Result<I, BadIndex> {
        Ok(self
            .pos_map
            .get(Wrapping(index))
            .ok_or(BadIndex)?
            .node()
            .id
            .clone())
    }

    pub fn remote_get(&self, id: I) -> Result<usize, BadId> {
        let node =
            self.sibling_set.find(&FindChildless(id)).ok_or(BadId)?.node();
        Ok(self
            .pos_map
            .position(PosMapNode::new(node, PosMapNodeKind::Normal))
            .0)
    }

    pub fn local_insert(
        &mut self,
        index: usize,
        id: I,
    ) -> Result<Insertion<I>, BadIndex> {
        Ok(if let Some(index) = index.checked_sub(1) {
            let pos_node =
                self.pos_map.get(Wrapping(index)).ok_or(BadIndex)?;
            let node = pos_node.node();
            let child = self
                .sibling_set
                .next(SiblingSetNode::new(node, SiblingSetNodeKind::Childless))
                .map(|n| n.node());

            let can_insert_after =
                child.map_or(true, |c| c.parent.as_ref() != Some(&node.id));
            if can_insert_after {
                Insertion {
                    id,
                    parent: Some(node.id.clone()),
                    direction: Direction::After,
                }
            } else {
                let next = self.pos_map.next(pos_node).unwrap().node();
                Insertion {
                    id,
                    parent: Some(next.id.clone()),
                    direction: Direction::Before,
                }
            }
        } else if let Some(node) = self.sibling_set.first().map(|n| n.node()) {
            Insertion {
                id,
                parent: Some(node.id.clone()),
                direction: Direction::Before,
            }
        } else {
            Insertion {
                id,
                parent: None,
                direction: Direction::After,
            }
        })
    }

    fn remote_insert_node(
        &mut self,
        node: &'static Node<I>,
        insertion: Insertion<I>,
    ) -> Result<(), BadId> {
        let sibling = self.sibling_set.find_closest(&insertion);
        if sibling.map_or(false, |s| s.node().id == insertion.id) {
            return Err(BadId);
        }

        let nodes = [PosMapNodeKind::Normal, PosMapNodeKind::Marker]
            .map(|kind| PosMapNode::new(node, kind));
        let node_as_sibling =
            SiblingSetNode::new(node, SiblingSetNodeKind::Normal);

        let neighbor = |sibling: SiblingSetNode<I>| {
            PosMapNode::new(
                sibling.node(),
                if sibling.child_id().is_some() {
                    PosMapNodeKind::Marker
                } else {
                    PosMapNodeKind::Normal
                },
            )
        };

        Ok(if let Some(sibling) = sibling {
            self.sibling_set.insert_after(sibling, node_as_sibling);
            match insertion.direction {
                Direction::After => {
                    self.pos_map.insert_after_from(neighbor(sibling), nodes);
                }
                Direction::Before => {
                    self.pos_map.insert_before_from(
                        neighbor(sibling),
                        nodes.into_iter().rev(),
                    );
                }
            }
        } else {
            debug_assert!(node.parent.is_none());
            self.sibling_set.insert_at_start(node_as_sibling);
            self.pos_map.insert_at_start_from(nodes);
        })
    }

    pub fn remote_insert(
        &mut self,
        insertion: Insertion<I>,
    ) -> Result<usize, BadId> {
        let node: &'static _ =
            self.alloc_node(Node::from_insertion(&insertion));
        self.remote_insert_node(node, insertion)?;
        Ok(self
            .pos_map
            .position(PosMapNode::new(node, PosMapNodeKind::Normal))
            .0)
    }

    pub fn local_remove(&mut self, index: usize) -> Result<I, BadIndex> {
        Ok(self
            .pos_map
            .get(Wrapping(index))
            .ok_or(BadIndex)?
            .node()
            .id
            .clone())
    }

    pub fn remote_remove(&mut self, id: I) -> Result<usize, BadId> {
        let node =
            self.sibling_set.find(&FindChildless(id)).ok_or(BadId)?.node();
        let pos_node = PosMapNode::new(node, PosMapNodeKind::Normal);
        let index = self.pos_map.position(pos_node).0;
        self.pos_map.update(pos_node, |node| {
            node.node().set_visibility(Visibility::Hidden);
        });
        Ok(index)
    }

    pub fn local_move(
        &mut self,
        old: usize,
        new: usize,
        id: I,
    ) -> Result<Move<I>, BadIndex> {
        let node = self.pos_map.get(Wrapping(old)).ok_or(BadIndex)?.node();
        Ok(Move {
            insertion: self.local_insert(new + (new > old) as usize, id)?,
            old: node.id.clone(),
            timestamp: node.move_timestamp.get() + 1,
        })
    }

    pub fn remote_move(
        &mut self,
        mv: Move<I>,
    ) -> Result<Option<(usize, usize)>, BadId> {
        let old =
            self.sibling_set.find(&FindChildless(mv.old)).ok_or(BadId)?.node();
        let old = old.new_location_or_self();
        let old_pos = PosMapNode::new(old, PosMapNodeKind::Normal);
        let new = self.alloc_node(Node::from_insertion(&mv.insertion));
        let new_pos = PosMapNode::new(old, PosMapNodeKind::Normal);

        Ok(
            if match mv.timestamp.cmp(&old.move_timestamp.get()) {
                Ordering::Greater => true,
                Ordering::Equal => old
                    .old_location
                    .get()
                    .map_or(true, |node| new.id > node.id),
                _ => false,
            } {
                old.new_location.with_mut(|n| n.set(Some(new)));
                self.pos_map.update(old_pos, |node| {
                    node.node().set_visibility(Visibility::Hidden);
                });
                let old_index = self.pos_map.position(old_pos).0;
                self.remote_insert_node(new, mv.insertion)?;
                Some((old_index, self.pos_map.position(new_pos).0))
            } else {
                new.set_visibility(Visibility::Hidden);
                self.remote_insert_node(new, mv.insertion)?;
                None
            },
        )
    }
}
