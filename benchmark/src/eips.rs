use btree_vec::BTreeVec;
use eips::{Eips, LocalChange, RemoteChange};

type EipsOptions = eips::Options</* SUPPORTS_MOVE */ false>;

#[derive(Clone, Copy, Debug, Eq, PartialEq, Ord, PartialOrd)]
struct Id {
    pub client_id: u64,
    pub counter: u64,
}

impl Id {
    fn increment(&mut self) -> Self {
        let id = *self;
        self.counter += 1;
        id
    }
}

pub struct Client {
    eips: Eips<Id, EipsOptions>,
    text: BTreeVec<char, 32>,
    next_id: Id,
}

#[derive(Copy, Clone)]
pub struct Update {
    change: RemoteChange<Id>,
    item: Option<char>,
}

impl super::Client for Client {
    type Update = Update;

    fn new(client_id: u64) -> Self {
        Self {
            eips: Eips::new(),
            text: Default::default(),
            next_id: Id {
                client_id,
                counter: 0,
            },
        }
    }

    fn len(&self) -> usize {
        self.text.len()
    }

    fn text(&self) -> impl Iterator<Item = char> + '_ {
        self.text.iter().copied()
    }

    fn insert(&mut self, index: usize, c: char) -> Self::Update {
        let update = Update {
            change: self
                .eips
                .insert(index, self.next_id.increment())
                .expect("bad index"),
            item: Some(c),
        };
        self.apply(update);
        update
    }

    fn remove(&mut self, index: usize) -> Self::Update {
        let update = Update {
            change: self.eips.remove(index).expect("bad index"),
            item: None,
        };
        self.apply(update);
        update
    }

    fn apply(&mut self, update: Self::Update) {
        let local =
            self.eips.apply_change(update.change).expect("bad remote change");
        match local {
            LocalChange::Insert(index) => {
                self.text.insert(
                    index,
                    update.item.expect("insertion should have item"),
                );
            }
            LocalChange::Remove(index) => {
                debug_assert!(update.item.is_none());
                self.text.remove(index);
            }
            LocalChange::Move {
                ..
            } => unreachable!("move operations not supported"),
            LocalChange::None => {}
            LocalChange::AlreadyApplied => {}
        }
    }
}
