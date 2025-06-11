/*
 * Copyright (C) 2025 taylor.fish <contact@taylor.fish>
 *
 * This file is part of Eips.
 *
 * Eips is free software: you can redistribute it and/or modify it under
 * the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Eips is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 * or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General
 * Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public
 * License along with Eips. If not, see <https://www.gnu.org/licenses/>.
 */

use eips::{Eips, LocalChange};
use rand::Rng;
use rand_chacha::ChaChaRng;
#[allow(dead_code)]
mod common;

const ITERATIONS: usize = 1000;

trait Id: Copy + Ord + std::fmt::Debug {
    fn new<R: Rng>(rng: &mut R) -> Self;
    fn increment<R: Rng>(&mut self, rng: &mut R);
}

impl Id for u32 {
    fn new<R: Rng>(_rng: &mut R) -> Self {
        0
    }

    fn increment<R: Rng>(&mut self, _rng: &mut R) {
        *self += 1;
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, Ord, PartialOrd)]
struct RandomId(u128);

impl Id for RandomId {
    fn new<R: Rng>(rng: &mut R) -> Self {
        Self(rng.random())
    }

    fn increment<R: Rng>(&mut self, rng: &mut R) {
        self.0 = rng.random();
    }
}

struct State<Id> {
    eips: Eips<Id>,
    id: Id,
    len: usize,
    rng: ChaChaRng,
}

impl<Id: self::Id> State<Id> {
    fn new() -> Self {
        let mut rng = common::make_rng();
        Self {
            eips: Eips::new(),
            id: Id::new(&mut rng),
            len: 0,
            rng,
        }
    }

    fn increment_id(&mut self) {
        self.id.increment(&mut self.rng);
    }

    fn insert(&mut self) {
        let index = self.rng.random_range(0..self.len + 1);
        let remote_change = self.eips.insert(index, self.id).unwrap();
        self.increment_id();
        let local_change = self.eips.apply_change(remote_change).unwrap();
        match local_change {
            LocalChange::Insert(i) => assert_eq!(i, index),
            c => panic!("unexpected local change: {c:?}"),
        }
        self.len += 1;
    }

    fn remove(&mut self) {
        let index = self.rng.random_range(0..self.len);
        let remote_change = self.eips.remove(index).unwrap();
        let local_change = self.eips.apply_change(remote_change).unwrap();
        match local_change {
            LocalChange::Remove(i) => assert_eq!(i, index),
            c => panic!("unexpected local change: {c:?}"),
        }
        self.len -= 1;
    }

    fn mv(&mut self) {
        let old = self.rng.random_range(0..self.len);
        let new = self.rng.random_range(0..self.len);
        let remote_change = self.eips.mv(old, new, self.id).unwrap();
        self.increment_id();
        let local_change = self.eips.apply_change(remote_change).unwrap();
        match local_change {
            LocalChange::Move {
                old: local_old,
                new: local_new,
            } => {
                assert_eq!(local_old, old);
                assert_eq!(local_new, new);
            }
            LocalChange::None => {
                assert_eq!(old, new);
            }
            c => panic!("unexpected local change: {c:?}"),
        }
    }
}

fn insertions<Id: self::Id>() {
    let mut state = State::<Id>::new();
    for _ in 0..ITERATIONS {
        state.insert();
    }
    assert_eq!(state.len, state.eips.len());
}

#[test]
fn insertions_u32() {
    insertions::<u32>();
}

#[test]
fn insertions_rand() {
    insertions::<RandomId>();
}

fn non_move<Id: self::Id>() {
    let mut state = State::<Id>::new();
    for _ in 0..ITERATIONS {
        if state.len > 0 && state.rng.random_range(0..3) == 0 {
            state.remove();
        } else {
            state.insert();
        }
    }
    assert_eq!(state.len, state.eips.len());
}

#[test]
fn non_move_u32() {
    non_move::<u32>();
}

#[test]
fn non_move_rand() {
    non_move::<RandomId>();
}

fn non_remove<Id: self::Id>() {
    let mut state = State::<Id>::new();
    for _ in 0..ITERATIONS {
        if state.len > 0 && state.rng.random_range(0..3) == 0 {
            state.mv();
        } else {
            state.insert();
        }
    }
    assert_eq!(state.len, state.eips.len());
}

#[test]
fn non_remove_u32() {
    non_remove::<u32>();
}

#[test]
fn non_remove_rand() {
    non_remove::<RandomId>();
}

fn all<Id: self::Id>() {
    let mut state = State::<Id>::new();
    for _ in 0..1000 {
        match state.rng.random_range(0..4) {
            0 if state.len > 1 => state.mv(),
            1 if state.len > 0 => state.remove(),
            _ => state.insert(),
        }
    }
    assert_eq!(state.len, state.eips.len());
}

#[test]
fn all_u32() {
    all::<u32>();
}

#[test]
fn all_rand() {
    all::<RandomId>();
}
