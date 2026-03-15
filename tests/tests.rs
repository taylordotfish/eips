/*
 * Copyright (C) 2025-2026 taylor.fish <contact@taylor.fish>
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

use rand::Rng;
#[allow(dead_code)]
mod common;
use btree_vec::BTreeVec;
use eips::Eips;

#[test]
fn get() {
    const NUM_INSERT: usize = 1000;
    const NUM_REMOVE: usize = 300;
    const NUM_MOVE: usize = 250;

    let mut rng = common::make_rng();
    let mut items = BTreeVec::<u128, 16>::create();
    let mut eips = Eips::<u128>::new();

    for i in 0..NUM_INSERT {
        let id = rng.random();
        let index = rng.random_range(0..i + 1);
        let change = eips.insert(index, id).unwrap();
        assert_eq!(change.id, id);
        eips.apply_change(change).unwrap();
        items.insert(index, id);
    }

    let mut removed = Vec::new();
    for i in 0..NUM_REMOVE {
        let index = rng.random_range(0..NUM_INSERT - i);
        let change = eips.remove(index).unwrap();
        eips.apply_change(change).unwrap();
        let id = items.remove(index);
        assert_eq!(change.id, id);
        removed.push(id);
    }

    const LEN: usize = NUM_INSERT - NUM_REMOVE;
    for _ in 0..NUM_MOVE {
        let old = rng.random_range(0..LEN);
        let new = rng.random_range(0..LEN);
        let id = rng.random();
        let change = eips.mv(old, new, id).unwrap();
        assert_eq!(change.id, id);
        eips.apply_change(change).unwrap();
        let old_id = items.remove(old);
        items.insert(new, old_id);
    }

    // These changes are intentionally not applied.
    for _ in 0..NUM_REMOVE {
        let index = rng.random_range(0..LEN);
        let change = eips.remove(index).unwrap();
        assert_eq!(change.id, items[index]);
    }

    assert_eq!(eips.len(), LEN);
    assert_eq!(items.len(), LEN);
    for (i, id) in items.iter().copied().enumerate() {
        assert_eq!(eips.get(i).unwrap(), id);
        assert_eq!(eips.remote_get(&id).unwrap(), Some(i));
        let (change, index) = eips.get_change(&id).unwrap();
        assert_eq!(change.id, id);
        assert_eq!(index, Some(i));
    }
    assert!(eips.get(LEN).is_err());
    assert!(eips.remote_get(&rng.random()).is_err());
    assert!(eips.get_change(&rng.random()).is_err());
    for id in removed {
        assert_eq!(eips.remote_get(&id).unwrap(), None);
        let (change, index) = eips.get_change(&id).unwrap();
        assert_eq!(change.id, id);
        assert_eq!(index, None);
    }
}

#[test]
fn malicious_move() {
    use std::num::NonZeroU64;
    let mut eips = Eips::<(u64, u64)>::new();
    for i in 0..100 {
        let change = eips.insert(i, (0, i as _)).unwrap();
        eips.apply_change(change).unwrap();
    }
    for i in 0..10 {
        let change = eips.mv(i * 10, 100 - i * 10 - 5, (1, i as _)).unwrap();
        eips.apply_change(change).unwrap();
    }
    for i in 0..100 {
        let change = eips.mv(i, (i * 2) % 100, (1, (i + 10) as _)).unwrap();
        let mut bad = change;
        bad.move_info.as_mut().unwrap().timestamp = NonZeroU64::new(match i {
            20 => u64::MAX,
            30 => u64::MAX - 1,
            40 => u64::MAX / 2,
            _ => (111 + i) as _,
        })
        .unwrap();
        assert!(eips.apply_change(bad).is_err());
        eips.apply_change(change).unwrap();
    }
}

#[test]
fn duplicate_id() {
    let mut eips = Eips::<(u64, u64)>::new();
    let c1 = eips.insert(0, (0, 0)).unwrap();
    let c2 = eips.insert(0, (1, 0)).unwrap();
    eips.apply_change(c1).unwrap();
    eips.apply_change(c2).unwrap();

    let c3 = eips.insert(1, (2, 0)).unwrap();
    let c4 = eips.insert(2, (2, 0)).unwrap();
    eips.apply_change(c3).unwrap();
    assert!(eips.apply_change(c4).is_err());
}
