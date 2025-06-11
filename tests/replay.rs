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

use rand::Rng;
#[allow(dead_code)]
mod common;
use common::{AllowedOps, Client, Update};

const ITERATIONS: usize = 500;

fn random_op<R: Rng>(
    client: &mut Client,
    allowed: AllowedOps,
    rng: &mut R,
) -> Update {
    let update = client.random_op(allowed, rng);
    client.apply(update);
    update
}

fn random_ops<R: Rng>(
    client: &mut Client,
    allowed: AllowedOps,
    rng: &mut R,
) -> Vec<Update> {
    (0..ITERATIONS).map(|_| random_op(client, allowed, rng)).collect()
}

fn replay_common(allowed: AllowedOps) -> Client {
    let mut rng = common::make_rng();
    let mut client1 = Client::new(rng.random_range(0..100) * 4);
    let updates1 = random_ops(&mut client1, allowed, &mut rng);

    let mut client2 = Client::new(rng.random_range(0..100) * 4 + 1);
    for &update in &updates1 {
        client2.apply(update);
    }
    assert!(client1.text() == client2.text());
    let updates2 = random_ops(&mut client2, allowed, &mut rng);
    for &update in &updates2 {
        client1.apply(update);
    }
    assert!(client1.text() == client2.text());

    let mut client3 = Client::new(rng.random_range(0..100) * 4 + 2);
    for update in updates1.into_iter().chain(updates2) {
        client3.apply(update);
    }
    assert!(client1.text() == client3.text());
    client2
}

#[test]
fn replay_insert() {
    replay_common(AllowedOps::INSERT_ONLY);
}

#[test]
fn replay_all() {
    let client1 = replay_common(AllowedOps::ALL);
    let mut client2 = Client::new(500);
    for update in client1.updates() {
        client2.apply(update);
    }
    assert!(client1.text() == client2.text());

    let mut client3 = Client::new(501);
    for update in client2.clone(502).updates() {
        client3.apply(update);
    }
    assert!(client1.text() == client3.text());
}
