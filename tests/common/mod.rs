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
 *
 * The Eips Lesser Network Exception and Eips Peer-to-Peer Exception
 * also apply to Eips. These exceptions are additional permissions
 * under section 7 of the GNU Affero General Public License, version 3.
 * You should have received a copy of these exceptions; if not, see
 * <https://codeberg.org/taylordotfish/eips-exceptions>.
 */

use btree_vec::BTreeVec;
use eips::{Eips, LocalChange, RemoteChange};
use rand::seq::IndexedRandom;
use rand::{Rng, SeedableRng};
use rand_chacha::ChaChaRng;
use std::fmt::{self, Display};

#[rustfmt::skip]
pub static CHARS: [char; 280] = [
    '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E',
    'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T',
    'U', 'V', 'W', 'X', 'Y', 'Z', 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i',
    'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x',
    'y', 'z', 'À', 'Á', 'Â', 'Ã', 'Ä', 'Å', 'Æ', 'Ç', 'È', 'É', 'Ê', 'Ë', 'Ì',
    'Í', 'Î', 'Ï', 'Ð', 'Ñ', 'Ò', 'Ó', 'Ô', 'Õ', 'Ö', '×', 'Ø', 'Ù', 'Ú', 'Û',
    'Ü', 'Ý', 'Þ', 'ß', 'à', 'á', 'â', 'ã', 'ä', 'å', 'æ', 'ç', 'è', 'é', 'ê',
    'ë', 'ì', 'í', 'î', 'ï', 'ð', 'ñ', 'ò', 'ó', 'ô', 'õ', 'ö', '÷', 'ø', 'ù',
    'ú', 'û', 'ü', 'ý', 'þ', 'ÿ', 'ぁ', 'あ', 'ぃ', 'い', 'ぅ', 'う', 'ぇ',
    'え', 'ぉ', 'お', 'か', 'が', 'き', 'ぎ', 'く', 'ぐ', 'け', 'げ', 'こ',
    'ご', 'さ', 'ざ', 'し', 'じ', 'す', 'ず', 'せ', 'ぜ', 'そ', 'ぞ', 'た',
    'だ', 'ち', 'ぢ', 'っ', 'つ', 'づ', 'て', 'で', 'と', 'ど', 'な', 'に',
    'ぬ', 'ね', 'の', 'は', 'ば', 'ぱ', 'ひ', 'び', 'ぴ', 'ふ', 'ぶ', 'ぷ',
    'へ', 'べ', 'ぺ', 'ほ', 'ぼ', 'ぽ', 'ま', 'み', 'む', 'め', 'も', 'ゃ',
    'や', 'ゅ', 'ゆ', 'ょ', 'よ', 'ら', 'り', 'る', 'れ', 'ろ', 'ゎ', 'わ',
    'ゐ', 'ゑ', 'を', 'ん', 'ゔ', 'ゕ', 'ゖ', '😀', '😁', '😂', '😃', '😄',
    '😅', '😆', '😇', '😈', '😉', '😊', '😋', '😌', '😍', '😎', '😏', '😐',
    '😑', '😒', '😓', '😔', '😕', '😖', '😗', '😘', '😙', '😚', '😛', '😜',
    '😝', '😞', '😟', '😠', '😡', '😢', '😣', '😤', '😥', '😦', '😧', '😨',
    '😩', '😪', '😫', '😬', '😭', '😮', '😯', '😰', '😱', '😲', '😳', '😴',
    '😵', '😶', '😷', '😸', '😹', '😺', '😻', '😼', '😽', '😾', '😿', '🙀',
    '🙁', '🙂', '🙃',
];

fn make_seed() -> u64 {
    if cfg!(miri) {
        return 10501891656698302774;
    }
    std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .unwrap()
        .as_micros() as u64
}

pub fn make_rng() -> ChaChaRng {
    let seed = std::env::var("EIPS_SEED")
        .ok()
        .and_then(|s| s.parse().ok())
        .unwrap_or_else(make_seed);
    println!("seed: {seed}");
    ChaChaRng::seed_from_u64(seed)
}

pub type ClientId = u64;

#[derive(Clone, Copy, Debug, Eq, PartialEq, Ord, PartialOrd)]
struct Id {
    client_id: ClientId,
    counter: u64,
}

impl Id {
    fn increment(&mut self) -> Self {
        let id = *self;
        self.counter += 1;
        id
    }
}

pub struct Client {
    eips: Eips<Id>,
    text: BTreeVec<char, 32>,
    next_id: Id,
}

#[derive(Clone, Copy, Debug)]
pub struct Update {
    change: RemoteChange<Id>,
    item: Option<char>,
}

#[derive(Clone, Copy)]
pub struct AllowedOps {
    pub remove: bool,
    pub mv: bool,
}

impl AllowedOps {
    pub const INSERT_ONLY: Self = Self {
        remove: false,
        mv: false,
    };

    pub const ALL: Self = Self {
        remove: true,
        mv: true,
    };
}

impl Client {
    pub fn new(client_id: ClientId) -> Self {
        Self {
            eips: Eips::new(),
            text: Default::default(),
            next_id: Id {
                client_id,
                counter: 0,
            },
        }
    }

    pub fn text(&self) -> &BTreeVec<char, 32> {
        &self.text
    }

    pub fn insert(&mut self, index: usize, c: char) -> Update {
        let id = self.next_id.increment();
        Update {
            change: self.eips.insert(index, id).expect("bad index"),
            item: Some(c),
        }
    }

    pub fn remove(&mut self, index: usize) -> Update {
        Update {
            change: self.eips.remove(index).expect("bad index"),
            item: None,
        }
    }

    pub fn mv(&mut self, old: usize, new: usize) -> Update {
        let id = self.next_id.increment();
        Update {
            change: self.eips.mv(old, new, id).expect("bad index"),
            item: None,
        }
    }

    pub fn random_op<R: Rng>(
        &mut self,
        allowed: AllowedOps,
        rng: &mut R,
    ) -> Update {
        let len = self.text().len();
        let rand_min = 2 - (allowed.remove as u32) - (allowed.mv as u32);
        let r = if rand_min < 2 {
            rng.random_range(rand_min..4)
        } else {
            2
        };
        match r {
            0 if allowed.mv && len > 1 => {
                self.mv(rng.random_range(0..len), rng.random_range(0..len))
            }
            1 if allowed.remove && len > 0 => {
                self.remove(rng.random_range(0..len))
            }
            _ => {
                let c = *CHARS.choose(rng).unwrap();
                self.insert(rng.random_range(0..len + 1), c)
            }
        }
    }

    pub fn apply(&mut self, update: Update) {
        let item = update.item;
        match self.eips.apply_change(update.change).expect("bad remote change")
        {
            LocalChange::Insert(index) => {
                self.text
                    .insert(index, item.expect("insertion should have item"));
            }
            LocalChange::Remove(index) => {
                debug_assert!(item.is_none());
                self.text.remove(index);
            }
            LocalChange::Move {
                old,
                new,
            } => {
                debug_assert!(item.is_none());
                let c = self.text.remove(old);
                self.text.insert(new, c);
            }
            LocalChange::AlreadyApplied => {}
            LocalChange::None => {
                debug_assert!(item.is_none());
            }
        }
    }

    pub fn updates(&self) -> impl Iterator<Item = Update> + '_ {
        self.eips.changes().map(|(change, i)| Update {
            change,
            item: i.map(|i| self.text[i]),
        })
    }

    pub fn clone(&self, new_id: ClientId) -> Self {
        Self {
            eips: self.eips.clone(),
            text: self.text.clone(),
            next_id: Id {
                client_id: new_id,
                counter: 0,
            },
        }
    }

    pub fn display_text(&self) -> impl Display + '_ {
        struct DisplayText<'a>(&'a BTreeVec<char, 32>);

        impl Display for DisplayText<'_> {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                self.0.iter().copied().try_for_each(|c| write!(f, "{c}"))
            }
        }

        DisplayText(&self.text)
    }
}
