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

use rand::seq::IndexedRandom;
use rand::{Rng, SeedableRng};
use rand_chacha::ChaChaRng;
use std::fmt::{self, Display};

pub mod diamond;
pub mod eips;

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

pub fn show_memory_use() {
    const AWK_SCRIPT: &str = r#"
        /^Pss:/ { n += $2 }
        END { printf "memory used: %d bytes\n", n * 1024 }
    "#;
    let pid = std::process::id();
    if std::process::Command::new("awk")
        .arg(AWK_SCRIPT)
        .arg(format!("/proc/{pid}/smaps"))
        .status()
        .is_err()
    {
        eprintln!("command failed: awk");
    }
}

#[derive(Clone, Copy)]
pub enum AllowedOps {
    InsertOnly,
    All,
}

#[allow(clippy::len_without_is_empty)]
pub trait Client {
    type Update: Clone;

    fn new(client_id: u64) -> Self;
    fn len(&self) -> usize;
    fn text(&self) -> impl Iterator<Item = char> + '_;
    fn insert(&mut self, index: usize, c: char) -> Self::Update;
    fn remove(&mut self, index: usize) -> Self::Update;
    fn apply(&mut self, change: Self::Update);

    fn random_op<R: Rng>(
        &mut self,
        allowed: AllowedOps,
        rng: &mut R,
    ) -> Self::Update {
        let len = self.len();
        let r = match allowed {
            AllowedOps::InsertOnly => 1,
            AllowedOps::All => rng.random_range(0..3),
        };
        if r == 0 && len > 0 {
            self.remove(rng.random_range(0..len))
        } else {
            let c = *CHARS.choose(rng).unwrap();
            self.insert(rng.random_range(0..len + 1), c)
        }
    }

    fn display_text(&self) -> impl Display + '_ {
        struct DisplayText<'a, C: ?Sized>(&'a C);

        impl<C: Client + ?Sized> Display for DisplayText<'_, C> {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                self.0.text().try_for_each(|c| write!(f, "{c}"))
            }
        }

        DisplayText(self)
    }
}
