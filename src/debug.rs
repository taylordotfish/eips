/*
 * Copyright (C) [unpublished] taylor.fish <contact@taylor.fish>
 *
 * This file is part of Eips.
 *
 * Eips is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published
 * by the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Eips is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Eips. If not, see <https://www.gnu.org/licenses/>.
 */

use super::{Eips, EipsOptions, Id, Options, PosMapNodeKind};
use core::marker::PhantomData;
use skippy::LeafRef;
use std::fmt::{self, Debug, Display};
use std::fs::{self, File};
use std::io::{self, Write};

#[cfg(skippy_debug)]
use {
    super::{PosMapNode, SiblingSetNode, SiblingSetNodeKind},
    skippy::debug::{LeafDebug, State as ListDebugState},
    std::process::Command,
};

mod wrapper {
    pub struct DisplayWrapper<T>(pub T);
}

use wrapper::DisplayWrapper;

impl<T: Display> Debug for DisplayWrapper<T> {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(fmt, "{}", self.0)
    }
}

#[cfg(skippy_debug)]
impl<Id, Opt> LeafDebug for PosMapNode<Id, Opt>
where
    Id: self::Id + Display,
    Opt: EipsOptions,
{
    type Id = (usize, PosMapNodeKind);
    type Data = DisplayWrapper<String>;

    fn id(&self) -> Self::Id {
        (self.node().ptr().as_ptr() as _, self.kind())
    }

    fn data(&self) -> Self::Data {
        DisplayWrapper(format!(
            "{}{}",
            self.node().id,
            match self.kind() {
                PosMapNodeKind::Marker => "[M]",
                PosMapNodeKind::Normal => "",
            },
        ))
    }
}

#[cfg(skippy_debug)]
impl<Id, Opt> LeafDebug for SiblingSetNode<Id, Opt>
where
    Id: self::Id + Display,
    Opt: EipsOptions,
{
    type Id = (usize, SiblingSetNodeKind);
    type Data = DisplayWrapper<String>;

    fn id(&self) -> Self::Id {
        (self.node().ptr().as_ptr() as _, self.kind())
    }

    fn data(&self) -> Self::Data {
        DisplayWrapper(format!(
            "{}{}",
            self.node().id,
            match self.kind() {
                SiblingSetNodeKind::Childless => "[C]",
                SiblingSetNodeKind::Normal => "",
            },
        ))
    }
}

pub struct State<Id, Opt = Options>
where
    Id: self::Id + Display,
    Opt: EipsOptions,
{
    #[cfg(skippy_debug)]
    pos_map_state: ListDebugState<PosMapNode<Id, Opt>>,
    #[cfg(skippy_debug)]
    sibling_set_state: ListDebugState<SiblingSetNode<Id, Opt>>,
    phantom: PhantomData<fn() -> (Id, Opt)>,
}

impl<Id, Opt> State<Id, Opt>
where
    Id: self::Id + Display,
    Opt: EipsOptions,
{
    pub fn new() -> Self {
        Self {
            #[cfg(skippy_debug)]
            pos_map_state: ListDebugState::new(),
            #[cfg(skippy_debug)]
            sibling_set_state: ListDebugState::new(),
            phantom: PhantomData,
        }
    }
}

impl<Id, Opt> Default for State<Id, Opt>
where
    Id: self::Id + Display,
    Opt: EipsOptions,
{
    fn default() -> Self {
        Self::new()
    }
}

impl<Id, Opt> Eips<Id, Opt>
where
    Id: self::Id + Debug + Display,
    Opt: EipsOptions,
{
    pub fn debug<S>(
        &self,
        #[allow(unused_variables)] state: &mut State<Id, Opt>,
        items: S,
    ) -> io::Result<()>
    where
        S: IntoIterator,
        S::Item: Debug,
    {
        fs::create_dir_all("debug")?;
        let mut file = File::create("debug/pos_map")?;
        for node in self.pos_map.iter() {
            writeln!(file, "{node:#?}")?;
        }
        file.sync_all()?;
        drop(file);

        let mut file = File::create("debug/sibling_set")?;
        for node in self.sibling_set.iter() {
            writeln!(file, "{node:#?}")?;
        }
        file.sync_all()?;
        drop(file);

        let mut file = File::create("debug/items")?;
        let mut items = items.into_iter();
        for node in self.pos_map.iter() {
            if node.kind() == PosMapNodeKind::Marker {
                continue;
            }
            write!(file, "{}:", node.node().id)?;
            if node.size() == 0 {
                writeln!(file)?;
                continue;
            }
            match items.next() {
                Some(item) => writeln!(file, " {item:?}"),
                None => writeln!(file, " [NONE]"),
            }?
        }
        file.sync_all()?;
        drop(file);

        #[cfg(skippy_debug)]
        self.write_skip_list_graphs(state)?;
        Ok(())
    }

    #[cfg(skippy_debug)]
    fn write_skip_list_graphs(
        &self,
        state: &mut State<Id, Opt>,
    ) -> io::Result<()> {
        let mut file = File::create("debug/pos_map.dot")?;
        write!(file, "{}", self.pos_map.debug(&mut state.pos_map_state))?;
        file.sync_all()?;
        drop(file);

        let mut file = File::create("debug/sibling_set.dot")?;
        write!(
            file,
            "{}",
            self.sibling_set.debug(&mut state.sibling_set_state)
        )?;
        file.sync_all()?;
        drop(file);

        Command::new("dot")
            .arg("-Tpng")
            .arg("-odebug/pos_map.png")
            .arg("debug/pos_map.dot")
            .status()?;

        Command::new("dot")
            .arg("-Tpng")
            .arg("-odebug/sibling_set.png")
            .arg("debug/sibling_set.dot")
            .status()?;
        Ok(())
    }
}

pub struct EipsDebug<'a, Id, Opt>
where
    Opt: EipsOptions,
{
    eips: &'a Eips<Id, Opt>,
}

impl<Id, Opt> Display for EipsDebug<'_, Id, Opt>
where
    Opt: EipsOptions,
{
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        Ok(())
    }
}
