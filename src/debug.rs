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

use super::{Direction, Eips, EipsOptions, Node, Options};
use super::{PosMapNodeKind, SiblingSetNodeKind};
use skippy::LeafRef;
use std::cell::RefCell;
use std::collections::BTreeMap;
use std::fmt::{self, Debug, Display};
use std::fs::{self, File};
use std::io::{self, Write};
use std::marker::PhantomData;

// Indent for use in format strings
const I1: &str = "    ";

#[cfg(skippy_debug)]
use {
    super::{PosMapNode, SiblingSetNode},
    skippy::debug::{LeafDebug, State as ListDebugState},
    std::process::Command,
};

fn write_parent<Id: Display, Opt>(
    f: &mut fmt::Formatter<'_>,
    node: &Node<Id, Opt>,
) -> fmt::Result {
    write!(f, "{}", match node.direction() {
        Direction::Before => "←",
        Direction::After => "→",
    },)?;
    if let Some(parent) = &node.parent {
        write!(f, " {parent}")?;
    }
    Ok(())
}

#[cfg(skippy_debug)]
impl<Id, Opt> LeafDebug for PosMapNode<Id, Opt>
where
    Id: Display,
    Opt: EipsOptions,
{
    type Id = (usize, PosMapNodeKind);

    fn id(&self) -> Self::Id {
        (self.node().ptr().as_ptr() as usize, self.kind())
    }

    fn fmt_data(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}\\n", self.node().id)?;
        match self.kind() {
            PosMapNodeKind::Normal => write_parent(f, &self.node()),
            PosMapNodeKind::Marker => write!(f, "(M)"),
        }
    }
}

#[cfg(skippy_debug)]
impl<Id, Opt> LeafDebug for SiblingSetNode<Id, Opt>
where
    Id: Display,
    Opt: EipsOptions,
{
    type Id = (usize, SiblingSetNodeKind);

    fn id(&self) -> Self::Id {
        (self.node().ptr().as_ptr() as usize, self.kind())
    }

    fn fmt_data(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}\\n", self.node().id)?;
        match self.kind() {
            SiblingSetNodeKind::Normal => write_parent(f, &self.node()),
            SiblingSetNodeKind::Childless => write!(f, "(C)"),
        }
    }
}

pub struct State<Id, Opt = Options>
where
    Id: Display,
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
    Id: Display,
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
    Id: Display,
    Opt: EipsOptions,
{
    fn default() -> Self {
        Self::new()
    }
}

impl<Id, Opt> Eips<Id, Opt>
where
    Id: super::Id + Debug + Display,
    Opt: EipsOptions,
{
    pub fn debug<I>(&self, items: I) -> EipsDebug<'_, Id, Opt, I::IntoIter>
    where
        I: IntoIterator,
        I::Item: Debug,
    {
        EipsDebug {
            eips: self,
            items: RefCell::new(items.into_iter()),
        }
    }

    pub fn save_debug<I>(
        &self,
        #[allow(unused_variables)] state: &mut State<Id, Opt>,
        items: I,
    ) -> io::Result<()>
    where
        I: IntoIterator + Clone,
        I::Item: Debug,
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
        let mut iter = items.clone().into_iter();
        for node in self.pos_map.iter() {
            if node.kind() == PosMapNodeKind::Marker {
                continue;
            }
            write!(file, "{}:", node.node().id)?;
            if node.size() == 0 {
                writeln!(file, " ∅")?;
                continue;
            }
            match iter.next() {
                Some(item) => writeln!(file, " {item:?}"),
                None => writeln!(file, " [?]"),
            }?
        }
        file.sync_all()?;
        drop(file);

        #[cfg(skippy_debug)]
        self.write_skip_list_graphs(state)?;

        let mut file = File::create("debug/document.dot")?;
        write!(file, "{}", self.debug(items))?;
        file.sync_all()?;
        drop(file);

        Command::new("dot")
            .arg("-Tpng")
            .arg("-odebug/document.png")
            .arg("debug/document.dot")
            .status()?;
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

struct IdMap<T>(BTreeMap<T, usize>);

impl<T> IdMap<T> {
    pub fn new() -> Self {
        Self(BTreeMap::new())
    }
}

impl<T: Clone + Ord> IdMap<T> {
    pub fn get(&mut self, value: Option<&T>) -> usize {
        let Some(value) = value else {
            return 0;
        };
        let len = self.0.len();
        *self.0.entry(value.clone()).or_insert(len + 1)
    }
}

pub struct EipsDebug<'a, Id, Opt, I>
where
    Opt: EipsOptions,
{
    eips: &'a Eips<Id, Opt>,
    items: RefCell<I>,
}

fn write_basic_node(
    f: &mut fmt::Formatter,
    id: usize,
    num_children: &BTreeMap<usize, (usize, usize)>,
) -> fmt::Result {
    writeln!(f, "{I1}n{id} [shape=rectangle];")?;
    let count = num_children.get(&id).copied().unwrap_or_default();
    if count.0 > 1 {
        writeln!(f, "{I1}l{id} [shape=point];")?;
    }
    if count.1 > 1 {
        writeln!(f, "{I1}r{id} [shape=point];")?;
    }
    Ok(())
}

impl<Id, Opt, I> Display for EipsDebug<'_, Id, Opt, I>
where
    Opt: EipsOptions,
    Id: super::Id + Display,
    I: Iterator,
    I::Item: Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut ids = IdMap::new();
        let mut num_children = BTreeMap::<_, (usize, usize)>::new();
        num_children.insert(0, (0, 0));
        for node in &self.eips.node_alloc {
            let parent_id = ids.get(node.parent.as_ref());
            let count = num_children.entry(parent_id).or_default();
            match node.direction() {
                Direction::Before => count.0 += 1,
                Direction::After => count.1 += 1,
            }
        }

        writeln!(f, "digraph {{")?;
        for (&id, &count) in &num_children {
            if count.0 == 0 && count.1 > 0 {
                writeln!(f, "{I1}i{id} [shape=point label=\"\"];")?;
            }
        }

        write_basic_node(f, 0, &num_children)?;
        writeln!(f, "{I1}n0 [label=\"(root)\"];")?;
        for node in &*self.eips.sibling_set {
            if node.kind() == SiblingSetNodeKind::Childless {
                continue;
            }
            let node = node.node();
            let id = ids.get(Some(&node.id));
            write_basic_node(f, id, &num_children)?;

            let parent_id = ids.get(node.parent.as_ref());
            // Number of (left, right) children parent has.
            let count = num_children[&parent_id];
            writeln!(f, "{I1}{}{parent_id} -> n{id};", match (
                count,
                node.direction()
            ) {
                ((2.., _), Direction::Before) => "l",
                ((_, 2..), Direction::After) => "r",
                _ => "n",
            },)?;
        }

        for (&id, &count) in &num_children {
            if count.0 > 0 && count.1 == 0 {
                writeln!(f, "{I1}i{id} [shape=point label=\"\"];")?;
            }
            if matches!(count, (0, 1..) | (1.., 0)) {
                writeln!(f, "{I1}n{id} -> i{id};")?;
            }
            if count.0 > 1 {
                writeln!(f, "{I1}n{id} -> l{id}")?;
            }
            if count.1 > 1 {
                writeln!(f, "{I1}n{id} -> r{id}")?;
            }
        }

        let mut items = self.items.borrow_mut();
        let mut index = 0;
        for node in &*self.eips.pos_map {
            if node.kind() == PosMapNodeKind::Marker {
                continue;
            }

            let size = node.size();
            debug_assert!(size <= 1);
            let node = node.node();
            let id = ids.get(Some(&node.id));

            write!(f, "{I1}n{id} [label=\"")?;
            if size == 0 {
                write!(f, "∅")
            } else if let Some(item) = items.next() {
                write!(f, "{item:?}")
            } else {
                write!(f, "[?]")
            }?;

            write!(f, "\\n#{index} {}", match node.direction() {
                Direction::Before => "←",
                Direction::After => "→",
            },)?;

            write!(f, "\\n{}", node.id)?;
            if let Some(old) = node.old_location() {
                write!(f, "\\n{} →", old.id)?;
            }
            if let Some(new) = node.new_location() {
                write!(f, "\\n← {}", new.id)?;
            }
            writeln!(f, "\"];")?;
            index += size;
        }
        writeln!(f, "}}")
    }
}
