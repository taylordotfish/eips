use super::{Allocators, Eips, Id};
use skip_list::LeafRef;
use std::fmt::{self, Debug, Display};
use std::fs::{self, File};
use std::io::{self, Write};
use std::marker::PhantomData;

#[cfg(skip_list_debug)]
use {
    super::{PosMapNode, PosMapNodeKind},
    super::{SiblingSetNode, SiblingSetNodeKind},
    skip_list::debug::{LeafDebug, State as ListDebugState},
    std::process::Command,
};

pub struct DisplayWrapper<T>(T);

impl<T: Display> Debug for DisplayWrapper<T> {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(fmt, "{}", self.0)
    }
}

#[cfg(skip_list_debug)]
impl<I> LeafDebug for PosMapNode<I>
where
    I: Id + Display,
{
    type Id = (usize, PosMapNodeKind);
    type Data = DisplayWrapper<String>;

    fn id(&self) -> Self::Id {
        (self.node().ptr().as_ptr() as _, self.kind())
    }

    fn data(&self) -> Self::Data {
        DisplayWrapper(format!(
            "{}{}",
            self.node().id(),
            match self.kind() {
                PosMapNodeKind::Marker => "[M]",
                PosMapNodeKind::Normal => "",
            },
        ))
    }
}

#[cfg(skip_list_debug)]
impl<I> LeafDebug for SiblingSetNode<I>
where
    I: Id + Display,
{
    type Id = (usize, SiblingSetNodeKind);
    type Data = DisplayWrapper<String>;

    fn id(&self) -> Self::Id {
        (self.node().ptr().as_ptr() as _, self.kind())
    }

    fn data(&self) -> Self::Data {
        DisplayWrapper(format!(
            "{}{}",
            self.node().id(),
            match self.kind() {
                SiblingSetNodeKind::Childless => "[C]",
                SiblingSetNodeKind::Normal => "",
            },
        ))
    }
}

pub struct State<I: Id + Display> {
    #[cfg(skip_list_debug)]
    pos_map_state: ListDebugState<PosMapNode<I>>,
    #[cfg(skip_list_debug)]
    sibling_set_state: ListDebugState<SiblingSetNode<I>>,
    phantom: PhantomData<I>,
}

impl<I: Id + Display> State<I> {
    pub fn new() -> Self {
        Self {
            #[cfg(skip_list_debug)]
            pos_map_state: ListDebugState::new(),
            #[cfg(skip_list_debug)]
            sibling_set_state: ListDebugState::new(),
            phantom: PhantomData,
        }
    }
}

impl<I: Id + Display> Default for State<I> {
    fn default() -> Self {
        Self::new()
    }
}

impl<I, A> Eips<I, A>
where
    I: Id + Debug + Display,
    A: Allocators,
{
    pub fn debug<S>(
        &self,
        #[allow(unused_variables)] state: &mut State<I>,
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
            write!(file, "{}:", node.node().id())?;
            if node.size() == 0 {
                writeln!(file)?;
                continue;
            }
            match items.next() {
                Some(item) => writeln!(file, " {:?}", item),
                None => writeln!(file, " [NONE]"),
            }?
        }
        file.sync_all()?;
        drop(file);

        #[cfg(skip_list_debug)]
        self.write_debug_graphs(state)?;
        Ok(())
    }

    #[cfg(skip_list_debug)]
    fn write_debug_graphs(&self, state: &mut State<I>) -> io::Result<()> {
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
