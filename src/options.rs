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

//! Options for [`Eips`].

use super::node::Node;
#[cfg(doc)]
use super::Eips;
use core::marker::PhantomData;
use fixed_typed_arena::options::{
    TypedOptions as TypedArenaOptions,
    {Bool as ArenaBool, Usize as ArenaUsize},
    {ChunkSize as ArenaChunkSize, SupportsPositions},
};

/// Represents a [`usize`].
pub struct Usize<const N: usize>(());

/// Represents a [`bool`].
pub struct Bool<const B: bool>(());

mod detail {
    use super::*;

    pub trait ChunkSizePriv {
        const VALUE: usize;
        type ArenaChunkSize<T>: ArenaChunkSize<T>;
    }

    pub trait ResumableIterPriv {
        type SupportsPositions: SupportsPositions;
    }
}

pub(crate) use detail::*;

/// Trait bound on [`EipsOptions::ChunkSize`].
pub trait ChunkSize: ChunkSizePriv {}

impl<const N: usize> ChunkSize for Usize<N> {}

impl<const N: usize> ChunkSizePriv for Usize<N> {
    const VALUE: usize = N;
    type ArenaChunkSize<T> = ArenaUsize<N>;
}

/// Trait bound on [`EipsOptions::ResumableIter`].
pub trait ResumableIter: ResumableIterPriv {}

impl ResumableIter for Bool<false> {}
impl ResumableIter for Bool<true> {}

impl ResumableIterPriv for Bool<false> {
    type SupportsPositions = ArenaBool<false>;
}

impl ResumableIterPriv for Bool<true> {
    type SupportsPositions = ArenaBool<true>;
}

mod sealed {
    pub trait Sealed: Sized {}
}

/// Options trait for [`Eips`].
///
/// This is a sealed trait; use the [`Options`] type, which implements this
/// trait.
pub trait EipsOptions: sealed::Sealed {
    /// Eips internally uses lists implemented as tree-like structures. This
    /// associated constant controls the maximum number of children each
    /// internal node can have.
    ///
    /// Increasing this value causes less auxiliary memory to be allocated, at
    /// the cost of decreasing runtime performance.
    const LIST_FANOUT: usize;

    /// Instead of allocating small regions of memory individually, Eips
    /// allocates larger chunks and uses them to serve small allocations. This
    /// integer-like associated type controls how many small allocations can be
    /// served by each chunk.
    ///
    /// Increasing this value decreases the amount of auxiliary memory used by
    /// Eips by an amount linearly proportional to *H*, the number of items
    /// ever inserted into the sequence, but increases the worst- and
    /// average-case memory overhead by a constant amount not dependent on *H*.
    type ChunkSize: ChunkSize;

    /// Whether or not iterators returned by [`Eips::changes`] can be paused
    /// and resumed (see [`Iter::pause`]).
    ///
    /// [`Iter::pause`]: crate::iter::Iter::pause
    type ResumableIter: ResumableIter;
}

/// Options for [`Eips`].
///
/// This type implements [`EipsOptions`]. Const parameters correspond to
/// associated items in [`EipsOptions`] as follows; see those associated types
/// for documentation:
///
/// Const parameter  | Associated item
/// ---------------- | ------------------------------
/// `LIST_FANOUT`    | [`EipsOptions::LIST_FANOUT`]
/// `CHUNK_SIZE`     | [`EipsOptions::ChunkSize`]
/// `RESUMABLE_ITER` | [`EipsOptions::ResumableIter`]
#[rustfmt::skip]
pub type Options<
    const LIST_FANOUT: usize = 8,
    const CHUNK_SIZE: usize = 16,
    const RESUMABLE_ITER: bool = false,
> = TypedOptions<
    LIST_FANOUT,
    Usize<CHUNK_SIZE>,
    Bool<RESUMABLE_ITER>,
>;

/// Like [`Options`], but uses types instead of const parameters.
///
/// [`Options`] is actually a type alias of this type.
#[allow(clippy::type_complexity)]
#[rustfmt::skip]
pub struct TypedOptions<
    const LIST_FANOUT: usize = 8,
    ChunkSize = Usize<16>,
    ResumableIter = Bool<false>,
>(PhantomData<fn() -> (
    ChunkSize,
    ResumableIter,
)>);

#[rustfmt::skip]
impl<
    const LIST_FANOUT: usize,
    ChunkSize,
    ResumableIter,
> sealed::Sealed for TypedOptions<
    LIST_FANOUT,
    ChunkSize,
    ResumableIter,
> {}

#[rustfmt::skip]
impl<
    const LIST_FANOUT: usize,
    ChunkSize: self::ChunkSize,
    ResumableIter: self::ResumableIter,
> EipsOptions for TypedOptions<
    LIST_FANOUT,
    ChunkSize,
    ResumableIter,
> {
    const LIST_FANOUT: usize = LIST_FANOUT;
    type ChunkSize = ChunkSize;
    type ResumableIter = ResumableIter;
}

type GetChunkSize<Opt> = <Opt as EipsOptions>::ChunkSize;
type GetResumableIter<Opt> = <Opt as EipsOptions>::ResumableIter;

pub(crate) type NodeAllocOptions<Id, Opt> = TypedArenaOptions<
    <GetChunkSize<Opt> as ChunkSizePriv>::ArenaChunkSize<Node<Id, Opt>>,
    <GetResumableIter<Opt> as ResumableIterPriv>::SupportsPositions,
    ArenaBool<false>,
>;

pub(crate) const fn chunk_size<Opt: EipsOptions>() -> usize {
    <Opt::ChunkSize as ChunkSizePriv>::VALUE
}
