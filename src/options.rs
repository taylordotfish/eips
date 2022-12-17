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

use super::node::Node;
use core::marker::PhantomData;
use fixed_typed_arena::options::{
    TypedOptions as TypedArenaOptions,
    {Bool as ArenaBool, Usize as ArenaUsize},
    {ChunkSize as ArenaChunkSize, SupportsPositions},
};

pub struct Usize<const N: usize>(());
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

pub trait ChunkSize: ChunkSizePriv {}

impl<const N: usize> ChunkSize for Usize<N> {}

impl<const N: usize> ChunkSizePriv for Usize<N> {
    const VALUE: usize = N;
    type ArenaChunkSize<T> = ArenaUsize<N>;
}

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

pub trait EipsOptions: sealed::Sealed {
    const LIST_FANOUT: usize;
    type ChunkSize: ChunkSize;
    type ResumableIter: ResumableIter;
}

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
