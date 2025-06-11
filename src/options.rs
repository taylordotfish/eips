/*
 * Copyright (C) 2025 taylor.fish <contact@taylor.fish>
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

#[cfg(doc)]
use crate::Eips;
use crate::node::Node;
use core::marker::PhantomData;
use integral_constant::{Bool, Constant, Usize};

mod arena {
    pub use fixed_typed_arena::options::*;
}

mod skippy {
    pub use skippy::options::*;
}

mod detail {
    use super::*;

    pub trait ListFanoutPriv: skippy::Fanout {}

    pub trait ChunkSizePriv: Constant<usize> {
        type ForArena<T>: arena::ChunkSize<T>;
    }

    pub trait SupportsMovePriv {
        type Packed<Id, Opt: EipsOptions>: crate::node::Packed<Id, Opt>;
    }
}

pub(crate) use detail::*;

/// Trait bound on [`EipsOptions::ListFanout`].
pub trait ListFanout: ListFanoutPriv {}

impl<const N: usize> ListFanout for Usize<N> {}
impl<const N: usize> ListFanoutPriv for Usize<N> {}

/// Trait bound on [`EipsOptions::ChunkSize`].
pub trait ChunkSize: ChunkSizePriv {}

impl<const N: usize> ChunkSize for Usize<N> {}
impl<const N: usize> ChunkSizePriv for Usize<N> {
    type ForArena<T> = Self;
}

/// Trait bound on [`EipsOptions::SupportsMove`].
pub trait SupportsMove: SupportsMovePriv {}

impl SupportsMove for Bool<false> {}
impl SupportsMove for Bool<true> {}

impl SupportsMovePriv for Bool<false> {
    type Packed<Id, Opt: EipsOptions> = crate::node::MinimalPacked;
}

impl SupportsMovePriv for Bool<true> {
    type Packed<Id, Opt: EipsOptions> = crate::node::FullPacked<Id, Opt>;
}

pub(crate) type Packed<Id, Opt> =
    <<Opt as EipsOptions>::SupportsMove as SupportsMovePriv>::Packed<Id, Opt>;

mod sealed {
    pub trait Sealed: Sized {}
}

/// Options trait for [`Eips`].
///
/// This is a sealed trait; use the [`Options`] type, which implements this
/// trait.
pub trait EipsOptions: sealed::Sealed {
    /// Whether move operations are supported. If true, you can call
    /// [`Eips::mv`] to move an item to another position.
    ///
    /// The value of this option should be the same for all clients in a
    /// distributed system. A client that supports move operations cannot
    /// communicate with one that doesn't.
    ///
    /// More memory is used when this option is enabled. Each list item
    /// (including deleted elements) will use 8 to 14 bytes of additional
    /// memory, depending on the size and alignment of the ID type (due to
    /// padding).
    ///
    /// *Default:* true
    type SupportsMove: SupportsMove;

    /// Eips internally uses tree-like structures implemented using linked
    /// lists. This option controls the maximum number of children each
    /// internal node can have.
    ///
    /// Increasing this value decreases the amount of auxiliary memory used,
    /// but also decreases performance.
    ///
    /// Specifically, the amount of auxiliary memory used by Eips is
    /// Θ(*[h]*/*f*),[^1] where *f* is this value ([`Self::ListFanout`]).
    ///
    /// Thus, increasing this value decreases the amount of auxiliary memory.
    /// However, it also increases the runtime of many operations by a constant
    /// factor, since many operations are O(*f*) (with respect to *f* only),
    /// such as [`Eips::remote_get`], which is O(*f* log *[h]*).
    ///
    /// *Default:* 12
    ///
    /// [^1]: With respect to *[h]* and *f* only.
    ///
    /// [h]: Eips#mathematical-variables
    type ListFanout: ListFanout;

    /// Instead of allocating small regions of memory individually, Eips
    /// allocates larger chunks and uses them to serve small allocations. This
    /// option controls how many small allocations can be served by each chunk.
    ///
    /// Increasing this value decreases the amount of a certain category of
    /// auxiliary memory, but also increases the amount of a different
    /// category. However, this usually results in a net decrease.
    ///
    /// Specifically, the amount of auxiliary memory used by Eips is
    /// worst-case[^1] Θ(*[h]*/*c*) + Θ(*c*),[^2] where *c* is this value
    /// ([`Self::ChunkSize`]).
    ///
    /// Thus, increasing this value decreases the Θ(*[h]*/*c*) portion of the
    /// auxiliary memory, but increases the Θ(*c*) portion. However, as *h*
    /// grows, larger values of *c* typically result in a net decrease of
    /// memory.
    ///
    /// *Default:* 32
    ///
    /// [^1]: And average-case. Best-case is Θ(*[h]*/*c*) only.
    ///
    /// [^2]: With respect to *[h]* and *c* only.
    ///
    /// [h]: Eips#mathematical-variables
    type ChunkSize: ChunkSize;
}

/// Options for [`Eips`].
///
/// This type implements [`EipsOptions`]. Const parameters correspond to
/// associated types in [`EipsOptions`] as follows; see those associated types
/// for documentation:
///
/// Const parameter  | Associated type
/// ---------------- | ------------------------------
/// `SUPPORTS_MOVE`  | [`EipsOptions::SupportsMove`]
/// `LIST_FANOUT`    | [`EipsOptions::ListFanout`]
/// `CHUNK_SIZE`     | [`EipsOptions::ChunkSize`]
#[rustfmt::skip]
pub type Options<
    const SUPPORTS_MOVE: bool = true,
    const LIST_FANOUT: usize = 12,
    const CHUNK_SIZE: usize = 32,
> = TypedOptions<
    Bool<SUPPORTS_MOVE>,
    Usize<LIST_FANOUT>,
    Usize<CHUNK_SIZE>,
>;

/// Like [`Options`], but uses types instead of const parameters.
///
/// [`Options`] is actually a type alias of this type.
#[allow(clippy::type_complexity)]
#[rustfmt::skip]
pub struct TypedOptions<
    SupportsMove = Bool<true>,
    ListFanout = Usize<12>,
    ChunkSize = Usize<32>,
>(PhantomData<fn() -> (
    SupportsMove,
    ListFanout,
    ChunkSize,
)>);

#[rustfmt::skip]
impl<
    SupportsMove,
    ListFanout,
    ChunkSize,
> sealed::Sealed for TypedOptions<
    SupportsMove,
    ListFanout,
    ChunkSize,
> {}

#[rustfmt::skip]
impl<
    SupportsMove: self::SupportsMove,
    ListFanout: self::ListFanout,
    ChunkSize: self::ChunkSize,
> EipsOptions for TypedOptions<
    SupportsMove,
    ListFanout,
    ChunkSize,
> {
    type SupportsMove = SupportsMove;
    type ListFanout = ListFanout;
    type ChunkSize = ChunkSize;
}

type GetChunkSize<Opt> = <Opt as EipsOptions>::ChunkSize;
type ArenaChunkSize<Opt, T> =
    <GetChunkSize<Opt> as ChunkSizePriv>::ForArena<T>;

pub(crate) type NodeAllocOptions<Id, Opt> = arena::TypedOptions<
    /* ChunkSize */ ArenaChunkSize<Opt, Node<Id, Opt>>,
    /* SupportsPositions */ Bool<true>,
    /* Mutable */ Bool<false>,
>;

pub(crate) type PosMapOptions<Id, Opt> = skippy::TypedOptions<
    /* SizeType */ usize,
    /* StoreKeys */ Bool<false>,
    /* Fanout */ <Opt as EipsOptions>::ListFanout,
    /* Align */ Node<Id, Opt>,
>;

pub(crate) type SiblingSetOptions<Id, Opt> = skippy::TypedOptions<
    /* SizeType */ skippy::NoSize,
    /* StoreKeys */ Bool<true>,
    /* Fanout */ <Opt as EipsOptions>::ListFanout,
    /* Align */ Node<Id, Opt>,
>;

pub(crate) const fn chunk_size<Opt: EipsOptions>() -> usize {
    Opt::ChunkSize::VALUE
}
