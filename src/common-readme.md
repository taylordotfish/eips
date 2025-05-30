Eips is the *efficient intention-preserving sequence*. It is a sequence
CRDT with worst-case non-amortized logarithmic-time operations, minimal
memory usage, and no concurrent interleaving issues or duplications from
concurrent moves as seen in other sequence CRDTs.

Features
--------

* No interleaving of characters when multiple users insert text at the same
  position, even when text is typed in reverse (by typing a letter, moving the
  cursor back one, typing the next letter, etc.).
* Support for move operations. Items can be moved to another position in the
  sequence and will not be duplicated if multiple users try to move the same
  item concurrently.
* Constant identifier size. Changes always take the same number of bytes to
  communicate, even as the editing history grows.
* Insertions, deletions, moves, and accesses are non-amortized O(log *h*),
  where *h* is the total number of items ever inserted in the document (i.e.,
  visible items plus tombstones).
* The [`Eips`] data structure doesn’t store items directly, but rather
  translates between *local changes* (which use simple integer indices) and
  *remote changes* (which use IDs and are suitable for sending over a network).
  This means the items themselves may be stored in any plain list-like type,
  such as a simple growable array like [`Vec`] or an [unsorted counted
  B-tree][cb] like [btree-vec]. The time complexity of local operations on the
  sequence then depends only on the number of *visible* items—tombstones don’t
  cause a performance penalty.

[cb]: https://www.chiark.greenend.org.uk/~sgtatham/algorithms/cbtree.html

Serialization
-------------

When the crate feature `serde` is enabled, [`RemoteChange`] \(and types it
contains) will implement [Serde][serde]’s [`Serialize`] and [`Deserialize`]
traits.
