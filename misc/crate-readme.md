Eips
====

Eips is the *efficient intention-preserving sequence*. It is a sequence
CRDT with worst-case non-amortized logarithmic-time operations, minimal
memory usage, and no concurrent interleaving issues or duplications from
concurrent moves as seen in other sequence CRDTs.

Serialization
-------------

When the crate feature `serde` is enabled, [`RemoteChange`] (and types it
contains) will implement [Serde]’s [`Serialize`] and [`Deserialize`]
traits.

[`RemoteChange`]:
https://docs.rs/eips/0.2/eips/changes/struct.RemoteChange.html
[Serde]: https://docs.rs/serde/1.0/serde/
[`Serialize`]: https://docs.rs/serde/1.0/serde/trait.Serialize.html
[`Deserialize`]: https://docs.rs/serde/1.0/serde/trait.Deserialize.html
