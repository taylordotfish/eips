Changelog
=========

0.2.3
-----

* Fixed compilation when crate feature `serde` is enabled.
* Fixed behavior when the same ID is used with multiple different parent IDs.
  Previously, this resulted in crashes; now, a new error variant,
  [`ChangeError::DuplicateId`] is returned.
* Fixed regression in the quality of custom Serde error messages in version
  0.2.2.

[`ChangeError::DuplicateId`]: https://docs.rs/eips/0.2.3/eips/error/enum.ChangeError.html#variant.DuplicateId

0.2.2
-----

Note: this version fails to build when the crate feature `serde` is enabled.

* Fixed an issue where a malicious client could cause other clients to crash
  upon trying to move a particular item. As part of this change, a new error
  variant, [`ChangeError::BadMoveTimestamp`], is added. This variant supersedes
  [`ChangeError::TimestampOverflow`], which is now unused and deprecated.
* Adjusted error messages for some variants of [`ChangeError`] to make it
  clearer when an ID is included in the message.
* Documented all fields of all variants in [`ChangeError`].
* Other minor improvements to documentation wording/formatting.

[`ChangeError::BadMoveTimestamp`]: https://docs.rs/eips/0.2.2/eips/error/enum.ChangeError.html#variant.BadMoveTimestamp
[`ChangeError::TimestampOverflow`]: https://docs.rs/eips/0.2.2/eips/error/enum.ChangeError.html#variant.TimestampOverflow
[`ChangeError`]: https://docs.rs/eips/0.2.2/eips/error/enum.ChangeError.html

0.2.1
-----

* Documented time complexity of [`Eips::deserialize`].
* Other minor improvements to documentation wording/formatting.

[`Eips::deserialize`]: https://docs.rs/eips/0.2.1/eips/struct.Eips.html#method.deserialize

0.2.0
-----

Initial release.
