# hilbert 0.2.1
- Added simple vignette for understanding the encoding process.
- Added return value documentation for core functions.

# hilbert 0.2.0
- Added 64-bit integer support for `index`, `position`,
  `coords_to_position`, and `position_to_coords`. If these
  functions are called with `n >= 16L`, then `bit64::integer64`
  is used in place of `base::integer` types.
- Added more unit tests for R functions.
- Added `cpp11` to `LinkingTo` field instead of vendoring.

# hilbert 0.1.0
- Added a `NEWS.md` file to track changes to the package.
- Inital version bump to 0.1.0.