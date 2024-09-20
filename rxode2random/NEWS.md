# rxode2random 2.1.0

- **Breaking Change** changed distributions from the standard C++
  `<random>` to `boost::random`.  Since this is not dependent on the
  compiler, it makes the random numbers generated from Mac, Windows
  and Linux the same for every distribution.  Unfortunately with a new
  random number transformation, the simulation results will likely be
  different than they were before.  The exception to this is the
  uniform number, which was always the same between platforms.

# rxode2random 2.0.13

- Fixed formatting issues (as requested by CRAN and identified on `m1mac`)

# rxode2random 2.0.12

- Added function `dfWishart` which gives (by simulation) an
  approximation of the degrees of freedom of a Wishart to match a
  `rse` value.

- Added function `swapMatListWithCube` which swaps omegaList with
  omegaCube values

- Ensure that the outputs are integers (instead of long integers) as
  requested by CRAN for some checking functions.

# rxode2random 2.0.11

- Fix qassert LTO

# rxode2random 2.0.10

- Moved fast factor to `rxode2parse` to allow `etTrans` to be moved there

# rxode2random 2.0.9

* Initial release of `rxode2random`, which separates the parallel
  safe, random number generation from 'rxode2' into a separate package to
  reduce 'rxode2' compilation time. This should make CRAN maintenance
  a bit easier.

* Added a `NEWS.md` file to track changes to the package.
