# SpaDES.tools 2.1.1

* `reproducible:::isGridded` now exported as `.isGridded` (#99);

# SpaDES.tools 2.0.9

* cleanup test files left behind in `distanceFromEachPoint`;

# SpaDES.tools 2.0.8

## Dependency changes
* drop support for R 4.2;

## Enhancements
* improved `spread` for use with `SpatRasters` (#93);
* improved speed of `.pointDistance` (#97);
* fix a failing test when using `testthat > 3.2.3` (#98);
* documentation improvements;

# SpaDES.tools 2.0.7

## Bug fixes
* fixed issue with `rasterizeReduced()`;
* updates to deal with changes to RNG in `dqrng` (#96; @rstub). if backwards compatibility is needed, set `dqrng::dqRNGkind("Xoroshiro128+")` before running `spread` to ensure numerical reproducibility with previous versions;

# SpaDES.tools 2.0.6

## Enhancements
* minor documentation updates

## Dependency changes
* drop support for R 4.1 due to changes in dependency packages

## Bug fixes
* none

# SpaDES.tools 2.0.5

## Enhancements
* `randomPolygons()` works with `sf` objects;
* use numeric vector in `crw` to accommodate downstream dependency changes;

## Dependency changes
* requires `reproducible (>= 2.0.9)`

## Bug fixes
* fixed `splitRaster()` tests that broke due to changes in `reproducible::Filenames()` (#91)

# SpaDES.tools 2.0.4

## Enhancements
* improved documentation
* address new features of `quickPlot`
* minor modifications to `crw` to allow downstream packages that use `matrix` classes instead of `sf` or `SpatVector` classes for "agents"
* address timing issue on CRAN machines

## Dependency changes
* removed Suggests dependencies `CircStats`, `dplyr`, `purrr`, `DBI`, `RColorBrewer`, `googledrive`, `snow`

## Bug fixes
* none

# SpaDES.tools 2.0.0

## Enhancements
* wholesale transition to `terra` from `raster` and `sp`. Some functionality is available for `sf`, but this will not be complete.
* `splitRaster()` uses `".tif"` as the default raster file type
* most example plotting uses `terra::plot` directly, but in a few cases, `quickPlot::Plot` is used as it is much better (e.g., `spread2(plot.it = TRUE)`)

## Dependency changes
* drop support for R 4.0 (dependency package `reproducible` supports R >= 4.1);
* now has only 7 Imports and Depends, which becomes 15 recursive dependencies (down from 40)
* add `terra` to Imports
* removed dependencies `magrittr`, `rgeos`, `rlang`, `Require`, `graphics`
* added to `Suggests`: `withr`
* moved to Suggests: `quickPlot`, `fastmatch`, `raster`, `sp`, `sf`, `CircStats`
* added `snow` to Suggests as it's used for parallel `raster` operations;
* added `tools` to Suggests for `file.ext()`

## Bugfixes
* all open issues dealt with
* fixes related to migration to `terra`
* other minor bug fixes

# SpaDES.tools 1.0.2

## Dependency changes
* none

## Enhancements
* `terra` compatibility added
* `neutralLandscapeMap` expanded to use more `NLMR` functions

## Bug Fixes
* suppress additional spurious warnings

# SpaDES.tools 1.0.1

## Enhancements
* a new function, `neutralLandscapeMap`, replaces the defunct `gaussMap` in creating a neutral landscape map using the midpoint displacement algorithm available in `NLMR::nlm_mpd`.

# SpaDES.tools 1.0.0

This is a major `SpaDES.tools` update causing breaking changes due to multiple CRAN packages no longer available as of R version 4.2 released April 2022.

## Dependency changes
* Removed dependencies `ffbase` and `ff` and deprecated use of `lowMemory` argument in `spread` (`ffbase` was removed from CRAN April 2022).
* Removed dependency `RandomFields` which is no longer maintained. We had to drop support for random landscape generation via `gaussMap()`; but see the `NLMR` package for tools to create random neutral landscapes.

## Enhancements
* `mergeRaster()` can deal with rasters with different origins extents (i.e., that were not produced using `splitRaster()`)

## Bug Fixes
* `spread2` was unable to use `maxSize` when `start` was the output of a previous `spread2` call to which `maxSize` had not been supplied. This has now been fixed.

# SpaDES.tools 0.3.10

## Dependency changes
* drop support for R 3.6 (#65)

## Enhancements
* none

## Bug Fixes
* fix error in `spread2()` causing failures on R-devel

# SpaDES.tools 0.3.9

## Dependency changes
* `Require` moved to `Suggests`, because it's only used in examples.

## Enhancements
* none

## Bug Fixes
* fixed misc CRAN check issues with Suggested packages
* fixed bug in `splitRaster()` (#62; @anbm-dk)

# SpaDES.tools 0.3.8

## Dependency changes
* Dropped support for R 3.5 due to changes in dependency packages. R versions 3.6 and higher are supported.

## Enhancements
* `distanceFromEachPoint`: `cl` argument can now accept a numeric scalar indicating how many cores to use with an `mclapply` internally
* `distanceFromEachPoint`: minor speed improvements
* `spread3` has new arguments (`sdDist`, `dispersalKernel`)
* improved speed for `spread` under most conditions, especially larger events, possibly up to 2x.
* `splitRaster()` can now specify file extension, instead of defaulting to `.grd`
* several under the hood improvements for stability, speed

## Bug Fixes
* `distanceFromEachPoint` bugfixes under some cases (some values of `toCells`, `angles` would cause errors)

# SpaDES.tools 0.3.6

## Dependency changes
* none

## Bug Fixes
* `spread` with `allowOverlap = TRUE` would give wrong results. Fixed.
* update a test to deal with forthcoming `raster` changes.

## Enhancements
* none

# SpaDES.tools 0.3.5

We have identified a non-trivial bug in `spread` when using `allowOverlap = TRUE`.
Until we can patch this bug, using this argument value will generate an error.
We are currently working on a fix, which may take some time.

## Dependency changes
* removed orphaned package `bit`; its reverse dependencies, `ff` and `ffbase`, moved to Suggests

## Bugfixes
* none

## Enhancements
* `spread` sped up for `allowOverlap = TRUE`
* New values for `returnIndices` arg in `spread`, allowing for `numeric`. New option, `2` allows for the fastest return possible, only `pixelIndices`

# SpaDES.tools 0.3.4

## Dependency changes
* none

## Bugfixes
* fix issues with failing tests on CRAN

## Enhancements
* none

# SpaDES.tools 0.3.3

## Dependency changes
* Requires R >= 3.5
* Added `animation` since it is used in the `spread3` example.
* Added `backports` for R-oldrel support
* Removed `profvis` and `microbenchmark` from Suggests: all benchmarking is removed from automated tests

## Bugfixes
* minor Bug Fixes

## Enhancements
* `spread2` speed enhancements

# SpaDES.tools 0.3.2

## Dependency changes
* Added `animation`, `fasterize`, and `sf` to suggests (used in examples)

## Bugfixes
* minor bug fixes

## Enhancements
* `spread2` now takes a numeric for `allowOverlap`, to allow for 1 more scenario of overlapping, namely, allow between event overlapping, and within event overlapping only if the overlap occurs in the same iteration, otherwise, do not allow overlap within event.
* new function `spread3` for simulating *e.g.*, insect spread
* `pointDistance` is now exported

# SpaDES.tools 0.3.1

## Dependency changes
* `RandomFields` moved to Suggests to prevent clashes with `data.table` multithreading

## Enhancements
* `randomPolygon` now has `area` argument, instead of `hectares` (deprecated), and it now succeeds for more cases.
* `mergeRaster` now accepts a function passed by the user when `mosaic` is triggered (#55, @tati-micheletti)
* `initiateAgents` example fixed to work with newer `raster` package versions (@rhijmans)

# SpaDES.tools 0.3.0

## New features
* `splitRaster` defaults to in-memory rasters when no file path specified (#47, @ianmseddy).

## Removed features
* Previously deprecated functions removed to fix CRAN check WARNINGs. These are now located in the `reproducible` package.
* `compiler` package no longer used; is automatic in R >= 3.5

# SpaDES.tools 0.2.0

## New features
* new arguments added to `gaussMap` (#20, @ianmseddy).
* move `fastMask` to `reproducible` package

## Deprecated functions
* `fastCrop()`: `raster::crop` is faster than `velox::crop` under many tests.
* `fastMask()`: moved to the `reproducible` package.

## Bug Fixes
* minor bug fixes (`spread2`)
* minor test fixes.

## Dependency changes
* `dplyr` and `mgcv` added to Suggests because they are used in tests.

# SpaDES.tools 0.1.1

* add more detailed information to package Description, per CRAN.
* add `randomPolygon` function for single random polygon.
* speed improvements with `spread2` when `asymmetry` used.
* minor bug fixes.

# SpaDES.tools 0.1.0

* A new package, which takes all auxiliary modelling functions out of the `SpaDES` package:

    - see `?SpaDES.tools::SpaDES.tools` for an overview of included functionality.
