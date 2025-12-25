
<!-- README.md is generated from README.Rmd. Please edit that file -->

# hilbert

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/hilbert)](https://CRAN.R-project.org/package=hilbert)
[![Codecov test
coverage](https://codecov.io/gh/program--/hilbert/branch/main/graph/badge.svg)](https://app.codecov.io/gh/program--/hilbert?branch=main)
[![R-CMD-check](https://github.com/program--/hilbert/workflows/R-CMD-check/badge.svg)](https://github.com/program--/hilbert/actions)
[![MIT
License](https://img.shields.io/badge/license-MIT-blue.svg)](https://opensource.org/licenses/MIT)
<!-- badges: end -->

**hilbert** provides utilities for quick indexing/encoding of
coordinates to a Hilbert Curve.

## Installation

You can install **hilbert** via either `remotes` or `pak`:

``` r
# pak
pak::pkg_install("program--/hilbert")

# remotes
remotes::install_github("program--/hilbert")
```

## Example

### Setting Up

``` r
x <- -77.85641
y <- 34.35935
n <- 24 # n > 15 requires `bit64`
e <- c(xmax = 180, xmin = -180, ymax = 90, ymin = -90)
```

### Coordinates to Position

``` r
pos <- hilbert::coords_to_position(x, y, n = n, extent = e)
pos
#>         x        y
#> 1 4760236 11591131
```

### Position to Index

``` r
index <- hilbert::index(pos, coords = c("x", "y"), n = n, attach = TRUE)
index
#>         x        y               h
#> 1 4760236 11591131 129470580596149
```

### Index to Position

``` r
new_pos <- hilbert::position(index, idx = "h", n = n, attach = FALSE)
new_pos
#>         x        y
#> 1 4760236 11591131
```

### Position to Coordinates

``` r
new_xy <- hilbert::position_to_coords(new_pos, coords = c("x", "y"), extent = e, n = n, attach = TRUE)
new_xy
#>           x        y
#> 1 -77.85641 34.35935
```
