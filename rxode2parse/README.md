
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rxode2parse

<!-- badges: start -->

[![R build
status](https://github.com/nlmixr2/rxode2parse/workflows/R-CMD-check/badge.svg)](https://github.com/nlmixr2/rxode2parse/actions)
[![codecov.io](https://codecov.io/gh/nlmixr2/rxode2parse/coverage.svg)](https://app.codecov.io/gh/nlmixr2/rxode2parse)
[![CRAN
version](http://www.r-pkg.org/badges/version/rxode2parse)](https://cran.r-project.org/package=rxode2parse)
[![CRAN total
downloads](https://cranlogs.r-pkg.org/badges/grand-total/rxode2parse)](https://cran.r-project.org/package=rxode2parse)
[![CRAN total
downloads](https://cranlogs.r-pkg.org/badges/rxode2parse)](https://cran.r-project.org/package=rxode2parse)

[![CodeFactor](https://www.codefactor.io/repository/github/nlmixr2/rxode2parse/badge)](https://www.codefactor.io/repository/github/nlmixr2/rxode2parse)
![r-universe](https://nlmixr2.r-universe.dev/badges/rxode2parse)
<!-- badges: end -->

The goal of rxode2parse is to split off the ‘rxode2’ parsing from the
ode solving and C compilation of models. This can allow the rxode2 ui to
be in a R only package (which is easier to update and check).

## Installation

You can install the development version of rxode2parse from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("nlmixr2/rxode2parse")
```

## Examples

This is mostly to parse an `rxode2` model and generate information about
it; eventually it will produce code but here it simply generates model
variables:

``` r
rxode2parse("a=3")
```
