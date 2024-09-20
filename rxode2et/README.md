
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rxode2et

<!-- badges: start -->

[![R build
status](https://github.com/nlmixr2/rxode2et/workflows/R-CMD-check/badge.svg)](https://github.com/nlmixr2/rxode2et/actions)
[![codecov.io](https://codecov.io/gh/nlmixr2/rxode2et/coverage.svg)](https://app.codecov.io/gh/nlmixr2/rxode2et)
[![CRAN
version](http://www.r-pkg.org/badges/version/rxode2et)](https://cran.r-project.org/package=rxode2et)
[![CRAN total
downloads](https://cranlogs.r-pkg.org/badges/grand-total/rxode2et)](https://cran.r-project.org/package=rxode2et)
[![CRAN total
downloads](https://cranlogs.r-pkg.org/badges/rxode2et)](https://cran.r-project.org/package=rxode2et)
[![CodeFactor](https://www.codefactor.io/repository/github/nlmixr2/rxode2et/badge)](https://www.codefactor.io/repository/github/nlmixr2/rxode2et)
![r-universe](https://nlmixr2.r-universe.dev/badges/rxode2et)
<!-- badges: end -->

The goal of rxode2et is to split off the ‘rxode2’ event table creation
and translation for ‘rxode2’ models.

## Installation

You can install the development version of rxode2et from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
#devtools::install_github("nlmixr2/rxode2ll") # not needed for rxode2et, but needed for 'rxode2'
devtools::install_github("nlmixr2/rxode2parse")
devtools::install_github("nlmixr2/rxode2random")
devtools::install_github("nlmixr2/rxode2et")
```

## Examples

This is mostly about event table creation `et()`, which you can access
with this package alone.

``` r
library(rxode2et)
e <- et(amt=3) %>% et(1:24)
print(e)
#> ── EventTable with 25 records ──
#> 1 dosing records (see $get.dosing(); add with add.dosing or et)
#> 24 observation times (see $get.sampling(); add with add.sampling or et)
#> ── First part of : ──
#> # A tibble: 25 × 3
#>     time   amt evid         
#>    <dbl> <dbl> <evid>       
#>  1     0     3 1:Dose (Add) 
#>  2     1    NA 0:Observation
#>  3     2    NA 0:Observation
#>  4     3    NA 0:Observation
#>  5     4    NA 0:Observation
#>  6     5    NA 0:Observation
#>  7     6    NA 0:Observation
#>  8     7    NA 0:Observation
#>  9     8    NA 0:Observation
#> 10     9    NA 0:Observation
#> # … with 15 more rows
```
