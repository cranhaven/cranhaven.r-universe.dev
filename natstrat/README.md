
<!-- README.md is generated from README.Rmd. Please edit that file -->

# natstrat

<!-- badges: start -->

[![R-CMD-check](https://github.com/kkbrum/natstrat/workflows/R-CMD-check/badge.svg)](https://github.com/kkbrum/natstrat/actions)
[![Codecov test
coverage](https://codecov.io/gh/kkbrum/natstrat/branch/master/graph/badge.svg)](https://app.codecov.io/gh/kkbrum/natstrat?branch=master)
<!-- badges: end -->

Natural strata can be used in observational studies to balance the
distributions of many covariates across any number of treatment groups
and any number of comparisons. These strata have proportional amounts of
units within each stratum across the treatments, allowing for simple
interpretation and aggregation across strata. Within each stratum, the
units are chosen using randomized rounding of a linear program that
balances many covariates.

## Installation

You can install the released version from CRAN with:

``` r
install.packages("natstrat")
```

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("kkbrum/natstrat", build_vignettes = TRUE)
```

## Usage

To learn about how to use this package, please see the associated
vignette with:

``` r
browseVignettes(package = "natstrat")
```
