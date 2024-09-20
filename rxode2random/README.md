
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rxode2random

<!-- badges: start -->

[![R build
status](https://github.com/nlmixr2/rxode2random/workflows/R-CMD-check/badge.svg)](https://github.com/nlmixr2/rxode2random/actions)
[![codecov](https://codecov.io/gh/nlmixr2/rxode2random/branch/main/graph/badge.svg?token=1mPFeTxSwS)](https://app.codecov.io/gh/nlmixr2/rxode2random)
[![CRAN
version](http://www.r-pkg.org/badges/version/rxode2random)](https://cran.r-project.org/package=rxode2random)
[![CRAN total
downloads](https://cranlogs.r-pkg.org/badges/grand-total/rxode2random)](https://cran.r-project.org/package=rxode2random)
[![CRAN total
downloads](https://cranlogs.r-pkg.org/badges/rxode2random)](https://cran.r-project.org/package=rxode2random)
[![CodeFactor](https://www.codefactor.io/repository/github/nlmixr2/rxode2random/badge)](https://www.codefactor.io/repository/github/nlmixr2/rxode2random)
![r-universe](https://nlmixr2.r-universe.dev/badges/rxode2random)
<!-- badges: end -->

The goal of rxode2random is to split off the ‘rxode2’ random number
generation from the ode solving and C compilation of models.

## Installation

You can install the development version of rxode2random from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("nlmixr2/rxode2parse")
devtools::install_github("nlmixr2/rxode2random")
```

## Examples

This is mostly about random number generation so you can select a matrix
from `cvPost()`

``` r
library(rxode2random)
set.seed(5) # set the seed so that the documentation stays the same, only
cvPost(10, lotri::lotri(a+b~c(1,0.5,1)), n=3)
#> [[1]]
#>           a         b
#> a 1.7297338 0.8219411
#> b 0.8219411 1.6643538
#> 
#> [[2]]
#>           a         b
#> a 0.5663245 0.2300909
#> b 0.2300909 1.3883101
#> 
#> [[3]]
#>          a         b
#> a 2.025453 0.4974050
#> b 0.497405 0.8670084
```
