
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rgeomstats <a href="https://lmjl-alea.github.io/rgeomstats/"><img src="man/figures/logo.png" align="right" height="138" /></a>

<!-- badges: start -->

[![check-standard](https://github.com/LMJL-Alea/rgeomstats/workflows/R-CMD-check/badge.svg)](https://github.com/LMJL-Alea/rgeomstats/actions)
[![test-coverage](https://github.com/LMJL-Alea/rgeomstats/workflows/test-coverage/badge.svg)](https://github.com/LMJL-Alea/rgeomstats/actions)
[![Codecov test
coverage](https://codecov.io/gh/LMJL-Alea/rgeomstats/branch/master/graph/badge.svg)](https://app.codecov.io/gh/LMJL-Alea/rgeomstats?branch=master)
[![pkgdown](https://github.com/LMJL-Alea/rgeomstats/workflows/pkgdown/badge.svg)](https://github.com/LMJL-Alea/rgeomstats/actions)
[![CRAN
status](https://www.r-pkg.org/badges/version/rgeomstats)](https://CRAN.R-project.org/package=rgeomstats)
<!-- badges: end -->

The goal of **rgeomstats** is to provide accessibility to the
[**Geomstats**](https://geomstats.github.io) Python library for the
community of `R` users through an R interface that mimics as closely as
possible the carefully designed Python API.

## Installation

You can install the development version of **rgeomstats** via:

``` r
# install.packages("remotes")
remotes::install_github("LMJL-Alea/rgeomstats")
```

## Example

You can instantiate the space $\mathrm{SO}(3)$ of 3D rotations and
sample random points in this space as follows:

``` r
library(rgeomstats)
so3 <- SpecialOrthogonal(n = 3)
spl <- so3$random_point(n_samples = 5)
dim(spl)
#> [1] 5 3 3
```

All Geomstats-like computations are stored in arrays. In particular,
sample IDs are always stored along the first dimension. Hence, it is
always possible to convert a sample into a list via:

``` r
purrr::array_tree(spl, margin = 1)
#> [[1]]
#>             [,1]       [,2]      [,3]
#> [1,] -0.03532813 -0.9154660 0.4008416
#> [2,]  0.68066355  0.2716386 0.6803746
#> [3,] -0.73174384  0.2968746 0.6135278
#> 
#> [[2]]
#>            [,1]          [,2]      [,3]
#> [1,] -0.3117327 -0.8280555376 0.4659901
#> [2,]  0.8305219  0.0007836864 0.5569855
#> [3,] -0.4615801  0.5606455319 0.6874739
#> 
#> [[3]]
#>            [,1]        [,2]       [,3]
#> [1,] -0.7025064  0.66878384 -0.2433370
#> [2,] -0.3513459 -0.02856766  0.9358098
#> [3,]  0.6189029  0.74290785  0.2550434
#> 
#> [[4]]
#>            [,1]       [,2]       [,3]
#> [1,] -0.8970871 -0.2258339  0.3797811
#> [2,] -0.1157827 -0.7093377 -0.6952945
#> [3,]  0.4264141 -0.6677119  0.6101900
#> 
#> [[5]]
#>            [,1]       [,2]       [,3]
#> [1,] -0.3877247 -0.3760100 -0.8415973
#> [2,]  0.4484487 -0.8746292  0.1841673
#> [3,] -0.8053343 -0.3060069  0.5077366
```
