
<!-- README.md is generated from README.Rmd. Please edit that file -->

# SpotSampling

[![Build
Status](https://travis-ci.org/EstherEustache/SpotSampling.svg?branch=master)](https://travis-ci.org/github/EstherEustache/SpotSampling)

<!-- badges: start -->

<!-- badges: end -->

In spatial data, information of two neighboring units are generally very
similar. For spatial sampling, it is therefore more efficient to select
samples that are well spread out in space. Often, the interest lies not
only in estimating a measure at one point in time, but rather in
estimating several points in time to also study evolution. Three new
methods called ORFS (Optimal Rotation with Fixed sample Size), ORSP
(Optimal Rotation with Spread sample), and SPOT (Spatial and Optimally
Temporal Sampling) are implemented in this package. ORFS allows to
select temporal samples with fixed size. ORSP select spatio-temporal
samples with random size that are well spread out in space at each point
in time. And SPOT generates spread samples with fixed sample size at
each wave.

## Installation

You can install the released version of SpotSampling from
[CRAN](https://CRAN.R-project.org) with:

    install.packages("SpotSampling")

You can install the latest version of SpotSampling with:

    devtools::install_github("EstherEustache/SpotSampling")

## Example

This is a basic example which shows you how to solve a common problem.
We first consider 20 plots with spatial coordinates and 5 different
points in time. Coordinates are generated randomly with function
`runif`:

``` r
library(SpotSampling)
N     <- 40
T     <- 3
coord <- cbind(x = runif(N), y = runif(N))
head(coord,5)
#>              x          y
#> [1,] 0.1290909 0.69662789
#> [2,] 0.2026614 0.24602977
#> [3,] 0.3183634 0.20790377
#> [4,] 0.5754210 0.08155243
#> [5,] 0.5846721 0.79485289
```

Matrix `pik` of size (20x5) contains temporal inclusion probabilities
for each unit. Columns of  correspond to the waves, and rows correspond
to the units. Inclusion probabilities can be totally unequal. We choose
to generate equal probabilities for all waves (with function `rep`):

``` r
pik <- matrix(rep(0.2, N*T), nrow = N, ncol = T)
head(pik,5)
#>      [,1] [,2] [,3]
#> [1,]  0.2  0.2  0.2
#> [2,]  0.2  0.2  0.2
#> [3,]  0.2  0.2  0.2
#> [4,]  0.2  0.2  0.2
#> [5,]  0.2  0.2  0.2
```

So, we can use the SPOT method with the function `Spot`:

``` r
S <- Spot(pik = pik, coord = coord)
#> 
#> 
#> Beginning of the SPOT method.
#> --------------------------------
#> - Time number 1
#> Sample selection time: 0.1403811
#> - Time number 2
#> Sample selection time: 0.03091693
#> - Time number 3
#> Sample selection time: 0.01894999
#> 
#> Landing phase required for 5 units.
```

We can plot the selected sample at each wave:

<img src="man/figures/README-pressure-1.png" width="100%" /><img src="man/figures/README-pressure-2.png" width="100%" /><img src="man/figures/README-pressure-3.png" width="100%" />
