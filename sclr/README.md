
<!-- README.md is generated from README.Rmd. Please edit that file -->

# sclr

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/sclr)](https://cran.r-project.org/package=sclr)
[![Travis build
status](https://travis-ci.org/khvorov45/sclr.svg?branch=master)](https://travis-ci.org/khvorov45/sclr)
[![codecov](https://codecov.io/gh/khvorov45/sclr/branch/master/graph/badge.svg)](https://codecov.io/gh/khvorov45/sclr)
<!-- badges: end -->

The goal of sclr is to fit the scaled logit model from Dunning (2006)
using the maximum likelihood method. The [package
website](https://khvorov45.github.io/sclr/) contains all documentation,
vignettes and version history.

## Installation

Install the [CRAN](https://CRAN.R-project.org/package=sclr) version with

``` r
install.packages("sclr")
```

Or the development version from
[GitHub](https://github.com/khvorov45/sclr) with:

``` r
# install.packages("devtools")
devtools::install_github("khvorov45/sclr")
```

## Model

The model is logistic regression with an added parameter for the top
asymptote. For model specification, log likelihood, scores and second
derivatives see the [math
vignette](https://khvorov45.github.io/sclr/articles/sclr-math.html).
Documentation of the main fitting function `?sclr` has details on how
the model is fit.

## Example

Usage is similar to other model fitting functions like `lm`.

``` r
library(sclr)
fit <- sclr(status ~ logHI, one_titre_data) # included simulated data
summary(fit)
#> Call: status ~ logHI
#> 
#> Parameter estimates
#>       theta      beta_0  beta_logHI 
#> -0.03497876 -5.42535734  2.14877741 
#> 
#> 95% confidence intervals
#>                 2.5 %      97.5 %
#> theta      -0.1350572  0.06509969
#> beta_0     -6.4417802 -4.40893449
#> beta_logHI  1.8146909  2.48286390
#> 
#> Log likelihood: -2469.765
```

For more details see the [usage
vignette](https://khvorov45.github.io/sclr/articles/sclr-usage.html).

## References

Dunning AJ (2006). “A model for immunological correlates of protection.”
Statistics in Medicine, 25(9), 1485-1497. doi: 10.1002/sim.2282.
