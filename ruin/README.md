
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ruin

[![Travis-CI Build
Status](https://travis-ci.org/irudnyts/ruin.svg?branch=master)](https://travis-ci.org/irudnyts/ruin)
[![AppVeyor Build
Status](https://ci.appveyor.com/api/projects/status/github/irudnyts/ruin?branch=master&svg=true)](https://ci.appveyor.com/project/irudnyts/ruin)

## Overview

The intention of the package is to provide simulation methods of common
risk processes in a framework of ruin theory. Each model is implemented
as an S4 class, having a simulator of its path, and a plotting function.
Further, a Monte-Carlo estimator of a ruin probability for a finite time
is implemented, using a parallel computation. Currently, the package
extends two classical risk models, namely, Cramer-Lundberg and Sparre
Andersen models by including capital injections (positive jumps).

## Installation

The package is not yet submitted to CRAN. Instead, you can install
`ruin` from github with:

``` r
# install.packages("devtools")
devtools::install_github("irudnyts/ruin")
```

## Example

``` r
library(ruin)
#> Set default RNG to L'Ecuyer-CMRG for a safe parallel simulation.

model <- CramerLundberg(
  initial_capital = 10,
  premium_rate = 1,
  claim_poisson_arrival_rate = 1,
  claim_size_generator = rexp,
  claim_size_parameters = list(rate = 1)
)

ruin_probability(model = model, time_horizon = 10, return_paths = FALSE)
#> $ruin_probability
#> lower_bound    estimate upper_bound 
#>  0.03692248  0.04080000  0.04467752
```
