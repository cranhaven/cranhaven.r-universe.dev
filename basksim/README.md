
<!-- README.md is generated from README.Rmd. Please edit that file -->

# basksim

<!-- badges: start -->

[![R-CMD-check](https://github.com/lbau7/basksim/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/lbau7/basksim/actions/workflows/R-CMD-check.yaml)
[![codecov](https://codecov.io/gh/lbau7/basksim/branch/master/graph/badge.svg?token=AVO4V52BTH)](https://app.codecov.io/gh/lbau7/basksim)
<!-- badges: end -->

## Overview

`basksim` calculates the operating characteristics of different basket
trial designs based on simulation.

## Installation

Install the development veresion with:

``` r
# install.packages("devtools")
devtools::install_github("lbau7/basksim")
```

## Usage

With `basksim` you can calculate the operating characteristics such as
rejection probabilities and mean squared error of single-stage basket
trials with different designs.

At first, you have to create a design-object using a setup-function. For
example to create a design-object for Fujikawa’s design (Fujikawa et
al., 2020):

``` r
library(basksim)
design <- setup_fujikawa(k = 3, shape1 = 1, shape2 = 1, p0 = 0.2)
```

`k` is the number of baskets, `shape1` and `shape2` are the shape
parameters of the Beta-prior of the response probabilities of each
baskets and `p0` is the response probability that defines the null
hypothesis.

Use `get_details` to estimate several important operating
characteristics:

``` r
get_details(
  design = design,
  n = 20,
  p1 = c(0.2, 0.5, 0.5),
  lambda = 0.95,
  epsilon = 1.5,
  tau = 0,
  iter = 5000
)

# $Rejection_Probabilities
# [1] 0.3448 0.9772 0.9764
# 
# $FWER
# [1] 0.3448
# 
# $Mean
# [1] 0.2781905 0.4795914 0.4789913
# 
# $MSE
# [1] 0.014837404 0.008647713 0.008620234
# 
# $Lower_CL
# [1] 0.1395151 0.3341910 0.3336988
# 
# $Upper_CL
# [1] 0.4262371 0.6252845 0.6245943
```
