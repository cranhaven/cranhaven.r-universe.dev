
<!-- README.md is generated from README.Rmd. Please edit that file -->

# vascr

<!-- badges: start -->

[![R-CMD-check](https://github.com/JamesHucklesby/vascr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/JamesHucklesby/vascr/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of vascr is to easily process impedance sensing data from ECIS,
xCELLigence, cellZscope and ScioSpec platforms.

## Installation

You can install the development version of vascr from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("JamesHucklesby/vascr")
```

## Example

Vascr can rapidly plot impedance sensing data.

``` r
library(vascr)
vascr_plot_line(growth.df %>% vascr_subset(unit = "R", frequency = "4000"))
```

<img src="man/figures/README-example-1.png" width="100%" />
