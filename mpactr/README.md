
# mpactr <a href="https://www.mums2.org/mpactr/"><img src="man/figures/logo.png" align="right" height="138" alt="mpactr website" /></a>

<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- badges: start -->

[![R-CMD-check](https://github.com/mums2/mpactr/actions/workflows/r.yml/badge.svg)](https://github.com/mums2/mpactr/actions/workflows/r.yml)
[![test-coverage](https://github.com/mums2/mpactr/actions/workflows/test-coverage.yml/badge.svg)](https://github.com/mums2/mpactr/actions/workflows/test-coverage.yml)
[![lint](https://github.com/mums2/mpactr/actions/workflows/lintr.yml/badge.svg)](https://github.com/mums2/mpactr/actions/workflows/lintr.yml)
[![pkgdown](https://github.com/mums2/mpactr/actions/workflows/pkgdown.yaml/badge.svg)](https://github.com/mums2/mpactr/actions/workflows/pkgdown.yaml)
<!-- badges: end -->

## Overview

mpactr is a collection of filters for the purpose of identifying high
quality MS1 features by correcting peak selection errors introduced
during the pre-processing of tandem mass spectrometry data.

Filters in this package address the following issues:

- `filter_mispicked_ions()`: removal of mispicked peaks, or those
  isotopic patterns that are incorrectly split during preprocessing.
- `filter_group()`: removal of features overrepresented in a specific
  group of samples; for example removal of features present in solvent
  blanks due to carryover between samples.
- `filter_cv()`: removal of non-reproducible features, or those that are
  inconsistent between technical replicates.
- `filter_insource_ions()`: removal of fragment ions created during the
  first ionization in the tandem MS/MS workflow.

All filters are independent, meaning they can be used to create a
project-specific workflow, or you can learn more in [the Getting Started
page](https://www.mums2.org/mpactr/articles/mpactr.html).

## Installation

You can install the CRAN version with:

``` r
install.packages("mpactr")
```

You can install the development version of mpactr from
[GitHub](https://github.com/mums2/mpactr) with:

``` r
# install.packages("devtools")
devtools::install_github("mums2/mpactr")
```

## Get started

See the [Getting
Started](https://www.mums2.org/mpactr/articles/mpactr.html) page to get
started.

## Getting help

If you encounter an issue, please file an issue on
[GitHub](https://github.com/mums2/mpactr/issues). Please include a
minimal reproducible example with your issue.

## Contributing

Is there a feature you’d like to see included, please let us know! Pull
requests are welcome on [GitHub](https://github.com/mums2/mpactr/pulls).
