
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tarchives

<!-- badges: start -->

[![R-CMD-check](https://github.com/UchidaMizuki/tarchives/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/UchidaMizuki/tarchives/actions/workflows/R-CMD-check.yaml)
[![CRAN
status](https://www.r-pkg.org/badges/version/tarchives)](https://CRAN.R-project.org/package=tarchives)
[![Codecov test
coverage](https://codecov.io/gh/UchidaMizuki/tarchives/graph/badge.svg)](https://app.codecov.io/gh/UchidaMizuki/tarchives)
[![R
Targetopia](https://img.shields.io/badge/R_Targetopia-member-blue?style=flat&labelColor=gray)](https://wlandau.github.io/targetopia/)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

## Overview

The goal of tarchives is to make targets pipelines into a package. It
runs targets pipeline in /inst/tarchives and stores the results in the R
user directory. This means that the user does not have to run the
process repeatedly, and the developer has the flexibility to update the
data as versions are updated.

This package is a wrapper for the targets package and contains the
following functions:

- `tar_archive()`: Convert the targets function to tarchives version
- `tar_make_archive()`: tarchives version of `targets::tar_make()`
  function
- `tar_read_archive()`: tarchives version of `targets::tar_read()`
  function
- `tar_target_archive()`: tarchives version of `targets::tar_target()`
  function

## Installation

``` r
install.packages("tarchives")
```

### Development version

You can install the development version of tarchives from
[GitHub](https://github.com/) with:

``` r
install.packages("pak")
pak::pak("UchidaMizuki/tarchives")
```

## Usage

To use tarchives, run the following code under the package you are
developing:

``` r
library(tarchives)
use_tarchives()
```

This will create an `inst/tarchives` directory in your package, where
your target pipelines will be stored. You can define your target
pipeline in the `_targets.R` file in the `inst/tarchives/example-model`
directory as follows:

``` r
# inst/tarchives/example-model/_targets.R
library(targets)

list(
  tar_target(
    data,
    iris[iris$Species != "setosa", ]
  ),
  tar_target(
    model,
    lm(Sepal.Width ~ Sepal.Length, data)
  )
)
```

If you have created a package called `your-package` and saved the
pipeline to a directory called `example-model` in the `inst/tarchives`
directory, you can run it using the following command:

``` r
tar_make_archive(
  package = "your-package",
  pipeline = "example-model"
)
```

Then you can read the results using the `tar_read_archive()` function:

``` r
tar_read_archive(
  model,
  package = "your-package",
  pipeline = "your-pipeline"
)
```

    #> 
    #> Call:
    #> lm(formula = Sepal.Width ~ Sepal.Length, data = data)
    #> 
    #> Coefficients:
    #>  (Intercept)  Sepal.Length  
    #>        1.131         0.278

### Declare a target from another pipeline

You can also declare a target from another pipeline using the
`tar_target_archive()` function. For example, if you want to declare the
`data` and `model` targets from the `example-model` pipeline in your
`_targets.R` file, you can do it as follows:

``` r
library(targets)

tar_source()

list(
  tarchives::tar_target_archive(
    data,
    package = "tarchives",
    pipeline = "example-model"
  ),
  tarchives::tar_target_archive(
    model,
    package = "tarchives",
    pipeline = "example-model"
  )
)
```
