
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rmdl

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/shah-in-boots/rmdl/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/shah-in-boots/rmdl/actions/workflows/R-CMD-check.yaml)
[![Github commit
frequency](https://img.shields.io/github/commit-activity/w/shah-in-boots/rmdl)](https://github.com/shah-in-boots/rmdl/graphs/commit-activity)
[![test-coverage](https://github.com/shah-in-boots/rmdl/actions/workflows/test-coverage.yaml/badge.svg)](https://github.com/shah-in-boots/rmdl/actions/workflows/test-coverage.yaml)
[![CRAN
status](https://www.r-pkg.org/badges/version/rmdl)](https://CRAN.R-project.org/package=rmdl)
<!-- badges: end -->

## Installation

This package can be downloaded from CRAN or from
[Github](https://github.com/shah-in-boots/rmdl) as below

``` r
# CRAN installation
install.packages("rmdl")
# Or remote/developmental version
remotes::install_github("shah-in-boots/rmdl")
```

## Introduction

The package `rmdl` was intended as a way to handle causal- and
epidemiology-based modeling by the following principles:

1.  Role determination of variables
2.  Generativity in formula creation
3.  Multiple model management

## Usage

The package is simple to use. The `mtcars` dataset will serve as the
example, and we will use linear regressions as the primary test. This
toy example shows that we will be building six models in parallel, with
the key exposure being the **wt** term, and the two outcomes being
**mpg** and **hp**.

``` r
library(rmdl)
#> Loading required package: vctrs
#> Loading required package: tibble
#> 
#> Attaching package: 'tibble'
#> The following object is masked from 'package:vctrs':
#> 
#>     data_frame

f <- fmls(mpg + hp ~ .x(wt) + disp + cyl + am, pattern = "parallel")
m <- fit(f, .fn = lm, data = mtcars, raw = FALSE)
mt <- model_table(mileage = m)
print(mt)
#> <mdl_tbl>
#>   id        formula_index data_id name  model_call formula_call outcome exposure
#>   <chr>     <list>        <chr>   <chr> <chr>      <chr>        <chr>   <chr>   
#> 1 b4845f79… <dbl [6]>     mtcars  mile… lm         mpg ~ wt + … mpg     wt      
#> 2 7830bcbe… <dbl [6]>     mtcars  mile… lm         mpg ~ wt + … mpg     wt      
#> 3 e3cf8431… <dbl [6]>     mtcars  mile… lm         mpg ~ wt + … mpg     wt      
#> 4 9a66d0e5… <dbl [6]>     mtcars  mile… lm         hp ~ wt + d… hp      wt      
#> 5 62f89920… <dbl [6]>     mtcars  mile… lm         hp ~ wt + c… hp      wt      
#> 6 193d7c3e… <dbl [6]>     mtcars  mile… lm         hp ~ wt + am hp      wt      
#> # ℹ 7 more variables: mediator <chr>, interaction <chr>, strata <lgl>,
#> #   level <lgl>, model_parameters <list>, model_summary <list>,
#> #   fit_status <lgl>
```

## Classes

There are several important extended classes that this package
introduces, however they are primarily used for internal validation and
for shortcuts to allow more effective communication.

- `fmls` are a *version* of the base `R` formula object, but contain
  additional information and have extra features
- `tm` are atomic elements used to describe individual variables, and
  departs from how terms are generally treated in the `{stats}` package
- `mdl` and `mdl_tbl` exist primarily as *tidy* versions of class
  regression modeling

## Advanced Usage

The `{rmdl}` package is intended to be flexible, extensible, and
easy-to-use (albeit opinionated). Please see the vignettes for
additional information.
