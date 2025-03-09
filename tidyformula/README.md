
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tidyformula

<!-- badges: start -->

[![R-CMD-check](https://github.com/damian-t-p/tidyformula/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/damian-t-p/tidyformula/actions/workflows/R-CMD-check.yaml)
[![codecov](https://codecov.io/gh/damian-t-p/tidyformula/branch/main/graph/badge.svg?token=WX5JP57DHW)](https://app.codecov.io/gh/damian-t-p/tidyformula/)
<!-- badges: end -->

`tidyformula()` translates formulas containing `tidyselect`-style
selection helpers, expanding these helpers by evaluating
`dplyr::select()` with the relevant selection helper on a supplied data
frame.

## Installation

You can install the development version of tidyformula from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("damian-t-p/tidyformula")
```

## Example

We demonstrate how to build formulas from the variables of the following
toy data frame:

``` r
library(tidyformula)

df <- data.frame(
   x1 = rnorm(5),
   x2 = rnorm(5),
   x3 = rnorm(5),
   y  = rnorm(5)
 ) 
```

The simplest usage is adding a selection of variables. The tidy-selected
variables can be combined with other variables in the formula:

``` r
tidyformula(y ~ starts_with("x") + z, data = df)
#> y ~ x1 + x2 + x3 + z
#> <environment: 0x00000202ad6c3730>
```

The selection helper can have additional arguments, as with `num_range`

``` r
tidyformula(y ~ num_range("x", 1:2) + z, data = df)
#> y ~ x1 + x2 + z
#> <environment: 0x00000202ad6c3730>
```

When the selection helper appears as the first argument of a function,
that function is distributed across the sum of the selected variables.

This works with single-argument functions

``` r
tidyformula(y ~ log(contains("x")), data = df)
#> y ~ log(x1) + log(x2) + log(x3)
#> <environment: 0x00000202ad6c3730>
```

as well as multiple-argument ones.

``` r
tidyformula(y ~ poly(contains("x"), 3), data = df)
#> y ~ poly(x1, 3) + poly(x2, 3) + poly(x3, 3)
#> <environment: 0x00000202ad6c3730>
```

The functions `+`, `-`, `*`, and `^` are not distributed by default.

``` r
tidyformula( ~ everything()*z + starts_with("x")^2, data = df)
#> ~(x1 + x2 + x3 + y) * z + (x1 + x2 + x3)^2
#> <environment: 0x00000202ad6c3730>
```

This behaviour can be overwritten with the `nodistribute` argument,
which is a character vector of functions that should not be distributed.

``` r
tidyformula( ~ everything()*z + starts_with("x")^2,
            data         = df,
            nodistribute = c("+", "-"))
#> ~x1 * z + x2 * z + x3 * z + y * z + (x1^2 + x2^2 + x3^2)
#> <environment: 0x00000202ad6c3730>
```
