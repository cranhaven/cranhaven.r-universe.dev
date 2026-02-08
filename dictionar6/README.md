dictionar6
================

<img src="man/figures/logo.png" align="right" alt="" width="120" />

[![CRAN Status
Badge](https://www.r-pkg.org/badges/version-ago/dictionar6)](https://cran.r-project.org/package=dictionar6)
[![CRAN
Checks](https://cranchecks.info/badges/worst/dictionar6)](https://cran.r-project.org/web/checks/check_results_dictionar6.html)
[![R-CMD-check /
codecov](https://github.com/xoopR/dictionar6/actions/workflows/check-covr.yml/badge.svg?branch=main)](https://github.com/xoopR/dictionar6/actions/workflows/check-covr.yml)

[![Repo
Status](https://www.repostatus.org/badges/latest/active.svg)](https://github.com/xoopR/dictionar6)
[![Lifecycle](https://lifecycle.r-lib.org/articles/figures/lifecycle-experimental.svg)](https://github.com/xoopR/dictionar6)

[![CRAN
Downloads](https://cranlogs.r-pkg.org/badges/grand-total/dictionar6)](https://cran.r-project.org/package=dictionar6)
[![codecov](https://codecov.io/gh/xoopR/dictionar6/branch/master/graph/badge.svg)](https://codecov.io/gh/xoopR/dictionar6)
[![dependencies](https://tinyverse.netlify.com/badge/dictionar6)](https://CRAN.R-project.org/package=dictionar6)
[![License:
MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

## What is dictionar6?

**dictionar6** is an R6 dictionary interface that makes use of symbolic
representation to efficiently store R6 and non-R6 objects as symbols to
prevent issues with cloning, large object sizes, or construction
bottlenecks. The dictionary contains all ‘usual’ methods for getting and
setting elements.

## Main Features

Some main features/key use-cases of **dictionar6** includes:

-   Construction of untyped dictionaries

``` r
dct(a = 1, b = "2")
```

    ## {a: 1, b: 2}

-   Construction of typed dictionaries

``` r
dct(a = 1L, b = 2L, c = 3L, types = "integer")
```

    ## {a: 1, b: 2, c: 3}

-   Getting values

``` r
d <- dct(a = 1, b = 2)
d$has("a")
```

    ## [1] TRUE

``` r
d$get("a")
```

    ## [1] 1

``` r
d[c("a", "b")]
```

    ## $a
    ## [1] 1
    ## 
    ## $b
    ## [1] 2

-   Setting values

``` r
d <- dct(a = 1, b = 2)
d$rekey("a", "c")
d$keys
```

    ## [1] "c" "b"

``` r
d$revalue("b", 3)
d$items
```

    ## $c
    ## [1] 1
    ## 
    ## $b
    ## [1] 3

-   Safe cloning of R6 objects

``` r
r <- RClass$new(1)
r$values()
```

    ## [1] 1

``` r
d <- dct(a = r)
# default is to clone
d$get("a", clone = TRUE)$add(2)
# r is unaffected
r$values()
```

    ## [1] 1

``` r
# if we don't clone...
d$get("a", clone = FALSE)$add(2)
# r is affected
r$values()
```

    ## [1] 1 2

## Installation

For the latest release on
[CRAN](https://CRAN.R-project.org/package=dictionar6), install with

``` r
install.packages("dictionar6")
```

Otherwise for the latest stable build

``` r
remotes::install_github("xoopR/dictionar6")
```

## Use-cases

**dictionar6** is currently used in
**[param6](https://CRAN.R-project.org/package=param6)** to provide a
symbolic representation for R6 classes. This is incredibly useful to
prevent storing unnecessarily large R6 objects.
**[param6](https://CRAN.R-project.org/package=param6)** comes pre-loaded
with a
[Dictionary](https://xoopr.github.io/dictionar6/reference/Dictionary.html)
of mathematical sets where the keys are symbolic representations of the
sets and the values are the sets themselves, this allows users to refer
to R6 objects with their character representation without having to
continually construct new objects.

## Future Plans

The **dictionar6** API is still experimental and may be subject to major
changes. Currently it’s primary use-case is in
**[param6](https://CRAN.R-project.org/package=param6)**, minor or major
changes will be made to satisfy this dependency. Future development will
then focus on code quality and speed.

## Package Development and Contributing

**dictionar6** is released under the [MIT
licence](https://opensource.org/licenses/MIT). We welcome and appreciate
all [new issues](https://github.com/xoopR/dictionar6/issues) relating to
bug reports, questions and suggestions. You can also [start a
discussion](https://github.com/xoopR/dictionar6/discussions) for more
extensive feedback or feature suggestion.
