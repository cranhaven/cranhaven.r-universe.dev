<!-- badges: start -->

[![R-CMD-check](https://github.com/bpollner/uniset/workflows/R-CMD-check/badge.svg)](https://github.com/bpollner/uniset/actions) [![codecov](https://codecov.io/gh/bpollner/uniset/branch/master/graph/badge.svg?token=QK8GPB9XLM)](https://app.codecov.io/gh/bpollner/uniset?branch=master) [![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/uniset)](https://cran.r-project.org/package=uniset) [![metacran downloads](https://cranlogs.r-pkg.org/badges/grand-total/uniset)](https://cran.r-project.org/package=uniset) <img src="man/figures/logo.png" align="right" height="139"/>

<!-- badges: end -->

# uniset

Package `uniset` provides an easily accessible, user-friendly text file as settings-file for your R-package.

## Summary

-   Package `uniset` enables any package (the 'target package') to provide its users an easily accessible, user-friendly and human readable text file where key=value pairs (used by functions defined in the target package) can be saved.
-   This settings file lives in a location defined by the user of the target package, and its **user-defined values remain unchanged** even when the author of the target package is introducing or deleting keys, or when the target package is updated or re-installed.
-   In order to enable the target package to make use of the functionality offered by package `uniset`, four files have to be exported by `uniset` and be placed into the target package.

## Installation

Install release version from CRAN via

``` r
install.packages("uniset") 
```

Or download from github:

``` r
library(devtools)
install_github(repo="bpollner/uniset", ref="master")
```

Please go to <https://bpollner.github.io/uniset/> to learn how to use `uniset`.
