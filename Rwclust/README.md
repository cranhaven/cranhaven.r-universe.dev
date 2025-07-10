
# Rwclust

<!-- badges: start -->

[![CRAN status](https://www.r-pkg.org/badges/version/Rwclust)](https://CRAN.R-project.org/package=Rwclust)
[![R-CMD-check](https://github.com/csprock/Rwclust/workflows/R-CMD-check/badge.svg)](https://github.com/csprock/Rwclust/actions)
[![Codecov test coverage](https://codecov.io/gh/csprock/Rwclust/branch/master/graph/badge.svg)](https://app.codecov.io/gh/csprock/Rwclust?branch=master)

<!-- badges: end -->

The goal of Rwclust is an implementation of the [random walk clustering algorithm for weighted graphs](https://www.wisdom.weizmann.ac.il/~harel/papers/Clustering_FSTTCS.pdf) by Harel and Koren. 

## Installation

You can install the latest version from CRAN with:

```r
install.packages("Rwclust")
```

Alternatively, you can install the development version from [GitHub](https://github.com/) with:

``` r
install.packages("devtools")
devtools::install_github("csprock/Rwclust")
```

## Example

For examples, see the Basic Usage vignette:
``` r
vignette("basic_usage", "Rwclust")
```
