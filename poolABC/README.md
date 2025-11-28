
# poolABC

<!-- badges: start -->

[![](https://zenodo.org/badge/564786207.svg)](https://zenodo.org/badge/latestdoi/564786207)

<!-- badges: end -->

The goal of `poolABC` is to help users perform model choice and
parameter inference using Pool-seq data integrated into an Approximate
Bayesian Computation (ABC) framework. This package provides functions to
simulate Pool-seq data under models of ecotype formation and to import
Pool-seq data from real populations. Two ABC algorithms are implemented
to perform parameter estimation and model selection using Pool-seq data.
Cross-validation can also be performed to assess the accuracy of ABC
estimates and calculate model misclassification.

## Installation

You can install the development version of `poolABC` from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("joao-mcarvalho/poolABC")
```

Alternatively, you can install the current stable version of `poolABC`
from CRAN:

``` r
install.packages("poolABC")
```
