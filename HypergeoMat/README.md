HypergeoMat
================

<!-- badges: start -->
[![R-CMD-check](https://github.com/stla/HypergeoMat/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/stla/HypergeoMat/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

``` r
library(HypergeoMat)
library(microbenchmark)
```

``` r
jhpq <- hypergeomPFQ_julia()
## Starting Julia ...
```

``` r
microbenchmark(
  Rcpp = hypergeomPFQ(m = 30, a = c(1, 2, 3), b = c(4, 5), x = c(0.1, 0.2, 0.3)),
  Julia = jhpq(m = 30, a = c(1, 2, 3), b = c(4, 5), x = c(0.1, 0.2, 0.3)),
  times = 10
)
## Unit: milliseconds
##   expr       min        lq      mean    median       uq      max neval
##   Rcpp 3171.9094 3266.6886 3386.5241 3317.9526 3531.426 3659.559    10
##  Julia  339.2297  355.8744  755.2628  377.9874  438.013 3866.369    10
```
