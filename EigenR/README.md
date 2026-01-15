EigenR
================

<!-- badges: start -->

[![R-CMD-check](https://github.com/stla/EigenR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/stla/EigenR/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

Originally, I entitled this package *Fast Matrix Algebra with ‘Eigen’*,
because I expected it to be faster than R base. But this is not the
case. So I entitled it *Complex Matrix Algebra with ‘Eigen’*, because it
supports some operations on complex matrices which are not supported by
R base: determinant, Cholesky decomposition, and linear least-squares
problems.

``` r
library(EigenR)
library(microbenchmark)
```

## Determinant

``` r
set.seed(666L)
M <- matrix(rnorm(300L*300L, mean = 1), 300L, 300L)
M[sample.int(300L*300L, 300L*270L)] <- 0 # 90% of zeros
Ms <- asSparseMatrix(M)
microbenchmark(
  base          = det(M),
  EigenR        = Eigen_det(M),
  EigenR_sparse = Eigen_det(Ms), # :-(
  times = 200L
)
## Unit: milliseconds
##           expr    min      lq     mean  median       uq     max neval cld
##           base 6.9588 7.90125 9.475266 8.90305 10.01820 24.8761   200 a  
##         EigenR 2.4274 3.20710 3.913163 3.68080  4.44480  7.0813   200  b 
##  EigenR_sparse 4.9384 5.73910 6.760806 6.36535  7.52195 12.4549   200   c
```

Determinants of complex matrices are supported:

``` r
set.seed(666L)
Mr <- matrix(rnorm(100L*100L, mean = 1), 100L, 100L)
Mi <- matrix(rnorm(100L*100L, mean = 1), 100L, 100L)
M <- Mr + 1i * Mi
library(complexplus)
microbenchmark(
  EigenR      = Eigen_det(M), # :-)
  complexplus = Det(M), 
  times = 30L
)
## Unit: microseconds
##         expr     min      lq     mean   median      uq     max neval cld
##       EigenR   744.8   879.6  1152.00  1017.95  1281.3  2035.1    30  a 
##  complexplus 16028.3 17791.2 19376.53 19287.40 20139.6 27735.5    30   b
```

## Inverse matrix

``` r
set.seed(666L)
M <- matrix(rnorm(100L*100L), 100L, 100L)
microbenchmark(
  base   = solve(M),
  EigenR = Eigen_inverse(M), # :-)
  times = 500L
)
## Unit: microseconds
##    expr    min      lq     mean  median      uq     max neval cld
##    base 1470.2 1699.55 2779.189 2191.10 2829.25 24528.5   500  a 
##  EigenR  952.3 1060.65 1907.809 1360.75 1884.10 34256.2   500   b
```

## Pseudo-inverse matrix

``` r
set.seed(666L)
M <- matrix(rnorm(100L*70L), 100L, 70L)
library(MASS)
library(pracma)
microbenchmark(
  MASS   = ginv(M),
  pracma = pinv(M),
  EigenR = Eigen_pinverse(M), # :-)
  times = 500L
)
## Unit: microseconds
##    expr    min      lq     mean  median      uq     max neval cld
##    MASS 2982.4 3380.50 4535.176 3882.15 5018.55 34714.0   500  a 
##  pracma 2935.6 3372.45 4470.872 3875.75 4960.50 43498.4   500  a 
##  EigenR  663.2  928.75 1229.255 1014.30 1421.85  8672.9   500   b
```

## Cholesky decomposition

``` r
set.seed(666L)
M <- matrix(rpois(10000L, 25), 100L, 100L)
A <- crossprod(M)
microbenchmark(
  base   = chol(A),
  EigenR = Eigen_chol(A), 
  times = 1000L
)
## Unit: microseconds
##    expr   min     lq     mean median     uq    max neval cld
##    base 179.2 232.70 285.8752 243.35 281.55 7893.7  1000   a
##  EigenR 132.2 185.25 267.9837 200.15 260.15 8256.3  1000   a
```

Cholesky decomposition of complex matrices is supported.

## Pivoted Cholesky decomposition

``` r
set.seed(666L)
M <- matrix(rgamma(202L*199L, 10), 202L, 199L)
M <- cbind(M, M[, 1L] + 3*M[, 2L])
A <- crossprod(M)
microbenchmark(
  base   = chol(A, pivot = TRUE),
  EigenR = Eigen_UtDU(A), # :-)
  times = 1000L
)
## Unit: microseconds
##    expr    min      lq     mean  median      uq     max neval cld
##    base 1449.0 1759.35 2258.477 1993.10 2475.95 13592.7  1000  a 
##  EigenR  695.9 1002.00 1344.194 1195.45 1554.65 10752.2  1000   b
```

Pivoted Cholesky decomposition of complex matrices is supported.

## Kernel

``` r
set.seed(666L)
M <- matrix(rpois(10000L, 25), 100L, 100L)
A <- cbind(M, M)
At <- t(A)
library(MASS)
microbenchmark(
  MASS       = Null(At),
  EigenR_LU  = Eigen_kernel(A, method = "LU"),  # :-)
  EigenR_COD = Eigen_kernel(A, method = "COD"), 
  times = 100L
)
## Unit: milliseconds
##        expr    min      lq      mean  median       uq      max neval cld
##        MASS 7.6587 9.73635 12.529576 10.4438 11.49775 154.0598   100 a  
##   EigenR_LU 1.6315 2.04355  2.782136  2.8114  3.25520   6.6788   100  b 
##  EigenR_COD 3.7744 5.05930  6.742621  6.4258  7.34365  20.8925   100   c
```

## Linear least-squares problems

``` r
set.seed(666L)
n <- 700L; p <- 200L
A <- matrix(rnorm(n * p), n, p)
b <- rnorm(n)
microbenchmark(
  stats       = lm.fit(A, b),
  EigenR_svd  = Eigen_lsSolve(A, b, method = "svd"), #  :-(
  EigenR_cod  = Eigen_lsSolve(A, b, method = "cod"), #  :-)
  times = 100L
)
## Unit: milliseconds
##        expr     min       lq     mean   median       uq      max neval cld
##       stats 26.2811 28.80175 32.88743 32.01755 36.12695  48.3139   100 a  
##  EigenR_svd 46.7250 53.45475 62.50637 59.51725 71.55925 116.0388   100  b 
##  EigenR_cod  9.4010 10.84750 13.85632 12.80600 15.69025  45.6681   100   c
```

Complex matrices `A` and `b` are supported.

## Exponential

``` r
set.seed(666L)
M <- matrix(rnorm(40L*40L, mean = 1), 40L, 40L)
microbenchmark(
  expm   = expm::expm(M),
  EigenR = Eigen_exp(M), # :-)
  times = 500L
)
## Unit: microseconds
##    expr   min      lq      mean  median     uq     max neval cld
##    expm 992.7 1315.20 1654.1064 1387.95 1646.0 11770.1   500  a 
##  EigenR 214.2  236.85  308.5246  261.65  305.9  2188.7   500   b
```

Exponential of complex matrices is supported:

``` r
set.seed(666L)
Mr <- matrix(rnorm(40L*40L, mean = 1), 40L, 40L)
Mi <- matrix(rnorm(40L*40L, mean = 1), 40L, 40L)
M <- Mr + 1i * Mi
library(complexplus)
microbenchmark(
  complexplus = matexp(M), 
  EigenR      = Eigen_exp(M), # :-)
  times = 500L
)
## Unit: microseconds
##         expr    min      lq      mean  median       uq      max neval cld
##  complexplus 7037.9 8469.85 11674.841 10282.9 13375.75 196628.8   500  a 
##       EigenR  934.4 1137.00  1652.693  1630.9  1945.55  10435.8   500   b
```

## Schur decomposition

``` r
set.seed(666L)
M <- matrix(rnorm(200L*200L, mean = 1), 200L, 200L)
library(Matrix)
microbenchmark(
  Matrix = Schur(M), 
  EigenR = Eigen_realSchur(M), # :-)
  times = 50L
)
## Unit: milliseconds
##    expr     min      lq      mean   median       uq      max neval cld
##  Matrix 89.2812 91.9326 107.39999 101.5227 120.2015 153.9915    50  a 
##  EigenR 69.7336 74.0251  87.22316  78.9152  92.7015 139.6560    50   b
```

Use `Eigen_complexSchur` for the complex Schur decomposition.
