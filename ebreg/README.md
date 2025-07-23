
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ebreg

<!-- badges: start -->

<!-- badges: end -->

The goal of ebreg is to implement a Bayesian-like approach to the
high-dimensional sparse linear regression problem based on an empirical
or data-dependent prior distribution, which can be used for
estimation/inference on the model parameters, variable selection, and
prediction of a future response.

## Installation

You can install the released version of ebreg from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("ebreg")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("antang93/Empirical-Bayes")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(ebreg)
#> Loading required package: lars
#> Loaded lars 1.2
## basic example code
n <- 70
p <- 100
beta <- rep(1, 5)
s0 <- length(beta)
sig2 <- 1
d <- 1
log.f <- function(x) -x * (log(1) + 0.05 * log(p)) + log(x <= n)
X <- matrix(rnorm(n * p), nrow=n, ncol=p)
X.new <- matrix(rnorm(p), nrow=1, ncol=p)
y <- as.numeric(X[, 1:s0] %*% beta[1:s0]) + sqrt(sig2) * rnorm(n)

res<-ebreg(y, X, X.new, TRUE, alpha=.99, gam=.005, NULL, FALSE, igpar=c(0.01, 4), log.f, M=5000, TRUE, FALSE, .95)
```

<img src="man/figures/README-example-1.png" width="100%" />
