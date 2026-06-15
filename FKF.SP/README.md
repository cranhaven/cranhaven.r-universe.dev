
# FKF.SP

<!-- badges: start -->
<!-- badges: end -->

Fast and flexible Kalman filtering and smoothing implementation utilizing sequential processing, designed for efficient parameter estimation through maximum likelihood estimation or expectation-maximization. Sequential processing is a univariate treatment of a multivariate series of observations and can benefit from computational efficiency over traditional Kalman filtering when independence is assumed in the variance of the disturbances of the measurement equation. Sequential processing is described in the textbook of Durbin and Koopman (2001, ISBN:978-0-19-964117-8). 'FKF.SP' was built upon the existing 'FKF' package and is, in general, a faster Kalman filter.

## Installation

You can install the released version of FKF.SP from [CRAN](https://CRAN.R-project.org) with:

```
install.packages("FKF.SP")
```

And the development version from [GitHub](https://github.com/) with:

```
devtools::install_github("TomAspinall/FKF.SP")
```
which contains source code for the package starting with version 0.1.0.
