
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rwicc

<!-- badges: start -->

[![R-CMD-check](https://github.com/d-morrison/rwicc/workflows/R-CMD-check/badge.svg)](https://github.com/d-morrison/rwicc/actions)
<!-- badges: end -->

`rwicc` (“Regression With Interval-Censored Covariates”) is an R
software package implementing an analysis for a regression model
involving an interval-censored covariate, as described in “Regression
with Interval-Censored Covariates: Application to Cross-Sectional
Incidence Estimation” by Morrison, Laeyendecker, and Brookmeyer
(Biometrics, 2021):
<https://onlinelibrary.wiley.com/doi/10.1111/biom.13472>.

This analysis uses a joint model for the distributions of the outcome of
interest and the interval-censored covariate, which is treated as a
latent variable; the model parameters are estimated by maximum
likelihood using an EM algorithm. The submodel used for the distribution
of the interval-censored covariate is somewhat specific to the
application of interest (estimation of the mean duration of a
biomarker-defined window period for cross-sectional incidence
estimation), so this package may not be immediately applicable to other
problems. We are publishing it with the goal of making the results in
our paper easier to reproduce and with the hope that others might adapt
pieces of this code for their own applications. Please feel free to
[contact us](mailto:dmorrison01@ucla.edu) with any questions about the
code or the paper!

## Installation

You can install the current released version from
[CRAN](https://cran.r-project.org) with:

``` r
install.packages("rwicc")
```

You can install the development version from
[GitHub](https://github.com/) with:

``` r
install.packages("devtools")
devtools::install_github("d-morrison/rwicc")
```

## Examples of use

See: <https://d-morrison.github.io/rwicc/articles/how-to-use-rwicc.html>

## Code of Conduct

Please note that the rwicc project is released with a [Contributor Code
of Conduct](https://d-morrison.github.io/rwicc/CODE_OF_CONDUCT.html). By
contributing to this project, you agree to abide by its terms.
