# mlmodels: Maximum Likelihood Models for R

<!-- badges: start -->
[![R-CMD-check](https://github.com/alfisankipan/mlmodels/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/alfisankipan/mlmodels/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

**mlmodels** provides a consistent and flexible framework for maximum likelihood estimation in R. It includes a wide range of models with a unified S3 interface, support for modeling scale parameters (heteroskedasticity), rich post-estimation tools, and excellent compatibility with the `marginaleffects` package.

## Key Features

- Consistent interface across models: `ml_lm()`, `ml_logit()`, `ml_probit()`, `ml_poisson()`, `ml_negbin()`, `ml_gamma()`, `ml_beta()`, etc.
- Flexible modeling of the **scale parameter** (variance, dispersion, precision, or shape) alongside the mean.
- Rich `predict()` method with many output types (response, mean, variance, probabilities, etc.).
- Multiple variance-covariance estimators: OIM, OPG, robust, cluster-robust, bootstrap, and jackknife.
- Comprehensive hypothesis testing: Wald, likelihood ratio, information matrix, Vuong, overdispersion, and goodness-of-fit tests.
- Full compatibility with [`marginaleffects`](https://marginaleffects.com/) for marginal effects and predictions.

## Installation

You can install the development version from GitHub:

```r
# install.packages("devtools")
devtools::install_github("alfisankipan/mlmodels")
```

(The package will soon be available on CRAN.)

## Documentation

* [Getting Started](https://alfisankipan.github.io/mlmodels/articles/mlmodels-basics.html)
* [Count Data Models](https://alfisankipan.github.io/mlmodels/articles/mlmodels-countintro.html)
* [Diagnostic Tools](https://alfisankipan.github.io/mlmodels/articles/mlmodels-diagnostics.html)
* [Fractional Response Outcomes](https://alfisankipan.github.io/mlmodels/articles/mlmodels-fractional.html)
* [Gamma and Lognormal Models](https://alfisankipan.github.io/mlmodels/articles/mlmodels-gamma-lognormal.html)
* [Predictions and Marginal Effects](https://alfisankipan.github.io/mlmodels/articles/mlmodels-predictions.html)
* [Variance Estimators and Inference](https://alfisankipan.github.io/mlmodels/articles/mlmodels-variance.html)

## Quick Example

```r
library(mlmodels)

data("mroz")

fit <- ml_logit(inlf ~ age + I(age^2) + huswage + educ + unem, 
                data = mroz)

summary(fit, vcov.type = "robust")
```

## Acknowledgements

This package builds on the excellent [maxLik](https://github.com/arne-henningsen/maxLik) package by Arne Henningsen and others for the underlying optimization engine.