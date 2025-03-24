
<!-- README.md is generated from README.Rmd. Please edit that file -->

# predkmeans

[![CRAN
Status](http://www.r-pkg.org/badges/version/predkmeans)](https://cran.r-project.org/package=predkmeans)

R Package for implementing the predictive k-means method.

Clusters multivariate exposures, using a mixture of experts model to
allow covariates to influence cluster centers. Motivated by air
pollution epidemiology settings, where cluster membership needs to be
predicted across space. Includes functions for predicting cluster
membership using spatial splines and PCA scores using either multinomial
logistic regression or SVMs. For method details see Keller et al. (2017)
[doi:10.1214/16-AOAS992](https://doi.org/10.1214/16-AOAS992)
