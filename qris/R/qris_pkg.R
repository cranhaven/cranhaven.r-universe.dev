#' qris: Quantile Regression Model for Residual Lifetime Using an Induced Smoothing Approach
#'
#' This package offers a collection of functions to fit quantiles regression models for censored residual lifetimes. It provides various options for regression parameters estimation: the induced smoothing approach (smooth), and L1-minimization (non-smooth).  It also implements the estimation methods for standard errors of the regression parameters estimates based on an efficient partial multiplier bootstrap method and robust sandwich estimator. Furthermore, a simultaneous procedure of estimating regression parameters and their standard errors via an iterative updating procedure is implemented (iterative). See Kim, K. (2022) "Smoothed quantile regression for censored residual life", <arXiv:2205.00413>.
#' 
#' @aliases qris-packages
#' 
#' @docType package
#' @useDynLib qris
"_PACKAGE"
NULL
