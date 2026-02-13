#' metapack: a package for Bayesian meta-analysis and network meta-analysis
#' 
#' The metapack package provides one category of functions:
#' bayes.parobs and bayes.nmr
#' 
#' @section Multivariate Meta-Regression function:
#' The bayes.parobs function fits the multivariate meta-regression model
#' with partially observed sample covariance matrix to the given data.
#' 
#' @section Network Meta-Regression function:
#' The bayes.nmr function fits the network meta-regression model with 
#' heavy-tailed random effects distribution to the given data.
#' 
#' @docType package
#' @name metapack
#' @useDynLib metapack, .registration = TRUE
#' @importFrom Rcpp evalCpp
NULL