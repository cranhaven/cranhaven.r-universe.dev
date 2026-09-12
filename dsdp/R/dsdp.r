#' dsdp: Density Estimation using Semidefinite Programming
#'
#' Density estimation with Semidefinite Programming.
#' The models of probability density functions are Gaussian or exponential
#' distributions with polynomial correction terms. Using a maximum likelihood
#' method, it computes parameters of Gaussian or exponential distributions
#' together with degrees of polynomials by a grid search, and coefficients
#' of polynomials by a variant of semidefinite programming.
#' It adopts Akaike Information Criterion for model selection.
#' See vignettes for tutorials and more information.
#' @docType package
#' @name dsdp
#' @useDynLib dsdp, .registration=TRUE
NULL
#> NULL