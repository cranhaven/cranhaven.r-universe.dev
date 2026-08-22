#' @title GLMMcosinor: A package for fitting generalized linear mixed-effects
#' models with cosinor terms
#' @description
#' To learn more about how to use GLMMcosinor, see the vignettes at the
#' \href{https://docs.ropensci.org/GLMMcosinor/}{website} or using
#' the following code:
#'
#' `browseVignettes(package = "GLMMcosinor")`
#'
#' This package provides functions for fitting generalized linear mixed-effects
#' models with cosinor terms. Cosinor terms can be used to model periodic
#' variations in time series data, and are particularly useful in chronobiology
#' and sleep research. The package uses the flexible glmmTMB framework that
#' allows users to specify a wide range of data families, and provides tools
#' for visualization.
#' @keywords internal
#'
#'
"_PACKAGE"


## usethis namespace: start
## usethis namespace: end
NULL

#' @importFrom glmmTMB bell
#' @export
glmmTMB::bell
