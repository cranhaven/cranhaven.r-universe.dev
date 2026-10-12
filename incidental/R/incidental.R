#' incidental: A package for computing incidence curves from delayed case records.
#'
#' @description
#' The incidental package provides an empirical Bayes estimator for fitting incidence curves
#' from a specified delay distribution, which describes the probability that a case will be
#' reported on day d after infection.
#' 
#' @keywords internal
#' 
#' @section Tutorial:
#' To learn more about incidental, start with the vignettes:
#' `browseVignettes(package = "incidental")`
#' 
#' @section Incidental functions:
#' \code{\link{fit_incidence}}: main function for fitting incidence.
#' 
#' \code{\link{incidence_to_df}}: utility function to translate output of \code{\link{fit_incidence}} into a data frame.
#' 
#' \code{\link{plot}}: plots the results of \code{\link{fit_incidence}}.
#' 
#' @section Data:
#' 
#' \code{\link{spanish_flu_delay_dist}}: incidence to death delay distribution for the Spanish Flu.
#' 
#' \code{\link{spanish_flu}}: flu mortality data for Indiana, Kansas, and Philadelphia from 1919-09-01 through 1919-12-31.
#' 
#' \code{\link{covid_delay_dist}}: incidence to recorded cases, hospitalization, and death delay distribution for COVID-19.
#' 
#' \code{\link{covid_new_york_city}}: incidence to recorded cases, hospitalization, and death for New York City for COVID-19.
#' 
#' @importFrom dlnm cr
#' @importFrom stats dpois median optim quantile rnorm sd toeplitz
#' @importFrom utils tail
#' @importFrom matrixStats rowLogSumExps
#' @importFrom numDeriv hessian
#'
#' @docType package
#' @name incidental
"_PACKAGE"
