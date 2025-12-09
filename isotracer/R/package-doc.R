### * Needed for correct NAMESPACE generation by Roxygen (for Rcpp functions)

# See https://ironholds.org/blog/adding-rcpp-to-an-existing-r-package-documented-with-roxygen2/
# and http://r-pkgs.had.co.nz/src.html

# Some bits obtained from running devtools::use_rcpp() and others from the
# skeleton package built using rstantools.

#' The 'isotracer' package
#'
#' The isotracer package allows modelling of fluxes across a network of
#' compartments. Parameters are estimated using a Bayesian MCMC approach.
#'
#' @name isotracer-package
#' @aliases isotracer
#' @useDynLib isotracer, .registration = TRUE
#' @import methods
#' @import Rcpp
#' @importFrom Rcpp sourceCpp
#' @importFrom rstan sampling
#'
#' @references
#' López-Sepulcre, A., M. Bruneaux, S. M. Collins, R. El-Sabaawi,
#' A. S. Flecker, and S. A. Thomas. The American Naturalist
#' (2020). "A New Method to Reconstruct Quantitative Food Webs and
#' Nutrient Flows from Isotope Tracer Addition Experiments."
#' https://doi.org/10.1086/708546.
#' 
#' Stan Development Team (2018). RStan: the R interface to Stan. R package
#' version 2.18.2. https://mc-stan.org
#'
"_PACKAGE"
