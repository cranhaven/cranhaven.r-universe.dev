#' @title
#' The 'GPCERF' package.
#'
#' @description
#' Provides a non-parametric Bayesian framework based on Gaussian process priors
#' for estimating causal effects of a continuous exposure and detecting change
#' points in the causal exposure response curves using observational data.
#'
#' @docType package
#' @name GPCERF-package
#' @aliases GPCERF
#' @author Naeem Khoshnevis
#' @author Boyu Ren
#' @author Danielle Braun
#' @import stats
#' @import xgboost
#' @import MASS
#' @import Rcpp
#' @import SuperLearner
#' @importFrom spatstat.geom crossdist
#' @import Rfast
#' @import RcppArmadillo
#' @useDynLib GPCERF, .registration = TRUE

#'
#' @references
#' Ren, B., Wu, X., Braun, D., Pillai, N. and Dominici, F., 2021. Bayesian
#' modeling for exposure response curve via gaussian processes: Causal effects
#' of exposure to air pollution on health outcomes. arXiv preprint
#' arXiv:2105.03454.
#'
NULL
