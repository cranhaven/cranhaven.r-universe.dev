#' ph2bye: A package for Phase II single-arm Bayesian design.
#'
#' The ph2bye package provides three categories of important functions:
#' PostP.design, PredP.design and MultPostP.design.
#'
#' @section Posterior probability criterion functions:
#' The posterior probability criterion functions include PostP and PostP.design functions.
#'
#' @section Predictive probability criterion functions:
#' The predictive probability criterion functions include PredP and PredP.design functions.
#'
#' @section Posterior probability criterion function for multiple outcomes:
#' The posterior probability criterion functions include MultPostP and MultPostP.design functions.
#'
#' @section Whole design function with double thresholds showing futility and efficacy boundary together:
#' The criterion function DT.design.
#'
#' @section Prior calculation function:
#' The function prior calculating Beta prior parameters accourding different prior information.
#'
#' @author Yalin Zhu <yalin.zhu@outlook.com>
#'
#' @name ph2bye-package
#' @rdname package-ph2bye
#' @aliases ph2bye
#' @docType package
#'
#' @importFrom Rcpp evalCpp
#' @useDynLib ph2bye
NULL
