#' msce: A package to compute hazard and survival function for 
#' Multi-Stage Clonal Expansion models
#'
#' Functions \code{\link{tsce}} and \code{\link{msce_numerical}} are provided
#' to compute hazard and survival function for the Two- and the more general
#' Multi-Stage Clonal Expansion model.
#' Models can be evaluated for time-dependent parameters.
#' However, parameters are assumed to be constant within the time intervals
#' specified by the user.
#' To allow parameter fits on many strata or cohort members, the models are
#' implemented with RcppParallel.
#' For details, see the package vignette.
#' 
#'
#' @docType package
#' @name msce
#' @useDynLib msce, .registration=TRUE
#' @importFrom Rcpp evalCpp
#' @importFrom RcppParallel RcppParallelLibs
NULL