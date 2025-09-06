#' Weighted conditional Kendall's tau
#'
#' This is function computes the perturbed version of the conditional Kendall's tau.
#'
#' @param trun left truncation time satisfying \code{trun} <= \code{obs}.
#' @param obs observed failure time, must be the same length as \code{trun}, might be right-censored.
#' @param delta an optional 0-1 vector of censoring indicator (0 = censored, 1 = event) for \code{obs}.
#' If this vector is not specified, \code{cKendall} assumes no censoring and all observed failure time
#' denote events.
#' @param weights an optional perturbation weights.
#' 
#' @return A numeric value representing the weighted conditional Kendall's tau.
#'
#' @export
#' @example inst/examples/ex_wKendall.R
wKendall <- function(trun, obs, delta = NULL, weights = NULL) {
    n <- length(obs)
    if (is.null(delta)) delta <- rep(1, n)
    if (is.null(weights)) weights <- rep(1, n)
    .C("wKendallC", as.double(trun), as.double(obs), as.integer(n),
       as.double(delta), as.double(weights),
       out = double(1), PACKAGE = "tranSurv")$out
}
2
