# Shortcut math functions
# Arseniy Khvorov
# Created 2019/07/31
# Last edit 2019/10/15

#' Inverse logit function
#'
#' @param x A numeric vector
#'
#' @noRd
invlogit <- function(x) {
  1 - 1 / (1 + exp(x))
}

#' Commonly occuring exp(Xb) expression
#'
#' @param y Model response
#' @param x Model matrix
#' @param pars Parameter matrix
#'
#' @noRd
get_exp_Xb <- function(y, x, pars) exp(get_Xb(y, x, pars))

#' Result of Xb matrix multiplication
#'
#' @param y Model response
#' @param x Model matrix
#' @param pars Parameter matrix
#'
#' @noRd
get_Xb <- function(y, x, pars) {
  pars_betas <- pars[-1, ]
  x %*% pars_betas
}
