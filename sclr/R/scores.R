# Score calculations
# Arseniy Khvorov
# Created 2019/11/01
# Last edit 2019/11/01

#' Score matrix (reparameterised)
#'
#' @param y Model response
#' @param x Model matrix
#' @param pars Parameter matrix
#' @param exp_Xb exp(xb) term
#'
#' @noRd
get_scores <- function(y, x, pars, exp_Xb) {
  dtheta <- get_dtheta(y, x, pars, exp_Xb)
  beta_part <- get_score_beta_part(y, x, pars, exp_Xb)
  matrix(c(dtheta, beta_part), ncol = 1)
}

#' First derivative of log-likelihood with respect to theta (reparameterised)
#'
#' @inheritParams get_scores_alt
#'
#' @noRd
get_dtheta <- function(y, x, pars, exp_Xb) {
  theta <- pars[1, ]
  dtheta <- y - invlogit(theta) + (1 - y) * exp_Xb * exp(theta) / 
    (1 + exp_Xb * (1 + exp(theta)))
  sum(dtheta)
}

#' First derivative of log-likelihood with respect to betas (reparameterised)
#'
#' @inheritParams get_scores
#'
#' @noRd
get_score_beta_part <- function(y, x, pars, exp_Xb) {
  theta <- pars[1, ]
  common <- -exp_Xb / (1 + exp_Xb) + (1 - y) * (1 + exp(theta)) * exp_Xb /
    (1 + exp_Xb * (1 + exp(theta)))
  beta_score_contr <- x * common[, 1] # Elementwise multiplication
  colSums(beta_score_contr)
}
