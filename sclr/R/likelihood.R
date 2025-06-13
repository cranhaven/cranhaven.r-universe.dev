# Likelihood-related functions
# Arseniy Khvorov
# Created 2019/07/31
# Last edit 2019/10/14

#' Log-likelihood
#'
#' Computes the log-likelihood of the scaled logit model at a given set of
#' parameter estimates (or the MLE if \code{pars} is not supplied). Either
#' \code{fit} or \code{x}, \code{y} and \code{pars} need to be supplied.
#'
#' @param fit An object returned by \code{\link{sclr}}. Or a list with
#'   parameters, x and y entries corresponding to the parameter matrix, model
#'   matrix and model response.
#' @param x Model matrix. Will be taken from \code{fit} if \code{fit} is
#'   provided.
#' @param y Model response. Will be taken from \code{fit} if \code{fit} is
#'   provided.
#' @param pars A named vector of parameter values. Will be taken from \code{fit}
#'   if \code{fit} is provided.
#'
#' @export
sclr_log_likelihood <- function(fit = NULL, x = NULL, y = NULL, pars = NULL) {
  
  if (is.null(fit)) {
    if (is.null(x) | is.null(y) | is.null(pars))
      abort("likelihood requires fit or x, y and pars")
  } else {
    if (!is_sclr(fit))
      abort("fit must be of type sclr")
    x <- model.matrix(fit)
    y <- model.response(model.frame(fit))
    pars <- coef(fit)
    if (is.null(pars)) return(NULL)
  }
  
  if (!is.matrix(pars)) {
    pars_vec <- pars
    pars <- as.matrix(pars_vec, ncol = 1)
    rownames(pars) <- names(pars_vec)
  }
  
  theta <- pars[1, ]
  
  exp_Xb <- get_exp_Xb(y, x, pars)
  
  l <- y * theta - log(1 + exp_Xb) - log(1 + exp(theta)) + 
    (1 - y) * log(1 + exp_Xb * (1 + exp(theta)))
  
  sum(l)
}
