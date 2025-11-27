#' Estimation of the generic form of the models
#'
#' This function computes the parameter estimates of the generic form of the models by using penalized JML estimation. It allows users to adjust the default settings of the estimation.
#'
#' @param X Input dataset as matrix or data frame with ordinal responses (starting from 0); rows represent individuals, column represent items.
#' @param init_par Initial values of the estimated parameters.
#' @param setting Parameter settings which are listed in \code{\link[autoRasch:autoRaschOptions]{autoRaschOptions()}}.
#'
#' @return
#' \item{X}{   The dataset that is used for estimation.}
#' \item{name}{   The name of each items in the dataset.}
#' \item{mt_vek}{   A vector of the highest response category as many as the number of items.}
#' \item{loglik}{   The log likelihood of the estimation.}
#' \item{objtype}{   Type of the model that is used.}
#' \item{delta}{   A vector of the DIF parameters of each items on each groups.}
#' \item{gamma}{   A vector of the natural logarithm of discrimination parameters of each items.}
#' \item{beta}{   A vector of the difficulty parameter of each items' categories (thresholds).}
#' \item{theta}{   A vector of the ability parameters of each individuals.}
#'
#' @details
#' In the discrimination parameters estimation, instead of estimating the discrimination parameters,
#' we are estimating the natural logarithm of the parameters to avoid negative values, \eqn{\alpha = exp(\gamma)}.
#'
#'
#' @export
generic_model <- function(X, init_par = c(), setting = c()){

  return(pjmle(X = X, init_par = init_par, setting = setting))

}



