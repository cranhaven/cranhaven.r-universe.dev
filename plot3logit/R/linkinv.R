
#' Computes trinomial probability distributions implied by linear predictors
#'
#' `linkinv_cat3logit` and `linkinv_ord3logit` compute the matrix
#' \eqn{P\in[0,\,1]^{n\times 3}} of probability distributions of the
#' dependent variable for categorical and ordinal models respectively.
#' Functions `X2P_cat3logit` and `X2P_ord3logit` perform the same computation,
#' except that the design matrix \eqn{X\in\textbf{R}^{n\times k}} and
#' the coefficient matrix \eqn{B\in\textbf{R}^{k\times 2}} are taken as
#' separate input arguments. `linkinv` and `X2P` apply the proper function
#' according to the type of model passed to argument `model`.
#'
#' @inheritParams linkfun
#' @param XB object of class `matrix` (or other coercible classes) such that
#'   \eqn{XB\in\textbf{R}^{n\times 2}} for *categorical* models or
#'   \eqn{XB\in\textbf{R}^n} for *ordinal* models.
#' @param X object of class `matrix` (or other coercible classes) such that
#'   \eqn{X\in\textbf{R}^{n\times k}}.
#' @param B object of class `matrix` (or other coercible classes) such that
#'   \eqn{B\in\textbf{R}^{k\times 2}} for *categorical* models or
#'   \eqn{B\in\textbf{R}^k} for *ordinal* models.
#'
#' @returns Numeric matrix \eqn{[0,\,1]^{n\times 3}} of probability
#' distributions.
#'
#' @inherit linkfun references
#'
#' @seealso [`linkfun`].
#'
#' @name linkinv
linkinv <- function(XB, model) {
  if (model$ordinal) {
  	out <- linkinv_ord3logit(XB)
  } else {
  	out <- linkinv_cat3logit(XB)
  }
  return(out)
}



#' @rdname linkinv
#' @keywords internal
X2P <- function(X, B, model) {
  if (model$ordinal) {
  	out <- X2P_ord3logit(X, B)
  } else {
  	out <- X2P_cat3logit(X, B)
  }
  return(out)
}



#' @rdname linkinv
#' @keywords internal
linkinv_cat3logit<- function(XB) {
  if (is.vector(XB)) { XB %<>% matrix(1) }
  
  XB %>%
    as.matrix %>%
    exp %>%
    cbind(1, .) %>%
    apply(1, function(x) x / sum(x)) %>%
    t %>%
    return()
}



#' @rdname linkinv
#' @keywords internal
X2P_cat3logit <- function(X, B) { linkinv_cat3logit(X %*% B) }



#' @rdname linkinv
#' @keywords internal
linkinv_ord3logit<- function(XB, alpha) {
  outer(-XB, alpha, '+') %>%
    exp %>%
    { . / (1 + .) } %>%
    cbind(0, ., 1) %>%
    apply(1, diff) %>%
    t %>%
    return()
}



#' @rdname linkinv
#' @keywords internal
X2P_ord3logit <- function(X, B, alpha) { linkinv_ord3logit(X %*% B, alpha) }

