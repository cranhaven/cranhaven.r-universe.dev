
#' Compute the linear predictors implied by trinomial probability distributions
#'
#' Given the probability distributions \eqn{P\in[0,\,1]^{n\times 3}} of the
#' dependent variable, it computes the the values of linear predictors
#' \eqn{XB\in\textbf{R}^{n\times 2}} according to notation used in
#' \insertCite{santi2019;textual}{plot3logit}.
#'
#' @param P object of class `matrix` (or other coercible classes) such that
#'   \eqn{P\in[0,\,1]^{n\times 3}}.
#' @param alpha `numeric` vector of length two where constants \eqn{\alpha^{(1)}}
#'   and \eqn{\alpha^{(2)}} are stored (only for ordinal models), as
#'   defined in Equation (7) of \insertCite{santi2019;textual}{plot3logit}.
#' @param model object of class `field3logit`.
#'
#' @returns Numeric matrix \eqn{\textbf{R}^{n\times 2}} of linear predictors.
#'
#' @references
#' \insertAllCited{}
#'
#' @seealso [`linkinv`].
#'
#' @name linkfun
linkfun <- function(P, model) {
  if (model$ordinal) {
  	out <- linkfun_ord3logit(P)
  } else {
  	out <- linkfun_cat3logit(P)
  }
  return(out)
}



#' @rdname linkfun
#' @keywords internal
linkfun_cat3logit<- function(P) {
  if (is.vector(P)) { P %<>% matrix(1) }
  
  P %>%
    as.matrix %>%
    { .[ , 2:3, drop = FALSE] / .[1] } %>%
    log %>%
    return()
}



#' @rdname linkfun
#' @keywords internal
linkfun_ord3logit<- function(P, alpha) {
  if (is.vector(P)) { P %<>% matrix(1) }
  
  P %>%
    as.matrix %>%
    extract(, 1) %>%
    { . / (1 - .) } %>%
    log %>%
    { alpha[1] - . } %>%
    return()
}


