#' convert a pair of simple logistic regression coefficients into P(Y|T) curve:

#'
#' @param coefs numeric vector of coefficients
#' @return function(t) P(Y=1|T=t)
#' @importFrom arm invlogit
build_phi_function_from_coefs <- function(coefs) {
  function(x) arm::invlogit(coefs[1] + coefs[2] * x)
}


#' compute mean window period duration from simple logistic regression coefficients

#' @param theta numeric vector of coefficients
#' @return numeric scalar: mean window period duration
compute_mu <- function(theta) {
  365 * -log(exp(theta[1]) + 1) / theta[2]
}
