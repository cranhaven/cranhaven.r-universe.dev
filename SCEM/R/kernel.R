#' @title Epanechnikov kernel
#'
#' @description Calculates the value of the Epanechnikov kernel function for any vector.
#'
#' @param v A vector of real numbers.
#'
#' @export
#'
#' @return A vector of the calculated kernel values for the input vector.
#'
#' @examples
#' x = runif(10)
#' kernel(x)
#'
#' @references
#' Epanechnikov, V. A. (1969). Non-parametric estimation of a multivariate probability density. Theory of Probability and its Applications, 14(1), 153-6.

kernel <- function(v){
  if (! is.atomic(v) || is.list(v)) {stop('v is not a vector')}
  if (any(is.na(v))) {stop('v has NA values')}
  return(0.75*(1-v^2)*(abs(v)<1))

}
