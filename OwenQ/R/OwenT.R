#' @title Owen T-function
#' @description Evaluates the Owen T-function.
#' @param h numeric scalar
#' @param a numeric scalar
#' @return A number between \code{0} and \code{0.25}.
#' @export
#' @importFrom Rcpp evalCpp
#' @useDynLib OwenQ
#' @export
#' @details This is a port of the function \code{owens_t} of the \strong{boost}
#' collection of C++ libraries.
#' @references
#' Owen, D. B. (1956).
#' Tables for computing bivariate normal probabilities.
#' \emph{Ann. Math. Statist.} \bold{27}, 1075-1090.
#' @examples
#' integrate(function(x) pnorm(1+2*x)^2*dnorm(x), lower=-Inf, upper=Inf)
#' pnorm(1/sqrt(5)) - 2*OwenT(1/sqrt(5), 1/3)
OwenT <- function (h, a)
{
  if (!is.numeric(a) || length(a) > 1L || is.na(a))
    stop("`a` must be a scalar number")
  if (!is.numeric(h) || length(h) > 1L || is.na(h))
    stop("`h` must be a scalar number")
  RcppOwenT(h, a)
}
