#' @description
#' fctbases is a fast and easy implementation of functional bases in R. Simply initialize the desired basis, which returns function of class \code{fctbasis}.
#' fctbases provides a convenient way of this.
#'
#' @details Internally, functions are stored as C++ objects, which are masked by the package.
#' The package maintains the bookkeeping of fctbasis objects. Parameters are validated at initialization which also reduces some of the overhead.
#' fctbases objects cannot be saved across sessions and must be re-initialised.
#'
#' Derivatives are provided. These are the mathematically correct ones and are as fast as the non-derivatives.
#'
#' @seealso \link{Functional basis function}
#'
"_PACKAGE"
#> [1] "_PACKAGE"

#' Functional basis function
#'
#' @description A fctbases object is a function of class \code{fctbases} which takes three arguments \code{(t, x, deriv)}
#'
#' @param t time points
#' @param x vector of coefficients (optional)
#' @param deriv Should the derivative be used and which order? Defaults to \code{FALSE}
#'
#' @details If \code{deriv} is zero or \code{FALSE}, the function itself is evaluated.
#' If \code{deriv} is one or \code{TRUE}, the first derivative is evaluated.
#' If \code{deriv} is two, the second derivative is evaluated.
#'
#' The dimension of \code{x} must match the number of basis functions.
#'
#' @return Returns a matrix of dimension \code{length(t)} times no. of bases if \code{x} is missing.
#' If \code{x} is provided and is a vector, it returns a vector of same length as \code{t}.
#' If \code{x} is provided and is a matrix, it returns a matrix of dimension \code{length(t)} times \code{ncol(x)}
#'
#' @examples
#' ## Create basis (here a b spline)
#' bf <- make.bspline.basis(knots = 0:12/12)
#'
#' ## Use a functional basis
#'
#' bf(0.2)
#' tt <- seq(0,1, length = 50)
#' bf(tt) ## evaluates bf in tt
#' bf(tt, deriv = TRUE) ## evaluates derivative of bf in tt
#'
#' ## Apply bf to some coefficients
#' set.seed(661)
#' x <- runif(15)
#' bf(tt, x) ## Evaluate bf in tt with coefficients x.
#'
#' bf(0.2, deriv = 2) ## Second derivative.
#' bf(0.2, x, deriv = 2) ## Second derivative with coefficients x.
#'
#' @name Functional basis function
NULL


#' Functional basis info
#'
#' @param fctbasis object of class \code{fctbasis}
#'
#' @description This function returns details about a functional basis.
#'
#' @return A named list including no. of basis, type of basis, and additional information.
#' @export
#'
object.info <- function(fctbasis) {
  describe_object(environment(fctbasis)$basis)
}
