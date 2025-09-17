
#' Make fourier basis
#'
#' @param range Left and right end points.
#' @param order Order of harmonics
#' @param use.trig.id Use trigonometrical identities with this function?
#'
#' @details The number of basis elements (degrees of freedom) is 2 * order + 1.
#'
#' The basis functions are ordered [1, sin(t), cos(t), sin(2t), cos(2t), ...]
#'
#' Using trigonometrical identities is faster, but introduces (negligible) round-off errors.
#'
#' @return Function of class "fctbasis"
#' @export
#'
#' @examples
#' ## A fourier basis with period 1 and 11 basis functions.
#' bf <- make.fourier.basis(c(0,1), order = 5)
#'
#' @seealso \link{Functional basis function}
#'
make.fourier.basis <- function(range, order, use.trig.id = FALSE) {
  basis <- init_fourier_basis(range, order, use.trig.id)
  basis.to.function(basis)
}

#' Make polynomial basis
#'
#' @param order Order of polynomial (= degree + 1)
#'
#' @details The polynomial basis is ordered [1, t, t^2, t^3, ..., t^n]
#'
#' @return Function of class "fctbasis"
#' @export
#'
#' @seealso \link{Functional basis function}
#'
#' @examples
#' ## A four-degree polynomial
#' mypol <- make.pol.basis(order = 5)
#'
make.pol.basis <- function(order) {

  basis <- init_pol_basis(order)
  basis.to.function(basis)
}

#' Make B-spline basis
#'
#' @param knots Knots of the basis, including endpoints
#' @param order Spline order. Defaults to 4.
#'
#' @return Function of class "fctbasis"
#' @export
#'
#' @seealso \link{Functional basis function}, \link{make.std.bspline.basis}
#'
#' @examples
#' ## B-spline with equidistant knots with 13 basis function
#' bf <- make.bspline.basis(knots = 0:10, order = 4)
#'
#' ## B-spline of order 2 (ie. a linear approximation) with some uneven knots
#' bf <- make.bspline.basis(knots = c(-1.3, 0, 0.5, 0.7, 1.1), order = 2)
#'
make.bspline.basis <- function(knots, order = 4) {

  deg <- order - 1
  if (order < 1) stop("Spline order must be positive!")
  basis.to.function(init_bspline(order, c(rep(knots[1], deg), knots)))
}

basis.to.function <- function(basis) {
  f <- function(t, x, deriv = FALSE) {
    if (missing(x)) {
      if (deriv > 1L) cpp_eval_D2(basis, t)
      else if (deriv) cpp_eval_D(basis, t)
      else cpp_eval_0(basis, t)
    }
    else {
      if (deriv > 1L) cpp_eval_D2_coefs(basis, t, x)
      else if (deriv) cpp_eval_Dcoefs(basis, t, x)
      else cpp_eval_coefs(basis, t, x)
    }
  }
  class(f) <- "fctbasis"
  f
}

#' 'Standard' B-spline basis
#'
#' @param range End points of spline
#' @param intervals Number of intervals
#'
#' @description This initializes a bspline of order 4 with uniformly places knots. df = intervals + 3.
#'
#' @details \code{make.std.bspline.basis} uses a different internal implementation than \code{make.bspline.basis},
#' but is not conclusively faster.
#'
#' @return function
#' @export
#'
#' @seealso \link{Functional basis function}, \link{make.bspline.basis}
#'
#' @examples
#' ## 16 equidistant knots between 0 and 2 (both included)
#' bf <- make.std.bspline.basis(range = c(0,2), intervals = 15)
#'
make.std.bspline.basis <- function(range = c(0,1), intervals) {
  if (intervals < 4L) stop("At least four intervals needed!")
  basis.to.function(init_bspline_u4(range[1], range[2], intervals))
}


