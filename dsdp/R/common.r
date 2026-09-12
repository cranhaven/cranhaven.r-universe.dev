#' Generic Method for evaluate the estimate
#'
#' @description This is a generic S3 method for estimate.
#' @param model An instance of a class model to be evaluated.
#' @param x A numeric vector for input.
#' @param ... additional arguments affecting the `func` produced.
#' @return An evaluation of `x` with `model`.
#' @export
func <- function(model, x, ...) UseMethod("func")


#' Generic Method for estimation
#'
#' @description This is a generic S3 method for estimation.
#' @param model An instance of a class model to be estimated.
#' @param ... additional arguments affecting the `estimate` produced.
#' @return An instance of a model with estimated data.
#' @export
estimate <- function(model, ...) UseMethod("estimate")


#' Evaluate a polynomial
#'
#' @description Evaluate the polynomial whose coefficients are represented in
#' `coeff` vector. The order of the coefficient is an increasing order, i.e.,
#' `coeff[1]` is a constant term, and `coeff[2]` is a coefficient of 1st degree
#' term, etc. Evaluation is done using Horner's method.
#' @param coeff A coefficient vector in increasing order of degrees;
#' the first element is 0th degree, ..., and the last element is the largest
#' degree of coefficients.
#' @param x A numeric input vector.
#' @return A vector of values of a polynomial whose coefficient is `coeff`.
#' @seealso [pdf_gaussmodel()] [pdf_expmodel()] [cdf_gaussmodel()]
#' [cdf_expmodel()]
#' @examples
#' ## Evaluate a polynomial x^2 - 2x + 2 with x = 1, 2, 3.
#' ## 0th, 1st, 2nd degree of coefficients
#' coeff <- c(2, -2, 1)
#' x <- c(1, 2, 3)
#' eval_poly(coeff, x)
#' @export
eval_poly <- function(coeff, x) .Call(reval_poly_, coeff, x)


#' Substitute a coefficient of polynomial
#'
#' @description Substitute a coefficient of a polynomial with
#' `a*x + b`.
#' For a polynomial with a coefficient vector \eqn{poly(x; coeff)},
#' compute the coefficient vector of
#' \deqn{poly(a*x + b; coeff).}
#' @param coeff A coefficient vector in increasing order of degrees;
#' the first element is 0th degree, ..., and the last element is the largest
#' degree of coefficients.
#' @param c A multiple factor of constant to be applied to all coefficients.
#' @param a A coefficient of 1st degree of `a*x + b`.
#' @param b A coefficient of 0th degree of `a*x + b`.
#' @return A substituted coefficient.
#' @seealso [eval_poly()]
#' @examples
#' coeff <- c(2, -2, 1)
#' a <- 1.1
#' b <- 1.2
#' coeff1 <- c(b, a)
#' coeff2 <- polyaxb(coeff, 1, a, b)
#' xv <- c(1, 2, 3)
#' ## a*x + b
#' yv1 <- eval_poly(coeff1, xv)
#' ## polynomial(a*x + b, coeff)
#' yv2 <- eval_poly(coeff, yv1)
#' ## polynomial(x, coeff2)
#' yv <- eval_poly(coeff2, xv)
#' ## This value is nearly 0 in the presence of rounding errors
#' yv - yv2
#' @export
polyaxb <- function(coeff, c, a, b) .Call(rpolyaxb_, coeff, c, a, b)


#' Compute the mean and the standard deviation of a data set
#'
#' @description Compute the mean and the standard deviation of a data set
#' represented by the pair of the numeric vectors `data` and optionally its
#' frequency vector `freq`.
#' @param data A numeric vector of a data set.
#' @param freq A frequency vector corresponding to the `data` vector.
#' The default value is `NULL`, which means all frequencies are one.
#' @return The mean and the standard deviation of a data set.
#' @seealso [histmean()]
#' @examples
#' ## Without a frequency data
#' datastats(mix2gauss$n200)
#' ## With a frequency data
#' datastats(mix2gaussHist$n200p, mix2gaussHist$n200f)
#' @export
datastats <- function(data, freq = NULL) .Call(rdatastats_, data, freq)


#' Compute the mean of a data set
#'
#' @description Compute the mean of a data set
#' represented by the pair of the numeric vectors `data` and optionally its
#' frequency vector `freq`.
#' @param data A numeric vector of a data set.
#' @param freq A frequency vector corresponding to the `data` vector.
#' The default value is `NULL`, which means all frequencies are one.
#' @return The mean of a data set.
#' @seealso [datastats()]
#' @examples
#' ## Without a frequency data
#' histmean(mix2gauss$n200)
#' ## With a frequency data
#' histmean(mix2gaussHist$n200p, mix2gaussHist$n200f)
#' @export
histmean <- function(data, freq = NULL) .Call(rhistmean_, data, freq)


#' printf
#' @param ... Any number of arguments to be printed.
#' @return None.
#' @export
printf <- function(...) cat(sprintf(...))

nearlyequal <- function(x, y, eps=1e-6) {
  abs(x - y) <= eps
}