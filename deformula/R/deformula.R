#' Integration of one-dimensional functions over infinite interval
#' with the double exponential formula.
#'
#' Numerical quadrature of functions of one variable over
#' [0, infinity) with the double exponential formula.
#'
#' @param f An R function taking a numeric first argument.
#' @param ... Additional arguments to be passed to ‘f’.
#' @param zero.eps A threshold value to be zero.
#' @param rel.tol A relative accuracy requested.
#' @param start.divisions An integer. The initial number of subintervals.
#' @param max.iter An integer for the maximum number of iterations to increase
#' subintervals.
#' @return A list with components;
#' \item{value}{A value for integral.}
#' \item{x}{A vector of subintervals.}
#' \item{w}{A vector of weights.}
#' \item{t}{A vector of subintervals for trapezoid integral.}
#' \item{h}{A value of subinterval.}
#' \item{message}{OK or a string for the error message.}
#' @examples
#' f <- function(x, a) exp(-a*x)
#' deformula.zeroinf(f, a=0.1)
#' @export

deformula.zeroinf <- function(f, ..., zero.eps = 1.0e-12,
	rel.tol = 1.0e-8, start.divisions = 8, max.iter = 12) {
	ff <- function(x) f(x, ...)
	res <- integrate_zero_to_inf(ff, zero.eps, rel.tol,
	                             start.divisions, max.iter)
	# names(res) <- c("value", "x", "w", "t", "h", "message")
    switch(as.character(res$message),
		"0"={res$message <- "OK"},
		"2"={res$message <- "Error: Some values become NaN."},
        stop("Unknown error code.")
    )
	res
}

#' Integration of one-dimensional functions over finite interval
#' with the double exponential formula.
#'
#' Numerical quadrature of functions of one variable over
#' (lower, upper) with the double exponential formula.
#'
#' @param f An R function taking a numeric first argument.
#' @param lower The lower limit of integration.
#' @param upper The upper limit of integration.
#' @param ... Additional arguments to be passed to ‘f’.
#' @param zero.eps A threshold value to be zero.
#' @param rel.tol A relative accuracy requested.
#' @param start.divisions An integer. The initial number of subintervals.
#' @param max.iter An integer for the maximum number of iterations to increase
#' subintervals.
#' @return A list with components;
#' \item{value}{A value for integral.}
#' \item{x}{A vector of subintervals.}
#' \item{w}{A vector of weights.}
#' \item{t}{A vector of subintervals for trapezoid integral.}
#' \item{h}{A value of subinterval.}
#' \item{message}{OK or a string for the error message.}
#' @examples
#' f <- function(x, a) exp(-a*x)
#' deformula.moneone(f, 0, 1, a=0.1)
#' @export

deformula.moneone <- function(f, lower, upper, ..., zero.eps = 1.0e-12,
	rel.tol = 1.0e-8, start.divisions = 8, max.iter = 12) {
	ff <- function(x) {
		(upper - lower) * f(((upper - lower) * x + (upper + lower)) / 2.0, ...) / 2.0
	}
	res <- integrate_mone_to_one(ff, zero.eps, rel.tol,
	                             start.divisions, max.iter)
	# names(res) <- c("value", "x", "w", "t", "h", "message")
	res$x <- ((upper - lower) * res$x + (upper + lower)) / 2.0
    switch(as.character(res$message),
		"0"={res$message <- "OK"},
		"2"={res$message <- "Error: Some values become NaN"},
        stop("Unknown error code.")
    )
	res
}
