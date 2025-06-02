#' Prints Fuzzy Linear Regression Summary
#'
#' Prints the models for the central tendency and spreads from the \code{fuzzylm} object. 
#' @param x a summary of a \code{fuzzylm} object.
#' @param ... further arguments passed to or from other methods.
#' @return No return value, called for side effects.
#' @keywords fuzzy
#' @export
#' @examples
#' x <- rep(1:3, each = 5)
#' y <- c(rnorm(5, 1), rnorm(5, 2), rnorm(5, 3))
#' dat <- data.frame(x = x, y = y)
#' f <- fuzzylm(y ~ x, dat)
#' sum.f <- summary(f)
#' sum.f


print.summary.fuzzylm = function(x, ...){
	n = length(x$center)
	cat("\nCentral tendency of the fuzzy regression model:\n", ...)
	cat(x$center[1], "+", 
		paste(x$center[2:n], x$xvars[1:(n-1)], sep=" * ", collapse = " + "), sep = " ", ...)
	cat("\n\nLower boundary of the model support interval:\n", ...)
	cat(x$center[1] - x$left.spread[1], "+", 
		paste(x$center[2:n] - x$left.spread[2:n], x$xvars[1:(n-1)], sep=" * ", collapse = " + "), sep = " ", ...)
	cat("\n\nUpper boundary of the model support interval:\n", ...)
	cat(x$center[1] + x$right.spread[1], "+", 
		paste(x$center[2:n] + x$right.spread[2:n], x$xvars[1:(n-1)], sep=" * ", collapse = " + "), sep = " ", ...)
	cat("\n\nThe total error of fit:", round(x$TEF, 2), ...)
	cat("\nThe mean squared distance between response and prediction:", round(x$GOF, 2), ...)
}