#' Fuzzy Linear Regression Using the Boskovitch Fuzzy Regression Line Method
#'
#' The function calculates fuzzy regression coeficients using the Boskovitch fuzzy
#' regression line method (BFRL) developed by Tanaka et al. (1989). Specifically, the 
#' \code{min} problem is implemented in this function.
#' @param x matrix with two colums, representing one independent variable observations. 
#'    The first column is
#'    related to the intercept, so it consists of ones. Missing values not allowed.
#' @param y three column matrix of dependent variable values and the respective spread. 
#'    Method assumes non-symmetric triangular fuzzy input. Missing values not allowed.
#' @details The function input expects the response in form of a non-symmetric fuzzy
#'    number and the predictors as crisp numbers. The prediction returns 
#'    non-symmetric triangular fuzzy numbers. The intercept is a non-symmetric triangular
#'    fuzzy number and the slope is a crisp number that is returned as a triangular fuzzy
#'    number with spreads equal to zero.
#' @note Preferred use is through the \code{\link{fuzzylm}} wrapper function with argument
#'    \code{method = "bfrl"}.
#' @inherit fuzzylm return
#' @inherit plrls seealso
#' @references Skrabanek, P., Marek, J. and Pozdilkova, A. (2021) Boscovich Fuzzy Regression 
#'     Line. \emph{Mathematics} 9: 685.
#' @keywords fuzzy
#' @export
#' @examples
#' data(fuzzydat)
#' fuzzylm(y ~ x, fuzzydat$tan, "bfrl", , , "yl", "yr")

bfrl <- function(x, y){
	n <- nrow(x)
	vars <- colnames(x)
	xavg <- colMeans(x)[2]
	yavg <- colMeans(y)
	deltax <- x[, 2] - xavg
	deltay <- (y[, 1] - yavg[1])
	mA1 <- deltay / deltax
	
	J <- rep(0, n)
	
	for(i in 1:n){
		J[i] <- sum(abs(deltay - deltax * mA1[i]))
	}
	A1 <- c(mA1[which.min(J)], 0, 0)
	A0 <- sumFuzzy(x = yavg, y = prodSfuzzy(x = -1, y = prodSfuzzy(x = xavg, y = A1)))
	
	res <- rbind(A0, A1)
	colnames(res) <- c("center", "left.spread", "right.spread")
	rownames(res) <- vars
	lims <- t(apply(x, 2, range))
	rownames(lims) <- vars
	colnames(lims) <- c("min", "max")
	fuzzy <- list(call = NULL, method = "BFRL", fuzzynum = "non-symmetric triangular", coef = res, 
		 		  lims = lims, x = x, y = y)
	class(fuzzy) <- "fuzzylm"
	fuzzy
}