#' Summarizes Fuzzy Linear Regression 
#'
#' Calculates the summary from the \code{fuzzylm} object. 
#' @param object a \code{fuzzylm} object.
#' @param ... additional parameters passed to and from other methods.
#' @return Returns a list with models for the central tendency and spreads from the fuzzy
#'   linear regression, total error of fit of the model and a goodness-of-fit measure.
#' @seealso \code{\link{TEF}}, \code{\link{GOF}}
#' @keywords fuzzy
#' @export
#' @examples
#' data(fuzzydat)
#' f <- fuzzylm(y ~ x, fuzzydat$lee)
#' sum.f <- summary(f)
#' sum.f

summary.fuzzylm = function(object, ...){
	vars = all.vars(object$call)
	yvars = vars[1]
	xvars = intersect(vars, colnames(object$x))
	n = length(xvars)
	center = round(object$coef[,1], 4)
	left.spread = round(object$coef[,2], 4)
	right.spread = round(object$coef[,3], 4)
	tef = TEF(object, ...)
	gof = GOF(object)
	
	s = list(call = object$call, method = object$method, xvars = xvars, yvars = yvars, 
			 n = n, center = center, left.spread = left.spread, right.spread = right.spread, 
			 TEF = tef, GOF = gof)
	class(s) = "summary.fuzzylm"
	s
}

