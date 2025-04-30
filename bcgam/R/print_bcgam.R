#' @export
#' @keywords internal
print.bcgam <- function(x,...)
{
	mean.alpha=apply(x$alpha.sims,2,"mean")
	names(mean.alpha)=x$znms	
	cat("Call:\n")
	print(x$call)
	cat("\nCoefficients:\n")
	print(mean.alpha)	
}
