#' @rdname pcoa.sig
#' @encoding UTF-8
#' @export
print.pcoasig <- function(x, ...){
	cat("Call:\n")
	cat(deparse(x$call), "\n\n")
	cat("PCoA values:\n")
	print(x$values)
	cat("\nProbabilities:\n")
	print(x$probabilities)
	invisible(x)
}