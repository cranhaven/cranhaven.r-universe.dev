#' @rdname pcps
#' @encoding UTF-8
#' @export
print.pcps <- function(x, ...){
	cat("Call:\n")
	cat(deparse(x$call), "\n\n")
	if(length(x$list.warning)>0){
	  cat("List of warning:\n")
	  namestemp <- SYNCSA::CollectNames(x$list.warning, prefix = "$list.warning$")
	  cat(unlist(namestemp, use.names = FALSE), sep = "\n")
	}
	cat("PCPS values:\n")
	print(as.matrix(x$values), ...)
	invisible(x)
}