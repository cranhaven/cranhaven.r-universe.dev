#' @rdname syncsa
#' @include syncsa.R
#' @encoding UTF-8
#' @export
print.syncsa <- function(x, ...)
{
  cat("Call:\n")
  cat(deparse(x$call), "\n\n")
  if(length(x$list.warning)>0){
    cat("List of warning:\n")
    namestemp <- CollectNames(x$list.warning, prefix = "$list.warning$")
    cat(unlist(namestemp, use.names = FALSE), sep = "\n")
  }
  cat("\n$notes:\n")
  print(x$notes)
  cat("\n$statistics:\n")
  print(as.matrix(x$statistics), ...)
  invisible(x)
}
