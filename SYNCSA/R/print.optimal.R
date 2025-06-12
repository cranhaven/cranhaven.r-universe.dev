#' @rdname optimal
#' @include optimal.R
#' @encoding UTF-8
#' @export
print.optimal <- function(x, ...)
{
  n <- 10
  if (nrow(x$optimization) < n) {
    n <- nrow(x$optimization)
  }
  cat("Call:\n")
  cat(deparse(x$call), "\n\n")
  if(length(x$list.warning)>0){
    cat("List of warning:\n")
    namestemp <- CollectNames(x$list.warning, prefix = "$list.warning$")
    cat(unlist(namestemp, use.names = FALSE), sep = "\n")
  }
  cat("\nNumber of subset:\n")
  cat(deparse(x$N_subset), "\n")
  cat("\n$optimization:\n")
  print(x$optimization[1:n,], ...)
  invisible(x)
}
