#' @rdname pcps.sig
#' @encoding UTF-8
#' @export
print.pcpssig <- function(x, ...){
  cat("Call:\n")
  cat(deparse(x$call), "\n\n")
  if(length(x$list.warning)>0){
    cat("List of warning:\n")
    namestemp <- SYNCSA::CollectNames(x$list.warning, prefix = "$list.warning$")
    cat(unlist(namestemp, use.names = FALSE), sep = "\n")
  }
  cat("$model:\n")
  print(x$model, ...)
  cat("\n$obs.statistic:\n")
  print(x$obs.statistic, ...)
  cat("\n$p.site.shuffle:\n")
  print(x$p.site.shuffle, ...)
  cat("\n$p.taxa.shuffle:\n")
  print(x$p.taxa.shuffle, ...)
  invisible(x)
}
