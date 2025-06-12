#' @rdname pcps
#' @encoding UTF-8
#' @export
print.summarypcps<-function(x, ...){
  cat("Call:\n")
  cat(deparse(x$call), "\n\n")
  if(length(x$list.warning)>0){
    cat("List of warning:\n")
    namestemp <- SYNCSA::CollectNames(x$list.warning, prefix = "$list.warning$")
    cat(unlist(namestemp, use.names = FALSE), sep = "\n")
  }
  cat("\n$values:\n")
  print(as.matrix(x$values), ...)
  cat("\n$vectors:\n")
  print(as.matrix(x$vectors), ...)
  cat("\n$correlations:\n")
  print(as.matrix(x$correlations), ...)
  cat("\n$scores$scores.sites:\n")
  print(as.matrix(x$scores$scores.sites), ...)
  cat("\n$scores$scores.species:\n")
  print(as.matrix(x$scores$scores.species), ...)
  invisible(x)
}