#' @rdname icrf
#' @export
"print.icrf" <-
function(x, ...) {
  cat("\nCall:\n", deparse(x$call), "\n")
  cat("                         Number of forests: ", x$nfold, "\n", sep="")
  cat("                           Number of trees: ", x$ntree, "\n",sep="")
  cat("      No. of variables tried at each split: ", x$mtry, "\n", sep="")
  cat("                       Splitting rule used: ", x$method[["split.rule"]], " - ",
      x$method[["quasihonesty"]], "\n",sep="")
  cat("                       Smoothing bandwidth: ",
      signif(as.numeric(x$method[["bandwidth"]]), digits=5), "\n",sep="")
  if(!is.null(x$imse.oob)) {
    cat("Integrated mean squared error (out-of-bag): type 1 and type 2 \n")
    for (i in 1:x$nfold)
      cat("                                    fold ", i, ": ",
          signif(x$imse.oob[i, 1], digits = 5), " ",
          signif(x$imse.oob[i, 2], digits = 5), "\n", sep="")
  }
}
