###############################################################################
## print.varrank.R ---
## Author          : Gilles Kratzer
## Last modified   : 05/02/2018
##                 :
###############################################################################

print.varrank <- function(x, digits=5, ...){

  out <- as.matrix(round(t(diag(x[[2]])),digits = digits))

  if(!is.null(rownames(out))) {
  rownames(out)<-c("Scores")
  cat("Ordered variables (decreasing importance):\n \n")
  print(out, ...)
  }else{
    print(x[[1]], ...)
  }

  invisible(x)

}#EOF
