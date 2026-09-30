print.qunormtrain <-
function(x, ...) {
    
  cat("Quantile normalized data with documentation by value.", "\n")
  cat(paste("Number of observations: ", nrow(x$xnorm), sep=""), "\n")
  cat(paste("Number of variables: ", ncol(x$xnorm), sep=""), "\n")
   
}
