
#' @rdname print
#' @export

print.HDBRR <- function(x, ...)
{
  if(!inherits(x, "HDBRR")) stop("This function only works for objects of class 'HDBRR'\n");
  cat("\nCall:\n", paste(deparse(x$call), sep = "\n", collapse = "\n"),
      "\n\n", sep = "")
  intercept <- x$intercept
  coeffs <- round(x$betahat,6)
  if(intercept == 1){
    files <- as.matrix(c("(Intercept)", colnames(x$x)[-1]))
    d <- which(duplicated(files))[1]
    if(!is.na(d)){
      p <- dim(as.matrix(x$x))[2]
      colnames(x$x)[-1] <- paste("X",1:(p-1),sep="")
    }
    files <- as.matrix(c("(Intercept)",colnames(x$x)[-1]))
    coeffs2 <- t(coeffs)
    colnames(coeffs2) <- files
    rownames(coeffs2) <- ""
  }
  else{
    files <- as.matrix(colnames(x$x))
    d <- which(duplicated(files))[1]
    if(!is.na(d)){
      p <- dim(as.matrix(x$x))[2]
      colnames(x$x) <- paste("X",1:p,sep="")
    }
    files <- as.matrix(colnames(x$x))
    coeffs2 <- t(coeffs)
    colnames(coeffs2) <- files
    rownames(coeffs2) <- ""
  }
  p <- length(coeffs)
  if(p > 250){
    coeffs2 <- coeffs2[1,1:250]
  }
  else{
    coeffs2 <- coeffs2[1,1:p]
  }
  cat("\nCoefficients:\n")
  print(coeffs2)
  if(p > 250){
    r <- p - 250
    cat("\n...",r,"coefficients was omitted")
  }
  cat("\n")
}
