#' @rdname coef
#' @export

coef.HDBRR <- function(object, all = FALSE, ...){
  if(!inherits(object, "HDBRR")) stop("This function only works for objects of class 'HDBRR'\n");
  coefs <- object$betahat
  
  if(all == FALSE){
    if(length(coefs) > 250){
      coefs1 <- as.vector(round(coefs[1:250],5))
    }
    else{
      coefs1 <- as.vector(round(coefs,5))
    }
    cat("\nCoefficients:\n\n")
    print(coefs1)
    if(length(coefs) > 250){
      r <- length(coefs) - 250
      cat("\n...",r,"coefficients was omitted")
    }
    cat("\n")
  }
  else{
    cat("\nCoefficients:\n\n")
    coefs <- as.vector(round(coefs,5))
    print(coefs)
  }
  
}
