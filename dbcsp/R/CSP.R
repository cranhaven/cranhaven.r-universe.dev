CSP <- function(X1, X2, q=15, mixture=FALSE,type='EUCL', w=0.5, vectors=TRUE, eig=TRUE, eig.tol = 1e-06, getWarning=TRUE,more=list())
{
  # Input: EEGs from 2 clases
  #        X1: object of class list with standarized EEGs from class 1
  #        X2: object of class list with standarized EEGs from class 2
  #
  #        q: 2*q is the number of interesting directions to be computed
  # Output: calculates variances of the projected channels, or
  #         vectors: logical if TRUE, gives the interesting projections
  #         eig: logical if TRUE, gives the eigen values
  #------------------------------------------------------------
  # For class 1
  n1 <- length(X1)
  #B1 <- compB(X=X1, mixture=mixture, type=type, w=w, eig.tol=eig.tol ,getWarning=getWarning,...)
  B1_1 <- do.call(compB, c(list(X=X1, mixture=mixture, type=type, w=w, eig.tol=eig.tol ,getWarning=getWarning),more))
  B1 <- B1_1$B
  B1_warn <- B1_1$warn

  # For class 2
  n2 <- length(X2)
  #B2 <- compB(X=X2, mixture=mixture, type=type, w=w, eig.tol=eig.tol, getWarning=getWarning,...)
  B2_1 <- do.call(compB, c(list(X=X2, mixture=mixture, type=type, w=w, eig.tol=eig.tol, getWarning=getWarning),more))
  B2 <- B2_1$B
  B2_warn <- B2_1$warn

  # DISPLAY WARNING: indicating which matrices are converted to be definite positive
  if(B1_warn && B2_warn) warning('Distance matrices for X1 and X2 were converted to be definite positive',immediate. = TRUE)
  else if(B1_warn) warning('Distance matrix for X1 was converted to be definite positive',immediate. = TRUE)
  else if(B2_warn) warning('Distance matrix for X2 was converted to be definite positive',immediate. = TRUE)


  #------------------------------
  result <- geigen::geigen(B1, B2)
  ch <- dim(B1)[1]
  selec <- c(1:q, ch:(ch-q+1))
  vp <- sort(result$values, decreasing=TRUE)
  o <- order(result$values, decreasing=TRUE)
  vectores <- result$vectors[,o]
  vectores <- apply(vectores, 2, function(x){x/sqrt(sum(x^2))})
  Wb <- vectores[,selec]
  colnames(Wb) <- c(paste("B", 1:q, sep=""), paste("S", 1:q, sep=""))
  var1 <- plyr::ldply(X1, calcVarianzas , W=Wb)
  var2 <- plyr::ldply(X2, calcVarianzas , W=Wb)
  proy <- data.frame(rbind(var1, var2), group=c(rep(1, n1), rep(2, n2)))

  if (eig ||vectors) {
    out <- list(proy = proy, eig = if (eig) vp, vectors = if (vectors) Wb)
  }
  else out <- proy

  return(out)
}
