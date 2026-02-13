
#====================================================================
# Kronecker penalization:
#
#    Kronecker(Sigma,K) + Kronecker(Theta,I),     if swap = FALSE
#    Kronecker(K,Sigma) + Kronecker(I,Theta),     otherwise
#====================================================================
# Sigma = 1; rows <- cols <- NULL; byrow <- FALSE; verbose = TRUE
Kronecker_cov <- function(Sigma = 1, K, Theta, swap = FALSE,
                          rows = NULL, cols = NULL, drop = TRUE,
                          inplace = FALSE)
{
  dmK <- dim(K)
  if((sum(dmK)/2)^2 != length(K)){
     stop("'K' must be a squared symmetric matrix")
  }

  if(length(dim(Theta)) != 2L){
    if(is.scalar(Theta)){ # Theta is an scalar
      Theta <- matrix(as.vector(Theta))
    }else{
      stop("'Theta' must be a matrix or a scalar")
    }
  }
  dmT <- dim(Theta)
  if((sum(dmT)/2)^2 != length(Theta)){
     stop("'Theta' must be a squared symmetric matrix")
  }

  if(length(dim(Sigma)) != 2L){
    if(is.scalar(Sigma)){ # Sigma is an scalar
      if(dmT[1]>1L){   # Create a diagonal matrix
        Sigma <- diag(rep(as.vector(Sigma)[1],dmT[1]))
      }else{
        Sigma <- matrix(as.vector(Sigma))
      }
    }else{
      stop("'Sigma' must be a matrix or a scalar")
    }
  }
  dmS <- dim(Sigma)
  if((sum(dmS)/2)^2 != length(Sigma)){
     stop("'Sigma' must be a squared symmetric matrix")
  }
  if((dmS[1]!=dmT[1]) | (dmS[2]!=dmT[2])) {
     stop("'Sigma' and 'Theta' must be of the same dimensions")
  }

  if(inplace){
    inplace <- ifelse(dmK[1] == 1L,1,ifelse(dmS[1]==1L,2,0))
    if(!(inplace>0) | !(is.null(rows)&is.null(cols))){
      stop("'inplace' calculation can be only applied when either 'Sigma' (and 'Theta') or 'K' are not resized:",
           "\n one of them is a scalar, and 'rows' and 'cols' are NULL")
    }
  }else{
    inplace <- 0
  }

  # Obtaining Kronecker IDs
  res <- kronecker_index(dimA=dmS, dimB=dmK, rows=rows, cols=cols, swap=swap)
  stopifnot(length(res$irowA) == length(res$irowB))
  stopifnot(length(res$icolA) == length(res$icolB))

  a <- 1
  #dyn.load("c_hadamard.so")
  return(.Call('R_hadamard', a, dmS[1], dmS[2], Sigma, dmK[1], dmK[2], K, Theta,
                             res$irowA, res$icolA, res$irowB, res$icolB,
                             NULL, drop, FALSE, inplace))
  #dyn.unload("c_hadamard.so")
}
