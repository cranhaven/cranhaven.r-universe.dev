#====================================================================
# Hadamard penalization:
#
#  Hadamard_cov(Sigma,K) + Kronecker(Theta,I),
#    K[IDK,IDK]*Sigma[IDS,IDS] + I[IDK,IDK]*Theta[IDS,IDS]
#  with I a diagonal of the same dimension as K. This is equal to:
#      Hadamard(K,Sigma,IDK,IDS) + Hadamard(I,Theta,IDK,IDS)
#====================================================================
# Sigma = 1; drop = TRUE, inplace = FALSE
Hadamard_cov <- function(Sigma = 1, K, Theta, IDS, IDK,
                         drop = TRUE, inplace = FALSE)
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

  # Match IDs
  fixedK <- FALSE
  if(missing(IDK)){
    indexK <- seq(0,dmK[1]-1)   # zero-based indices
    fixedK <- TRUE
  }else{
    indexK <- match_ID(K, IDK, check=TRUE)
    if(is.null(indexK)){
      stop("'IDK' could not be matched to rows/columns of 'K'")
    }
  }

  fixedS <- FALSE
  if(missing(IDS)){
    indexS <- seq(0,dmS[1]-1)   # zero-based indices
    fixedS <- TRUE
  }else{
    indexS <- match_ID(Sigma, IDS, check=TRUE)
    if(is.null(indexS)){
      stop("'IDS' could not be matched to rows/columns of 'Sigma' and 'Theta'")
    }
  }

  # Checkpoint for rows IDs
  if(length(indexK) != length(indexS)){
    stop("No compatibility. Provide either matrices with equal dimensions\n",
         "  or 'IDK' and/or 'IDS' vectors of the same length")
  }

  if(inplace){
    inplace <- ifelse(fixedS,1,ifelse(fixedK,2,0))
    if(inplace == 0){
      stop("'inplace' calculation can be only applied when either 'Sigma' or 'K' are not resized as per ",
           "the 'IDS' and 'IDK' parameters")
    }
  }else{
    inplace <- 0
  }

  a <- 1
  #dyn.load("c_hadamard.so")
  return(.Call('R_hadamard', a, dmS[1], dmS[2], Sigma, dmK[1], dmK[2], K, Theta,
                             indexS, indexS, indexK, indexK,
                             NULL, drop, FALSE, inplace))
  #dyn.unload("c_hadamard.so")
}
