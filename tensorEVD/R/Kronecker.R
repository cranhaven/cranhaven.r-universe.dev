
#====================================================================
# Kronecker product between matrices A and B
#====================================================================
# rows <- cols <- NULL; drop = TRUE; inplace=TRUE
Kronecker <- function(A, B, rows = NULL, cols = NULL, a = 1,
                      make.dimnames = FALSE, drop = TRUE,
                      inplace = FALSE)
{

  if(length(dim(A)) != 2L){
    A <- as.matrix(A, ncol=1L)
  }
  if(length(dim(B)) != 2L){
    B <- as.matrix(B, ncol=1L)
  }

  dmA <- dim(A)
  dmB <- dim(B)

  if(!is.scalar(a)){
    stop("'a' must be a scalar")
  }

  if(inplace){
    inplace <- ifelse((dmB[1]*dmB[2])==1,1,ifelse((dmA[1]*dmA[2])==1,2,0))
    if(!(inplace>0) | !(is.null(rows)&is.null(cols))){
      stop("'inplace' calculation can be only applied when either 'A' or 'B' are not resized:",
           "\n one of them is a scalar, and 'rows' and 'cols' are NULL")
    }
  }else{
    inplace <- 0
  }

  res <- kronecker_index(dimA=dmA, dimB=dmB, rows=rows, cols=cols)

  # Checkpoint for IDs
  stopifnot(length(res$irowA) == length(res$irowB))
  stopifnot(length(res$icolA) == length(res$icolB))

  #dyn.load("c_hadamard.so")
  return(.Call('R_hadamard', a, dmA[1], dmA[2], A, dmB[1], dmB[2], B, NULL,
                             res$irowA, res$icolA, res$irowB, res$icolB,
                             NULL, drop, make.dimnames, inplace))
  #dyn.unload("c_hadamard.so")
}
