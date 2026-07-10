# This function checks the data for any issues.
# Author:             Sen Zhao
# Email:              sen-zhao@sen-zhao.com
# ----------------------------------------------------------------------------
# Arguments:
# Y:                  n by 1 vector of the response variable.
# X:                  n (number of rows) by p (number of columns) design matrix.
# L:                  p by p symmetric matrix of the penalty weight matrix.
# ----------------------------------------------------------------------------



checkdata <- function(X, Y, L = NULL){
  if(!is.matrix(X)){
    stop("Error: X needs to be a matrix.")
  }
  if(!is.vector(Y)){
    stop("Error: Y needs to be a vector.")
  }
  if(nrow(X) != length(Y)){
    stop("Error: Dimensions of X and Y do not match.")
  }
  if(!is.numeric(X)){
    stop("Error: X needs to be a numeric matrix.")
  }
  if(!is.numeric(Y)){
    stop("Error: Y needs to be a numeric vector.")
  }
  if(!is.null(L)){
    if(!is.matrix(L)){
      stop("Error: L needs to be a matrix.")
    }
    if(ncol(X) != ncol(L))
    {
      stop("Error: Dimensions of X and L do not match.")
    }
    if(!is.numeric(L)){
      stop("Error: L needs to be a numeric matrix.")
    }
    if(!isSymmetric(L)){
      stop("Error: L is not a symmetric matrix.")
    }
  }
}