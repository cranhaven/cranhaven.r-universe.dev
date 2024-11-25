#' Create a Point Configuration from a Distance Matrix
#' 
#' \code{getConfig} - given an nxn Euclidean distance matrix, produces a d-dimensional
#' point configuration of size n via eigendecomposition
#' 
#' @details
#' 
#' Given a distance matrix D, transform to a semi-definite matrix S using the linear transformation \eqn{\tau(D)}.
#' Using S, compute the eigen-decomposition \eqn{S = ULV'}, where L is a diagonal matrix containing the singular-values of S,
#' and the columns of U contain the eigen-vectors. A point configuration X is then computed as:
#' 
#' \deqn{X = US^{.5}}
#' 
#' To compute a configuration in d dimensions, the first d eigenvalues of S are used.
#' 
#' @param D an nxn Euclidean distance matrix
#' @param d the dimension for the configuration
#' 
#' @return 
#' \item{Y}{an nxd matrix containing the d-dimensional point configuration}
#' \item{Accuracy}{the ratio of the sum of retained eigenvalues to the sum of all n eigenvalues obtained during decomposition}
#' 
#' @examples 
#' set.seed(1337)
#' D <- matrix(c(0,3,4,3,4,3,
#'              3,0,1,NA,5,NA,
#'              4,1,0,5,NA,5,
#'              3,NA,5,0,1,NA,
#'              4,5,NA,1,0,5,
#'              3,NA,5,NA,5,0),byrow=TRUE, nrow=6)
#' d <- 3
#' DStar <- dpf(D,d)$D
#' 
#' getConfig(DStar,3)
#' 
#' @export

getConfig <- function(D,d){
  
  if(nrow(D) != ncol(D)){
    stop("D must be a symmetric matrix")
  }else if(!is.numeric(D)){
    stop("D must be a numeric matrix")
  }else if(!is.matrix(D)){
    stop("D must be a numeric matrix")
  }else if(any(diag(D) != 0)){
    stop("D must have a zero diagonal")
  }else if(!isSymmetric(D,tol=1e-8)){
    stop("D must be a symmetric matrix")
  }else if(any(is.na(D))){
    stop("D must be a distance matrix")
  }else if(any(D < 0)){
    stop("All entries in D must be non-negative")
  }
  
  if(!is.numeric(d)){
    stop("d must be a numeric integer")
  }else if(length(d) != 1){
    stop("d must be a numeric integer")
  }else if(!(d %% 2 == 1 | d %% 2 == 0)){
    stop("d must an integer")
  } 
  
  TauD <- round(edm2gram(D * D),10)
  
  Eigs <- eigen(TauD,symmetric=TRUE)
  n <- length(Eigs$values)
  
  if(any(Eigs$values < 0)){
    Remove <- which(Eigs$values < 0)
    Eigs$values[Remove] <- 0
  }
  
  Accuracy <- sum(abs(Eigs$values[(1:d)]))/sum(abs(Eigs$values))
  Eigs$values[(d+1):n] <- 0
  Y <- Eigs$vectors %*% diag(sqrt(Eigs$values))
  Y <- Y[,c(1:d)]
  
  return(list(X=Y, Accuracy = Accuracy))
}