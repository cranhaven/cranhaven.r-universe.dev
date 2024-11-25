#' Column Approximate Minimum Degree Permutation
#' 
#' \code{colamdR} returns the column approximate minimum degree permutation of a 
#' sparse matrix S. The permutation of S, S[,p], will result in LU factors sparser
#' than S.  
#' 
#' @details This is an implementation of the colamd function available in SuiteSparse, and also implemented in Matlab.
#' 
#' @param M A matrix to be permuted.
#' @return A vector containing the column minimum degree permutation of the matrix M.
#' 
#' @examples
#' M <- matrix(c(1,1,0,0,1,0,0,1,0,1,1,1,1,1,0,0,1,0,1,0), ncol=4)
#' p <- colamdR(M)
#' M[,p]
#' 
#' @references The authors of the code for "colamd" are Stefan I. Larimore and Timothy A. Davis (davis@cise.ufl.edu), University of Florida.
#' 
#' @export
#' @useDynLib edmcr
colamdR <- function(M){
  
  #Check Inputs
  
  #M must be a numeric matrix
  
  #if(!is.numeric(M)){
  #  stop("M must be a numeric matrix")
  #}else if(!is.matrix(M)){
  #  stop("M must be a numeric matrix")
  #}
  
  
  ####### FUNCTION START #######
  
  n_row <- nrow(M)
  n_col <- ncol(M)
  nnz <- length(which(M != 0))
  
  ALEN <- ceiling(2*nnz + 6*(n_col+1) + 4*(n_row+1) + n_col + nnz/5)
  
  A <- c()
  
  for(i in 1:ncol(M)){
    for(j in 1:nrow(M)){
      if(M[j,i] != 0){
        A <- c(A,(j-1))
      }
    }
  }
  
  A <- as.numeric(c(A,rep(0,ALEN-length(A))))
  
  p <- rep(0,ncol(M)+1)
  
  for(i in 1:ncol(M)){
    p[i+1] <- p[i] + length(which(M[,i] != 0))
  }
  
  p <- as.integer(p)
  A <- as.integer(A)
  ALEN <- as.integer(ALEN)
  n_row <- as.integer(n_row)
  n_col <- as.integer(n_col)

  Out <- .C("colamdWrapper",n_row, n_col, ALEN, A, p, numeric(length(p)), numeric(1))
  Out <- Out[[5]] + 1
  Out <- Out[-length(Out)]
  
  return(Out)

  #.Call(colamdWrapper2,n_row,n_col,ALEN,A,p)
}