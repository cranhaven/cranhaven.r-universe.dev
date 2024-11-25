#'Linear Matrix Operator
#'
#'\code{edm2gram} Linear transformation of a Euclidean Distance Matrix to a Gram Matrix
#'
#'@details
#'
#'While we specify that the input should be a Euclidean Distance Matrix (as this results in a Gram Matrix)
#'the domain of edm2gram is the set of all real symmetric matrices. This function is particularly useful
#'as it has the following property:
#'
#'\deqn{edm2gram(D_{n}^{-}) = B_{n}^{+}} 
#'
#'where \eqn{D_{n}^{-}} is the space of symmetric, hollow matrices, negative definite on the space spanned by \eqn{x'e = 0}
#'and \eqn{B_{n}^{+}} is the space of centered positive definite matrices. 
#'
#'We can combine these two properties with a well known result: If D is a real symmetric matrix with 0 diagonal (call this matrix pre-EDM),
#'then D is a Euclidean Distance Matrix iff D is negative semi-definite on \eqn{D_{n}^{-}}.
#' 
#'Using this result, combined with the properties of edm2gram we therefore have that 
#'D is an EDM iff D is pre-EDM and \eqn{edm2gram{D}} is positive semi-definite. 
#'
#'@param D A Euclidean Distance Matrix
#'
#'@return G A Gram Matrix, where G = XX', and X is an nxp matrix containing the point configuration.
#'
#'@examples
#'  
#' XY <- cbind(runif(100,0,1),runif(100,0,1))
#' D <- dist(XY)
#' edm2gram(as.matrix(D))
#' 
#'@export
edm2gram <- function(D){
  n <- nrow(D)
  I <- diag(1,n)
  e <- as.matrix(rep(1,n))
  
  J <- I - e %*% t(e)/n
  
  TauD <- -.5 * (J %*% D %*% J)
  
  return(TauD)
}

#'Linear Matrix Operator
#'
#'\code{gram2edm} Inverse Operator of edm2gram
#'
#'@details
#'
#'The edm2gram function performs the following transformation:
#'
#'\deqn{edm2gram(D_{n}^{-}) = B_{n}^{+}} 
#'
#'where \eqn{D_{n}^{-}} is the space of symmetric, hollow matrices, negative definite on the space spanned by \eqn{x'e = 0}
#'and \eqn{B_{n}^{+}} is the space of centered positive definite matrices. 
#' 
#'The gram2edm function performs the inverse operation, taking a matrix in \eqn{B_{n}^{+}} and transforming it to a matrix in \eqn{D_{n}^{-}}.
#'
#'\deqn{gram2edm(B_{n}^{+}) = D_{n}^{-}}
#'
#'Therfore, gram2edm on \eqn{B_{n}^{+}} is the inverse operator of edm2gram on \eqn{D_{n}^{-}}. 
#'
#'@param B A centered, positive semi-definite matrix. 
#'
#'@return D A matrix in \eqn{D_{n}^{-}}. If the input matrix B is a gram matrix, D is a Euclidean Distance Matrix.
#'
#'@examples
#'  
#' X <- cbind(runif(100,0,1),runif(100,0,1))
#' G <- X %*% t(X)
#' gram2edm(G)
#' 
#'@seealso \code{\link{edm2gram}}
#' 
#'@export
gram2edm <- function(B){
  n <- nrow(B)
  Value <- matrix(diag(B),nrow=n) %*% matrix(1,ncol=n)
  Result <- Value + t(Value) - 2*B
  
  return(Result)
}

#'Linear Matrix Operator
#'
#'\code{edm2psd} Convert an Euclidean Distance Matrix to a Positive Semi-definite Matrix
#'
#'@details
#'
#'For a matrix D in \eqn{D_{n}^{-}}, edm2psd will be in the space of positive
#'semi-definite matrices. Therefore, if D also has zero diagonal, we have the following property:
#'
#'D is a Euclidean Distance Matrix if and only if edm2psd is positive semi-definite.
#'
#'This operator gives us another method to characterize the existence of a Euclidean distance matrix.
#'
#'@param D A matrix in the set D_{n}^{-}.
#'@param V A projection matrix satisfying V'1 = 0 and VV' = I
#'
#'@return S A symmetric, positive semi-definite matrix
#'
#'@examples
#'
#'XY <- cbind(runif(100,0,1),runif(100,0,1))
#'D <- dist(XY)
#'edm2psd(as.matrix(D))
#'
#'@seealso \code{\link{psd2edm}} \code{\link{edm2gram}}
#' 
#'@export
edm2psd <- function(D,V=NULL){

  if(is.null(V)){
    n <- nrow(D)
    y <- -1/sqrt(n)
    x <- -1/(n + sqrt(n))
    
    Y <- y * rep(1,n-1)
    X <- x * matrix(1,n-1,n-1) + diag(n-1)
    
    V <- as.matrix(rbind(Y,X))
    rownames(V) <- NULL
  }
  
  Result <- t(V) %*% edm2gram(D) %*% V
  return(Result)
}

#'Linear Matrix Operator
#'
#'\code{psd2edm} Transform a positive semi-definite matrix to a Euclidean Distance Matrix
#'
#'@details
#'
#'The psd2edm function performs the inverse operation of the edm2psd function, 
#'taking a matrix in \eqn{S_{n-1}^{+}} and transforming it to a matrix in \eqn{D_{n}^{-}}.
#'
#'\deqn{psd2edm(S_{n-1}^{+}) = D_{n}^{-}}
#'
#'Therefore, psd2edm on \eqn{S_{n-1}^{+}} is the inverse operator of  edm2psd on \eqn{D_{n}^{-}}. 
#'
#'For a symmetric positive semi-definite matrix S, psd2edm(S) will be in \eqn{D_{n}^{-}}. 
#'
#'@param S A symmetric, positive semi-definite matrix
#'@param V A projection matrix satisfying V'1 = 0 and VV' = I
#'
#'@return D A Euclidean Distance Matrix.
#'
#'@examples
#'
#'XY <- cbind(runif(100,0,1),runif(100,0,1))
#'S <- edm2psd(as.matrix(dist(XY)))
#'D <- psd2edm(S)
#' 
#'@seealso \code{\link{gram2edm}} \code{\link{edm2psd}}
#' 
#'@export
psd2edm <- function(S,V=NULL){
  
  if(is.null(V)){
    n <- nrow(S) + 1 
    y <- -1/sqrt(n)
    x <- -1/(n + sqrt(n))
    
    Y <- y * rep(1,n-1)
    X <- x * matrix(1,n-1,n-1) + diag(n-1)
    
    V <- as.matrix(rbind(Y,X))
    rownames(V) <- NULL
  }
  
  Result <- gram2edm(V %*% S %*% t(V))
  return(Result)
}

#Adjoint Operator of Kv
KVs <- function(D,V){
  D <- Matrix(D, sparse=TRUE)
  V <- Matrix(V, sparse=TRUE)
  
  Result <- Matrix(t(V) %*% (2*(diag(colSums(D))) - 2*D) %*% V, sparse=TRUE)
  return(Result)
}


#Weighted KV matrix
KW <- function(X,V,H){
  
  X <- Matrix(X, sparse=TRUE)
  V <- Matrix(V, sparse=TRUE)
  H <- Matrix(H, sparse=TRUE)
  
  Result <- Matrix(H * psd2edm(X,V), sparse=TRUE)
  return(Result)
}


#Weighted KVs matrix
KWs <- function(D,V,H){
  
  D <- Matrix(D, sparse=TRUE)
  V <- Matrix(V, sparse=TRUE)
  H <- Matrix(H, sparse=TRUE)
  
  Result <- Matrix(KVs(H*D,V), sparse=TRUE)
  
  return(Result)
}