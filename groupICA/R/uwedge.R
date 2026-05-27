##' Performs an approximate joint matrix diagonalization on a list of
##' matrices. More precisely, for a list of matrices Rx the algorithm
##' finds a matrix V such that for all i V Rx[i] t(V) is approximately
##' diagonal.
##'
##' For further details see the references.
##' @title uwedge
##' @param Rx list of matrices to be diagaonlized.
##' @param init matrix used in first step of initialization. If NA a
##'   default based on PCA is used
##' @param rm_x0 boolean whether to also diagonalize first matrix in
##'   \code{Rx} or only use it for scaling.
##' @param return_diag boolean. Specifies whether to return the list
##'   of diagonalized matrices.
##' @param tol float, optional. Tolerance for terminating the
##'   iteration.
##' @param max_iter int, optional. Maximum number of iterations.
##' @param n_components number of components to extract. If NA is
##'   passed, all components are used.
##' @param silent boolean whether to supress status outputs.
##' 
##' @return object of class 'uwedge' consisting of the following
##'   elements
##'
##' \item{V}{joint diagonalizing matrix.}
##' 
##' \item{Rxdiag}{list of diagonalized matrices.}
##'
##' \item{converged}{boolean specifying whether the algorithm
##' converged for the given \code{tol}.}
##'
##' \item{iterations}{number of iterations of the approximate joint
##' diagonalisation.}
##'
##' \item{meanoffdiag}{mean absolute value of the off-diagonal values
##' of the to be jointly diagonalised matrices, i.e., a proxy of the
##' approximate joint diagonalisation objective function.}
##'
##' 
##' @export
##'
##' @import stats utils
##'
##' @author Niklas Pfister and Sebastian Weichwald
##'
##' @references
##' Pfister, N., S. Weichwald, P. Bühlmann and B. Schölkopf (2017).
##' GroupICA: Independent Component Analysis for grouped data.
##' ArXiv e-prints (arXiv:1806.01094).
##'
##' Tichavsky, P. and Yeredor, A. (2009).
##' Fast Approximate Joint Diagonalization Incorporating Weight Matrices.
##' IEEE Transactions on Signal Processing.
##'
##' @seealso The function \code{\link{groupICA}} uses \code{uwedge}.
##'
##' @examples
##' ## Example
##' set.seed(1)
##' 
##' # Generate data 20 matrix that can be jointly diagonalized
##' d <- 10
##' A <- matrix(rnorm(d*d), d, d)
##' A <- A%*%t(A)
##' Rx <- lapply(1:20, function(x) A %*% diag(rnorm(d)) %*% t(A))
##'
##' # Perform approximate joint diagonalization
##' ptm <- proc.time()
##' res <- uwedge(Rx,
##'               rm_x0=FALSE,
##'               return_diag=TRUE,
##'               max_iter=1000)
##' print(proc.time()-ptm)
##' 
##' # Average value of offdiagonal elements:
##' print(res$meanoffdiag)


uwedge <- function(Rx,
                   init=NA,
                   rm_x0=TRUE,
                   return_diag=FALSE,
                   tol=1e-10,
                   max_iter=1000,
                   n_components=NA,
                   silent=TRUE){

  
  # 0) Preprocessing
  
  # Remove and remember 0st matrix
  Rx0 <- Rx[[1]]
  if(rm_x0){
    Rx <- Rx[-1]
  }
  d <- dim(Rx0)[1]
  M <- length(Rx)

  if(is.na(n_components)){
    n_components <- d
  }

  # Initial guess
  if(!is.matrix(init) & n_components == d){
    EH <- eigen(Rx[[1]], symmetric=TRUE)
    V <- diag(1/sqrt(abs(EH$values))) %*% t(EH$vectors)
  }
  else if(!is.matrix(init)){
    EH <- eigen(Rx[[1]], symmetric=TRUE)
    mat <- matrix(0, n_components, d)
    diag(mat) <- 1/sqrt(abs(EH$values))[1:n_components]
    V <- mat %*% t(EH$vectors)
  }
  else{
    V <- init[1:n_components,]
  }

  V <- V / matrix(sqrt(rowSums(V^2)), n_components, d)

  converged <- FALSE
  for(iteration in 1:max_iter){
    
    # 1) Generate Rs
    Rs <- lapply(Rx, function(x) V %*% x %*% t(V))

    # 2) Use Rs to construct A, equation (24) in paper with W=Id
    # 3) Set A1=Id and substitute off-diagonals
    Rsdiag <- sapply(Rs, diag)
    Rsdiagprod <- Rsdiag %*% t(Rsdiag)
    denom_mat <- outer(diag(Rsdiagprod), diag(Rsdiagprod)) - Rsdiagprod^2
    Rkl_list = lapply(Rs, function(x) matrix(diag(x), n_components, n_components, byrow=T)*x)
    Rkl <- Reduce("+", Rkl_list)/M
    num_mat <- matrix(diag(Rsdiagprod), n_components, n_components)*Rkl - Rsdiagprod*t(Rkl)
    denom_mat[abs(denom_mat) < .Machine$double.tol] <- .Machine$double.tol
    diag(denom_mat) <- 1
    A <- num_mat / denom_mat
    diag(A) <- 1
    
    # 4) Set new V
    Vold <- V
    V <- qr.solve(A, Vold)
    
    # 5) Normalise V
    V <- V / matrix(sqrt(rowSums(V^2)), n_components, d)
    
    # 6) Check convergence
    changeinV = max(abs(V-Vold))
    if(changeinV < tol){
      converged = TRUE
      break
    }
  }
  
  # Rescale
  normaliser <- diag(V %*% Rx0 %*% t(V))
  V <- V / matrix((sign(normaliser)*sqrt(abs(normaliser))), n_components, d)
  Rxdiag <- lapply(Rx, function(x) V %*% x %*% t(V))
  entries_tot <- M*(n_components^2-n_components)
  meanoffdiag <- sqrt(sum(sapply(Rxdiag, function(x) sum(x^2)-sum(diag(x^2))))/entries_tot)
  
  # Return
  if(return_diag){
    res <- list(V=V,
                Rxdiag=Rxdiag,
                converged=converged,
                iteration=iteration,
                meanoffdiag=meanoffdiag)
  }
  else{
    res <- list(V=V,
                Rxdiag=NA,
                converged=converged,
                iteration=iteration,
                meanoffdiag=meanoffdiag)
  }
  
  return(res)
}
