# This function constructs graph Laplacian matrices from adjacency matrices
# Author: Sen Zhao
# Email: sen-zhao@sen-zhao.com
# ----------------------------------------------------------------------------
# Arguments:
# adj: p by p (weighted) adjacency matrix of a graph.
# normalize.Laplacian: binary variable indicating whether the laplacian matrix 
# should be normalized so that the diagonal entries equal to one.
# ----------------------------------------------------------------------------
# Outputs:
# A (nromalized) laplacian matrix corresponding to the adjacency matrix.

make.L <- function(adj, normalize.Laplacian = FALSE){
  if(!is.matrix(adj)){
    stop("Error: adj needs to be a matrix")
  }
  if(nrow(adj) < 2){
    stop("Error: The adjacenecy matrix needs to have at least two rows and two columns.")
  }
  if(!isSymmetric(adj)){
    stop("Error: The adjacency matrix needs to be symmetric.")
  }
  if(max(abs(adj)) > 1){
    stop("Error: Entries in the adjacency matrix need to be between -1 and 1.")
  }
  if(sum(abs(adj)) == 0){
    stop("Error: Adjacency matrix is empty.")
  }
  L <- -adj
  diag(L) <- 0
  diag(L) <- -rowSums(L)
  if(normalize.Laplacian){
    diag(L)[diag(L) == 0] <- 1
    L <- diag(1 / sqrt(diag(L))) %*% L %*% diag(1 / sqrt(diag(L)))
  }
  return(L)
}
