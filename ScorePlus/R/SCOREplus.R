#' community detection method called SCORE+
#'
#' @param A n-by-n binary symmtric adjacency matrix.
#' @param k number of communities (>1).
#' @param c (optional) tuning parameter for Graph Laplacian, default is 0.1.
#' @param r (optional) latent dimension (>1), if not given, chosen between k and k+1 determined by eigen gap
#'
#' @return A list containing \describe{
#'   \item{label}{Predicted community labels}
#'   \item{ratios}{n-by-(K-1) or n-by-r ratio matrix.}
#'   \item{delta}{calculated delta parameter}
#'   \item{eig.vec}{Top r eigen vectors}
#'   \item{eig.val}{Top r eigen values}
#' }
#' @import RSpectra
#' @import stats
#' @import igraphdata
#' @import utils
#' @export
#' @examples
#' library(igraphdata)
#' library(igraph)
#' data('karate')
#' A = get.adjacency(karate)
#' karate.plus.out = SCOREplus(A, 2)
#' karate.plus.out$labels


SCOREplus <- function(A, k, c = 0.1, r = NULL){

  # check A is matrix
  if (!is.matrix(A)){
    A = as.matrix(A)
    if (any( A < 0)){
      stop('Entries of adjacency matrix A should be nonegative!')
    }
  }

  # check symmetry
  if(any(A != t(A))){
    stop("Adjacency matrix A is not symmetric!")
  }

  # check connectivity of the network
  if(!igraph::is.connected(igraph::graph_from_adjacency_matrix(A, mode = 'undirected'))){
      warnings("Network disconnected!")
  }

  # number of communities greater than 2
  if (k < 2){
    stop('Number of communities k should be greater or equal to 2!')
  }

  # check c is positive
  if (c < 0){
    stop('Tunning parameter c should be positive!')
  }

  # if r not give, set to be k+1
  if (is.null(r)){
    fix.latent.dim = F
    r = k + 1
  } else {
    fix.latent.dim = T # otherwise latent dimension is given and fixed
  }

  n = nrow(A) # number of nodes
  degrees = rowSums(A)
  delta = c * max( degrees) # tunning parameter for graph laplacian
  d.inv = 1 / sqrt( delta + degrees )

  L.delta = t(d.inv * A) * d.inv # graph laplacian with ridge regularization

  # get top r eigenvectors
  eig.out = RSpectra::eigs(L.delta, k = r)
  eig.vec.w = eig.out$vectors %*% diag(eig.out$values) # reweight eigenvectors by eigen values

  # get ratio matrix
  ratios = eig.vec.w[,2:r] / eig.vec.w[,1]

  if(!fix.latent.dim){
    # decide latent dimension by eigen-gap
    signal.weakness = 1 - eig.out$values[k+1] / eig.out$values[k]
    if ( signal.weakness > 0.1 ){
      ratios = ratios[,1:(k-1)]
    }
  }


  # k-means
  labels = kmeans(ratios, k, nstart = 100)$cluster

  return(list(labels = labels,
              ratios = ratios,
              delta = delta,
              eig.vec = eig.out$vectors,
              eig.val = eig.out$values))
}


