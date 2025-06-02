#' community detection method called SCORE
#' Spectral Clustering On Ratios-of-Eigenvectors (SCORE)
#'
#' @param A n-by-n binary symmtric adjacency matrix.
#' @param K number of communities.
#' @param threshold (optional) the threshold of ratio matrix. By defalt is \code{log(n)}.
#'
#' @return A list containing \describe{
#'   \item{R}{n-by-(K-1) ratio matrix.}
#'   \item{labels}{A vector of integer indicating the cluster to which each point allocated.}
#' }
#' @import RSpectra
#' @import stats
#' @import utils
#' @export
#' @examples
#' library(igraphdata)
#' library(igraph)
#' data('karate')
#' A = get.adjacency(karate)
#' karate.out = SCORE(A, 2)
#' karate.out$labels
SCORE <- function(A, K, threshold = NULL){

  # check A is matrix
  if (!is.matrix(A)){
    A = as.matrix(A)
    if (any( A < 0)){
      stop('Entries of adjacency matrix A should be nonegative!')
    }
  }

  # check symetry
  if(any(A != t(A))){
    stop("Aadjacency matrix A is not symmetric!")
  }

  # check connectivity of the network
  if(!igraph::is.connected(igraph::graph_from_adjacency_matrix(A, mode = 'undirected'))){
    warnings("Network disconnected!")
  }



  eig.out = RSpectra::eigs(A, k = K)
  ev = eig.out$vectors[,1:K]
  R = ev[,2:K] / matrix(rep(ev[,1], K-1), ncol = K-1, byrow = F)
  R = as.matrix(R, nrow = nrow(ev))

  if (is.null(threshold)){
    threshold = log(nrow(A))
  }

  R[R > threshold] = threshold
  R[R < -threshold] = -threshold

  labels.hat = kmeans(R, K, nstart = 100, iter.max = 100)$cluster

  return(list(R = R,
              labels = labels.hat,
              eig.values = eig.out$values[1:K],
              eig.vectors = ev))
}

