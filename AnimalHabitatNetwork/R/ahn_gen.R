#' @title Generate networks characterising habitat physical configurations
#' @description Generate undirected networks (weighted or unweighted, connected or disconnected) characterising the physical attributes and spatial organizations (or distributions) of habitat components (i.e. habitat configurations).
#'
#' @param N The number of nodes
#' @param L A side length of the rectangle landscape within which nodes are anchored
#' @param mu the critical \code{Dij} (i.e. Euclidean distance between node \code{i} and \code{j}) at which the link removing probability curve \code{P(Dij, mu, lamda)} transits from concave to convex (see \code{\link{ahn_prob}})
#' @param lamda the steepness of the link removing probability curve \code{P(Dij, mu, lamda)}, see \code{\link{ahn_prob}}
#' @param Connected \code{TRUE} for connected while \code{FALSE} ignores whether the networks are connected or not
#' @param Weighted \code{TRUE} for weighted while \code{FALSE} for unweighted networks
#' @param eta mediates the weight, i.e. \code{(Dij)^-eta}, of the link rewiring node \code{i} from one network component and node \code{j} from another network component (\code{i} and \code{j} are with an Euclidean distance of \code{Dij}) when the network becomes disconnected after removing links from the initial complete network with the probability \code{P(Dij, mu, lamda) = [1 + exp(-lamda(Dij - mu))]^-1} when both \code{Connected = TRUE} and \code{Weighted = TRUE}
#' @param A The area of the rectangle landscape within which the network is defined
#' @param X A vector of \code{X} coordinates for the \code{N} nodes (sampled from \code{[0, L]} uniformly at random if \code{NULL})
#' @param Y A vector of \code{Y} coordinates for the \code{N} nodes (sampled from \code{[0, A/L]} uniformly at random if \code{NULL})
#' @param U A vector with \code{N} elements specifying node attributes (qualitative or quantitive), by default \code{NULL}
#' @param V A vector with \code{N} elements specifying node attributes (qualitative or quantitive), by default \code{NULL}
#'
#' @importFrom stats runif dist rnorm
#' @import igraph
#' @export
#' @return Return an animal habitat network (an \code{igraph} object)
#' @examples
#' # generate a connected and weighted network
#' ahn_gen(N = 10, L = 5, mu = 1, lamda = 5)
#'
#'\donttest{
#'
#' N <- 10
#' x <- runif(N, 0, 5)
#' ql <- sample(LETTERS, N, replace = TRUE)
#' qn <- sample(1:20, N, replace = TRUE)
#'
#' # specify the X coordinates, node attributes U and V for a connected and unweighted network
#' ahn_gen(N, L = 5, mu = 1, lamda = 5, Weighted = FALSE, X = x, U = ql, V = qn)
#'
#' # specify the Y coordinates, node attributes U and V for a weighted network, no matter if the
#' # network will be connected or not
#' ahn_gen(N, L = 5, mu = 1, lamda = 5, Weighted = TRUE, Connected = FALSE, Y = x, U = ql, V = qn)
#'
#'}
#'
ahn_gen <- function(N, L, mu, lamda, Connected = TRUE, Weighted = TRUE, eta = 1, A = 25, X = NULL, Y = NULL, U = NULL, V = NULL){
  ifelse(is.null(X), x <- runif(N, 0, L), ifelse(max(X) > L || min(X) < 0 || length(X) != N, stop('Wrong X coordinate(s)!'), x <- X))
  ifelse(is.null(Y), y <- runif(N, 0, A/L), ifelse(max(Y) > A/L || min(Y) < 0 || length(Y) != N, stop('Wrong Y coordinate(s)!'), y <- Y))
  xy_coords <- data.frame(x = x, y = y)
  dm <- as.matrix(dist(xy_coords), method = 'euclidean', diag = FALSE)
  dm_0 <- 1/dm
  dm_0[is.infinite(dm_0)] <- 0
  ahn_wei_matrix <- dm_0
  ahn_wei_matrix[lower.tri(ahn_wei_matrix, diag = TRUE)] <- NA
  tr <- which(!is.na(ahn_wei_matrix))
  prob <- 1/(1 + exp(-lamda*(dm[tr] - mu)))
  for(u in 1:length(tr)){
    if(sample(c(1, 0), size = 1, prob = c(prob[u], 1 - prob[u]))){
      ahn_wei_matrix[tr][u] <- 0
    }
  }
  ahn_wei_matrix[lower.tri(ahn_wei_matrix)] <- t(ahn_wei_matrix)[lower.tri(ahn_wei_matrix)]
  if(Weighted){
    ahn <- graph_from_adjacency_matrix(ahn_wei_matrix, mode = 'undirected', diag = FALSE, weighted = TRUE)
  } else{
    ahn_wei_matrix[ahn_wei_matrix > 0] <- 1
    ahn <- graph_from_adjacency_matrix(ahn_wei_matrix, mode = 'undirected', diag = FALSE, weighted = NULL)
  }
  if(!is.connected(ahn) && Connected){
    memb <- unname(components(ahn)$membership)
    ncomp <- max(memb)
    while(ncomp > 1){
      r_memb <- sample(memb, 1)
      temp <- dm_0[which(memb == r_memb), which(memb != r_memb), drop = FALSE]
      rn <- as.numeric(rownames(temp)[which(temp == max(temp), arr.ind = T)[1]])
      cn <- as.numeric(colnames(temp)[which(temp == max(temp), arr.ind = T)[2]])
      if(Weighted){
        ahn_wei_matrix[rn, cn] <- (dm_0[rn, cn])^eta
        ahn_wei_matrix[cn, rn] <- (dm_0[rn, cn])^eta
        ahn <- graph_from_adjacency_matrix(ahn_wei_matrix, mode = 'undirected', diag = FALSE, weighted = TRUE)
      } else{
        ahn_wei_matrix[rn, cn] <- 1
        ahn_wei_matrix[cn, rn] <- 1
        ahn_wei_matrix[ahn_wei_matrix > 0] <- 1
        ahn <- graph_from_adjacency_matrix(ahn_wei_matrix, mode = 'undirected', diag = FALSE, weighted = NULL)
      }
      memb <- unname(components(ahn)$membership)
      ncomp <- max(memb)
    }
  }
  if(!is.null(U)){vertex_attr(ahn, name = 'U') <- U}
  if(!is.null(V)){vertex_attr(ahn, name = 'V') <- V}
  vertex_attr(ahn, name = 'X') <- xy_coords$x
  vertex_attr(ahn, name = 'Y') <- xy_coords$y
  return(ahn)
}
