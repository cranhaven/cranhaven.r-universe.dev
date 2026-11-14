## ------- distances.R ------------ ##
#                                    #
#   pairwise.dist                    #
#                                    #
## -------------------------------- ##

## ---------------------------------------------------------------- ##
#         pairwise.dist <- function(a, b, squared = TRUE)            #
## ---------------------------------------------------------------- ##
#' Compute Euclidean Distances
#' @description Computes the pairwise distance matrix between two sets of points
#' @usage pairwise.dist(a, b, squared = TRUE)
#' @param a,b matrices (NxD) and (MxD), respectively, where each row represents a D-dimensional point.
#' @param squared return containing squared Euclidean distance
#' @return Euclidean distance matrix (NxM). An attribute "squared" set to the
#' value of param \code{squared} is provided.
#' @examples pairwise.dist(matrix(1:9, ncol = 3), matrix(9:1, ncol = 3))
#' @export

pairwise.dist <- function(a, b, squared = TRUE){
  an <- apply(a, 1, function(x) crossprod(x, x))
  bn <- apply(b, 1, function(x) crossprod(x, x))

  m <- length(an)
  n <- length(bn)

  an_bn <- matrix(rep(an, n), nrow=m) + matrix(rep(bn, m), nrow=m, byrow=T)
  d2 <- an_bn - 2 * tcrossprod(a, b)

  if(!squared){
    d2 <- sqrt(d2)
  }
  attr(d2, "squared") <- squared
  return(d2)
}
