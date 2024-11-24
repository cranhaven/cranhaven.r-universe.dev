#' @title Mean distance matrix
#'
#' @description
#' Given a sequence of distance matrices, expected to correspond to sequential increasing values of time, calculate the average distance between any pairs of nodes
#' shortest-path (or geodesic) distance
#' @param DM_list list of distance matrices
#' @return mean-distance matrix
#' @keywords distance
#' @seealso \code{\link{get_diffusion_probability_matrix}}, \code{\link{get_distance_matrix}}
#' @references
#' De Domenico, M. (2017). Diffusion Geometry Unravels the Emergence of
#' Functional Clusters in Collective Phenomena. Physical Review Letters.
#' \doi{10.1103/PhysRevLett.118.168301}
#'
#' Bertagnolli, G., & De Domenico, M. (2020). Diffusion Geometry of Multiplex
#' and Interdependent Systems.
#' \href{https://arxiv.org/abs/2006.13032}{arxiv preprint arxiv:2006.13032}
#' @export
get_mean_distance_matrix <- function(DM_list){
    #DM.list is a simple list, where each element is a distance matrix
    M <- length(DM_list)
    return( Reduce('+', DM_list) / M )
}

#' @describeIn get_mean_distance_matrix Old deprecated function
#' @usage getMeanDistanceMatrix(DM_list)
#' @export
getMeanDistanceMatrix <- function(DM_list) {
  .Deprecated("get_mean_distance_matrix")
  return(get_mean_distance_matrix(DM_list))
}
