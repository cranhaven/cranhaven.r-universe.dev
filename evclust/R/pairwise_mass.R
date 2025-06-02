#' Computes the relational representation
#'
#' \code{pairwise_mass} computes the relational representation of a credal partition, as defined 
#' in Denoeux et al (2018).
#'
#' Given a credal partition, we can compute, for each pair of objects, a "pairwise mass function"
#' on a frame \eqn{\Theta=\{s,\neg s\}}, where \eqn{s} means that the two objects belong to the same
#' cluster, and \eqn{\neg s} is the negation of \eqn{s}. Function \code{pairwise_mass} compute these
#' pairwise mass functions for all object pairs. The result is return as a list with "dist" objects 
#' containing the masses of each of the two elements of \eqn{\Theta}, and the masses on the empty set.
#'
#'
#' @param clus A credal partition (a matrix of n rows and f columns, where n is the
#' number of objects and f is the number of focal sets).
#'
#' @return A list with three "dist" objects:
#' \describe{
#' \item{M0}{The masses assigned to the assumption that each pair of object (i,j) do not belong 
#' to the same class.}
#' \item{M1}{The masses assigned to the assumption that each pair of object (i,j) belongs to 
#' the same class.}
#' \item{Me}{The masses assigned to the empty set, for each pair of object (i,j).}
#' }
#' @export
#' @importFrom stats as.dist
#' 
#' @seealso \code{\link{credal_RI}}, \code{\link{nonspecificity}}
#'
#' @references
#'  T. Denoeux, S. Li and S. Sriboonchitta. Evaluating and Comparing Soft Partitions: an 
#'  Approach Based on Dempster-Shafer Theory. IEEE Transactions on Fuzzy Systems, 
#'  26(3):1231-1244, 2018.
#'
#' @examples
#' ## Butterfly data
#' data(butterfly)
#' clus<-kevclus(butterfly,c=2)
#' P<-pairwise_mass(clus)
#' 
pairwise_mass<- function(clus){
  n<-nrow(clus$mass)
  M<-clus$mass
  mat<-build_matrices(clus$F)
  Me<-as.dist(M %*% mat$E %*% t(M))
  M1<-as.dist(M %*% mat$S %*% t(M))
  M0<-as.dist(M %*% (mat$C-mat$E) %*% t(M))
  return(list(Me=Me,M1=M1,M0=M0))
}

