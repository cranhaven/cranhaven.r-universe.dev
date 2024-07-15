#' Calculate NODF_c for a bipartite network
#'
#' Calculates the NODF_c metric proposed by Song et al (2017) for a bipartite incidence matrix
#' @param web A numeric matrix describing a bipartite network (a bipartite incidence matrix where elements are positive numbers if nodes interact, and 0 otherwise).
#' @param quality An optional quality parameter to control the tradeoff between computation time and result quality. Can be 0, 1 or 2.
#' @details For a given network, \code{NODFc} calculates the NODF_c metric proposed by Song et al (2017), defined as \eqn{(NODF/max(NODF))/(C * log(S))} where C is the network connectance, S is the geometric mean of the number of plants and pollinators in the network,
#' NODF is the raw NODF of the network and max(NODF) is the maximum nestedness that can be achieved in a network with the same number of rows, columns and links as \code{web}, subject to the constraint that all rows and columns must have at least one link (i.e. marginal totals must always be >= 1). 
#' \code{NODFc} has three algorithms for finding the maximum nestedness of a bipartite network. These can be set using the \code{quality} argument. Lower quality settings are faster, but find worse optima. Higher quality settings
#' are slower, but find better optima.
#' \itemize{
#' \item{\code{quality} = 0, uses a greedy algorithm.}
#' \item{\code{quality} = 1, uses a greedy algorithm plus hillclimbing.}
#' \item{\code{quality} = 2, uses a simulated annealing algorithm, with the greedy algorithm output as the start point. Best results, but requires the most computation time.}
#' }
#' @return Returns the value of NODF_c as a single number.
#' @references 
#' Song, C., Rohr, R.P. and Saavedra, S., 2017. Why are some plant–pollinator networks more nested than others? Journal of Animal Ecology, 86(6), pp.1417-1424
#' @examples
#' set.seed(123)
#' NODFc(matrix(sample(x = 0:1, size = 100, replace = TRUE),10,10), quality = 0)
#' @useDynLib maxnodf
#' @import Rcpp
#' @export
NODFc <- function(web, quality = 0){
  mn <- maxnodf(web, quality = quality)$max_nodf
  if(mn == 0){stop("Maximum nestedness of this network is 0, cannot calculate NODF_c")}
  web[web>0] <- 1
  gm <- sqrt(prod(dim(web)))
  connectance <- sum(web)/prod(dim(web))
  nodf_n <- nodf_cpp(web)/mn
  nodf_c <- nodf_n/(connectance * log10(gm))
  nodf_c
}
