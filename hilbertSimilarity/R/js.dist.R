#' Compute the Jensen-Shannon Distance between 2 sets of Hilbert Index
#'
#' The \href{https://en.wikipedia.org/wiki/Jensen-Shannon_divergence}{Jensen-Shannon distance} is a method to
#' measure the distance between discrete probability distributions. To measure the distance between 2 high-dimensional
#' datasets, we cut the space into sub-cubes, then count the number of events per cube. The resulting probability
#' distributions can be compared using the Jensen-Shannon distance.
#'
#' @param mat a matrix of counts, where rows correspond to samples and columns to Hilbert index
#' @param pc a pseudo-count that is added to all samples to avoid divide-by-zero errors
#'
#' @return a S3 distance object
#'
#' @example examples/example.js.dist.R
#'
#' @author Yann Abraham
#' @importFrom stats as.dist
#' @importFrom entropy freqs.empirical KL.plugin
#' @export
js.dist <- function(mat,pc=0.0001) {
  dst <- matrix(rep(0,nrow(mat)^2),nrow=nrow(mat))
  dimnames(dst) <- list(rownames(mat),
                        rownames(mat))
  for(i in seq(1,nrow(mat)-1)) {
    for(j in seq(i+1,nrow(mat))) {
      s1 <- freqs.empirical(mat[i,]+pc)
      s2 <- freqs.empirical(mat[j,]+pc)
      m <- (s1+s2)/2
      kl1 <- KL.plugin(s1,m)
      kl2 <- KL.plugin(s2,m)
      dst[i,j] <- dst[j,i] <- (0.5*kl1+0.5*kl2)^0.5
    }
  }
  return(as.dist(dst))
}
