#' weightedMscale
#' the M scale of an univariate sample (see reference below)
#'
#' @param u an univariate sample of size n.
#' @param b the desired break down point
#' @param weights the weights of each observation.
#' @param initialsc the initial scale value, defaults to 0
#' @param c a tuning constant, if consistency to standard normal distribution is desired use
#' \code{\link{normal_consistency_constants}}
#' @return the weighted-Mscale value
#' @importFrom stats median
#' @references Maronna, R. A., Martin, R. D., Yohai, V. J., & Salibián-Barrera, M. (2018).
#' Robust statistics: theory and methods (with R). Wiley.
#' 
#' 
weightedMscale <- function(u,b=0.5,weights,c, initialsc=0){
  maxit=100
  # from Kristel's fastSreg
  if (initialsc==0){  initialsc = median(abs(u))/.6745}
  sc=initialsc 
  if (initialsc==0){  sc=0}
  if (sc!=0){
    sc <- initialsc
    i <- 0 
    eps <- 1e-10
    err <- 1
    while  (( i < maxit ) & (err > eps)) {
      sc2 <- sqrt( sc^2 * mean(weights*ktaucenters::rhoOpt(u/sc,c)) / b)
      err <- abs(sc2/sc - 1)
      sc <- sc2
      i <- i+1
    }
  }
  return(sc)
}
