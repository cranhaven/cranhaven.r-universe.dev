#'  spreadout
#'
#' This function unbins data. If qnull is given it uses quantiles, otherwise uniform
#' @param x data set
#' @param  case setup info
#' @return A numeric vector of observations without ties. 
#' @export
#' @examples
#' case <- list(B=1000, param = NULL, n = 1000, pnull = function(x, param) punif(x), 
#'     rnull = function(n, param) runif(n), qnull = function(x, param) qunif(x), 
#'     est.mle = function(x) NA, nbins = 10)
#' y=runif(1000)
#' bins=seq(0, 1, length=11)
#' counts=hist(y, bins, plot=FALSE)$counts
#' x=list(bins=bins,counts=counts)
#' spreadout(x, case)

spreadout <- function(x, case) {
  if(!is.list(x)) return(x)
  bins <- x$bins
  O <- x$counts
  y <- NULL
  k <- length(O)
  for(i in 1:k) {
    if(O[i]==0) next
    if(!is.function(case$qnull)) {
      #no quantile function, spread out uniformly in bins
      tmp <- bins[i]+c(1:O[i])/(O[i]+1)*(bins[i+1]-bins[i])
      y <- c(y, tmp)
    }
    else {
      #spread out according to quantiles
      tmp <- case$pnull(bins[i], case$param) +
        c(1:O[i])/(O[i]+1)*(case$pnull(bins[i+1], case$param)-case$pnull(bins[i], case$param))
      y <- c(y, case$qnull(tmp))
    }
  }
  y
}
