#'  TS
#'
#' This function finds various gof statistics
#' @param  x  data
#' @param  case  setup info
#' @return A numeric vector with the values of various test statistics.
#' @export
#' @examples
#' case <- list(B=1000, param = NULL, n = 1000, pnull = function(x, param) 
#'          punif(x), rnull = function(n, param) runif(n), qnull = function(x, param) 
#'         qunif(x), est.mle = function(x) NA, nbins = 10)
#' case$methods=c("KS", "AD", "CdM", "W", "ZA", "ZK", "ZC")
#' x <- runif(1000)
#' TS(x, case)

TS <- function(x, case) {
  if(is.list(x))
    x <- simgof::spreadout(x, case)
  # data is binned, unbin it
  n <- length(x)
  x <- sort(x)
  param <- case$est.mle(x)
  y <- case$pnull(x, param)
  m <- 1:n-0.5
  out <- rep(0, length(case$methods))
  names(out) <- case$methods
  tmp <- c(  max(c(y-0:(n-1)/n, 1:n/n-y)),
            -n-mean((2*1:n-1)*(log(y)+log(1-y[n:1]))),
            1/(12*n)+sum( ((2*(1:n)-1)/2/n- y)^2 ),
            1/(12*n)+sum( ((2*(1:n)-1)/2/n- y)^2 )-n*(mean(y)-0.5)^2,
            max(m*log(m/n/y)+(n-m)*log((n-m)/n/(1-y))),
            (-1)*sum(log(y)/(n-m)+log((1-y))/m),
            sum(log( (1/y-1)/((n-0.5)/(1:n-0.75)-1)  )^2))
  names(tmp) <- c("KS", "AD", "CdM", "W", "ZK", "ZA", "ZC")
  for(m in case$methods) {
    if(m %in% c("KS", "AD", "CdM", "W", "ZK", "ZA", "ZC"))
      out[m] <- tmp[m]
  }
  if("SW" %in% case$methods)
    out["SW"] <- 1-stats::shapiro.test(x)$statistic
  if("ppcc" %in% case$methods)
    out["ppcc"] <- 1-stats::cor(x, case$qnull(stats::ppoints(case$n), param))
  if("JB" %in% case$methods) {
    mu <- mean(x)
    S <- mean((x-mu)^3)/(mean((x-mu)^2))^(3/2)
    K <- mean((x-mu)^4)/(mean((x-mu)^2))^2
    out["JB"] <- n/6*(S^2+(K-3)^2/4)
  }
  if("RGd" %in% case$methods) {
    out["RGd"] <- chisquare.test(x, case, "RGd")
  }
  if("Equal Size" %in% case$methods) {
    out["Equal Size"] <- chisquare.test(x, case, "Equal Size")
  }
  if("Equal Prob" %in% case$methods) {
    out["Equal Prob"] <- chisquare.test(x, case, "Equal Prob")
  }
  if("sNor" %in% case$methods) {
    out["sNor"] <- ddst::ddst.norm.test(x, compute.p = FALSE)$statistic
  }
  if("sUnif" %in% case$methods) {
    out["sUnif"] <- ddst::ddst.uniform.test(x, compute.p = FALSE)$statistic
  }
  if("sExp" %in% case$methods) {
    out["sExp"] <- ddst::ddst.exp.test(x, compute.p = FALSE)$statistic
  }
  out
}
