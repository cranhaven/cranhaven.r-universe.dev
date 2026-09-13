#'  simgof.test
#'
#' This function performs a number of gof tests and rejects the null if any of the tests does so. Then it finds the adjusted p-value.
#' @param  x data set
#' @param  pnull  distribution function under the null hypothesis
#' @param  rnull  routine to generate data under the null hypothesis
#' @param  qnull  quantile function under the null hypothesis
#' @param  do.estimation TRUE if parameters are to be estimated
#' @param  estimate routine for parameter estimation
#' @param  include.methods  which methods should be used, a vector of length 16 of T/F
#' @param  B   =10000  number of simulation runs
#' @param  lambda rate of Poisson if sample size is random
#' @param  nbins number of bins for chisquare test
#' @return A numeric vector of p values
#' @export
#' @examples
#'  x <- runif(1000)
#'  pnull <- function(x) x 
#'  rnull <- function(n) runif(n) 
#'  qnull <- function(x) x 
#'  simgof.test(x, pnull, rnull, qnull, FALSE, B=500)
#'  x <- rnorm(1000, 100, 20)
#'  pnull <- function(x, param) pnorm(x, param[1], param[2])
#'  rnull <- function(n, param) rnorm(x, param[1], param[2])
#'  qnull <- function(x, param) qnorm(x, param[1], param[2])
#'  estimate <- function(x) c(mean(x), sd(x))
#'  simgof.test(x, pnull, rnull, qnull, TRUE, estimate, B=500) 


simgof.test <- function(x, pnull, rnull, qnull=function(x) NULL, 
    do.estimation=TRUE, estimate = function(x) NULL,                     
    include.methods = c(rep(TRUE, 7), rep(FALSE, 9)),    
    B=10000, lambda, nbins=NULL) {
  methods <- c("KS", "AD", "CdM",  "W", "ZA", "ZK",  "ZC",
           "RGd", "Equal Size", "Equal Prob", 
           "ppcc", "JB", "SW", "sNor", "sUnif", "sExp")
# step 1: do some setup work
    param <- NULL
    if(do.estimation) param <- estimate(x)
    case <- list(B=B, 
               param = param,
               methods = methods[include.methods],
               n = ifelse(is.list(x), sum(x$counts), length(x)),
               pnull = ifelse(do.estimation, pnull, function(x, param=1) pnull(x)), 
               rnull = ifelse(do.estimation, rnull, function(n, param=1) rnull(n)),
               qnull = ifelse(do.estimation, qnull, function(x, param=1) qnull(x)),
               est.mle = estimate,
               nbins = nbins,
               dta = x
               )
# step 2: find null distributions of each test
  znull <- matrix(0, B, length(case$methods))
  colnames(znull) <- case$methods
  for(i in 1:B) {
    case$n <- ifelse(missing(lambda), case$n, stats::rpois(1, case$lambda))
    znull[i, ] <- simgof::TS(case$rnull(case$n, case$param), case)
  }
# step 3: find p values for each test, find their minimum
  tmp <- rep(0, length(case$methods))
  names(tmp) <- case$methods
  pval <- rep(0, case$B)
  for(i in 1:case$B) {
    xsim <- znull[sample(1:B, 1), ]
    for(k in case$methods) 
      tmp[k] <- sum(xsim[k]<znull[, k])/case$B      
    pval[i] <- min(tmp)
  }
# step 4: find cdf of p values
  x <- seq(0, 1, length=250)
  y <- 0*x
  for(i in 1:250) y[i] <- sum(pval<=x[i])/length(pval)
  xy <- cbind(x, y)
  adjust <- function(xy, a) {
    stats::approx(x=xy[, 1], y=xy[, 2], xout=a, rule=2)$y
  }
# step 5: run test on data  
  TS.data <- simgof::TS(case$dta, case)
  pvals <- rep(0, length(case$methods))
  names(pvals) <- case$methods
  for(k in case$methods)
    pvals[k] <- sum(TS.data[k]<znull[, k])/case$B
  pvals <- c(adjust(xy, min(pvals)), pvals)
  names(pvals)[1] <- "RC"
  round(pvals, 4)
}