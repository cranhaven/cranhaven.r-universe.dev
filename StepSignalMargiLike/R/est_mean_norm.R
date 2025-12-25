#'@title est.mean.norm
#'
#'@description
#'This function estimates the posterior mean for each segments under the normal assumption with conjugate prior. The variance \eqn{\sigma^2} is assumed to be drawn from an inverse Gamma distribution with shape parameter \eqn{\nu0} and scale parameter \eqn{\sigma0^2}, while mean is assumed to be drawn from a normal distribution with mean \eqn{\mu0} and variance \eqn{\sigma^2/\kappa0}.
#'
#'@details
#'See Manual.pdf in "data" folder.
#'
#'@param
#'data.x      Observed data in vector form where each element represents a single observation.
#'@param
#'index.ChPT  The set of the index of change points in a vector. Must be in accending order. This could be obtained by \code{est.changepoints}.
#'@param
#'prior       Vector contatining prior parameters in the order of (\eqn{\mu0, \kappa0, \nu0, \sigma0^2})}.
#'
#'@return
#'Vector containing estimated mean for each segments.
#'
#'@references
#'Chao Du, Chu-Lan Michael Kao and S. C. Kou (2015), "Stepwise Signal Extraction via Marginal Likelihood." Forthcoming in Journal of American Statistical Association.
#'
#'@examples
#'library(StepSignalMargiLike)
#'
#'n <- 5
#'data.x <- rnorm(n, 1, 1)
#'data.x <- c(data.x, rnorm(n, 10,1))
#'data.x <- c(data.x, rnorm(n, 2,1))
#'data.x <- c(data.x, rnorm(n, 10,1))
#'data.x <- c(data.x, rnorm(n, 1,1))
#'
#'prior <- prior.norm.A(data.x) 
#'index.ChPT <- c(n,2*n,3*n,4*n)
#'est.mean.norm(data.x, index.ChPT, prior)
#'
#'@export
est.mean.norm <- function(data.x, index.ChPT, prior)
{
  num.segs <- length(index.ChPT)+1
  num.data <- length(data.x)
  index.temp <- c(0, index.ChPT, num.data)
  m0 <- prior[1]
  k0 <- prior[2]
  est.mean <- rep(0, num.segs)
  for (i in 1:num.segs)
  {
    data.temp <- data.x[(index.temp[i]+1):index.temp[i+1]]
    n <- length(data.temp)
    x.bar <- mean(data.temp)
    est.mean[i] <- (k0*m0+n*x.bar)/(k0+n)
  }
  return(est.mean)
}