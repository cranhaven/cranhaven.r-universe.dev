#'@title est.mean.pois
#'
#'@description
#'This function estimates the posterior mean for each segments under the Poisson assumption with conjugate prior. The data is assumed to follow Poisson(\eqn{\lambda}), where \eqn{\lambda} is assumed to have Beta prior with shape parameters \eqn{\alpha} and \eqn{\beta}.
#'
#'@details
#'See Manual.pdf in "data" folder.
#'
#'@param
#'data.x     Observed data in vector form where each element represents a single observation.
#'@param
#'index.ChPT The set of the index of change points in a vector. Must be in accending order. This could be obtained by \code{est.changepoints}.
#'@param
#'prior      Vector contatining prior parameters in the order of (\eqn{\alpha, \beta})}.
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
#'n <- 20
#'data.x <- rpois(n, 1)
#'data.x <- c(data.x, rpois(n, 10))
#'data.x <- c(data.x, rpois(n, 50))
#'data.x <- c(data.x, rpois(n, 20))
#'data.x <- c(data.x, rpois(n, 80))
#'
#'prior <- c(1,2)
#'index.ChangePTs <- c(n, 2*n, 3*n, 4*n)
#'est.mean.pois(data.x, index.ChangePTs, prior)
#'
#'@export
est.mean.pois <- function(data.x, index.ChPT, prior)
{
  num.segs <- length(index.ChPT)+1
  num.data <- length(data.x)
  index.temp <- c(0, index.ChPT, num.data)
  alpha <- prior[1]
  beta <- prior[2]
  est.mean <- rep(0, num.segs)
  for (i in 1:num.segs)
  {
    data.temp <- data.x[(index.temp[i]+1):index.temp[i+1]]
    n <- length(data.temp)
    m <- sum(data.temp)
    est.mean[i] <- (alpha+m)/(beta+n)
  }
  return(est.mean)
}