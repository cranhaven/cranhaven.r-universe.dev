\name{est.mean.pois}
\alias{est.mean.pois}
\title{est.mean.pois}
\usage{
est.mean.pois(data.x, index.ChPT, prior)
}
\arguments{
  \item{data.x}{Observed data in vector form where each element represents a single observation.}
  \item{index.ChPT}{The set of the index of change points
  in a vector. Must be in accending order. This could be
  obtained by \code{est.changepoints}.}

  \item{prior}{Vector contatining prior parameters in the
  order of (\eqn{\alpha, \beta})}.}
\value{
Vector containing estimated mean for each segments.
}
\description{
This function estimates the posterior mean for each
segments under the Poisson assumption with conjugate prior.
The data is assumed to follow Poisson(\eqn{\lambda}), where
\eqn{\lambda} is assumed to have Beta prior with shape
parameters \eqn{\alpha} and \eqn{\beta}.
}
\details{
See Manual.pdf in "data" folder.
}
\examples{
library(StepSignalMargiLike)

n <- 20
data.x <- rpois(n, 1)
data.x <- c(data.x, rpois(n, 10))
data.x <- c(data.x, rpois(n, 50))
data.x <- c(data.x, rpois(n, 20))
data.x <- c(data.x, rpois(n, 80))
data.x <- matrix(data.x,1)

prior <- c(1,2)
index.ChangePTs <- c(n, 2*n, 3*n, 4*n)
est.mean.pois(data.x, index.ChangePTs, prior)
}
\references{
Chao Du, Chu-Lan Michael Kao and S. C. Kou (2015),
"Stepwise Signal Extraction via Marginal Likelihood".
Forthcoming in Journal of American Statistical Association.
}

