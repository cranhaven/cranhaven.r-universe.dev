\name{prior.norm.C}
\alias{prior.norm.C}
\title{prior.norm.C}
\usage{
prior.norm.C(data.x)
}
\arguments{
  \item{data.x}{Observed data in vector form where each element represents a single observation.}
}
\value{
Vector for prior parameters in the order of (\eqn{\mu0,
\kappa0, \nu0, \sigma0^2})
}
\description{
This function computes the Norm-C prior proposed in Du, Kao
and Kou (2015), which is used under conjugate normal
assumption. The variance \eqn{\sigma^2} is assumed to be
drawn from an inverse Gamma distribution with shape
parameter \eqn{\nu0} and scale parameter \eqn{\sigma0^2},
while mean is assumed to be drawn from a normal
distribution with mean \eqn{\mu0} and variance
\eqn{\sigma^2/\kappa0}.
}
\details{
See Manual.pdf in "data" folder.
}
\examples{
library(StepSignalMargiLike)

n <- 5
data.x <- rnorm(n, 1, 1)
data.x <- c(data.x, rnorm(n, 10,1))
data.x <- c(data.x, rnorm(n, 2,1))
data.x <- c(data.x, rnorm(n, 10,1))
data.x <- c(data.x, rnorm(n, 1,1))

prior.norm.C(data.x)
}
\references{
Chao Du, Chu-Lan Michael Kao and S. C. Kou (2015),
"Stepwise Signal Extraction via Marginal Likelihood".
Forthcoming in Journal of American Statistical Association.
}

