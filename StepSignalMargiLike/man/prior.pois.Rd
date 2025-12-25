\name{prior.pois}
\alias{prior.pois}
\title{prior.pois}
\usage{
prior.pois(data.x)
}
\arguments{
  \item{data.x}{Observed data in vector form where each element represents a single observation.}
}
\value{
Vector for prior parameters in the order of (\eqn{\alpha,
\beta})
}
\description{
This function computes the Pois prior proposed in Du, Kao
and Kou (2015), which is used under the Poisson assumption
with conjugate prior. The data is assumed to follow
Poisson(\eqn{\lambda}), where \eqn{\lambda} is assumed to
have Beta prior with shape parameters \eqn{\alpha} and
\eqn{\beta}.
}
\details{
See Manual.pdf in "data" folder.
}
\examples{
n <- 20

data.x <- rpois(n, 1)
data.x <- c(data.x, rpois(n, 10))
data.x <- c(data.x, rpois(n, 50))
data.x <- c(data.x, rpois(n, 20))
data.x <- c(data.x, rpois(n, 80))

prior.pois(data.x)
}
\references{
Chao Du, Chu-Lan Michael Kao and S. C. Kou (2015),
"Stepwise Signal Extraction via Marginal Likelihood".
Forthcoming in Journal of American Statistical Association.
}

