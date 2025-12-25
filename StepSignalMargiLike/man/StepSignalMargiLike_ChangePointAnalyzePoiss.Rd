\name{StepSignalMargiLike_ChangePointAnalyzePoiss}
\alias{StepSignalMargiLike_ChangePointAnalyzePoiss}
\title{StepSignalMargiLike_ChangePointAnalyzePoiss}
\usage{
StepSignalMargiLike_ChangePointAnalyzePoiss
}
\description{
Supported C++ function used in function \code{est.changepoints}.
}
\examples{
n <- 20
max.segs <- 20

data.x <- rpois(n, 1)
data.x <- c(data.x, rpois(n, 10))
data.x <- c(data.x, rpois(n, 50))
data.x <- c(data.x, rpois(n, 20))
data.x <- c(data.x, rpois(n, 80))

prior <- prior.pois(data.x)

ChangePointAnalyzePoiss(data.x, n, max.segs, prior)
}
