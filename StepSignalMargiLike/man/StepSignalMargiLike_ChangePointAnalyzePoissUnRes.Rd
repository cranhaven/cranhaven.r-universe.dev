\name{StepSignalMargiLike_ChangePointAnalyzePoissUnRes}
\alias{StepSignalMargiLike_ChangePointAnalyzePoissUnRes}
\title{StepSignalMargiLike_ChangePointAnalyzePoissUnRes}
\usage{
StepSignalMargiLike_ChangePointAnalyzePoissUnRes
}
\description{
Supported C++ function used in function \code{est.changepoints}.
}
\examples{
n <- 20

data.x <- rpois(n, 1)
data.x <- c(data.x, rpois(n, 10))
data.x <- c(data.x, rpois(n, 50))
data.x <- c(data.x, rpois(n, 20))
data.x <- c(data.x, rpois(n, 80))

prior <- prior.pois(data.x)

ChangePointAnalyzePoissUnRes(data.x, n, prior)
}
