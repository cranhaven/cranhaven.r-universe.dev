\name{ChangePointAnalyzeNorm}
\alias{ChangePointAnalyzeNorm}
\title{ChangePointAnalyzeNorm}
\usage{
ChangePointAnalyzeNorm
}
\description{
Supported C++ function used in function \code{est.changepoints}.
}
\examples{
n <- 5
max.segs <- 5
data.x <- rnorm(n, 1, 1)
data.x <- c(data.x, rnorm(n, 10,1))
data.x <- c(data.x, rnorm(n, 2,1))
data.x <- c(data.x, rnorm(n, 10,1))
data.x <- c(data.x, rnorm(n, 1,1))
data.t <- 1:(5*n)

prior <- prior.norm.A(data.x)

ChangePointAnalyzeNorm(data.x, n, max.segs, prior)
}
