\name{StepSignalMargiLike_ChangePointAnalyzeNormUnRes}
\alias{StepSignalMargiLike_ChangePointAnalyzeNormUnRes}
\title{StepSignalMargiLike_ChangePointAnalyzeNormUnRes}
\usage{
StepSignalMargiLike_ChangePointAnalyzeNormUnRes
}
\description{
Supported C++ function used in function \code{est.changepoints}.
}
\examples{
n <- 5
data.x <- rnorm(n, 1, 1)
data.x <- c(data.x, rnorm(n, 10,1))
data.x <- c(data.x, rnorm(n, 2,1))
data.x <- c(data.x, rnorm(n, 10,1))
data.x <- c(data.x, rnorm(n, 1,1))
data.t <- 1:(5*n)

prior <- prior.norm.A(data.x)

ChangePointAnalyzeNormUnRes(data.x, n, prior)
}
