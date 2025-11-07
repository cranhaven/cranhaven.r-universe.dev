\name{print.funLBM}
\alias{print.funLBM}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Printing co-clustering results of funLBM
}
\description{
Printing a summary of the funLBM co-clustering results
}
\usage{
\method{print}{funLBM}(x,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
An object produced by the funLBM function,
}
  \item{\dots}{
Additional arguments to provide.
}
}
\seealso{
\code{\link{funLBM}}
}
\examples{
\donttest{
## Co-clustering of simulated data
set.seed(12345)
X = simulateData(n = 30, p = 30, t = 15)
out = funLBM(X$data,K=4,L=3)
out
}
}
