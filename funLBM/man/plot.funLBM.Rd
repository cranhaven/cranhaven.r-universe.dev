\name{plot.funLBM}
\alias{plot.funLBM}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plotting co-clustering results of funLBM
}
\description{
Plotting of funLBM co-clustering results:  functional means, block matrix, parameters, ...
}
\usage{
\method{plot}{funLBM}(x,type='blocks',...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
An object produced by the funLBM function,
}
  \item{type}{
The type of plot to display. Possible plots are 'blocks' (default), 'means', 'evolution', 'likelihood', 'proportions',
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

# Visualization of results
plot(out,type='blocks')
plot(out,type='proportions')
plot(out,type='means')
}
}
