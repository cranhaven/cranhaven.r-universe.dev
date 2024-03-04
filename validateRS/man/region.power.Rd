\name{region.power}
\alias{region.power}
\title{
Compute the power for a region and a given simple alternative hypothesis.
}
\description{
The power of a region is computed for a simple alternative hypothesis.  The region should be created by a call to \code{region.acceptance}. 
}
\usage{
region.power(region, p.1)
}
\arguments{
  \item{region}{
The region for which the power should be computed.  Should be the return value of a call to \code{region.acceptance}. 
}
  \item{p.1}{
A vector with the values for the simple alternative hypothesis. 
}
}
\details{
The power of a region is computed for a simple alternative hypothesis.  The region should be created by a call to \code{region.acceptance}. 
}
\value{
The power of the region for the given simple alternative hypothesis. 
}
\seealso{
\code{\link{region.acceptance}}
}
\examples{
mp<-region.acceptance(hypo.test="minP", p.0=c(0.001, 0.004), size=c(1000, 1000), alpha=0.5)
region.power(mp, p.1=c(0.001, 0.01))
}
