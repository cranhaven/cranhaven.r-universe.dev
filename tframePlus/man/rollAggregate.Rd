\name{rollAggregate}
\alias{rollAggregate}

\title{Rolling Time Series Aggregate}
\description{Aggregate time series using a rolling window.}

\usage{
    rollAggregate(x, FUN=sum, na.rm=FALSE, aggPeriods=4, ...)
}

\arguments{
    \item{x}{a time series object.}
    \item{FUN}{the function to use for aggregating.}
    \item{na.rm}{Logical indicating if \code{NA} should be removed from 
       the beginning and end of a series}
    \item{aggPeriods}{the number of observations to use in the rolling window.}
    \item{...}{additional arguments passed to \code{apply}}
}
\value{time series}
\details{
  Functions \code{rollAggregate} uses apply with \code{FUN} on a rolling 
  window set by \code{aggPeriods}. 
  Partial windows may result in \code{NA},
  depending on \code{FUN}. \code{NA} in the beginning or end are 
  dropped if \code{na.rm=TRUE}. 
  The frequency of the resulting series remains the same as the original, but
  the result will be \code{aggPeriods} shorter (i.e. have NAs).
  The default will be a four period moving sum (which roughly converts weekly
  data to something approximating monthly, but at a weekly frequency).
}
\seealso{
    \code{\link{as.annually}},
    \code{\link{as.quarterly}},
    \code{\link{as.weekly}},
    \code{\link[base]{apply}}
}
\examples{
  rollAggregate(ts(1:20, start = c(1999,1), frequency=1)) 
  rollAggregate(ts(1:20, start = c(1999,1), frequency=12), aggPeriods=3) 
  
  if(require("zoo")) {
    z <- zoo(rnorm(100),  order.by = Sys.Date() + 1:100)
    rollAggregate(as.weekly(z), aggPeriods=4, FUN=mean)
    require("tfplot")
    tfplot(as.weekly(z),rollAggregate(as.weekly(z), aggPeriods=4, 
       FUN=mean, na.rm=TRUE))
    tfplot(z,rollAggregate(z, aggPeriods=28, FUN=mean))
    }
}
\keyword{utilities}
\keyword{ts}
