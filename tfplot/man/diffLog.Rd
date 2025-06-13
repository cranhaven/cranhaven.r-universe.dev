\name{diffLog}
\alias{diffLog}
\alias{diffLog.default}
\title{Calculate the difference of log data}
\description{Calculate the difference from lag periods prior for log of data.}
\usage{
    diffLog(obj, lag=1, base = exp(1),
         names=paste("diff of log of ", seriesNames(obj)))
    \method{diffLog}{default}(obj, lag=1, base = exp(1),
         names=paste("diff of log of ", seriesNames(obj)))
}
\arguments{
    \item{obj}{A time series object.}
    \item{lag}{The difference is calculated relative to lag periods prior.}
    \item{base}{Base to use when calculating logrithms.}
    \item{names}{names for the new series (but is details).}
}
\value{A time series vector or matrix.}
\details{
  The result is a time series of the difference relative to lag periods prior for the log of 
the data. lag data points are lost from the beginning of the series. Negative
values will result in NAs.

  The \code{names} are not applied to the new series if the global option
  ModSeriesNames is FALSE. This can be set 
  with \code{options(ModSeriesNames=FALSE)}. This provides a convenient 
  mechanism to prevent changing series labels on plot axis, when the title
  may indicate that data is in year-to-year percent change so the axis label
  does not need this.
}
\examples{
z <- matrix(100 + rnorm(200),100,2)
z[z <= 0] <- 1 # not to likely, but it can happen
z <- diffLog(z)
}
\keyword{ts}

