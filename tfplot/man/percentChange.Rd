\name{percentChange}
\alias{percentChange}
\alias{percentChange.default}
\alias{ytoypc}
\alias{ytoypc.default}
\alias{annualizedGrowth}
\alias{annualizedGrowth.default}

\title{Various Time Series Calculations}
\description{Calculate various conversions of time series.}
\usage{
    percentChange(obj, ...)
    \method{percentChange}{default}(obj, base=NULL, lag=1, cumulate=FALSE, e=FALSE, ...)

    ytoypc(obj, names = paste("y to y \%ch", seriesNames(obj)))
    \method{ytoypc}{default}(obj, names = paste("y to y \%ch", seriesNames(obj)))

    annualizedGrowth(obj, ...)
    \method{annualizedGrowth}{default}(obj, lag=1, freqLagRatio=frequency(obj)/lag,
        names=paste("Annual Growth of", seriesNames(obj)), ...) 
      }
\arguments{
    \item{obj}{
    An object on which the calculation is to be done. The default method works
    for a time series vector or matrix (with columns corresponding to series, which are treated 
    individually).}
    \item{e}{
    If e is TRUE the exponent of the series is used (after cumulating 
    if cumulate is TRUE). e can be
    a logical vector with elements corresponding to columns of obj.}
    \item{base}{
      If base is provided it is treated as the first period value 
      (that is, prior to differencing). It is prefixed to the m prior to 
      cumulating. It should be a vector of length dim(m)[2]. 
      (If e is TRUE then base should be log of the original data).}
    \item{lag}{integer indicating the number of periods relative to which the
      change should be calculated.}
    \item{cumulate}{logical indicating if the series should be cumulated before
      the percent change is calculated.}
    \item{freqLagRatio}{the ratio of \code{obj}'s frequency to the number of lags.}
    \item{names}{gives new names to be given to the calculated series.}
    \item{...}{arguments passed to other methods.}
}
\value{A time series or time series matrix.}
\details{
  \code{percentChange} calculate the percent change relative to the data lag periods prior.
  If \code{cumulate} is TRUE then the data is cumulated first. \code{cumulate} can be
  a logical vector with elements corresponding to columns of obj.

  The result is a time series of the year over year percent change. This uses 
percentChange with lag=frequency(obj).

  The \code{names} are not applied to the new series if the global option
  ModSeriesNames is FALSE. This can be set 
  with \code{options(ModSeriesNames=FALSE)}. This provides a convenient 
  mechanism to prevent changing series labels on plot axis, when the title
  may indicate that data is in year-to-year percent change so the axis label
  does not need this.

  \code{annualizedGrowth} calculates the year to year percentage growth rate using
  \code{100*((obj/shift(obj, periods= -lag))^freqLagRatio - 1)}. The default
  gives the annualized one period growth.  If \code{lag}
  is equal to the frequency of \code{obj} then the result is year-over-year 
  growth.
  }
\seealso{
\code{\link{diff}}
}
\examples{
z <- ts(matrix(100 + rnorm(200),100,2), start=c(1990,1), frequency=12)
z[z == 0] <- 1 # not to likely, but it can happen
zyypc <- ytoypc(z)
zpc <- percentChange(z)
zag <- annualizedGrowth(z)
}

\keyword{ts}

