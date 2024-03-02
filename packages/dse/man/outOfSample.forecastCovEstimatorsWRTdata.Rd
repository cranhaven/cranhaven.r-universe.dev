\name{outOfSample.forecastCovEstimatorsWRTdata}
\alias{outOfSample.forecastCovEstimatorsWRTdata}
\title{Calculate Out-of-Sample Forecasts}
\description{Calculate out-of-sample forecasts.}
\usage{
    outOfSample.forecastCovEstimatorsWRTdata(data, zero=FALSE, trend=FALSE,
                       estimation.sample=.5, horizons=1:12,quiet=FALSE,
                       estimation.methods=NULL, compiled=.DSEflags()$COMPILED)
}
\arguments{
    \item{data}{an object of class TSdata.}
    \item{estimation.methods}{a list as used by estimateModels.}
    \item{zero}{if TRUE then forecastCov is also calculated for a forecast of zero.}
    \item{trend}{
    if TRUE then forecastCov is also calculated for a forecast of a linear trend.}
    \item{estimation.sample}{
    indicates the portion of the data to use for estimation.
If estimation.sample is an integer then it is used to indicate the number
of points in the sample to use for estimation. If it is a fracton it is
used to indicate the portion of points to use for estimation. The remainder
of the sample is used for evaluating forecasts.}
    \item{horizons}{horizons for which forecast covariance should be calculated.}
    \item{quiet}{if TRUE then estimation information is not printed.}
    \item{compiled}{a logical indicating if compiled code should be
      used. (Usually true except for debugging.)}
}
\value{An object as returned by forecastCovEstimatorsWRTdata.}
\details{
The data is slpit into a sub-sample used for estimation and another
sub-sample used for calculating the forecast covariance.
}
\seealso{
\code{\link{forecastCovEstimatorsWRTdata}},
\code{\link{forecastCovEstimatorsWRTtrue}},
\code{\link{estimateModels}}
}
\examples{
data("eg1.DSE.data.diff", package="dse")
z <-  outOfSample.forecastCovEstimatorsWRTdata(eg1.DSE.data.diff,
           estimation.methods=list(estVARXls=list(max.lag=4)))
}
\concept{DSE}
\keyword{ts}

