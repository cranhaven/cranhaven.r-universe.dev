\name{forecastCovEstimatorsWRTdata}
\alias{forecastCovEstimatorsWRTdata}
\alias{is.forecastCovEstimatorsWRTdata}

\title{Calculate Forecast Cov of Estimators WRT Data}
\description{forecast covariance of estimated models with respect to a
 given sample}
\usage{
    forecastCovEstimatorsWRTdata(data, estimation.sample=NULL, 
                       compiled=.DSEflags()$COMPILED, discard.before=10,
                       horizons=1:12, zero=FALSE, trend=FALSE,quiet=FALSE,
                       estimation.methods=NULL)
    is.forecastCovEstimatorsWRTdata(obj)
}
\arguments{
    \item{data}{an object of class TSdata.}
    \item{estimation.methods}{a list as used by estimateModels.}
    \item{discard.before}{
       an integer indicating the number of points in the
       beginning of forecasts to discard for calculating covariances.}
    \item{zero}{if TRUE then forecastCov is also calculated for a 
       forecast of zero.}
    \item{trend}{
      if TRUE then forecastCov is also calculated for a forecast of a linear trend.}
    \item{estimation.sample}{
       an integer indicating the number of points in the
       sample to use for estimation. If it is NULL the whole sample is used.}
    \item{horizons}{horizons for which forecast covariance should be calculated.}
    \item{quiet}{if TRUE then estimation information is not printed.}
    \item{compiled}{a logical indicating if the compiled version of the code
       should be used. (FALSE would typically only be used for debugging.)}
    \item{obj}{an object.}
}
\value{
A list with the forecast covariance for supplied models on the given
sample. This is in the element \code{forecastCov} of the result. Other
elements contain information in the arguments. 
}
\details{
Calculate the forecasts cov of models estimated from data with estimation
methods indicated by estimation.methods  (see estimateModels).
estimation.sample is an integer indicating the number of points in the
sample to use for estimation. If it is NULL the whole sample is used.
}
\seealso{
\code{\link{outOfSample.forecastCovEstimatorsWRTdata}},
\code{\link{estimateModels}}
}
\examples{
data("eg1.DSE.data.diff", package="dse")
z <- forecastCovEstimatorsWRTdata(eg1.DSE.data.diff, 
    estimation.methods=list(estVARXls=list(max.lag=4)))
}
\concept{DSE}
\keyword{ts}

