\name{estimatorsHorizonForecastsWRTdata}
\alias{estimatorsHorizonForecastsWRTdata}
\title{Estimate models and forecast at given horizons}
\description{Estimate models and forecast at given horizons.}
\usage{
    estimatorsHorizonForecastsWRTdata(data, 
                       estimation.sample=.5, horizons=1:12,quiet=FALSE,
                       estimation.methods=NULL)
}
\arguments{
    \item{data}{A TSdata object.}
    \item{estimation.methods}{
    A list of estimation methods to use. (See estimateModels.)}
    \item{estimation.sample}{The portion  of the sample to use for estimation.}
    \item{horizons}{The horizons for which forecasts are to be produced.}
    \item{quiet}{If true no estimation information is printed.}
}
\value{A list of forecasts at different horizons as returned by horizonForecasts.
}
\details{
estimation.sample indicates the part of the data to use for estimation.
If estimation.sample is less than or equal 1.0 it is
used to indicate the portion of points to use for estimation.
Otherwise it should be an integer and is used to indicate the number
of points from the beginning of the sample to use for estimation. 
}
\seealso{
\code{\link{estimateModels}},
\code{\link{horizonForecasts}}
}
\examples{
data("eg1.DSE.data.diff", package="dse")
z <-  estimatorsHorizonForecastsWRTdata(eg1.DSE.data.diff, 
    estimation.methods=list(estVARXls=list(max.lag=3), 
                estVARXar=list(max.lag=3)))
}
\concept{DSE}
\keyword{ts}

