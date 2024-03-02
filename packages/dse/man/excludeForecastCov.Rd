\name{excludeForecastCov}
\alias{excludeForecastCov}
\title{Filter Object to Remove Forecasts}
\description{Filter object to remove forecasts.}
\usage{
    excludeForecastCov(obj, exclude.series=NULL)
}
\arguments{
    \item{obj}{An object as returned by stripMine.}
    \item{exclude.series}{An indication of series to which should be excluded.}
}
\value{
The returned result is a forecastCov object like obj, but filtered to
remove any forecasts from models which depend on the series which are
indicated for exclusion.
}
\details{
Exclude results which depend on the indicated series from a 
(forecastCovEstimatorsWRTdata.subsets forecastCov) object.
}
\seealso{
\code{\link{minForecastCov}},
\code{\link{selectForecastCov}}
}
\examples{
data("eg1.DSE.data.diff", package="dse")
z <- stripMine(eg1.DSE.data.diff, essential.data=c(1,2),
                   estimation.methods=list(estVARXls=list(max.lag=3)))
z <-  excludeForecastCov(z, exclude.series=3)
}
\concept{DSE}
\keyword{ts}



