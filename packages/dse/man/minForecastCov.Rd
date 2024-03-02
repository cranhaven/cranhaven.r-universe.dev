\name{minForecastCov}
\alias{minForecastCov}
\title{Minimum Forecast Cov Models}
\description{Extract the minimum forecastCov at each horizon}
\usage{
    minForecastCov(obj, series=1, verbose=TRUE)
}
\arguments{
    \item{obj}{An object as returned by stripMine.}
    \item{series}{
    An indicator of the series which are to be used as the bases for selection.}
    \item{verbose}{If true additional information is printed.}
}
\value{
The returned object is a vector indicating the element of forecastCov which
was the min at each horizon.
}
\details{
Select the min covariance (for series only!) at each horizon and
print.  The returned object is a vector indicating the element of
forecastCov which was the min at each horizon. It is suitable as an
argument to plot eg:  tfplot(obj, select.cov=minForecastCov(obj)) The
results of this plot are similar to the default results of
tfplot(selectForecastCov(obj)). Covariance information and information
about the horizon where the model is optimal are given.
}
\seealso{
\code{\link{selectForecastCov}},
\code{\link{excludeForecastCov}}
}
\examples{
data("eg1.DSE.data.diff", package="dse")
z <- stripMine(eg1.DSE.data.diff, essential.data=c(1,2),
                   estimation.methods=list(estVARXls=list(max.lag=3)))
z <-  minForecastCov(z)
}
\concept{DSE}
\keyword{ts}

