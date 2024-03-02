\name{selectForecastCov}
\alias{selectForecastCov}
\title{Select Forecast Covariances Meeting Criteria}
\description{Select forecast covariances meeting given criteria.}
\usage{
    selectForecastCov(obj, series=1, 
    select.cov.best=1,
    select.cov.bound=NULL,
    ranked.on.cov.bound=NULL,
    verbose=TRUE)
}
\arguments{
    \item{obj}{an object as returned by stripMine.}
    \item{series}{
    an indication of series to which the tests should be applied.}
    \item{select.cov.best}{the number of 'best' forecasts to select.}
    \item{select.cov.bound}{a bound to use as criteria for selection.}
    \item{ranked.on.cov.bound}{see details.}
    \item{verbose}{if verbose=TRUE then summary results are printed.}
}
\value{
The returned result is a forecastCov object like obj, but filtered
to remove models which do not meet criteria.
}
\details{
Select models with forecast covariance for \code{series} meeting criteria.
The default \code{select.cov.best=1} selects the best model at each horizon.
\code{select.cov.best=3} would select the best 3 models at each horizon.
If \code{select.cov.bound} is not \code{NULL} then  \code{select.cov.best}
 is ignored and
any model which is better than the bound at all horizons is selected.
\code{select.cov.bound} can be a vector of the same length 
as \code{series},
in which case corresponding elements are applied to the different series.
  Any model which is better than the bound at all horizons is selected.
 \code{ranked.on.cov.bound} is is used if it is not NULL and 
 \code{select.cov.bound} is
  \code{NULL}. In this case \code{select.cov.best} is ignored.
  \code{ranked.on.cov.bound} should be a positive integer. The forecast
  covariances are ranked by there maximum over the horizon and the
  lowest number up to \code{ranked.on.cov.bound} are selected. This amounts
  to adjusting the covariance bound to allow for the given number of
  models to be selected. If \code{series} is a vector the results are 
  the best up to the given number on any series!
 \code{select.cov.bound} can be a vector of the same length as 
 \code{series},
  in which case corresponding elements are applied to the different series.
 If \code{verbose=TRUE} then summary results are printed.
 The returned result is a \code{forecastCov} object like obj, but filtered
  to remove models which do not meet criteria.
}
\seealso{
\code{\link{minForecastCov}},
\code{\link{excludeForecastCov}}
}
\examples{
data("eg1.DSE.data.diff", package="dse")
z <- stripMine(eg1.DSE.data.diff, essential.data=c(1,2),
                   estimation.methods=list(estVARXls=list(max.lag=3)))
z <-  selectForecastCov(z)
tfplot(selectForecastCov(z, select.cov.bound=20000))
tfplot(selectForecastCov(z, select.cov.best=1))
}
\concept{DSE}
\keyword{ts}

