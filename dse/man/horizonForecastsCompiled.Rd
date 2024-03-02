\name{horizonForecastsCompiled}
\alias{horizonForecastsCompiled}
\alias{horizonForecastsCompiled.ARMA}
\alias{horizonForecastsCompiled.SS}

\title{Calculate forecasts at specified horizons}
\description{Calculate forecasts at specified horizons.}
\usage{
   horizonForecastsCompiled(obj, data, horizons=1:4,
	  discard.before=minimumStartupLag(obj))
   \method{horizonForecastsCompiled}{SS}(obj, data, horizons=1:4,
	 discard.before=minimumStartupLag(obj))
   \method{horizonForecastsCompiled}{ARMA}(obj, data, horizons=1:4,
	  discard.before=minimumStartupLag(obj))
}
\arguments{
    \item{obj}{see horizonForecasts.}
    \item{data}{see horizonForecasts.} 
    \item{horizons}{see horizonForecasts.}
    \item{discard.before}{see horizonForecasts.}
}
\value{See horizonForecasts.}
\details{Internal function not to be called by users. See horizonForecasts.}
\seealso{
\code{\link{horizonForecasts}}
}
\examples{
data("eg1.DSE.data.diff", package="dse")
model <- estVARXls(eg1.DSE.data.diff)
z <-  horizonForecasts(model, eg1.DSE.data.diff)
}
\concept{DSE}
\keyword{ts}

