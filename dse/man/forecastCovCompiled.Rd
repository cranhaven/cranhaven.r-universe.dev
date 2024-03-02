\name{forecastCovCompiled}
\alias{forecastCovCompiled}
\alias{forecastCovCompiled.ARMA}
\alias{forecastCovCompiled.SS}
\alias{forecastCovCompiled.innov}
\alias{forecastCovCompiled.nonInnov}
\alias{forecastCovSingleModel}

\title{Forecast covariance for different models - internal}
\description{See forecastCov.}
\usage{
    forecastCovCompiled(model, data, horizons = 1:12,
         discard.before=minimumStartupLag(model))
    \method{forecastCovCompiled}{ARMA}(model, data, horizons = 1:12,
         discard.before=minimumStartupLag(model))
    \method{forecastCovCompiled}{SS}(model, data, horizons = 1:12,
         discard.before=minimumStartupLag(model))
    \method{forecastCovCompiled}{innov}(model, data, horizons = 1:12,
         discard.before=minimumStartupLag(model))
    \method{forecastCovCompiled}{nonInnov}(model, data, horizons = 1:12,
         discard.before=minimumStartupLag(model))
     forecastCovSingleModel( model, data=NULL, horizons=1:12, 
          discard.before=minimumStartupLag(model), compiled=.DSEflags()$COMPILED)
}
\arguments{
    \item{obj}{TSdata or one or more TSmodels or TSestModels}
    \item{data}{an object of class TSdata.}
    \item{discard.before}{
       period before which forecasts should be discarded when calculating covariance.}
    \item{horizons}{horizons for which forecast covariance should be calculated.}
    \item{zero}{if TRUE the covariance is calculated for a forecast of zero.}
    \item{trend}{if TRUE the covariance is calculated for a forecast of trend.}
    \item{estimation.sample}{portion of the sample to use for calculating the trend.}
    \item{compiled}{a logical indicating if compiled code should be
      used. (Usually true except for debugging.)}
    \item{...}{arguments passed to other methods.}
}
\value{
    A list with the forecast covariance for supplied models on the given
    sample. This is in the element \code{forecastCov} of the result. Other
    elements contain information in the arguments.
}
\details{
Not to be called by users. See \code{forecastCov}.
}

\concept{DSE}
\keyword{internal}

