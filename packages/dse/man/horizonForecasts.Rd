\name{horizonForecasts}
\alias{horizonForecasts}
\alias{horizonForecasts.TSdata}
\alias{horizonForecasts.TSmodel}
\alias{horizonForecasts.TSestModel}
\alias{horizonForecasts.forecastCov}
\alias{is.horizonForecasts}

\title{Calculate forecasts at specified horizons}
\description{Calculate forecasts at specified horizons.}
\usage{
    is.horizonForecasts(obj)
    horizonForecasts(obj, ...)
    \method{horizonForecasts}{TSmodel}(obj, data, horizons=1:4,
	 discard.before=minimumStartupLag(obj), compiled=.DSEflags()$COMPILED, ...)
    \method{horizonForecasts}{TSestModel}(obj, data=NULL, ...)
    \method{horizonForecasts}{TSdata}(obj, model, ...)
    \method{horizonForecasts}{forecastCov}(obj,horizons=NULL, 
         discard.before=NULL, ...)
}
\arguments{
    \item{obj}{an object of class TSmodel, TSdata, or TSestModel.}
    \item{model}{an object of class TSmodel.}
    \item{data}{an object of class TSdata} 
    \item{horizons}{
    a vector of integers indicating the horizon at which forecasts should be 
    produced.}
    \item{discard.before}{period before which forecasts are not calculated.}
    \item{compiled}{if TRUE compiled code is called.}
    \item{...}{arguments passed to other methods.}
}
\value{
The result is a list of class horizonForecasts with elements model (a 
TSmodel), data, horizons, discard.before, and horizonForecasts.
horizonForecasts is an array with three dimension: 
\code{c(length(horizons), dim(model$data))}.
Projections are not calculated before discard.before or after
the end of outputData(data).
Each horizon is aligned so that horizonForecasts[h,t,] contains the forecast
for the data point outputData(data)[t,] (from horizon[h] periods prior).
}
\details{
Calculate multiple 'horizon'-step ahead forecasts 
ie. calculate forecasts but return only those indicated by horizons.
   Thus, for example, the result of
 horizonForecasts(model, data horizons=c(1,5))    
 would be the one-step ahead and five step ahead forecasts.
}
\seealso{
\code{\link{featherForecasts}}
}
\examples{
data("eg1.DSE.data.diff", package="dse")
model <- estVARXls(eg1.DSE.data.diff)
z <-  horizonForecasts(model, eg1.DSE.data.diff)
}
\concept{DSE}
\keyword{ts}

