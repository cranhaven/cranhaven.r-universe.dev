\name{featherForecasts}
\alias{featherForecasts}
\alias{featherForecasts.TSdata}
\alias{featherForecasts.TSmodel}
\alias{featherForecasts.TSestModel}
\alias{is.featherForecasts}

\title{Multiple Horizon-Step Ahead Forecasts}
\description{Calculate multiple horizon-step ahead forecasts.}
\usage{
    featherForecasts(obj, ...)
    \method{featherForecasts}{TSestModel}(obj, data=NULL, ...)
    \method{featherForecasts}{TSdata}(obj, model, ...)
    \method{featherForecasts}{TSmodel}(obj, data, horizon=36,
             from.periods =NULL, ...)
    is.featherForecasts(obj)
}
\arguments{
    \item{obj}{an object of class TSmodel.}
    \item{data}{an object of class TSdata.}
    \item{model}{an object of class TSmodel.}
    \item{from.periods}{the starting points to use for forecasts.}
    \item{horizon}{the number of periods to forecast.}
    \item{...}{for a TSmodel additional arguments are passed to \code{l()}}
}
\value{
The result is a list of class featherForecasts with elements \code{model} (a 
TSestModel), \code{data}, \code{from.periods}, \code{featherForecasts}.
The element \code{featherForecasts} is a list with length(from.periods) elements, each of which
is a tframed matrix.
There is a plot method for this class.
}
\details{
Calculate multiple horizon-step ahead forecasts 
ie. use the samples indicated by from.periods to calculate forecasts for 
horizon periods.
Thus, for example, the result of
featherForecasts(model, data, from.periods=c(200,250,300))
would be forecasts for 1 through 36 steps ahead (the default),
starting at the 200th,250th, and 300th point of outputData(data).
This function assumes that inputData(data) (the exogenous 
variable) is as long as necessary for the most future forecast.
}
\seealso{
\code{\link{forecast}},
\code{\link{horizonForecasts}}
}
\examples{
data("egJofF.1dec93.data", package="dse")
model <- estVARXls(egJofF.1dec93.data)
pr <- featherForecasts(model, egJofF.1dec93.data)
tfplot(pr)
}
\concept{DSE}
\keyword{ts}

