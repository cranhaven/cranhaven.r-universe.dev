\name{forecast}
\alias{forecast}
\alias{forecast.TSdata}
\alias{forecast.TSmodel}
\alias{forecast.TSestModel}
\alias{is.forecast}

\title{Forecast Multiple Steps Ahead}
\description{Calculate forecasts multiple steps ahead.}
\usage{
    is.forecast(obj)
    forecast(obj, ...)
    \method{forecast}{TSmodel}(obj, data,  horizon=36,
       conditioning.inputs=NULL, 
       conditioning.inputs.forecasts=NULL, percent=NULL, ...)
    \method{forecast}{TSestModel}(obj, ...)
    \method{forecast}{TSdata}(obj, model, ...)
}
\arguments{
    \item{obj}{An object of a class for which a specific method is available.}
    \item{model}{An object of class TSmodel.}
    \item{data}{An object of class TSdata.}
    \item{conditioning.inputs}{
    A time series matrix or list of time series matrices to use as input variables.}
    \item{conditioning.inputs.forecasts}{
    A time series matrix or list of time series matrices to append to input
variables for the forecast periods.}
    \item{horizon }{The number of periods to forecast.}
    \item{percent}{
    A vector indication percentages of the last input to use for 
    forecast periods.}
    \item{...}{arguments passed to l().}
}
\value{
The result is an object of class forecast which is a list with 
elements \code{model}, \code{horizon}, \code{conditioning.inputs}, 
\code{percent}, \code{pred} and \code{forecast}.
The element \code{forecast} is a list with TSdata objects as elements, one for each element
in the list conditioning.inputs.
The element \code{pred} contains the one-step ahead forecasts for the preiods when output data
is available. 
There is a plot method for this class.
}
\details{
Calculate (multiple) forecasts from the end of data to a horizon determined
either from supplied input data or the argument horizon (more details below).
In  the case of a model with no inputs the horizon is determined by the
argument horizon.
In the case of models with inputs, on which the forecasts
are conditioned, the argument horizon is ignored (except when percent is
specified) and the actual horizon is determined by the inputs in the 
following way:
If inputs are not specified by optional arguments (as below) then the default
will be to use inputData(data). This will be the same as the function l() unless
inputData(data) is longer than
outputData(data) (after NAs are trimmed from each separately).
Otherwise, if conditioning.inputs is specified it is used for inputData(data).
It must be a time series matrix or a list of time series matrices each
of which is used in turn as inputData(data). The default above is the same as
forecast(model, trimNA(data), conditioning.inputs=trimNA(inputData(data)) )
Otherwise, if conditioning.inputs.forecasts is specified it is appended 
to inputData(data). It must be a time series  
matrix or a list of time series matrices each of which is 
appended to inputData(data) and the concatenation used as conditioning.inputs.
Both conditioning.inputs and conditioning.inputs.forecasts should not be
specified.
Otherwise, if percent is specified then conditioning.inputs.forecasts are set
to percent/100 times the value of input corresponding to the last period
of outputData(data) and used for horizon periods. percent can be a vector, 
in which case each value is applied in turn. ie c(90,100,110) would would 
give results for conditioning.input.forecasts 10 percent above and below 
the last value of input.
}
\seealso{
\code{\link{featherForecasts}},
\code{\link{horizonForecasts}}
}
\examples{
    data("egJofF.1dec93.data", package="dse")
    model <- estVARXls(window(egJofF.1dec93.data, end=c(1985,12)))
    pr <- forecast(model, conditioning.inputs=inputData(egJofF.1dec93.data))
    #tfplot(pr) Rbug 0.90.1
    is.forecast(pr)
} 
\concept{DSE}
\keyword{ts}

