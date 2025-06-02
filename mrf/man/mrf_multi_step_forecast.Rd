\name{mrf_multi_step_forecast}
\alias{mrf_multi_step_forecast}
\title{Multiresolution Forecast}
\description{
This function creates a multi step forecast for all horizons from 1 to steps
based on the manuscript [Stier et al., 2021] which is currently in press.
The deployed forecast method can be an autoregression or a neural network
(multilayer perceptron with one hidden layer). Multi step forecasts are computed
recursively.
}
\usage{
mrf_multi_step_forecast(UnivariateData, Horizon, Aggregation,
CoefficientCombination=NULL, Method = "r", Threshold="hard", Lambda=0.05)
}
\arguments{
\item{UnivariateData}{[1:n] Numerical vector with n values.}
\item{Horizon}{Number indicating horizon for forecast from 1 to horizon.}
\item{CoefficientCombination}{[1:Scales+1] Numerical vector with numbers which
are associated with wavelet levels. The last number is associated with the
smooth level. Each number determines the number of coefficient used per level.
The selection follows a specific scheme.}
\item{Aggregation}{[1:Scales] Numerical vector carrying numbers whose index is
associated with the wavelet level. The numbers indicate the number of time in
points used for aggregation from the original time series.}
\item{Method}{String indicating which method to use. Available methods: 'r'  =
Autoregression. 'nn' = Neural Network. 'elm' = Extreme Learning Machine.
'nnetar' = forecast::nnetar. Default: Method="r".}
\item{Threshold}{Character indicating if Thresholding is done on the wavelet
decomposition or not. Default: Threshold="hard".
Possible entries:
Threshold="hard" for hard thresholding.
Threshold="soft" for soft thresholding.
Any other input indicates no thresholding.}
\item{Lambda}{Numeric value indicating the threshold for computing a hard or
soft threshold on the wavelet decomposition.}
}
\value{
List of
\item{multistep}{[1:Horizon] Numerical vector with forecast of horizon according
to its index.}
}
\references{
[Stier et al., 2021] Stier, Q.,Gehlert, T. and Thrun, M. C.: Multiresolution Forecasting for
Industrial Applications, Processess, 2021.
}
\author{
Quirin Stier
}
\examples{
data(AirPassengers)
len_data = length(array(AirPassengers))
UnivariateData = as.vector(AirPassengers)[1:(len_data-1)]
# One-step forecast (Multiresolution Forecast)
one_step = mrf_multi_step_forecast(UnivariateData = UnivariateData,
                                    Horizon = 2,
                                    CoefficientCombination = c(1,1,1),
                                    Aggregation = c(2,4),
                                    Method="r")
# Multi-step forecast (Multiresolution Forecast)
# Horizon = 2 => Forecast with Horizon 1 and 2 as vector
multi_step = mrf_multi_step_forecast(UnivariateData = UnivariateData,
                                      Horizon = 2,
                                      CoefficientCombination = c(1,1,1),
                                      Aggregation = c(2,4),
                                      Method="r")
}
\concept{Multi step forecasts}

