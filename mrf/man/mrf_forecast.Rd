\name{mrf_forecast}
\alias{mrf_forecast}
\title{Multiresolution Forecast}
\description{
Creates a multiresolution forecast for a given multiresolution model based on [Stier et al., 2021] which is currently in press.
(mrf_train).
}
\usage{
mrf_forecast(Model, Horizon=1)
}
\arguments{
\item{Model}{List containing model specifications from mrf_train().}
\item{Horizon}{Number indicating horizon for forecast from 1 to horizon.}
}
\value{
List of
\item{Forecast}{[1:Horizon] Numerical vector with forecast of horizon according
to its index.}
\item{Model}{List containing model specifications from mrf_train().}
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
Data = as.vector(AirPassengers)
len_data = length(Data)
Train = Data[1:(len_data-2)]
Test = Data[(len_data-1):len_data]
# One-step forecast (Multiresolution Forecast)
model = mrf_train(Train)
one_step = mrf_forecast(model, Horizon=1)
Error = one_step$Forecast - Test[1]
# Multi-step forecast (Multiresolution Forecast)
# Horizon = 2 => Forecast with Horizon 1 and 2 as vector
model = mrf_train(Train, Horizon=2)
multi_step = mrf_forecast(model, Horizon=2)
Error = multi_step$Forecast - Test
}
\concept{Time Series Forecasting}
\concept{Univariate Time Series Forecasting}
\concept{Seasonal Univariate Time Series Forecasting}
\concept{Multiresolution Forecasting}
\concept{One-step forecasts}
\concept{Multi-step forecasts}
\concept{Evolutionary Optimization}
\concept{Differential Evolution}
