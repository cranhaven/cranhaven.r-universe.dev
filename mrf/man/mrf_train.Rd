\name{mrf_train}
\alias{mrf_train}
\title{Multiresolution Forecast}
\description{
Creates a multiresolution forecast model which can be used for forecasting with
method 'mrf_forecast' based on the manuscript [Stier et al., 2021] which is
currently in press.
}
\usage{
mrf_train(Data, Horizon=1, Aggregation="auto", Method = "r",
TimeSteps4ModelSelection=2, crit="AIC", InSample=FALSE, Threshold="hard",
Lambda=0.05, NumClusters=1, itermax=1)
}
\arguments{
\item{Data}{[1:n] Numerical vector with n values from the training data.}
\item{Horizon}{Number indicating forecast horizon. Horizon = 1 means one-step
forecast and Horizon > 1 means a one-step forecast and all multi-step forecasts
from horizon 2 to 'Horizon'. Default: Horizon = 1.}
\item{Aggregation}{[1:Scales] Numerical vector carrying numbers whose index is
associated with the wavelet level. The numbers indicate the number of time in
points used for aggregation from the original time series. Default:
Aggregation = "auto".}
\item{Method}{String indicating which method to use. Available methods: 'r'  =
Autoregression. 'nn' = Neural Network. 'elm' = Extreme Learning Machine.
'nnetar' = forecast::nnetar. Default: Method="r".}
\item{TimeSteps4ModelSelection}{Number of time steps of data (newest part) on
which a model selection is performed. Default: TimeSteps4ModelSelection = 2.}
\item{crit}{String with criterion. Available criterions: "AIC" = Akaikes Info.
Crit. "MAE" = Mean Abs. Error. "MRE" = Mean Root Error. Default: crit = "AIC".}
\item{InSample}{Boolean, deciding if in-sample-forecast based on rolling
forecasting origin is computed or not. TRUE = Computation of in-sample-forecast.
FALSE = No computation. Default: InSample = FALSE}
\item{Threshold}{Character indicating if Thresholding is done on the wavelet
decomposition or not. Default: Threshold="hard".
Possible entries:
Threshold="hard" for hard thresholding.
Threshold="soft" for soft thresholding.
Any other input indicates no thresholding.}
\item{Lambda}{Numeric value indicating the threshold for computing a hard or
soft threshold on the wavelet decomposition.}
\item{NumClusters}{Number of clusters used for parallel computing. Default:
NumClusters = 1.}
\item{itermax}{Number of iterations used in the differential evolutionary
optimization algorithm. Default: itermax = 1.}
}
\value{
List with
\item{Data}{[1:n] Numerical vector with n values from the training data.}
\item{Method}{String indicating which method to use.}
\item{Aggregation}{[1:Scales] Numerical vector carrying numbers whose index is
associated with the wavelet level. The numbers indicate the number of time in
points used for aggregation from the original time series.}
\item{CoefficientCombination}{[1:Scales+1] Numerical vector with numbers which
are associated with wavelet levels. The last number is associated with the
smooth level. Each number determines the number of coefficient used per level.
The selection follows a specific scheme.}
\item{Horizon}{Number indicating forecast horizon. Horizon = 1 means one-step
forecast and Horizon > 1 means a one-step forecast and all multi-step forecasts
from horizon 2 to 'Horizon'.}
\item{ModelError}{[1:TimeSteps4ModelSelection, 1:Horizon] Numerical matrix with
one-/multi-steps in columns and the time steps rowwise. The error is according
to the scheme of a rolling forecasting origin. The length depends on the minimum
required length for constructing the wavelet model and the length of data. The
newer part of the data is used for the model fit truncating the oldest data
according to the minimum required length for constructing the model.}
\item{ModelMAE}{Integer: Mean Absolute Error (MAE) computed for the
in-sample-forecast resulting from a rolling forecasting origin.}
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

