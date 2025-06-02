\name{mrf_rolling_forecasting_origin}
\alias{mrf_rolling_forecasting_origin}
\title{Rolling forecasting origin for Multiresolution Forecasts}
\description{
This function computes a rolling forecasting origin for one- or multi-step
forecasts with a specific method based on the manuscript [Stier  et al., 2021]
which is currently in press. Multi-step forecasts are computed recursively
with the one step forecast method.
}
\usage{
mrf_rolling_forecasting_origin(UnivariateData, Aggregation,
CoefficientCombination=NULL, Horizon = 2, Window = 3, Method = "r",
NumClusters = 1,
Threshold="hard", Lambda=0.05)
}
\arguments{
\item{UnivariateData}{[1:n] Numerical vector with n values.}
\item{CoefficientCombination}{[1:Scales+1] Numerical vector with numbers which
are associated with wavelet levels. The last number is associated with the
smooth level. Each number determines the number of coefficient used per level.
The selection follows a specific scheme.}
\item{Aggregation}{[1:Scales] Numerical vector carrying numbers whose index is
associated with the wavelet level. The numbers indicate the number of time in
points used for aggregation from the original time series.}
\item{Horizon}{Number indicating horizon for forecast from 1 to horizon.}
\item{Window}{Number indicating how many points are used for cross validation.}
\item{Method}{String indicating which method to use. Available methods: 'r'  =
Autoregression. 'nn' = Neural Network. 'elm' = Extreme Learning Machine.
'nnetar' = forecast::nnetar. Default: Method="r".}
\item{NumClusters}{Number of clusters used for parallel computing.}
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
\item{Error}{[1:Window,1:Horizon] Numerical Matrix with 'Window' many row
entries indicating one time point with 'Horizon' many forecast errors.}
\item{Forecast}{[1:Window,1:Horizon] Numerical Matrix with 'Window' many row
entries indicating one time point with 'Horizon' many forecasts.}
}
\details{
Thus, h-step forecast for h = 1,..., horizon for window_size many steps can be
computed.
The forecasting method can be an autoregression or a neural network (multilayer
perceptron).
The CoefficientCombination parameter controls the number of coefficients chosen for each wavelet
and smooth part level individually.
The NumClusters parameter determines the number of cluster used for parallel
computation. NumClusters = 1 performs a non parallel version.
NumClusters is constrained by the maximum number of clusters available minus one
to prevent the machine to be overchallenged.
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
UnivariateData=as.vector(array(AirPassengers))
res = mrf_rolling_forecasting_origin(UnivariateData,
                                 CoefficientCombination = c(10,10,10),
                                 Aggregation = c(2,4),
                                 Horizon = 2, Window = 3, Method = "r",
                                 NumClusters = 1)
Error = res$Error
Forecast = res$Forecast
}
\concept{Rolling forecasting origin}
