\name{wavelet_prediction_equation}
\alias{wavelet_prediction_equation}
\title{One Step Forecast with Regression}
\description{
This function delivers the required wavelet and smooth coefficients from the
decomposition based on a prediction scheme.
}
\usage{
wavelet_prediction_equation(WaveletCoefficients, SmoothCoefficients,
CoefficientCombination, Aggregation)
}
\arguments{
\item{WaveletCoefficients}{[Scales, n] Matrix with 'Scales' many wavelet scales
row-wise with n columns corresponding to the time domain of a time series.}
\item{SmoothCoefficients}{[Scales, n] Matrix with 'Scales' many smooth
approximation scales row-wise with n columns corresponding to the time domain of
a time series.}
\item{CoefficientCombination}{[1:Scales+1] Numerical vector with numbers which
are associated with wavelet levels. The last number is associated with the
smooth level. Each number determines the number of coefficient used per level.
The selection follows a specific scheme.}
\item{Aggregation}{[1:Scales] Numerical vector carrying numbers whose index is
associated with the wavelet level. The numbers indicate the number of time in
points used for aggregation from the original time series.}
}
\value{
\item{future_point}{Numerical value carrying one step forecast.}
}
\references{
Aussem, A., Campbell, J., and Murtagh, F. Waveletbased Feature Extraction
and Decomposition Strategies for Financial Forecasting. International Journal of
Computational Intelligence in Finance, 6,5-12, 1998.

Renaud, O., Starck, J.-L., and Murtagh, F. Prediction based on a Multiscale De-
composition. International Journal of Wavelets, Multiresolution and Information
Processing, 1(2):217-232. doi:10.1142/S0219691303000153, 2003.

Murtagh, F., Starck, J.-L., and Renaud, O. On Neuro-Wavelet Modeling. Decision
Support Systems, 37(4):475-484. doi:10.1016/S0167-9236(03)00092-7, 2004.

Renaud, O., Starck, J.-L., and Murtagh, F. Wavelet-based combined Signal Filter-
ing and Prediction. IEEE Transactions on Systems, Man, and Cybernetics, Part
B (Cybernetics), 35(6):1241-1251. doi:10.1109/TSMCB.2005.850182, 2005.
}
\author{
Quirin Stier
}
\examples{
data(AirPassengers)
len_data = length(array(AirPassengers))
CoefficientCombination = c(1,1,1)
Aggregation = c(2,4)
UnivariateData = as.vector(AirPassengers)
# Decomposition
dec_res <- wavelet_decomposition(UnivariateData, Aggregation)
# Training
trs_res <- wavelet_training_equations(UnivariateData,
                                      dec_res$WaveletCoefficients,
                                      dec_res$SmoothCoefficients,
                                      dec_res$Scales,
                                      CoefficientCombination, Aggregation)
arr_future_points = trs_res$points_in_future
matrix = trs_res$lsmatrix
# Optimization method
weights = mrf_regression_lsm_optimization(arr_future_points, matrix)
# Forecast
scheme = wavelet_prediction_equation(dec_res$WaveletCoefficients,
dec_res$SmoothCoefficients, CoefficientCombination, Aggregation)
forecast = weights %*% scheme
}
\keyword{Wavelets}
