\name{wavelet_decomposition}
\alias{wavelet_decomposition}
\title{Redundant Haar Wavelet Decomposition}
\description{
This function decomposes a time series in its wavelet and smooth coefficients
using the redundant Haar wavelet transform.
}
\usage{
wavelet_decomposition(UnivariateData, Aggregation = c(2, 4, 8, 16, 32),
Threshold="hard", Lambda=0.05)
}
\arguments{
\item{UnivariateData}{[1:n] Numerical vector with n time series values}
\item{Aggregation}{[1:Scales] Numerical vector of length 'Scales' carrying
numbers whose index is associated with the wavelet level. The numbers indicate
the number of values used for aggregation from the original time series.}
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
\item{UnivariateData}{[1:n] Numerical vector with n time series values.}
\item{WaveletCoefficients}{[Scales, n] Matrix with 'Scales' many wavelet scales
row-wise with n columns corresponding to the time domain of a time series.}
\item{SmoothCoefficients}{[Scales, n] Matrix with 'Scales' many smooth
approximation scales row-wise with n columns corresponding to the time domain of
a time series.}
\item{Scales}{Number of wavelet levels.}
}
\details{
The resulting wavelet and smooth coefficients are stored in so called wavelet
and smooth part levels.
The smooth part level is created from the original times series by aggregation
(average). This makes the times series in some sense smoother, hence the naming.
Each individual smooth part level can be created from the original time series
by aggregating over different number of values. The different smooth part levels
are ordered, so that the number of values used for aggregation are ascending.
A dyadic scheme is recommended (increasing sequences of the power of two). The
dyadic scheme for 5 levels would require agg_per_lvl = c(2, 4, 8, 16, 32). So
the first smooth part level would be the average of two points of the original
time series, the second smooth part level would be the average of four points,
and so on.
This averaging is applied asymmetrical. That means, that the result of the
average of a sequence of points is obtained for the last point in time of that
sequence. So each smooth part level starts with a certain offset, since no
average can be obtained for the first particular points in time.
The wavelet levels are the differences between the original time series and
the smooth levels. The first wavelet level is the difference of the original
time series and the first smooth part level. The second wavelet level is the
difference of the first and second smooth part level and so on.
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
plot(AirPassengers, type = "l", col = "black")
UnivariateData = as.vector(array(AirPassengers))
dec_res = wavelet_decomposition(UnivariateData, Aggregation = c(2,4))
plot(dec_res$SmoothCoefficients[2,4:length(dec_res$SmoothCoefficients[2,])],
type = "l", col = "blue")
lines(array(AirPassengers)[4:length(dec_res$SmoothCoefficients[2,])],
col = "black")
}
\keyword{Wavelets}
