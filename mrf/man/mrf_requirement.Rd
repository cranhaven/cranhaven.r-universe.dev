\name{mrf_requirement}
\alias{mrf_requirement}
\title{Multiresolution Forecast Requirements}
\description{
Computes requirements for given model using insights of various published papers
and the manuscript [Stier et al., 2021] which is currently in press.
}
\usage{
mrf_requirement(UnivariateData, CoefficientCombination, Aggregation)
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
}
\value{
List of
\item{MinLen}{Integer minimum required length for model.}
\item{StartTraining}{Integer indicating the index of time series at which the
training equations can be built up.}
\item{NumberWeights}{Number of weights required for building model.}
\item{NumberEquations}{Number of equations which can be built with given data.}
}
\references{
[Stier et al., 2021] Stier, Q.,Gehlert, T. and Thrun, M. C.: Multiresolution Forecasting for
Industrial Applications, Processess, 2021.

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
data(entsoe)
UnivariateData = entsoe$value
mrf_requirement(UnivariateData, c(2,3,4), c(2,4))
}
\concept{Multiresolution Forecasting}
