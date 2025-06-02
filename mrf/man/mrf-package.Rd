\name{mrf-package}
\alias{mrf-package}
\title{\packageTitle{mrf}}
\description{
\packageDescription{mrf}
The package consists of a multiresolution forecasting method using a
redundant Haar wavelet transform based on the manuscript [Stier et al., 2021]
which is currently in press.
One-step and multi-step forecasts are computable with this method.
Nested and non-nested cross validation is possible.
}
\details{
Forecasting of univariate time series using feature extraction with variable
prediction methods is provided. Feature extraction is done with a redundant Haar
wavelet transform with filter h = (0.5, 0.5). The advantage of the approach
compared to typical Fourier based methods is an dynamic adaptation to varying
seasonalities. Currently implemented prediction methods based on the selected
wavelets levels and scales are a regression and a multi-layer perceptron.
Forecasts can be computed for horizon 1 or higher. Model selection is performed
with an evolutionary optimization. Selection criterias are currently the AIC
criterion, the Mean Absolute Error or the Mean Root Error. The data is split
into three parts for model selection: Training, test, and evaluation dataset.
The training data is for computing the weights of a parameter set. The test data
is for choosing the best parameter set. The evaluation data is for assessing the
forecast performance of the best parameter set on new data unknown to the model.

\tabular{ll}{
Package: \tab mrf \cr
Type: \tab Package\cr
Version: \tab 0.1.4 \cr
Date: \tab 2021-09-20\cr
License: \tab CC BY-NC-SA 4.0\cr
}
}
\references{
[Stier et al., 2021] Stier, Q.; Gehlert, T.; Thrun, M.C. Multiresolution
Forecasting for Industrial Applications. Processes 2021, 9, 1697.
https://doi.org/10.3390/pr9101697
}
\author{
Quirin Stier
}
\keyword{Multiresolution}
\keyword{Wavelets}
\concept{Univariate Time Series Forecasting}
\concept{Seasonal Univariate Time Series Forecasting}
\concept{Cross Validation}
\concept{Nested Cross Validation}
\concept{Rolling Forecasting Origin}
