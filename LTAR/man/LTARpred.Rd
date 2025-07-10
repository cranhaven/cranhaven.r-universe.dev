\name{LTARpred}
\alias{LTARpred}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Forecast for a 3D Tensor Autoregression Model
}
\description{
Using a historical 3D tensor, the LTARpred function will forecast h steps into the future.
}
\usage{
LTARpred(p, tnsr, h, type = c("const", "trend", "both", "none"), season = NULL)
}

\arguments{
  \item{p}{: Number of time series lags}
  \item{tnsr}{: A 3D tensor}
  \item{h}{: Number of steps to forecast}
  \item{type}{Type of deterministic regressors to include.}
  \item{season}{: Inclusion of centered seasonal dummy variables (integer value of frequency).}

}

\value{
A \href{/library/rTensor/help/Tensor-class}{Tensor-class} object which contains the h step forecasts.
}
\references{Cates, J., Hoover, R. C., Caudle, K., Kopp, R., & Ozdemir, C. (2021, December). Transform-Based Tensor Auto Regression for Multilinear Time Series Forecasting. In 2021 20th IEEE International Conference on Machine Learning and Applications (ICMLA) (pp. 461-466). IEEE.

}
\author{
Kyle Caudle

Randy Hoover

Jackson Cates
}

\examples{
require(rTensor)
data(tensor)
tnsr <- as.tensor(tensor)
result <- LTARpred(p=5,tnsr,h=2,type="trend",season=12)
}
