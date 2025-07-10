\name{LTAR}
\alias{LTAR}
\title{Tensor Autoregression (TAR) Model}

\description{
Fits a Tensor Autoregression (TAR) Model to historical 3D tensor data and returns the coefficient tensor (A) and the constant matrix (C).

\deqn{\mathbf{A}=[A_1 | A_2 | \ldots | A_p], \mathbf{C}}
}

\usage{
LTAR(p,tnsr,type = c("const", "trend", "both", "none"),season=NULL)
}

\arguments{
\item{p}{: Number of lags}
\item{tnsr}{: A 3D tensor}
\item{type}{:Type of deterministic regressors to include.}
  \item{season}{: Inclusion of centered seasonal dummy variables (integer value of frequency).}
}

\value{
{The coefficient tensor \deqn{\mathbf{A}=[A_1 | A_2 | \ldots | A_p]} and the constant matrix \deqn{C} for the LTAR model:
\deqn{\mathbf{y}_t = A_1\mathbf{y}_{t-1}+\ldots+A_p\mathbf{y}_p+CD_t+\mathbf{u}_t.}
}
}

\author{
Kyle Caudle

Randy Hoover

Jackson Cates
}

\references{Cates, J., Hoover, R. C., Caudle, K., Kopp, R., & Ozdemir, C. (2021, December). Transform-Based Tensor Auto Regression for Multilinear Time Series Forecasting. In 2021 20th IEEE International Conference on Machine Learning and Applications (ICMLA) (pp. 461-466). IEEE.}

\examples{
require(rTensor)
data(tensor)
tnsr <- as.tensor(tensor)
# an LTAR(1) model with trend
model <- LTAR(p=1,tnsr,type="trend")
}
