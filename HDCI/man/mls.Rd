\name{mls}
\alias{mls}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Modified Least Squares
}
\description{
Computes modified Least Squares estimate.
}
\usage{
mls(x, y, tau = 0, standardize = TRUE, intercept = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
   \item{x}{
Input matrix as in glmnet, of dimension nobs x nvars; each row is an observation vector.
}
  \item{y}{
Response variable.
}
\item{tau}{
Tuning parameter in modified Least Squares (mls). Default value is 0, which corresponds to Ordinary Least Squares (OLS).
}
\item{standardize}{
Logical flag for x variable standardization, prior to fitting the model. Default is standardize=TRUE.
}
\item{intercept}{
Should intercept be fitted (default is TRUE) or set to zero (FALSE).
}
}
\details{
The function is used to compute the modified Least Squares (mLS) estimator defined in the paper: Liu H, Yu B. Asymptotic Properties of Lasso+mLS and Lasso+Ridge in Sparse High-dimensional Linear Regression. Electronic Journal of Statistics, 2013, 7.
}
\value{
A list consisting of the following elements is returned.

  \item{beta}{
The mLS coefficient of variables/predictors.
  }
  \item{beta0}{
A value of intercept term.
  }
  \item{meanx}{
The mean vector of variables/predictors if intercept=TRUE, otherwise is a vector of 0's.
  }
  \item{mu}{  
The mean of the response if intercept=TRUE, otherwise is 0.
}
\item{normx}{
The vector of standard error of variables/predictors if standardize=TRUE, otherwise is a vector of 1's.
}
\item{tau}{
The tuning parameter in mLS.
}
}
\examples{
library("mvtnorm") 

## generate the data
set.seed(2015)
n <- 200      # number of obs
p <- 500
s <- 10
beta <- rep(0, p)
beta[1:s] <- runif(s, 1/3, 1)
x <- rmvnorm(n = n, mean = rep(0, p), method = "svd")
signal <- sqrt(mean((x \%*\% beta)^2))
sigma <- as.numeric(signal / sqrt(10))  # SNR=10
y <- x \%*\% beta + rnorm(n)

## modified Least Squares
set.seed(0)
obj <- mls(x = x[, 1:20], y = y)
# the OLS estimate of the regression coefficients
obj$beta
# intercept term
obj$beta0
# prediction
mypredict(obj, newx = matrix(rnorm(10*20), 10, 20))

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.