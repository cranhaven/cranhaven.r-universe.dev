\name{mypredict}
\alias{mypredict}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
My Predict
}
\description{
Returns the predicted values.
}
\usage{
mypredict(object, newx)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{object}{
An object from mls, Lasso, LassoOLS or PartialRidge. 
}
\item{newx}{
Matrix of the values of variables/predictors for doing prediction; each row is an observation vector.
}
}
\value{
The predicted values for a give newx matrix is returned.
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