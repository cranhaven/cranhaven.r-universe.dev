\name{PartRidge}
\alias{PartRidge}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Partial Ridge
}
\description{
Computes the Partial Ridge estimator.
}
\usage{
PartRidge(x, y, lambda2 = 0, varset, standardize = TRUE, intercept = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
   \item{x}{
Input matrix as in glmnet, of dimension nobs x nvars; each row is an observation vector. 
}
  \item{y}{
Response variable.
}
\item{lambda2}{
Tuning parameter for the Partial Ridge. The default value is 0, which gives back to the OLS estimator.
}
\item{varset}{
A set indicating which variable/predictors are not penalized. Partial Ridge puts l2 penalty only on the coefficients of the variables/predictors not included in the varset.
}

\item{standardize}{
Logical flag for x variable standardization, prior to fitting the model. Default is standardize=TRUE.
}
  \item{intercept}{
Should intercept be fitted (default is TRUE) or set to zero (FALSE).
}
}
\details{
This function computes the Partial Ridge estimator, which adds l2 penalty only on the coefficients of variabels/predictors not included in the set varset, to the loss function (residual sum of squares).
}
\value{
A list consisting of the following elements is returned.
  \item{beta}{
The Lasso+Partial Ridge estimator for the coefficients of variables/predictors.
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
\item{lambda2}{
The value of lambda2.
}
}
\examples{
library("glmnet")
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

## Partial Ridge Regression
# Lasso
set.seed(0)
obj.escv <- escv.glmnet(x, y)
obj <- Lasso(x, y, lambda = obj.escv$lambda.cv)
# variable set
betalasso <- obj$beta
selectvar <- betalasso != 0
# partial ridge 
PR.obj <- PartRidge(x = x, y = y, lambda2 = 1/n, varset = selectvar)
# parial ridge estimate of the regression coefficients
beta <- PR.obj$beta
# intercept term
beta0 <- PR.obj$beta0
# prediction
mypredict(PR.obj, newx = matrix(rnorm(10*p), 10, p))

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.