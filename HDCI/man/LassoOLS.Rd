\name{LassoOLS}
\alias{LassoOLS}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Lasso OLS
}
\description{
Computes the two-stage estimator Lasso+OLS (default) or the Lasso estimator (if OLS=FALSE).

}
\usage{
LassoOLS(x, y, OLS = TRUE, lambda = NULL, fix.lambda = TRUE, cv.method = "cv", nfolds 
         = 10, foldid, cv.OLS = TRUE, tau = 0, parallel = FALSE, standardize = TRUE, 
         intercept = TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{

   \item{x}{
Input matrix as in glmnet, of dimension nobs x nvars; each row is an observation vector.
}
  \item{y}{
Response variable.
}
\item{OLS}{
If TRUE, computes Lasso+OLS; otherwise, computes Lasso estimator. The default is TRUE.
}
\item{lambda}{  
A value of lambda - default is NULL. lambda should be given a value when fix.lambda=TRUE.
}
\item{fix.lambda}{
If TRUE, computes Lasso+OLS (or Lasso) for a fix value of lambda given by the argument "lambda"; otherwise, computes Lasso+OLS (or Lasso) for the value of lambda choosing by cv/cv1se/escv.
}
\item{cv.method}{
The method used to select lambda -- can be cv, cv1se, and escv; the default is cv. cv.method is useful only when fix.lambda=FALSE.
}

  \item{nfolds, foldid, cv.OLS, tau, parallel}{
Arguments that can be passed to escv.glmnet (useful only when fix.lambda=FALSE). Note that, the default value of cv.OLS is TRUE, which means using Lasso+OLS in the cv fits.
}
\item{standardize}{
Logical flag for x variable standardization, prior to fitting the model. Default is standardize=TRUE.
}
  \item{intercept}{
Should intercept be fitted (default is TRUE) or set to zero (FALSE).
}
\item{...}{
Other arguments that can be passed to glmnet.
}
}
\details{
If OLS=TRUE (default), this function computes the Lasso+OLS estimator for a give value of lambda (if fix.lambda=TRUE) or for the value of lambda choosing by cv/cv1se/escv (if fix.lambda=FALSE). If OLS=FALSE, this function computes the Lasso estimator in the same way as the function "Lasso". Note that, we use the easy-to-understand notation "Lasso+OLS" denoting the "Lasso+mLS" estimator defined in the paper: Liu H, Yu B. Asymptotic Properties of Lasso+mLS and Lasso+Ridge in Sparse High-dimensional Linear Regression. Electronic Journal of Statistics, 2013, 7.
}
\value{
A list consisting of the following elements is returned.

  \item{beta}{
The Lasso+OLS (or Lasso when OLS=FALSE) estimate for the coefficients of variables/predictors.
}
\item{beta0}{
A value of intercept term.
}
\item{lambda}{
The value/values of lambda.
}
\item{meanx}{
The mean vector of variables/predictors if intercept=TRUE, otherwise is a vector of 0's.
}
\item{mu}{
The mean of the response if intercept=TRUE, otherwise is 0.
}
\item{tau}{
Tuning parameter in modified Least Squares (mls).
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

## Lasso+OLS estimator
# for a given value of lambda
set.seed(0)
obj.escv <- escv.glmnet(x, y)
obj <- LassoOLS(x, y, lambda = obj.escv$lambda.cv)
# Lasso+OLS estimate of the regression coefficients
obj$beta
# intercept term
obj$beta0
# prediction
mypredict(obj, newx = matrix(rnorm(10*p), 10, p))

# for lambda choosing by cross-validation (cv) which uses Lasso+OLS in the cv fit
set.seed(0)
obj <- LassoOLS(x, y, fix.lambda = FALSE)

# for lambda choosing by cross-validation (cv) which uses Lasso in the cv fit
set.seed(0)
obj <- LassoOLS(x, y, fix.lambda = FALSE, cv.OLS = FALSE)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.