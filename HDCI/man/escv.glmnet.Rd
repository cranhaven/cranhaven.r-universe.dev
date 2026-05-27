\name{escv.glmnet}
\alias{escv.glmnet}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
escv glmnet
}
\description{
Does k-fold estimation stability with cross-validation (escv) for glmnet and returns optimal values for lambda.
}
\usage{
escv.glmnet(x, y, lambda = NULL, nfolds = 10, foldid, cv.OLS = FALSE, tau = 0, parallel 
            = FALSE, standardize = TRUE, intercept = TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{

   \item{x}{
Input matrix as in glmnet, of dimension nobs x nvars; each row is an observation vector. Can be in sparse matrix format (inherit from class "sparseMatrix" as in package Matrix).
}
  \item{y}{
Response variable.
}
\item{lambda}{
Optional user-supplied lambda sequence for the Lasso; default is NULL, and glmnet chooses its own sequence.
}
\item{nfolds}{
Number of folds - default is 10.
}
\item{foldid}{
An optional vector of values between 1 and nfold identifying what fold each observation is in. If supplied, nfolds can be missing.
}
\item{cv.OLS}{
If TRUE, uses two-stage estimator Lasso+OLS in the fits (using Lasso to select variables/predictors and then using OLS to refit the coefficients for the selected variables/predictors. The default value is FALSE.
}
\item{tau}{
Tuning parameter in modified Least Squares (mls). Default value is 0, which corresponds to OLS.
}
\item{parallel}{
If TRUE, use parallel foreach to fit each fold. Must register parallel before hand, such as doParallel or others. See the example below.
}
\item{standardize}{
Logical flag for x variable standardization, prior to fitting the model sequence. Default is standardize=TRUE.
}
\item{intercept}{
Should intercept be fitted (default is TRUE) or set to zero (FALSE).
}
\item{...}{
Other arguments that can be passed to glmnet.
}
}
\details{
The function is similar to cv.glmnet, and returns the values of lambda selected by cross-validation (cv), by cross-validation within 1 standard error (cv1se) and by estimation stability with cross-validation (escv). The function runs glmnet nfolds+1 times; the first to get the lambda sequence, and then the remainder to compute the first stage fit (i.e., Lasso) with each of the folds omitted. The error (cv and also es) is accumulated, and the average error and standard deviation over the folds is computed. Note that, similar to cv.glmnet,  the results of escv.glmnet are random, since the folds are selected at random. Users can reduce this randomness by running escv.glmnet many times, and averaging the error curves.
}
\value{
A list consisting of the following elements is returned.

\item{lambda}{
The values of lambda used in the fits.
}  
\item{glmnet.fit}{
A fitted glmnet object for the full data.
}
\item{cv}{
The mean cross-validated error - a vector of length length(lambda).
}
\item{cv.error}{
Estimate of standard error of cv.
}
\item{es}{
The mean estimation stability (es) value - a vector of length length(lambda).
}
\item{es.error}{
Estimate of standard error of es.
}
\item{lambda.cv}{
Value of lambda that gives minimum cv.
}
\item{lambda.cv1se}{
Largest value of lambda such that cross-validated error is within 1 standard error of the minimum.
}
\item{lambda.escv}{
Value of lambda selected by escv -- giving the minimum es within the range of lambdas which are no less than lambda.cv.
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

## escv without parallel
# using Lasso+OLS in the cv fit.
set.seed(0)
obj <- escv.glmnet(x, y, cv.OLS = TRUE) 

# using Lasso in the cv fit.
set.seed(0)
obj <- escv.glmnet(x, y)

## escv with parallel
#library("doParallel")
#library("doRNG")
#registerDoParallel(2)

# using Lasso+OLS in the cv fit.
#registerDoRNG(seed = 0)
#obj <- escv.glmnet(x, y, cv.OLS = TRUE, nfolds = 4, parallel = TRUE)

# using Lasso in the cv fit.
#registerDoRNG(seed = 0) 
#obj <- escv.glmnet(x, y, parallel = TRUE)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.