\name{lasso}
\alias{lasso}

\title{
Estimate GGM with nodewise regression and the lasso.
}
\description{
Estimate a Gaussian Graphical Model with lasso-regularized nodewise regression, where the regularization parameter is selected with cross-validation. This is a wrapper around the function \code{cv.glmnet()} from the \code{glmnet} package.
}
\usage{
lasso(data, pbar = TRUE, nfolds = 10, rulereg = "and")
}

\arguments{
  \item{data}{
An n x p matrix containing the data, where n are cases and p are variables
}
  \item{pbar}{
If \code{pbar = TRUE}, a progress bar will be displayed.
}
  \item{nfolds}{
Specifies the number of folds used to select the regularization parameter in each of the p nodewise regressions.
}
  \item{rulereg}{
Specifies how parameter estimates should be combined across nodewise regressions. The options are the AND-rule (requiring both estimates to be significant) or the OR-rule (only requiring one estimate to be significant). Defaults to \code{rulereg = "and"}.
}
}
\value{
The function returns a list with the following entries:

\item{est}{A p x p matrix with point estimates for all partial correlations}
\item{select}{A p x p indicator matrix indicating which edges have been selected to be present.}
\item{ints}{A p-vector of estimated intercepts.}
}
\references{
Friedman, J., Hastie, T., & Tibshirani, R. (2010). Regularization paths for generalized linear models via coordinate descent. Journal of statistical software, 33(1), 1.
}
\author{
Jonas Haslbeck <jonashaslbeck@gmail.com>
}
\examples{

# Toy example that runs relatively quickly
library(MASS)
p <- 5 # number of variables
data <- mvrnorm(n=100, mu=rep(0, p), Sigma = diag(p))
set.seed(1)
out <- lasso(data = data)

\dontrun{

# Fit GGM to PTSD data
set.seed(1)
out <- lasso(data = ptsd_data)

}

}
