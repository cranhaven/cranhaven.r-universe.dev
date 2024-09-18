\name{cv.SBGAM}
\alias{cv.SBGAM}

\title{Cross-Validation for Sparse Bayesian Generalized Additive Model}

\description{This function implements \eqn{K}-fold cross-validation for sparse Bayesian generalized additive models (GAMs) with the spike-and-slab group lasso (SSGL) penalty. The identity link function is used for Gaussian GAMs, the logit link is used for binomial GAMs, and the log link is used for Poisson, negative binomial, and gamma GAMs.
}

\usage{
cv.SBGAM(y, X, df=6, 
         family=c("gaussian","binomial","poisson","negativebinomial","gamma"), 
         nb.size=1, gamma.shape=1, nfolds=5, nlambda0=20, lambda0, lambda1, 
         a, b, max.iter=100, tol = 1e-6, print.fold=TRUE) 
}

\arguments{
  \item{y}{\eqn{n \times 1} vector of responses.}
  \item{X}{\eqn{n \times p} design matrix, where the \eqn{j}th column of \code{X} corresponds to the \eqn{j}th overall covariate.}
  \item{df}{number of B-spline basis functions to use in each basis expansion. Default is \code{df=6}, but the user may specify degrees of freedom as any integer greater than or equal to 3.}
  \item{family}{exponential dispersion family. Allows for \code{"gaussian"}, \code{"binomial"}, \code{"poisson"}, \code{"negativebinomial"}, and \code{"gamma"}. Note that for \code{"negativebinomial"}, the size parameter must be specified, while for \code{"gamma"}, the shape parameter must be specified.}
    \item{nb.size}{known size parameter \eqn{\alpha} in \eqn{NB(\alpha,\mu_i)} distribution for negative binomial responses. Default is \code{nb.size=1}. Ignored if \code{family} is not \code{"negativebinomial"}.}
  \item{gamma.shape}{known shape parameter \eqn{\nu} in \eqn{Gamma(\mu_i,\nu)} distribution for gamma responses. Default is \code{gamma.shape=1}. Ignored if \code{family} is not \code{"gamma"}.}
      \item{nfolds}{number of folds \eqn{K} to use in \eqn{K}-fold cross-validation. Default is \code{nfolds=5}.}
  \item{nlambda0}{number of spike hyperparameter \eqn{L}. Default is \code{nlambda0=20}.}
  \item{lambda0}{grid of \eqn{L} spike hyperparameters \eqn{\lambda_0}. The user may specify either a scalar or a vector. If the user does not provide this, the program chooses the grid automatically.}
  \item{lambda1}{slab hyperparameter \eqn{\lambda_1} in the SSGL prior. Default is \code{lambda1=1}.}
  \item{a}{shape hyperparameter for the \eqn{Beta(a,b)} prior on the mixing proportion in the SSGL prior. Default is \code{a=1}.}
  \item{b}{shape hyperparameter for the \eqn{Beta(a,b)} prior on the mixing proportion in the SSGL prior. Default is \code{b=dim(X)[2]}.}
  \item{max.iter}{maximum number of iterations in the algorithm. Default is \code{max.iter=100}.}
  \item{tol}{convergence threshold for algorithm. Default is \code{tol=1e-6}.}
  \item{print.fold}{Boolean variable for whether or not to print the current fold in the algorithm. Default is \code{print.fold=TRUE}.}
}

\value{The function returns a list containing the following components:
    \item{lambda0}{\eqn{L \times 1} vector of spike hyperparameters \code{lambda0} used to fit the model. \code{lambda0} is displayed in descending order.}
  \item{cve}{\eqn{L \times 1} vector of mean cross-validation error across all \eqn{K} folds. The \eqn{k}th entry in \code{cve} corresponds to the \eqn{k}th regularization parameter in \code{lambda0}.}
  \item{cvse}{\eqn{L \times 1} vector of standard errors for cross-validation error across all \eqn{K} folds. The \eqn{k}th entry in \code{cvse} corresponds to the \eqn{k}th regularization parameter in \code{lambda0}.}
  \item{lambda0.min}{value of \code{lambda0} that minimizes mean cross-validation error \code{cve}.}
}

\references{
Bai R. (2021). "Spike-and-slab group lasso for consistent Bayesian estimation and variable selection in non-Gaussian generalized additive models." \emph{arXiv pre-print arXiv:2007.07021}.

  Bai, R., Moran, G. E., Antonelli, J. L., Chen, Y., and Boland, M.R. (2021). "Spike-and-slab group lassos for grouped regression and sparse generalized additive models." \emph{Journal of the American Statistical Association}, in press.
}

\examples{
## Generate data
set.seed(12345)
X = matrix(runif(30*3), nrow=30)
n = dim(X)[1]
y = 2.5*sin(pi*X[,1]) + rnorm(n)

## K-fold cross-validation for 4 degrees of freedom and 4 values of lambda0
## Note that if user does not specify lambda0, cv.SBGAM chooses a grid automatically.

cv.mod = cv.SBGAM(y, X, df=4, family="gaussian", lambda0=seq(from=25,to=5,by=-10))

## Plot CVE curve
plot(cv.mod$lambda0, cv.mod$cve, type="l", xlab="lambda0", ylab="CVE")
## lambda which minimizes cross-validation error
cv.mod$lambda0.min
}