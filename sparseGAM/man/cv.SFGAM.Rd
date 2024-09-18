\name{cv.SFGAM}
\alias{cv.SFGAM}

\title{Sparse Frequentist Generalized Additive Models}

\description{This function implements \eqn{K}-fold cross-validation for sparse frequentist generalized additive models (GAMs) with the group LASSO, group SCAD, and group MCP penalties. The identity link function is used for Gaussian GAMs, the logit link is used for binomial GAMs, and the log link is used for Poisson, negative binomial, and gamma GAMs.
}

\usage{
cv.SFGAM(y, X, df=6, 
         family=c("gaussian","binomial", "poisson", "negativebinomial","gamma"),
         nb.size=1, gamma.shape=1, penalty=c("gLASSO","gMCP","gSCAD"), taper,
         nfolds=10, nlambda=100, lambda, max.iter=10000, tol=1e-4)
}

\arguments{
  \item{y}{\eqn{n \times 1} vector of responses.}
  \item{X}{\eqn{n \times p} design matrix, where the \eqn{j}th column of \code{X} corresponds to the \eqn{j}th overall covariate.}
  \item{df}{number of B-spline basis functions to use in each basis expansion. Default is \code{df=6}, but the user may specify degrees of freedom as any integer greater than or equal to 3.}
  \item{family}{exponential dispersion family. Allows for \code{"gaussian"}, \code{"binomial"}, \code{"poisson"}, \code{"negativebinomial"}, and \code{"gamma"}. Note that for \code{"negativebinomial"}, the size parameter must be specified, while for \code{"gamma"}, the shape parameter must be specified.}
    \item{nb.size}{known size parameter \eqn{\alpha} in \eqn{NB(\alpha,\mu_i)} distribution for negative binomial responses. Default is \code{nb.size=1}. Ignored if \code{family} is not \code{"negativebinomial"}.}
  \item{gamma.shape}{known shape parameter \eqn{\nu} in \eqn{Gamma(\mu_i,\nu)} distribution for gamma responses. Default is \code{gamma.shape=1}. Ignored if \code{family} is not \code{"gamma"}.}
 \item{penalty}{group regularization method to use on the groups of basis coefficients. The options are \code{"gLASSO"}, \code{"gSCAD"}, and \code{"gMCP"}. To implement sparse GAMs with the SSGL penalty, use the \code{SBGAM} function.}
  \item{taper}{tapering term \eqn{\gamma} in group SCAD and group MCP controlling how rapidly the penalty tapers off. Default is \code{taper=4} for group SCAD and \code{taper=3} for group MCP. Ignored if \code{"gLASSO"} is specified as the penalty.}
      \item{nfolds}{number of folds \eqn{K} to use in \eqn{K}-fold cross-validation. Default is \code{nfolds=10}.}
  \item{nlambda}{number of regularization parameters \eqn{L}. Default is \code{nlambda=100}.}
  \item{lambda}{grid of \eqn{L} regularization parameters. The user may specify either a scalar or a vector. If the user does not provide this, the program chooses the grid automatically.}
  \item{max.iter}{maximum number of iterations in the algorithm. Default is \code{max.iter=10000}.}
  \item{tol}{convergence threshold for algorithm. Default is \code{tol=1e-4}.}
}

\value{The function returns a list containing the following components:
    \item{lambda}{\eqn{L \times 1} vector of regularization parameters \code{lambda} used to fit the model. \code{lambda} is displayed in descending order.}
  \item{cve}{\eqn{L \times 1} vector of mean cross-validation error across all \eqn{K} folds. The \eqn{k}th entry in \code{cve} corresponds to the \eqn{k}th regularization parameter in \code{lambda}.}
  \item{cvse}{\eqn{L \times 1} vector of standard errors for cross-validation error across all \eqn{K} folds. The \eqn{k}th entry in \code{cvse} corresponds to the \eqn{k}th regularization parameter in \code{lambda}.}
  \item{lambda.min}{value of \code{lambda} that minimizes mean cross-validation error \code{cve}.}
}

\references{
  Breheny, P. and Huang, J. (2015). "Group descent algorithms for nonconvex penalized linear and logistic regression models with grouped predictors." \emph{Statistics and Computing}, \bold{25}:173-187.
  
  Wang, H. and Leng, C. (2007). "Unified LASSO estimation by least squares approximation." \emph{Journal of the American Statistical Association}, \bold{102}:1039-1048.
  
  Yuan, M. and Lin, Y. (2006). Model selection and estimation in regression with grouped variables. \emph{Journal of the Royal Statistical Society: Series B (Statistical Methodology)}, \bold{68}: 49-67.
}

\examples{
## Generate data
set.seed(12345)
X = matrix(runif(100*20), nrow=100)
n = dim(X)[1]
y = 5*sin(2*pi*X[,1])-5*cos(2*pi*X[,2]) + rnorm(n)

## Test data with 50 observations
X.test = matrix(runif(50*20), nrow=50)

## Fit sparse Gaussian generalized additive model to data with the MCP penalty
gam.mod = SFGAM(y, X, X.test, family="gaussian", penalty="gMCP")

## The model corresponding to the 75th tuning parameter
gam.mod$lambda[75] 
gam.mod$classifications[,75] ## The covariate index is listed first

## Plot first function f_1(x_1) in 75th model
x1 = X.test[,1] 
## Estimates of all 20 function evaluations on test data
f.hat = gam.mod$f.pred[[75]] 
## Extract estimates of f_1 
f1.hat = f.hat[,1] 

## Plot X_1 against f_1(x_1)
plot(x1[order(x1)], f1.hat[order(x1)], xlab=expression(x[1]), 
     ylab=expression(f[1](x[1])))
}