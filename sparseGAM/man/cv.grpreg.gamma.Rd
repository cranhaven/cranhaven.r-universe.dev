\name{cv.grpreg.gamma}
\alias{cv.grpreg.gamma}

\title{Cross-validation for Group-regularized Gamma Regression}

\description{This function implements \eqn{K}-fold cross-validation for group-regularized gamma regression with a known shape parameter \eqn{\nu} and the log link. For a description of group-regularized gamma regression, see the description for the \code{grpreg.gamma} function.

  Our implementation is based on the least squares approximation approach of Wang and Leng (2007), and hence, the function does not allow the total number of covariates \eqn{p} to be greater than \eqn{\frac{K-1}{K} \times} sample size, where \eqn{K} is the number of folds.
}

\usage{
cv.grpreg.gamma(y, X, groups, gamma.shape=1, penalty=c("gLASSO","gSCAD","gMCP"),
                nfolds=10, weights, taper, nlambda=100, lambda, max.iter=10000, 
                tol=1e-4)
}

\arguments{
  \item{y}{\eqn{n \times 1} vector of responses.}
  \item{X}{\eqn{n \times p} design matrix, where the \eqn{j}th column of \code{X} corresponds to the \eqn{j}th overall covariate.}
  \item{groups}{\eqn{p}-dimensional vector of group labels. The \eqn{j}th entry in \code{groups} should contain either the group number \emph{or} the name of the factor level that the \eqn{j}th covariate belongs to. \code{groups} must be either a vector of integers or factors.}
  \item{gamma.shape}{known shape parameter \eqn{\nu} in \eqn{Gamma(\mu_i,\nu)} distribution for the responses. Default is \code{gamma.shape=1}.}
  \item{penalty}{group regularization method to use on the groups of coefficients. The options are \code{"gLASSO"}, \code{"gSCAD"}, and \code{"gMCP"}. To implement cross-validation for gamma regression with the SSGL penalty, use the \code{cv.SSGL} function.}
  \item{nfolds}{number of folds \eqn{K} to use in \eqn{K}-fold cross-validation. Default is \code{nfolds=10}.}
  \item{weights}{group-specific, nonnegative weights for the penalty. Default is to use the square roots of the group sizes.}
  \item{taper}{tapering term \eqn{\gamma} in group SCAD and group MCP controlling how rapidly the penalty tapers off. Default is \code{taper=4} for group SCAD and \code{taper=3} for group MCP. Ignored if \code{"gLASSO"} is specified as the penalty.}
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
}

\examples{
## Generate data
set.seed(12345)
X = matrix(runif(100*11), nrow=100)
n = dim(X)[1]
groups = c(1,1,1,2,2,2,3,3,4,5,5)
true.beta = c(-1,1,1,0,0,0,0,0,0,1.5,-1.5)

## Generate responses from gamma regression with known shape parameter 1
eta = crossprod(t(X), true.beta)
shape = 1
y = rgamma(n, rate=shape/exp(eta), shape=shape)

## 10-fold cross-validation for group-regularized gamma regression
## with the group LASSO penalty
gamma.cv = cv.grpreg.gamma(y, X, groups, penalty="gLASSO")

## Plot cross-validation curve
plot(gamma.cv$lambda, gamma.cv$cve, type="l", xlab="lambda", ylab="CVE")
## lambda which minimizes mean CVE
gamma.cv$lambda.min
}