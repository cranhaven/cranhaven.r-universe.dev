\name{cv.grpreg.nb}
\alias{cv.grpreg.nb}

\title{Cross-validation for Group-regularized Negative Binomial Regression}

\description{This function implements \eqn{K}-fold cross-validation for group-regularized negative binomial regression with a known size parameter \eqn{\alpha} and the log link. For a description of group-regularized negative binomial regression, see the description for the \code{grpreg.nb} function.

  Our implementation is based on the least squares approximation approach of Wang and Leng (2007), and hence, the function does not allow the total number of covariates \eqn{p} to be greater than \eqn{\frac{K-1}{K} \times} sample size, where \eqn{K} is the number of folds.
}

\usage{
cv.grpreg.nb(y, X, groups, nb.size=1, penalty=c("gLASSO","gSCAD","gMCP"),
            nfolds=10, weights, taper, nlambda=100, lambda, max.iter=10000, 
            tol=1e-4) 
}

\arguments{
  \item{y}{\eqn{n \times 1} vector of responses.}
  \item{X}{\eqn{n \times p} design matrix, where the \eqn{j}th column of \code{X} corresponds to the \eqn{j}th overall covariate.}
  \item{groups}{\eqn{p}-dimensional vector of group labels. The \eqn{j}th entry in \code{groups} should contain either the group number \emph{or} the name of the factor level that the \eqn{j}th covariate belongs to. \code{groups} must be either a vector of integers or factors.}
  \item{nb.size}{known size parameter \eqn{\alpha} in \eqn{NB(\alpha,\mu_i)} distribution for the responses. Default is \code{nb.size=1}.}
  \item{penalty}{group regularization method to use on the groups of coefficients. The options are \code{"gLASSO"}, \code{"gSCAD"}, and \code{"gMCP"}. To implement cross-validation for negative binomoial regression with the SSGL penalty, use the \code{cv.SSGL} function.}
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
set.seed(1234)
X = matrix(runif(100*16), nrow=100)
n = dim(X)[1]
groups = c(1,1,1,2,2,2,2,3,4,5,5,6,7,8,8,8)
true.beta = c(-2,2,2,0,0,0,0,0,0,1.5,-1.5,0,0,-2,2,2)

## Generate count responses from negative binomial regression
eta = crossprod(t(X), true.beta)
y = rnbinom(n,size=1, mu=exp(eta))

## 10-fold cross-validation for group-regularized negative binomial
## regression with the group SCAD penalty
nb.cv = cv.grpreg.nb(y,X,groups,penalty="gMCP")

## Plot cross-validation curve
plot(nb.cv$lambda, nb.cv$cve, type="l", xlab="lambda", ylab="CVE")
## lambda which minimizes mean CVE
nb.cv$lambda.min 
}