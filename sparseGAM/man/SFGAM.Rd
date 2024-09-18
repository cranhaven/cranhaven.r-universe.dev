\name{SFGAM}
\alias{SFGAM}

\title{Sparse Frequentist Generalized Additive Models}

\description{This function implements sparse frequentist generalized additive models (GAMs) with the group LASSO, group SCAD, and group MCP penalties. Let \eqn{y_i} denote the \eqn{i}th response and \eqn{x_i} denote a \eqn{p}-dimensional vector of covariates. GAMs are of the form,

\deqn{g(E(y_i)) = \beta_0 + \sum_{j=1}^{p} f_j (x_{ij}), i = 1, ..., n,}

where \eqn{g} is a monotone increasing link function. The identity link function is used for Gaussian regression, the logit link is used for binomial regression, and the log link is used for Poisson, negative binomial, and gamma regression. The univariate functions are estimated using linear combinations of B-spline basis functions. Under group regularization of the basis coefficients, some of the univariate functions \eqn{f_j(x_j)} will be estimated as \eqn{\hat{f}_j(x_j) = 0}, depending on the size of the regularization parameter \eqn{\lambda}. 
  
For implementation of sparse \emph{Bayesian} GAMs with the SSGL penalty, use the \code{SBGAM} function.
}

\usage{
SFGAM(y, X, X.test, df=6, 
      family=c("gaussian","binomial", "poisson", "negativebinomial","gamma"),
      nb.size=1, gamma.shape=1, penalty=c("gLASSO","gMCP","gSCAD"), taper, 
      nlambda=100, lambda, max.iter=10000, tol=1e-4)
}

\arguments{
  \item{y}{\eqn{n \times 1} vector of responses for training data.}
  \item{X}{\eqn{n \times p} design matrix for training data, where the \eqn{j}th column of \code{X} corresponds to the \eqn{j}th overall covariate.}
  \item{X.test}{\eqn{n_{test} \times p} design matrix for test data to calculate predictions. \code{X.test} must have the \emph{same} number of columns as \code{X}, but not necessarily the same number of rows. If \emph{no} test data is provided or if in-sample predictions are desired, then the function automatically sets \code{X.test=X} in order to calculate \emph{in-sample} predictions.}
  \item{df}{number of B-spline basis functions to use in each basis expansion. Default is \code{df=6}, but the user may specify degrees of freedom as any integer greater than or equal to 3.}
  \item{family}{exponential dispersion family. Allows for \code{"gaussian"}, \code{"binomial"}, \code{"poisson"}, \code{"negativebinomial"}, and \code{"gamma"}. Note that for \code{"negativebinomial"}, the size parameter must be specified, while for \code{"gamma"}, the shape parameter must be specified.}
    \item{nb.size}{known size parameter \eqn{\alpha} in \eqn{NB(\alpha,\mu_i)} distribution for negative binomial responses. Default is \code{nb.size=1}. Ignored if \code{family} is not \code{"negativebinomial"}.}
  \item{gamma.shape}{known shape parameter \eqn{\nu} in \eqn{Gamma(\mu_i,\nu)} distribution for gamma responses. Default is \code{gamma.shape=1}. Ignored if \code{family} is not \code{"gamma"}.}
 \item{penalty}{group regularization method to use on the groups of basis coefficients. The options are \code{"gLASSO"}, \code{"gSCAD"}, and \code{"gMCP"}. To implement sparse GAMs with the SSGL penalty, use the \code{SBGAM} function.}
  \item{taper}{tapering term \eqn{\gamma} in group SCAD and group MCP controlling how rapidly the penalty tapers off. Default is \code{taper=4} for group SCAD and \code{taper=3} for group MCP. Ignored if \code{"gLASSO"} is specified as the penalty.}
  \item{nlambda}{number of regularization parameters \eqn{L}. Default is \code{nlambda=100}.}
  \item{lambda}{grid of \eqn{L} regularization parameters. The user may specify either a scalar or a vector. If the user does not provide this, the program chooses the grid automatically.}
  \item{max.iter}{maximum number of iterations in the algorithm. Default is \code{max.iter=10000}.}
  \item{tol}{convergence threshold for algorithm. Default is \code{tol=1e-4}.}
}

\value{The function returns a list containing the following components:
    \item{lambda}{\eqn{L \times 1} vector of regularization parameters \code{lambda} used to fit the model. \code{lambda} is displayed in descending order.}
    \item{f.pred}{List of \eqn{L} \eqn{n_{test} \times p} matrices, where the \eqn{k}th matrix in the list corresponds to the \eqn{k}th regularization parameter in \code{lambda}. The \eqn{j}th column in each matrix in \code{f.pred} is the estimate of the \eqn{j}th function evaluated on the test data in \code{X.test} for the \eqn{j}th covariate (or training data \code{X} if \code{X.test} was not specified). }
      \item{mu.pred}{\eqn{n_{test} \times L} matrix of predicted mean response values \eqn{\mu_{test} = E(Y_{test})} based on the \emph{test} data in \code{X.test} (or training data \code{X} if no argument was specified for \code{X.test}). The \eqn{k}th column in \code{mu.pred} corresponds to the predictions for the \eqn{k}th regularization parameter in \code{lambda}.}
        \item{classifications}{\eqn{p \times L} matrix of classifications. An entry of "1" indicates that the corresponding function was classified as nonzero, and an entry of "0" indicates that the function was classified as zero. The \eqn{k}th column of \code{classifications} corresponds to the \eqn{k}th regularization parameter in \code{lambda}.}
  \item{beta0}{\eqn{L \times 1} vector of estimated intercepts. The \eqn{k}th entry in \code{beta0} corresponds to the \eqn{k}th regularization parameter in \code{lambda}.}
  \item{beta}{\eqn{dp \times L} matrix of estimated basis coefficients. The \eqn{k}th column in \code{beta} corresponds to the \eqn{k}th regularization parameter in \code{lambda}.}
  \item{loss}{vector of either the residual sum of squares (\code{"gaussian"}) or the negative log-likelihood (\code{"binomial"}, \code{"poisson"}, \code{"negativebinomial"}, \code{"gamma"}) of the fitted model. The \eqn{k}th entry in \code{loss} corresponds to the \eqn{k}th regularization parameter in \code{lambda}.}
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

## K-fold cross-validation with group MCP penalty
cv.mod = cv.SFGAM(y, X, family="gaussian", penalty="gMCP")
## Plot CVE curve
plot(cv.mod$lambda, cv.mod$cve, type="l", xlab="lambda", ylab="CVE")
## lambda which minimizes cross-validation error
lambda.opt = cv.mod$lambda.min

## Fit a single model with lambda.opt
SFGAM.mod = SFGAM(y, X, X.test, penalty="gMCP", lambda=lambda.opt)

## Classifications
SFGAM.mod$classifications
## Predicted function evaluations on test data
f.pred = SFGAM.mod$f.pred  

## Plot estimated first function
x1 = X.test[,1]
f1.hat = f.pred[,1]

## Plot x_1 against f_1(x_1)
plot(x1[order(x1)], f1.hat[order(x1)], xlab=expression(x[1]), 
     ylab=expression(f[1](x[1])))
}