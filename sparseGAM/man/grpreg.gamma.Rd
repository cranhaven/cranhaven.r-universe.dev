\name{grpreg.gamma}
\alias{grpreg.gamma}

\title{Group-regularized Gamma Regression}

\description{This function implements group-regularized gamma regression with a known shape parameter \eqn{\nu} and the log link. In gamma regression, we assume that \eqn{y_i \sim Gamma(\mu_i, \nu)}, where
  
  \deqn{f(y_i | \mu_i, \nu ) = \frac{1}{\Gamma(\nu)} (\frac{\nu}{\mu_i})^{\nu} \exp(-\frac{\nu}{\mu_i}y_i) y_i^{\nu-1}, y > 0.}
  
  Then \eqn{E(y_i) = \mu_i}, and we relate \eqn{\mu_i} to a set of \eqn{p} covariates \eqn{x_i} through the log link,
  
  \deqn{\log(\mu_i) = \beta_0 + x_i^T \beta, i=1,..., n}
  
  If the covariates in each \eqn{x_i} are grouped according to known groups \eqn{g=1, ..., G}, then this function may estimate some of the \eqn{G} groups of coefficients as all zero, depending on the amount of regularization. 
  
  Our implementation for regularized gamma regression is based on the least squares approximation approach of Wang and Leng (2007), and hence, the function does not allow the total number of covariates \eqn{p} to be greater than sample size.
}

\usage{
grpreg.gamma(y, X, X.test, groups, gamma.shape=1, 
             penalty=c("gLASSO","gSCAD","gMCP"),
             weights, taper, nlambda=100, lambda, max.iter=10000, tol=1e-4)
}

\arguments{
  \item{y}{\eqn{n \times 1} vector of responses for training data.}
  \item{X}{\eqn{n \times p} design matrix for training data, where the \eqn{j}th column of \code{X} corresponds to the \eqn{j}th overall covariate.}
  \item{X.test}{\eqn{n_{test} \times p} design matrix for test data to calculate predictions. \code{X.test} must have the \emph{same} number of columns as \code{X}, but not necessarily the same number of rows. If \emph{no} test data is provided or if in-sample predictions are desired, then the function automatically sets \code{X.test=X} in order to calculate \emph{in-sample} predictions.}
  \item{groups}{\eqn{p}-dimensional vector of group labels. The \eqn{j}th entry in \code{groups} should contain either the group number \emph{or} the name of the factor level that the \eqn{j}th covariate belongs to. \code{groups} must be either a vector of integers or factors.}
  \item{gamma.shape}{known shape parameter \eqn{\nu} in \eqn{Gamma(\mu_i,\nu)} distribution for the responses. Default is \code{gamma.shape=1}.}
  \item{penalty}{group regularization method to use on the groups of coefficients. The options are \code{"gLASSO"}, \code{"gSCAD"}, \code{"gMCP"}. To implement gamma regression with the SSGL penalty, use the \code{SSGL} function.}
  \item{weights}{group-specific, nonnegative weights for the penalty. Default is to use the square roots of the group sizes.}
  \item{taper}{tapering term \eqn{\gamma} in group SCAD and group MCP controlling how rapidly the penalty tapers off. Default is \code{taper=4} for group SCAD and \code{taper=3} for group MCP. Ignored if \code{"gLASSO"} is specified as the penalty.}
  \item{nlambda}{number of regularization parameters \eqn{L}. Default is \code{nlambda=100}.}
  \item{lambda}{grid of \eqn{L} regularization parameters. The user may specify either a scalar or a vector. If the user does not provide this, the program chooses the grid automatically.}
  \item{max.iter}{maximum number of iterations in the algorithm. Default is \code{max.iter=10000}.}
  \item{tol}{convergence threshold for algorithm. Default is \code{tol=1e-4}.}
}

\value{The function returns a list containing the following components:
    \item{lambda}{\eqn{L \times 1} vector of regularization parameters \code{lambda} used to fit the model. \code{lambda} is displayed in descending order.}
  \item{beta0}{\eqn{L \times 1} vector of estimated intercepts. The \eqn{k}th entry in \code{beta0} corresponds to the \eqn{k}th regularization parameter in \code{lambda}.}
  \item{beta}{\eqn{p \times L} matrix of estimated regression coefficients. The \eqn{k}th column in \code{beta} corresponds to the \eqn{k}th regularization parameter in \code{lambda}.}
  \item{mu.pred}{\eqn{n_{test} \times L} matrix of predicted mean response values \eqn{\mu_{test} = E(Y_{test})} based on the \emph{test} data in \code{X.test} (or training data \code{X} if no argument was specified for \code{X.test}). The \eqn{k}th column in \code{mu.pred} corresponds to the predictions for the \eqn{k}th regularization parameter in \code{lambda}.}
  \item{classifications}{\eqn{G \times L} matrix of classifications, where \eqn{G} is the number of groups. An entry of "1" indicates that the group was classified as nonzero, and an entry of "0" indicates that the group was classified as zero. The \eqn{k}th column of \code{classifications} corresponds to the \eqn{k}th regularization parameter in \code{lambda}.}
  \item{loss}{\eqn{L \times 1} vector of negative log-likelihood of the fitted models. The \eqn{k}th entry in \code{loss} corresponds to the \eqn{k}th regularization parameter in \code{lambda}.}
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
groups = c("a","a","a","b","b","b","c","c","d","e","e")
groups = as.factor(groups)
true.beta = c(-1,1,1,0,0,0,0,0,0,1.5,-1.5)

## Generate responses from gamma regression with known shape parameter 1
eta = crossprod(t(X), true.beta)
shape = 1
y = rgamma(n, rate=shape/exp(eta), shape=shape)

## Generate test data
n.test = 50
X.test = matrix(runif(n.test*11), nrow=n.test)

## Fit gamma regression models with the group LASSO penalty
gamma.mod = grpreg.gamma(y, X, X.test, groups, penalty="gLASSO")

## Tuning parameters used to fit models 
gamma.mod$lambda

# Predicted n.test-dimensional vectors mu=E(Y.test) based on test data, X.test. 
# The kth column of 'mu.pred' corresponds to the kth entry in 'lambda.'
gamma.mod$mu.pred 

# Classifications of the 5 groups. The kth column of 'classifications'
# corresponds to the kth entry in 'lambda.'
gamma.mod$classifications
}