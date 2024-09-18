\name{grpreg.nb}
\alias{grpreg.nb}

\title{Group-regularized Negative Binomial Regression}

\description{This function implements group-regularized negative binomial regression with a known size parameter \eqn{\alpha} and the log link. In negative binomial regression, we assume that \eqn{y_i \sim NB(\alpha, \mu_i)}, where
  
  \deqn{f(y_i | \alpha, \mu_i ) = \frac{\Gamma(y+\alpha)}{y! \Gamma(\alpha)} (\frac{\mu_i}{\mu_i+\alpha})^{y}(\frac{\alpha}{\mu_i +\alpha})^{\alpha}, y = 0, 1, 2, ...}
  
  Then \eqn{E(y_i) = \mu_i}, and we relate \eqn{\mu_i} to a set of \eqn{p} covariates \eqn{x_i} through the log link,
  
  \deqn{\log(\mu_i) = \beta_0 + x_i^T \beta, i=1,..., n}
  
  If the covariates in each \eqn{x_i} are grouped according to known groups \eqn{g=1, ..., G}, then this function may estimate some of the \eqn{G} groups of coefficients as all zero, depending on the amount of regularization. 
  
  Our implementation for regularized negative binomial regression is based on the least squares approximation approach of Wang and Leng (2007), and hence, the function does not allow the total number of covariates \eqn{p} to be greater than sample size.
}

\usage{
grpreg.nb(y, X, X.test, groups, nb.size=1, penalty=c("gLASSO","gSCAD","gMCP"),
          weights, taper, nlambda=100, lambda, max.iter=10000, tol=1e-4)
}

\arguments{
  \item{y}{\eqn{n \times 1} vector of responses for training data.}
  \item{X}{\eqn{n \times p} design matrix for training data, where the \eqn{j}th column of \code{X} corresponds to the \eqn{j}th overall covariate.}
  \item{X.test}{\eqn{n_{test} \times p} design matrix for test data to calculate predictions. \code{X.test} must have the \emph{same} number of columns as \code{X}, but not necessarily the same number of rows. If \emph{no} test data is provided or if in-sample predictions are desired, then the function automatically sets \code{X.test=X} in order to calculate \emph{in-sample} predictions.}
  \item{groups}{\eqn{p}-dimensional vector of group labels. The \eqn{j}th entry in \code{groups} should contain either the group number \emph{or} the name of the factor level that the \eqn{j}th covariate belongs to. \code{groups} must be either a vector of integers or factors.}
  \item{nb.size}{known size parameter \eqn{\alpha} in \eqn{NB(\alpha,\mu_i)} distribution for the responses. Default is \code{nb.size=1}.}
  \item{penalty}{group regularization method to use on the groups of coefficients. The options are \code{"gLASSO"}, \code{"gSCAD"}, \code{"gMCP"}. To implement negative binomial regression with the SSGL penalty, use the \code{SSGL} function.}
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
## Generate training data
set.seed(1234)
X = matrix(runif(100*16), nrow=100) 
n = dim(X)[1]
groups = c("A","A","A","B","B","B","C","C","D","E","E","F","G","H","H","H")
groups = as.factor(groups)
true.beta = c(-2,2,2,0,0,0,0,0,0,1.5,-1.5,0,0,-2,2,2)
  
## Generate count responses from negative binomial regression
eta = crossprod(t(X), true.beta)
y = rnbinom(n,size=1, mu=exp(eta))
  
## Generate test data
n.test = 50
X.test = matrix(runif(n.test*16), nrow=n.test)
  
## Fit negative binomial regression models with the group SCAD penalty
nb.mod = grpreg.nb(y, X, X.test, groups, penalty="gSCAD")
  
## Tuning parameters used to fit models 
nb.mod$lambda
  
# Predicted n.test-dimensional vectors mu=E(Y.test) based on test data, X.test. 
# The kth column of 'mu.pred' corresponds to the kth entry in 'lambda.'
nb.mod$mu.pred 
  
# Classifications of the 8 groups. The kth column of 'classifications'
# corresponds to the kth entry in lambda.
nb.mod$classifications
}