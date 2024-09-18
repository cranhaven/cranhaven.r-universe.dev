\name{SSGL}
\alias{SSGL}

\title{Spike-and-Slab Group Lasso Regression}

\description{This is a stand-alone function for group-regularized regression models in the exponential dispersion family with the spike-and-slab group lasso (SSGL) penalty.  Let \eqn{y_i} denote the \eqn{i}th response and \eqn{x_i} denote a \eqn{p}-dimensional vector of covariates. We fit models of the form,

\deqn{g(E(y_i)) = \beta_0 + x_i^T \beta, i = 1, ..., n,}

where \eqn{g} is a monotone increasing link function. The identity link function is used for Gaussian regression, the logit link is used for binomial regression, and the log link is used for Poisson, negative binomial, and gamma regression.
  
  If the covariates in each \eqn{x_i} are grouped according to known groups \eqn{g=1, ..., G}, then this function may estimate some of the \eqn{G} groups of coefficients as all zero, depending on the amount of regularization. 
  
Another implementation of the SSGL model for Gaussian regression models is available on Github at https://github.com/jantonelli111/SSGL. This package \code{sparseGAM} also implements the SSGL model for binomial, Poisson, negative binomial, and gamma regression.
}

\usage{
SSGL(y, X, X.test, groups, 
     family=c("gaussian","binomial","poisson","negativebinomial","gamma"), 
     nb.size=1, gamma.shape=1, weights, nlambda0=20, lambda0, lambda1, a, b, 
     max.iter=100, tol = 1e-6, print.iter=TRUE) 
}

\arguments{
  \item{y}{\eqn{n \times 1} vector of responses for training data.}
  \item{X}{\eqn{n \times p} design matrix for training data, where the \eqn{j}th column of \code{X} corresponds to the \eqn{j}th overall covariate.}
  \item{X.test}{\eqn{n_{test} \times p} design matrix for test data to calculate predictions. \code{X.test} must have the \emph{same} number of columns as \code{X}, but not necessarily the same number of rows. If \emph{no} test data is provided or if in-sample predictions are desired, then the function automatically sets \code{X.test=X} in order to calculate \emph{in-sample} predictions.}
  \item{groups}{\eqn{p}-dimensional vector of group labels. The \eqn{j}th entry in \code{groups} should contain either the group number \emph{or} the name of the factor level that the \eqn{j}th covariate belongs to. \code{groups} must be either a vector of integers or factors.}
  \item{family}{exponential dispersion family. Allows for \code{"gaussian"}, \code{"binomial"}, \code{"poisson"}, \code{"negativebinomial"}, and \code{"gamma"}. Note that for \code{"negativebinomial"}, the size parameter must be specified, while for \code{"gamma"}, the shape parameter must be specified.}
    \item{nb.size}{known size parameter \eqn{\alpha} in \eqn{NB(\alpha,\mu_i)} distribution for the negative binomial responses. Default is \code{nb.size=1}. Ignored if \code{family} is not \code{"negativebinomial"}.}
  \item{gamma.shape}{known shape parameter \eqn{\nu} in \eqn{Gamma(\mu_i,\nu)} distribution for gamma responses. Default is \code{gamma.shape=1}. Ignored if \code{family} is not \code{"gamma"}.}
  \item{weights}{group-specific, nonnegative weights for the penalty. Default is to use the square roots of the group sizes.}
  \item{nlambda0}{number of spike hyperparameters \eqn{L}. Default is \code{nlambda0=20}.}
  \item{lambda0}{grid of \eqn{L} spike hyperparameters \eqn{\lambda_0}. The user may specify either a scalar or a vector. If the user does not provide this, the program chooses the grid automatically.}
  \item{lambda1}{slab hyperparameter \eqn{\lambda_1} in the SSGL prior. Default is \code{lambda1=1}.}
  \item{a}{shape hyperparameter for the \eqn{Beta(a,b)} prior on the mixing proportion in the SSGL prior. Default is \code{a=1}.}
  \item{b}{shape hyperparameter for the \eqn{Beta(a,b)} prior on the mixing proportion in the SSGL prior. Default is \code{b=dim(X)[2]}.}
  \item{max.iter}{maximum number of iterations in the algorithm. Default is \code{max.iter=100}.}
  \item{tol}{convergence threshold for algorithm. Default is \code{tol=1e-6}.}
  \item{print.iter}{Boolean variable for whether or not to print the current \code{nlambda0} in the algorithm. Default is \code{print.iter=TRUE}.}
}

\value{The function returns a list containing the following components:
    \item{lambda0}{\eqn{L \times 1} vector of spike hyperpameters \code{lambda0} used to fit the model. \code{lambda0} is displayed in descending order.}
  \item{beta0}{\eqn{L \times 1} vector of estimated intercepts. The \eqn{k}th entry in \code{beta0} corresponds to the \eqn{k}th spike hyperparameter in \code{lambda0}.}
  \item{beta}{\eqn{p \times L} matrix of estimated regression coefficients. The \eqn{k}th column in \code{beta} corresponds to the \eqn{k}th spike hyperparameter in \code{lambda0}.}
  \item{mu.pred}{\eqn{n_{test} \times L} matrix of predicted mean response values \eqn{\mu_{test} = E(Y_{test})} based on the \emph{test} data in \code{X.test} (or training data \code{X} if no argument was specified for\code{X.test}). The \eqn{k}th column in \code{mu.pred} corresponds to the predictions for the \eqn{k}th spike hyperparameter in \code{lambda0}.}
  \item{classifications}{\eqn{G \times L} matrix of classifications, where \eqn{G} is the number of groups. An entry of "1" indicates that the group was classified as nonzero, and an entry of "0" indicates that the group was classified as zero. The \eqn{k}th column of \code{classifications} corresponds to the \eqn{k}th spike hyperparameter in \code{lambda0}.}
  \item{loss}{vector of either the residual sum of squares (\code{"gaussian"}) or the negative log-likelihood (\code{"binomial"}, \code{"poisson"}, \code{"negativebinomial"}, \code{"gamma"}) of the fitted model. The \eqn{k}th entry in \code{loss} corresponds to the \eqn{k}th spike hyperparameter in \code{lambda0}.}
}

\references{
Bai R. (2021). "Spike-and-slab group lasso for consistent Bayesian estimation and variable selection in non-Gaussian generalized additive models." \emph{arXiv pre-print arXiv:2007.07021}.

  Bai, R., Moran, G. E., Antonelli, J. L., Chen, Y., and Boland, M.R. (2021). "Spike-and-slab group lassos for grouped regression and sparse generalized additive models." \emph{Journal of the American Statistical Association}, in press.
}

\examples{
## Generate data
set.seed(12345)
X = matrix(runif(100*10), nrow=100)
n = dim(X)[1]
groups = c("A","A","A","B","B","B","C","C","D","D")
groups = as.factor(groups)
true.beta = c(-2.5,1.5,1.5,0,0,0,2,-2,0,0)

## Generate responses from Gaussian distribution
y = crossprod(t(X),true.beta) + rnorm(n)

## Generate test data
n.test = 50
X.test = matrix(runif(n.test*10), nrow=n.test)

## Fit SSGL model with 10 spike hyperparameters
## Note that if user does not specify lambda0, the SSGL function chooses a grid automatically.

SSGL.mod = SSGL(y, X, X.test, groups, family="gaussian", lambda0=seq(from=50,to=5,by=-5))

## Regression coefficient estimates
SSGL.mod$beta

# Predicted n.test-dimensional vectors mu=E(Y.test) based on test data, X.test. 
# The kth column of 'mu.pred' corresponds to the kth entry in 'lambda.'
SSGL.mod$mu.pred 

# Classifications of the 8 groups. The kth column of 'classifications'
# corresponds to the kth entry in 'lambda.'
SSGL.mod$classifications

\donttest{
## Example with binomial regression

## Generate binary responses
eta = crossprod(t(X), true.beta)
y = rbinom(n, size=1, prob=1/(1+exp(-eta)))

## Fit SSGL model with 10 spike hyperparameters
## Note that if user does not specify lambda0, the SSGL function chooses a grid automatically.

SSGL.mod = SSGL(y, X, X.test, groups, family="binomial", 
		lambda0=seq(from=10,to=1,by=-1))

## Predicted probabilities of success mu=E(Y.test) based on test data, X.test
SSGL.mod$mu.pred

## Classifications of the 8 groups. 
SSGL.mod$classifications
}
}