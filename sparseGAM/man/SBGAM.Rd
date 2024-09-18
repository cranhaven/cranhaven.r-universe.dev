\name{SBGAM}
\alias{SBGAM}

\title{Sparse Bayesian Generalized Additive Models}

\description{This function implements sparse Bayesian generalized additive models (GAMs) with the spike-and-slab group lasso (SSGL) penalty. Let \eqn{y_i} denote the \eqn{i}th response and \eqn{x_i} denote a \eqn{p}-dimensional vector of covariates. GAMs are of the form,

\deqn{g(E(y_i)) = \beta_0 + \sum_{j=1}^{p} f_j (x_{ij}), i = 1, ..., n,}

where \eqn{g} is a monotone increasing link function. The identity link function is used for Gaussian regression, the logit link is used for binomial regression, and the log link is used for Poisson, negative binomial, and gamma regression. With the SSGL penalty, some of the univariate functions \eqn{f_j(x_j)} will be estimated as \eqn{\hat{f}_j(x_j) = 0}, depending on the size of the spike hyperparameter \eqn{\lambda_0} in the SSGL prior. The functions \eqn{f_j(x_j), j = 1, ..., p}, are modeled using B-spline basis expansions. 
  
There is another implementation of sparse Gaussian GAMs with the SSGL penalty available at https://github.com/jantonelli111/SSGL, which uses natural cubic splines as the basis functions. This package \code{sparseGAM} uses B-spline basis functions and also implements sparse GAMs with the SSGL penalty for binomial, Poisson, negative binomial, and gamma regression.

For implementation of sparse \emph{frequentist} GAMs with the group LASSO, group SCAD, and group MCP penalties, use the \code{SFGAM} function.
}

\usage{
SBGAM(y, X, X.test, df=6, 
      family=c("gaussian","binomial","poisson","negativebinomial","gamma"), 
      nb.size=1, gamma.shape=1, nlambda0=20, lambda0, lambda1, a, b, 
      max.iter=100, tol = 1e-6, print.iter=TRUE)
}

\arguments{
  \item{y}{\eqn{n \times 1} vector of responses for training data.}
  \item{X}{\eqn{n \times p} design matrix for training data, where the \eqn{j}th column of \code{X} corresponds to the \eqn{j}th overall covariate.}
  \item{X.test}{\eqn{n_{test} \times p} design matrix for test data to calculate predictions. \code{X.test} must have the \emph{same} number of columns as \code{X}, but not necessarily the same number of rows. If \emph{no} test data is provided or if in-sample predictions are desired, then the function automatically sets \code{X.test=X} in order to calculate \emph{in-sample} predictions.}
  \item{df}{number of B-spline basis functions to use in each basis expansion. Default is \code{df=6}, but the user may specify degrees of freedom as any integer greater than or equal to 3.}
  \item{family}{exponential dispersion family. Allows for \code{"gaussian"}, \code{"binomial"}, \code{"poisson"}, \code{"negativebinomial"}, and \code{"gamma"}. Note that for \code{"negativebinomial"}, the size parameter must be specified, while for \code{"gamma"}, the shape parameter must be specified.}
    \item{nb.size}{known size parameter \eqn{\alpha} in \eqn{NB(\alpha,\mu_i)} distribution for negative binomial responses. Default is \code{nb.size=1}. Ignored if \code{family} is not \code{"negativebinomial"}.}
  \item{gamma.shape}{known shape parameter \eqn{\nu} in \eqn{Gamma(\mu_i,\nu)} distribution for gamma responses. Default is \code{gamma.shape=1}. Ignored if \code{family} is not \code{"gamma"}.}
  \item{nlambda0}{number of spike hyperparameter \eqn{L}. Default is \code{nlambda0=20}.}
  \item{lambda0}{grid of \eqn{L} spike hyperparameters \eqn{\lambda_0}. The user may specify either a scalar or a vector. If the user does not provide this, the program chooses the grid automatically.}
  \item{lambda1}{slab hyperparameter \eqn{\lambda_1} in the SSGL prior. Default is \code{lambda1=1}.}
  \item{a}{shape hyperparameter for the \eqn{Beta(a,b)} prior on the mixing proportion in the SSGL prior. Default is \code{a=1}.}
  \item{b}{shape hyperparameter for the \eqn{Beta(a,b)} prior on the mixing proportion in the SSGL prior. Default is \code{b=dim(X)[2]}.}
  \item{max.iter}{maximum number of iterations in the algorithm. Default is \code{max.iter=100}.}
  \item{tol}{convergence threshold for algorithm. Default is \code{tol=1e-6}.}
  \item{print.iter}{Boolean variable for whether or not to print the current \code{nlambda0} in the algorithm. Default is \code{print.iter=TRUE}.}
}

\value{The function returns a list containing the following components:
    \item{lambda0}{\eqn{L \times 1} vector of spike hyperparameters \code{lambda0} used to fit the model. \code{lambda0} is displayed in descending order.}
    \item{f.pred}{List of \eqn{L} \eqn{n_{test} \times p} matrices, where the \eqn{k}th matrix in the list corresponds to the \eqn{k}th spike hyperparameter in \code{lambda0}. The \eqn{j}th column in each matrix in \code{f.pred} is the estimate of the \eqn{j}th function evaluated on the test data in \code{X.test} for the \eqn{j}th covariate (or training data \code{X} if \code{X.test} was not specified). }
      \item{mu.pred}{\eqn{n_{test} \times L} matrix of predicted mean response values \eqn{\mu_{test} = E(Y_{test})} based on the \emph{test} data in \code{X.test} (or training data \code{X} if no argument was specified for \code{X.test}). The \eqn{k}th column in \code{mu.pred} corresponds to the predictions for the \eqn{k}th spike hyperparameter in \code{lambda0}.}
        \item{classifications}{\eqn{p \times L} matrix of classifications. An entry of "1" indicates that the corresponding function was classified as nonzero, and an entry of "0" indicates that the function was classified as zero. The \eqn{k}th column of \code{classifications} corresponds to the \eqn{k}th spike hyperparameter in \code{lambda0}.}
  \item{beta0}{\eqn{L \times 1} vector of estimated intercepts. The \eqn{k}th entry in \code{beta0} corresponds to the \eqn{k}th spike hyperparameter in \code{lambda0}.}
  \item{beta}{\eqn{dp \times L} matrix of estimated basis coefficients. The \eqn{k}th column in \code{beta} corresponds to the \eqn{k}th spike hyperparameter in \code{lambda0}.}
  \item{loss}{vector of either the residual sum of squares (\code{"gaussian"}) or the negative log-likelihood (\code{"binomial"}, \code{"poisson"}, \code{"negativebinomial"}, \code{"gamma"}) of the fitted model. The \eqn{k}th entry in \code{loss} corresponds to the \eqn{k}th spike hyperparameter in \code{lambda0}.}
}

\references{
Bai R. (2021). "Spike-and-slab group lasso for consistent Bayesian estimation and variable selection in non-Gaussian generalized additive models." \emph{arXiv pre-print arXiv:2007.07021}.

  Bai, R., Moran, G. E., Antonelli, J. L., Chen, Y., and Boland, M.R. (2021). "Spike-and-slab group lassos for grouped regression and sparse generalized additive models." \emph{Journal of the American Statistical Association}, in press.
}

\examples{
## Generate data
set.seed(12345)
X = matrix(runif(100*5), nrow=100)
n = dim(X)[1]
y = 3*sin(2*pi*X[,1])-3*cos(2*pi*X[,2]) + rnorm(n)

## Test data with 30 observations
X.test = matrix(runif(30*5), nrow=30)

## Fit sparse Bayesian generalized additive model to data with the SSGL penalty
## and 5 spike hyperparameters
SBGAM.mod = SBGAM(y, X, X.test, family="gaussian", lambda0=seq(from=50,to=10,by=-10))

## The model corresponding to the 1st spike hyperparameter
SBGAM.mod$lambda[1] 
SBGAM.mod$classifications[,1] 

## Plot first function f_1(x_1) in 2nd model
x1 = X.test[,1] 
## Estimates of all 20 function evaluations on test data
f.hat = SBGAM.mod$f.pred[[1]] 
## Extract estimates of f_1 
f1.hat = f.hat[,1] 

## Plot X_1 against f_1(x_1)
plot(x1[order(x1)], f1.hat[order(x1)], xlab=expression(x[1]), 
     ylab=expression(f[1](x[1])))
}