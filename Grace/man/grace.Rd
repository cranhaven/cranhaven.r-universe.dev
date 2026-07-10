\name{grace}
\alias{grace}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
  Graph-Constrained Estimation
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
  Calculate coefficient estimates of Grace based on methods described in Li and Li (2008).
}
\usage{
  grace(Y, X, L, lambda.L, lambda.1=0, lambda.2=0, normalize.L=FALSE, K=10, verbose=FALSE)
}
\arguments{
  \item{Y}{
	  outcome vector.
  }
  \item{X}{
  	matrix of predictors.
  }
  \item{L}{
	  penalty weight matrix L.
  }
  \item{lambda.L}{
	  tuning parameter value for the penalty induced by the L matrix (see details). If a sequence of lambda.L values is supplied, K-fold cross-validation is performed.
  }
  \item{lambda.1}{
    tuning parameter value for the lasso penalty (see details). If a sequence of lambda.1 values is supplied, K-fold cross-validation is performed.
  }
  \item{lambda.2}{
    tuning parameter value for the ridge penalty (see details). If a sequence of lambda.2 values is supplied, K-fold cross-validation is performed.
  }
  \item{normalize.L}{
	  whether the penalty weight matrix L should be normalized.
  }
  \item{K}{
    number of folds in cross-validation.
  }
  \item{verbose}{
    whether computation progress should be printed.
  }
}
\details{
The Grace estimator is defined as

\deqn{(\hat\alpha, \hat\beta) = \arg\min_{\alpha, \beta}{\|Y-\alpha 1 -X\beta\|_2^2+lambda.L(\beta^T L\beta)+lambda.1\|\beta\|_1+lambda.2\|\beta\|_2^2}}

In the formulation, L is the penalty weight matrix. Tuning parameters lambda.L, lambda.1 and lambda.2 may be chosen by cross-validation. In practice, X and Y are standardized and centered, respectively, before estimating \eqn{\hat\beta}. The resulting estimate is then rescaled back into the original scale. Note that the intercept \eqn{\hat\alpha} is not penalized.

The Grace estimator could be considered as a generalized elastic net estimator (Zou and Hastie, 2005). It penalizes the regression coefficient towards the space spanned by eigenvectors of L with the smallest eigenvalues. Therefore, if L is informative in the sense that \eqn{L\beta} is small, then the Grace estimator could be less biased than the elastic net.
}
\value{
  An R `list' with elements:
  \item{intercept}{fitted intercept.}
  \item{beta}{fitted regression coefficients.}
}
\references{
  Zou, H., and Hastie, T. (2005). Regularization and variable selection via the elastic net. Journal of the Royal Statistical Society: Series B, 67, 301-320.
  
  Li, C., and Li, H. (2008). Network-constrained regularization and variable selection for analysis of genomic data. Bioinformatics, 24, 1175-1182.
}
\author{
  Sen Zhao
}

\examples{
set.seed(120)
n <- 100
p <- 200

L <- matrix(0, nrow = p, ncol = p)
for(i in 1:10){
	L[((i - 1) * p / 10 + 1), ((i - 1) * p / 10 + 1):(i * (p / 10))] <- -1
}
diag(L) <- 0
ind <- lower.tri(L, diag = FALSE)
L[ind] <- t(L)[ind]
diag(L) <- -rowSums(L)

beta <- c(rep(1, 10), rep(0, p - 10))

Sigma <- solve(L + 0.1 * diag(p))
sigma.error <- sqrt(t(beta) \%*\% Sigma \%*\% beta) / 2

X <- mvrnorm(n, mu = rep(0, p), Sigma = Sigma)
Y <- c(X \%*\% beta + rnorm(n, sd = sigma.error))

grace(Y, X, L, lambda.L = c(0.08, 0.12), lambda.2 = c(0.08, 0.12))
}