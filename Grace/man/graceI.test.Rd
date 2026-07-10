\name{graceI.test}
\alias{graceI.test}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
  Graph-Constrained Hypothesis Testing Procedure
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
  Test for the association between Y and each column of X using the GraceI test based on Zhao and Shojaie (2016).
}
\usage{
  graceI.test(Y,X,lambda.2,eta=0.05,C=4*sqrt(3),K=10,sigma.error=NULL,verbose=FALSE)
}
\arguments{
  \item{Y}{
	  outcome vector.
  }
  \item{X}{
  	matrix of predictors.
  }
  \item{lambda.2}{
    tuning parameter value for the ridge penalty (see details). If a sequence of lambda.2 values is supplied, K-fold cross-validation is performed.
  }
  \item{eta}{
    sparsity parameter \eqn{\eta} (see details).
  }
  \item{C}{
    parameter for the initial estimator (see details). It could also be "cv" or "scaled.lasso", in which case cross-validation or the scaled lasso are applied to estimate the initial estimator.
  }
  \item{K}{
    number of folds in cross-validation.
  }
  \item{sigma.error}{
    error standard deviation. If NULL, scaled lasso is then applied to estimate it.
  }
  \item{verbose}{
    whether computation progress should be printed.
  }
}
\details{
This function performs the GraceI test. See the documentation for grace.test.
}
\value{
  An R `list' with elements:
  \item{intercept}{fitted intercept.}
  \item{beta}{fitted regression coefficients.}
  \item{pvalue}{p-values based on the Grace tests.}
  \item{group.test}{function to perform the group test, with the null hypothesis being that all the regression coefficients in the group equal zero. The argument of this function needs to be an index vector of variables. There is an optimal second argument, which specifies whether the group test should be performed based on the "holm" procedure (default), or based on the "max" test statstic. The output is the p-value of the group test. See examples below.}
}
\references{
  Li, C., and Li, H. (2008). Network-constrained regularization and variable selection for analysis of genomic data. Bioinformatics, 24, 1175-1182.
  
  Sun, T., and Zhang, C.-H. (2012). Scaled sparse linear regression. Biometrika, 99, 879-898.
  
  Buhlmann, P. (2013). Statistical significance in high-dimensional linear models. Bernoulli, 19, 1212-1242.
  
  van de Geer, S., Buhlmann, P., Rotiv, Y., and Dezeure, R. (2014). On asymptotically optimal confidence regions and tests for high-dimensional models. The Annals of Statistics, 42, 1166-1202.
  
  Zhang, C.-H., and Zhang, S.S. (2014). Confidence intervals for low dimensional parameters in high dimensional linear models. Journal of the Royal Statistical Society: Series B, 76, 217-242.
  
  Zhao, S., and Shojaie, A. (2016). A signifiance test for graph-constrained estimation. Biometrics, 72, 484-493.
}
\author{
  Sen Zhao
}