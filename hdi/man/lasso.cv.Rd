\name{lasso.cv}
\alias{lasso.cv}
\title{Select Predictors via (10-fold) Cross-Validation of the Lasso}
\description{
  Performs (n-fold) cross-validation of the lasso (via
  \code{\link[glmnet]{cv.glmnet}}) and determines the prediction
  optimal set of parameters.
}
\usage{
lasso.cv(x, y,
         nfolds = 10,
         grouped = nrow(x) > 3*nfolds,
         \dots)
}
\arguments{
  \item{x}{numeric design matrix (without intercept) of dimension \eqn{n
      \times p}{n * p}.}
  \item{y}{response vector of length \eqn{n}.}
  \item{nfolds}{the number of folds to be used in the cross-validation}
  \item{grouped}{corresponds to the \code{grouped} argument to \code{\link[glmnet]{cv.glmnet}}. This has
  a smart default such that glmnet does not give a warning about too
  small sample size.}
  \item{\dots}{further arguments to be passed to
    \code{\link[glmnet]{cv.glmnet}}.}
}
\details{
  The function basically only calls \code{\link[glmnet]{cv.glmnet}}, see source
  code.
}
\value{Vector of selected predictors.}
\author{Lukas Meier}

\seealso{\code{\link{hdi}} which uses \code{lasso.cv()} by default;
  \code{\link[glmnet]{cv.glmnet}}.
  An alternative for \code{hdi()}: \code{\link{lasso.firstq}}.
}
\examples{
x <- matrix(rnorm(100 * 1000), nrow = 100, ncol = 1000)
y <- x[,1] * 2 + x[,2] * 2.5 + rnorm(100)
sel <- lasso.cv(x, y)
sel
}
\keyword{models}
\keyword{regression}
