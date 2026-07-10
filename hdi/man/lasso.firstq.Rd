\name{lasso.firstq}
\alias{lasso.firstq}
\title{Determine the first q Predictors in the Lasso Path}
\description{
  Determines the q predictors that enter the lasso path first.
}
\usage{lasso.firstq(x, y, q, ...)
}
\arguments{
  \item{x}{numeric design matrix (without intercept) of dimension \eqn{n
      \times p}{n * p}.}
  \item{y}{response vector of length \eqn{n}.}
  \item{q}{number of predictors that should be selected, a positive integer.}
  \item{...}{optional additional arguments to be passed to \code{\link[glmnet]{glmnet}}.}
}
\details{The \code{lasso.firstq} function calls
  \code{\link[glmnet]{glmnet}} in a special way and simply postprocesses its
  nonzero predictor list, see its source code.
}
\value{Vector of selected predictors.}
\author{Lukas Meier}

\seealso{\code{\link{hdi}};
  the default choice for \code{hdi()}, \code{\link{lasso.cv}}.
  \code{\link[glmnet]{glmnet}}
}
\examples{
x <- matrix(rnorm(100 * 1000), nrow = 100, ncol = 1000)
y <- x[,1] * 2 + x[,2] * 2.5 + rnorm(100)
sel <- lasso.firstq(x, y, q = 5)
sel # 5 integers from {1,2, ..., 1000},  including '1' and '2', typically
}
\keyword{models}
\keyword{regression}
