\name{MatrixAlgebra}
\alias{Eigen}
\alias{CppSolve}
\alias{LowerChol}
\title{A Set of Functions for Doing Some Calculations on Matrices in \code{GPM} Package}
\usage{
Eigen(A)
CppSolve(A, B)
LowerChol(A)
}
\arguments{
\item{A}{Numeric, symmetric, and positive definite matrix.}

\item{B}{Numeric matrix or vector.}

}
\value{
\code{Eigen(A))} returns the smallest eigen value of A.
\code{CppSolve(A, B)} solves for \code{X} in \code{AX=B}.
\code{LowerChol(A)} return the lower triangular Cholesky decomposition of \code{A}.
}
\description{
These functions perform some matrix algebra to calculate the log-likelihood function.
}
\note{
These functions are \strong{NOT} exported once the GPM package is loaded.
}
\examples{
# see the examples in \\code{\\link[GPM]{Fit}}
}
\seealso{
\code{\link[GPM]{Fit}} to see how a GP model can be fitted to a training dataset.\cr
\code{\link[GPM]{Predict}} to use the fitted GP model for prediction.\cr
\code{\link[GPM]{Draw}} to plot the response via the fitted model.
}
