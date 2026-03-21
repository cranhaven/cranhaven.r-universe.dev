\name{as.cndkernmatrix}
\docType{methods}
\alias{cndkernmatrix-class}
\alias{as.cndkernmatrix}
\alias{as.cndkernmatrix-methods}
\alias{as.cndkernmatrix,matrix-method}
\title{Assing cndkernmatrix class to matrix objects}

\description{\code{as.cndkernmatrix} in package \pkg{qkerntool} can be used
  to create the cndkernmatrix class to matrix objects representing a
  CND kernel matrix.  These matrices can then be used with the cndkernmatrix
  interfaces which most of the functions in \pkg{qkerntool} support.}

\usage{
\S4method{as.cndkernmatrix}{matrix}(x, center = FALSE)
}
\arguments{
  \item{x}{matrix to be assigned the \code{cndkernmatrix} class }
  \item{center}{center the cndkernel matrix in feature space (default: FALSE) }
}

\author{
  Yusen Zhang \cr
    \email{yusenzhang@126.com}
    }

\seealso{\code{\link{cndkernmatrix}},\code{\link{qkernmatrix}}}

\keyword{methods}


\examples{
## Create the data
x <- rbind(matrix(rnorm(10),,2),matrix(rnorm(10,mean=3),,2))
y <- matrix(c(rep(1,5),rep(-1,5)))

### Use as.cndkernmatrix to label the cov. matrix as a CND kernel matrix
### which is eq. to using a linear kernel

K <- as.cndkernmatrix(crossprod(t(x)))

K


}
