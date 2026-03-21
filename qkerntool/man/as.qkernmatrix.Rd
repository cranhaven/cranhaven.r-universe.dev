\name{as.qkernmatrix}
\docType{methods}
\alias{qkernmatrix-class}
\alias{as.qkernmatrix}
\alias{as.qkernmatrix-methods}
\alias{as.qkernmatrix,matrix-method}
\title{Assing qkernmatrix class to matrix objects}

\description{\code{as.qkernmatrix} in package \pkg{qkerntool} can be used
  to create the qkernmatrix class to matrix objects representing a
  q kernel matrix.  These matrices can then be used with the qkernmatrix
  interfaces which most of the functions in \pkg{qkerntool} support.}

\usage{
\S4method{as.qkernmatrix}{matrix}(x, center = FALSE)
}
\arguments{
  \item{x}{matrix to be assigned the \code{qkernmatrix} class }
  \item{center}{center the kernel matrix in feature space (default: FALSE) }
}

\author{
  Yusen Zhang \cr
    \email{yusenzhang@126.com}
    }

\seealso{\code{\link{qkernmatrix}},\code{\link{cndkernmatrix}}}

\keyword{methods}


\examples{
## Create the data
x <- rbind(matrix(rnorm(10),,2),matrix(rnorm(10,mean=3),,2))
y <- matrix(c(rep(1,5),rep(-1,5)))

### Use as.qkernmatrix to label the cov. matrix as a qkernel matrix
### which is eq. to using a linear kernel

K <- as.qkernmatrix(crossprod(t(x)))

K


}
