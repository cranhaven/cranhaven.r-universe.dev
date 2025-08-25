\name{mGSc}
\alias{mGSc}

\title{Modified Gram-Schmidt orthogonalization of a matrix}

\description{
  This is an internal function which uses a C code to calculate an
  orthogonalization of a matrix. This function is used in the power method
  allowing to compute an eigendecomposition of a symmetric square.
}

\usage{
  mGSc(amat, m, n)
}

\arguments{
  \item{amat}{Matrix in vector form.}
  \item{m}{Number of rows of the matrix amat.}
  \item{n}{Number of columns of the matrix amat.}
}

\value{This function returns  a data frame containing 1 component
  \item{wp}{Eigenvectors matrix.}
}

\examples{
d <- 3
r <- 2
amat <- c(rnorm(d*r,0,1))
res <- mGSc(amat, d, r)
wp <- matrix(res$wp, nrow=d, ncol=r)
t(wp) %*% wp
}
