\name{mGS}
\alias{mGS}

\title{Modified Gram-Schmidt orthogonalization of a matrix}

\description{
  This is an internal function which uses a R code to calculate an
  orthogonalization of a matrix. This function is used in the power method
  allowing to compute an eigendecomposition of a symmetric square.
}

\usage{
  mGS(A)
}

\arguments{
  \item{A}{Matrix in vector form.}
}

\value{This function returns a matrix which columns are the eigenvectors
  \item{wp}{Eigevectors matrix.}
}

\examples{
A <- matrix(rnorm(6,0,1), ncol=2)
res <- mGS(A)
t(res) %*% res
}
