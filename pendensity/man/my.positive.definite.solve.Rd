\name{my.positive.definite.solve}
\alias{my.positive.definite.solve}
\title{my.positive.definite.solve}
\description{
Reverses a quadratic positive definite matrix.
}
\usage{
my.positive.definite.solve(A, eps = 1e-15)
}
\arguments{
  \item{A}{quadratic positive definite matrix}
  \item{eps}{level of the lowest eigenvalue to consider}
}
\details{The program makes an eigenvalue decomposition of the positive definite matrix A and searches all eigenvalues greater than eps. The value of return is the inverse matrix of A, constructed with the matrix product of the corresponding eigenvalues and eigenvectors. }
\value{The return is the inverse matrix of A.}
\author{Christian Schellhase <cschellhase@wiwi.uni-bielefeld.de>}
\keyword{algebra}

