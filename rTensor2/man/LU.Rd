\name{LU}
\alias{LU}
\title{LU Decomposition of a Complex Matrix}
\usage{
LU(A)
}
\arguments{
\item{A}{: an  \eqn{n} x \eqn{n} matrix}
}
\value{
a lower triangular matrix L and an upper triangular matrix U so that A = LU.
}
\description{
Decompose a square matrix A into the product of a lower triangular matrix L and an upper triangular matrix U.
}
\author{
Kyle Caudle

Randy Hoover

Jackson Cates
}
\examples{
z <- complex(real = rnorm(16), imag = rnorm(16))
A <- matrix(z,nrow=4)
LU(A)
}
\keyword{misc}
