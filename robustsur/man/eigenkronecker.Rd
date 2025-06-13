\name{eigenkronecker}
\alias{eigenkronecker}
\title{Spectral Decomposition of a kronecker product of a matrix
  with an identity matrix}
\description{
Computes eigenvalues and eigenvectors of the kronecker product of a
  matrix with an identity matrix.}
\usage{
eigenkronecker(x, n)
}
\arguments{
  \item{x}{a numeric or complex symmetric matrix whose spectral
  decomposition is to be computed. Logical matrices are coerced to numeric.}
  \item{n}{dimension of the identity matrix.}
}
\details{
  Only symmetric matrices are considered.
}
\value{
  The spectral decomposition of kronecher product between \code{x} and
  an identity matrix of dimesion \code{n} is returned as a list with
     components
  \item{values}{a vector containing the eigenvalues.}
  \item{vectors}{a matrix whose columns contain the eigenvectors.}
}
\references{
 R.A. Horn and C.R. Johnson (1994) Topics in Matrix Analysis, Cambridge University Press. Theorem 4.2.12.
}
\author{
  Claudio Agostinelli and Giovanni Saraceno
}
\seealso{
  \code{\link{eigen}} and \code{\link{kronecker}}.
}
\examples{
 eigenkronecker(x=cbind(c(1,-1), c(-1,1)), n=2)
}

\keyword{algebra}
\keyword{array}
