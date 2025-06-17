\name{basis}
\alias{basis}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Basis of column space of a matrix
}
\description{
Computes an orthonormal basis of the column space of a given matrix.
}
\usage{
basis(M, tol=sqrt(.Machine$double.eps))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{M}{
Matrix for which basis of the column space is needed.
}
 \item{tol}{
A relative tolerance to determine rank through qr  decomposition \cr (default = sqrt(.Machine$double.eps)).
 }
}
\value{
Returns a semi-orthogonal matrix with columns forming an orthonormal basis of the column space of M.
}
\references{
Sengupta and Jammalamadaka (2019), Linear Models and Regression with R: An Integrated Approach.
}
\author{
Debasis Sengupta <shairiksengupta@gmail.com>, Jinwen Qiu <qjwsnow_ctw@hotmail.com>
}
\examples{
  basis(matrix(c(2,1,3,4,2,3,2,6,4,2,6,8),4,3))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ algebra }
\keyword{ array }
