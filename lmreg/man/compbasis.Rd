\name{compbasis}
\alias{compbasis}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Basis of orthogonal complement of column space of a matrix
}
\description{
Computes an orthonormal basis of the orthogonal complement of the column space of a given matrix.
}
\usage{
compbasis(M, tol=sqrt(.Machine$double.eps))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{M}{
Matrix for which basis of the orthogonal complement of the column space is needed.
}
\item{tol}{
A relative tolerance to determine rank through qr  decomposition \cr (default = sqrt(.Machine$double.eps)).
 }
}

\value{
Returns a semi-orthogonal matrix with columns forming an orthonormal basis of the orthogonal complement of the column space of M.
}

\references{
Sengupta and Jammalamadaka (2019), Linear Models and Regression with R: An Integrated Approach.
}
\author{
Debasis Sengupta <shairiksengupta@gmail.com>, Jinwen Qiu <qjwsnow_ctw@hotmail.com>
}

\examples{
compbasis(matrix(c(3,3,3,3),2,2))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ algebra }
\keyword{ array }
