\name{supplbasis}
\alias{supplbasis}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Supplementary basis vectors for column space of a matrix
}
\description{
Computes a basis which, together with a basis of some columns of a matrix, constitute a basis of the column space of the entire matrix.
}
\usage{
supplbasis(A, B, tol=sqrt(.Machine$double.eps))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{A}{
Sub-matrix containing some columns of a matrix.
}
  \item{B}{
Sub-matrix containing remaining columns of same matrix.
}
 \item{tol}{
A relative tolerance to detect rank deficiency during qr decomposition (default = sqrt(.Machine$double.eps)).
 }
}

\value{
Returns a semi-orthogonal matrix whose columns, together with a basis of the column space of A, constitute a basis of the column space of the entire matrix (A:B).
}
\references{
Sengupta and Jammalamadaka (2019), Linear Models and Regression with R: An Integrated Approach.
}
\author{
Debasis Sengupta <shairiksengupta@gmail.com>, Jinwen Qiu <qjwsnow_ctw@hotmail.com>
}

\examples{
A <- cbind(c(2,1,-2),c(3,1,-1))
B <- diag(c(1,1,0))
supplbasis(A,B)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ algebra }
\keyword{ array }
