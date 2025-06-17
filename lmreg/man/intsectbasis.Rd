\name{intsectbasis}
\alias{intsectbasis}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Basis of intersection of two column spaces
}
\description{
Computes an orthonormal basis of the intersection of column spaces of two given matrices.
}
\usage{
intsectbasis(A, B, tol1=sqrt(.Machine$double.eps), tol2=sqrt(.Machine$double.eps))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{A}{
First matrix.
}
  \item{B}{
Second matrix with identical number of rows.
}
 \item{tol1}{
A relative tolerance to detect zero singular values while computing generalized inverse, in case the matrix concerned is rank deficient (default = sqrt(.Machine$double.eps)).
 }
 \item{tol2}{
A tolerance to detect if there is any non-zero singular value of a 'parallel sum' matrix, without which the intersection space is null (default = sqrt(.Machine$double.eps)).
 }
}

\value{
Returns a semi-orthogonal matrix with columns forming an orthonormal basis of the intersection of the column spaces of A and B.
}
\references{
Sengupta and Jammalamadaka (2019), Linear Models and Regression with R: An Integrated Approach.
}
\author{
Debasis Sengupta <shairiksengupta@gmail.com>, Jinwen Qiu <qjwsnow_ctw@hotmail.com>
}


\examples{
A<-matrix(2,3,5)
B<-matrix(3,3,2)
intsectbasis(A,B, tol1=sqrt(.Machine$double.eps), tol2=1e-14)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ algebra }
\keyword{ array }
