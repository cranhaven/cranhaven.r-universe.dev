\name{is.included}
\alias{is.included}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Whether one column space is contained in another
}
\description{
Checks whether column space of one matrix is a subset of the column space of another matrix.
}
\usage{
is.included(B, A, tol1=sqrt(.Machine$double.eps), tol2=sqrt(.Machine$double.eps))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{B}{
The matrix whose column space is to be checked for being a subset.
}
  \item{A}{
The matrix whose column space is to be checked for being a superset.
}
 \item{tol1}{
A relative tolerance to detect zero singular values while computing generalized inverse, in case A is rank deficient (default = sqrt(.Machine$double.eps)).
 }
 \item{tol2}{
A relative tolerance to detect whether there is sufficient closeness between B and A.ginv(A).B (default = sqrt(.Machine$double.eps)).
 }
}

\value{
A logical value (TRUE if the column space of B is contained in the column space of A).
}
\references{
Sengupta and Jammalamadaka (2019), Linear Models and Regression with R: An Integrated Approach.
}
\author{
Debasis Sengupta <shairiksengupta@gmail.com>, Jinwen Qiu <qjwsnow_ctw@hotmail.com>
}

\examples{
A <- cbind(c(2,1,-2),c(3,1,-1))
I <- diag(1,3)
is.included(A, I, tol1=sqrt(.Machine$double.eps), tol2=1e-15)
is.included(I, A, tol1=1e-14, tol2=sqrt(.Machine$double.eps))
is.included(projector(A), A, tol1=1e-15, tol2=1e-14)
is.included(A, projector(A))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ algebra }
\keyword{ array }
