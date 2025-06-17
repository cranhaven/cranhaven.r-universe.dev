\name{projector}
\alias{projector}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Orthogonal projector of a matrix
}
\description{
Computes the orthogonal projection matrix for the column space of a given matrix.
}
\usage{
projector(M, tol=sqrt(.Machine$double.eps))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{M}{
A matrix for which the orthogonal projection matrix is to be computed.
}
 \item{tol}{
A relative tolerance to detect zero singular values while computing generalized inverse, in case M is rank deficient (default = sqrt(.Machine$double.eps)).
 }
}

\value{
Returns the orthogonal projection matrix for the column space of M.
}
\references{
Sengupta and Jammalamadaka (2019), Linear Models and Regression with R: An Integrated Approach.
}
\author{
Debasis Sengupta <shairiksengupta@gmail.com>, Jinwen Qiu <qjwsnow_ctw@hotmail.com>
}

\examples{
projector(matrix(3,3,3))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ algebra }
\keyword{ array }
