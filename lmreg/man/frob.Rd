\name{frob}
\alias{frob}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Frobenius norm of a matrix
}
\description{
Computes the Frobenius norm of a given matrix.
}
\usage{
frob(M)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{M}{
Matrix whose Frobenius norm is to be computed.
}
}

\value{
A scalar value, describing the Frobenius norm (positive square root of sum of squared elements) of M.
}
\references{
Sengupta and Jammalamadaka (2019), Linear Models and Regression with R: An Integrated Approach.
}
\author{
Debasis Sengupta <shairiksengupta@gmail.com>, Jinwen Qiu <qjwsnow_ctw@hotmail.com>
}


\examples{
frob(matrix(2,3,2))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ algebra }
\keyword{ array }
