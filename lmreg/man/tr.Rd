\name{tr}
\alias{tr}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Trace of matrix
}
\description{
Computes the trace of a given matrix.
}
\usage{
tr(M)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{M}{
A matrix whose trace is to be computed.
}
}

\value{
A scalar value, describing the trace of M.
}

\references{
Sengupta and Jammalamadaka (2019), Linear Models and Regression with R: An Integrated Approach.
}
\author{
Debasis Sengupta <shairiksengupta@gmail.com>, Jinwen Qiu <qjwsnow_ctw@hotmail.com>
}

\examples{
tr(matrix(2,2,2))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ algebra }

