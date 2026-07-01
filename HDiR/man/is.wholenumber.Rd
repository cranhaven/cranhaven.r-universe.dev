\name{is.wholenumber}
\alias{is.wholenumber}
\title{
Checking if a value is integer
}
\description{
This function checks if a value is integer.

}
\usage{
is.wholenumber(x, tol = .Machine$double.eps^0.5)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
Numeric value to check if it is integer.}
  \item{tol}{
Tolerance parameter.}
}

\value{
Logical value: \code{TRUE} if \code{x} is integer and \code{FALSE} if \code{x} is not integer.
}

\author{
Paula Saavedra-Nieves and Rosa M. Crujeiras.
}

\keyword{internal}
