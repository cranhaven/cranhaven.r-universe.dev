\name{aslength1}
\alias{aslength1}
\title{
Subset the First Element of a Vector
}
\description{
Subset the first element of a vector.
}
\usage{
aslength1(x)
}
\arguments{
  \item{x}{
vector (or an object which can be coerced) with at least one element.
}
}
\details{
Vectors of length one return themselves. Vectors of length greater than one
return the first element with a warning. Vectors of length zero raise an error.
If \code{x} is a vector (determined by \code{\link{is.vector}}), names will be
preserved.
}
\value{
A vector of length 1.
}
\examples{
aslength1(1)
aslength1(1:10)
try(aslength1(integer(0)))
}
