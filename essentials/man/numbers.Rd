\name{numbers}
\alias{numbers}
\alias{as.numbers}
\alias{is.numbers}
\title{
Number Vectors
}
\description{
Creates or coerces objects of type \dQuote{numeric} or \dQuote{complex}.
\code{is.numbers} is a more general test of an object being interpretable as
numbers.
}
\usage{
numbers(length = 0)
as.numbers(x, strict = TRUE, ...)
is.numbers(x)
}
\arguments{
  \item{length}{
A non-negative integer specifying the desired length. Double values will be
coerced to integer: supplying an argument of length other than one is an error.
}
  \item{x}{
object to be coerced or tested.
}
  \item{strict}{
\code{TRUE} or \code{FALSE}, should a vector of complex numbers where each
number is strictly real (real component is NA or NaN or imaginary component is
NA or NaN or 0) be converted to a vector of real numbers?
}
  \item{...}{
further arguments passed to or from other methods. \code{as.numbers} is not a
generic function, but it can be made into one if desired. See
\code{\link[methods]{setGeneric}}.
}
}
\details{
\code{numbers} is identical to \code{\link[base]{numeric}} and
\code{\link[base]{double}} (and \code{real}). It creates a double-precision
vector of the specified length with each element equal to 0.

\code{as.numbers} attempts to coerce its argument to be of double or complex
type: like \code{\link{as.vector}} it strips attributes including names.

\code{is.numbers} is a more general test of an object being considered numbers,
meaning the base type of the class is \code{double} or \code{integer} or
\code{complex} \var{and} values can reasonably be regarded as numbers (e.g.,
arithmetic on them makes sense, and comparison should be done via the base
type).
}
\value{
for \code{numbers} see \code{\link[base]{double}}.

\code{as.numbers} returns either a \code{double} or \code{complex} vector.

\code{is.numbers(x)} is defined as \code{\link{is.numeric}(x) ||
\link{is.complex}(x)}.
}
\examples{
x <- 1:5
names(x) <- c("a", "b", "c", "d", "e")
as.numbers(x)  # vector converted from integer to double, names removed


x <- x + 0i  # x is now a complex vector
as.numbers(x)  # vector of type double since all numbers were purely real


## vector of type complex, despite being purely real
as.numbers(x, strict = FALSE)


x <- x + 1i
## vector remains of type complex since numbers are not purely real
as.numbers(x)
}
