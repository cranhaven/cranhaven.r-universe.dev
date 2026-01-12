\name{essentials-package}
\alias{essentials-package}
\alias{essentials}
\docType{package}
\title{
\packageTitle{essentials}
}
\description{
\packageDescription{essentials}
}
\details{
The four most important functions from this package are \code{as.numbers},
\code{is.numbers}, \code{as.scalar} and \code{aslength1}.

\code{as.numbers} coerces its argument to type double or complex.
\code{is.numbers} tests if its argument is interpretable as numbers.

\code{as.scalar} coerces its argument to an scalar (an atomic vector of length
1). It strips attributes including names.

\code{aslength1} coerces its argument to a vector of length 1 (not necessarily
atomic). It strips attributes from arguments that are not vectors, but preserves
names for arguments that are vectors.
}
\author{
\packageAuthor{essentials}

Maintainer: \packageMaintainer{essentials}
}
\keyword{ package }
\examples{
as.numbers("4")
as.numbers("4+0i")  # imaginary component is removed
as.numbers("4+1i")

is.numbers(4L)
is.numbers(4)
is.numbers(4+1i)

as.scalar(1:100)
as.scalar(as.list(1:100))  # coerced to NA_character_ since argument isn't atomic

aslength1(1:100)  # identical to as.scalar(1:100)
aslength1(as.list(1:100))  # returns a list of length 1
}
