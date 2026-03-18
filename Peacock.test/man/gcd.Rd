\name{gcd}
\alias{gcd}
\title{Greatest Common Divisor}
\description{This function returns the greatest common divisor of two integers}
\usage{gcd(x, y)}
\arguments{
  \item{x}{a nonnegative integer}
  \item{y}{a nonnegative integer}
}
\details{
The function utilizes the Euclidean algorithm to compute the greatest common divisor of two integers
}
\value{
An integer, which is the greatest common divisor. If both arguments are zero, then the returned value is 1. 
}

\author{Yuanhui Xiao}

\examples{
x <- 4
y <- 6
d <- gcd(x, y)
d
}
\keyword{ greatest common divisor}

