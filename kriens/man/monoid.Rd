\name{monoid}
\alias{monoid}
\title{
  Creates the monoid binary operator
}
\description{
  Creates the monoid binary operator for a monoid in the Continuation category.
}
\usage{
monoid(op)
}
\arguments{
  \item{op}{
The binary operator to be be insert in the monoid (multiplication).
}
}
\value{
It returns a function of the type \code{h(f, g)} where \code{f} and \code{g} must be elements of the monoid and objects in the Continuation category. The function \code{h} will return a function of the type \code{t(x, ret)} which can be used in the Continuation category.
}
\references{
\url{https://en.wikipedia.org/wiki/Monoid_(category_theory)}
}
\author{
Matteo Provenzano\cr
\url{http://www.alephdue.com}
}
\note{
The developer must make sure that the function \code{f} and \code{g} are elements of a monoid and of the Continuation category. The developer must also ensure that the operator \code{op} is the monoid's binary operator.
}
\seealso{
\code{\link{do}}
}
\examples{
# A list is a monoid
replicate.10 <- function(x, ret) {
  ret(rep(x, 10))
}

# concatenation is the binary operator for the list monoid
# the empty list is the unit
`\%et\%` <- monoid(c)

replicate.20 <- do(replicate.10 \%et\% replicate.10)

# returns a list of 20 "a"s
replicate.20("a")

}
