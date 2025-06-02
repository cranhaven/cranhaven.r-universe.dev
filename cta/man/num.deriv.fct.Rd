\name{num.deriv.fct}
\alias{num.deriv.fct}
\title{
Numerical Derivatives Based on Central Difference Formula
}
\description{
Computes the numerical derivative of the transpose of the
vector-valued function \eqn{f} evaluated at the point \eqn{m}, based
on the central difference formula.

If \eqn{f} is a mapping from \eqn{R^p} to \eqn{R^q}, then the result
is a \eqn{p}-by-\eqn{q} matrix.
i.e. The result is an approximation to
\eqn{\partial f'(m)/\partial m}.
}
\usage{
num.deriv.fct(f.fct, m)
}
\arguments{
  \item{f.fct}{An \R function object that defines a vector-valued
               function \eqn{f}.}
  \item{m}{A vector, indicating the point \eqn{m} at which the numerical
           derivative is to be computed.}
}
\value{
\code{num.deriv.fct} returns a matrix, which is the numerical
derivative of the transpose of the function \eqn{f} evaluated at \eqn{m}.
}
\author{
Joseph B. Lang
}
\examples{
# Let x = (x[1], x[2], x[3])', and
# f(x) = (x[1]^3 - 2 * x[2] + 1, sin(x[1] * x[3]), log(x[2] + x[3]))'.
# Approximate d f^{T}(x) / d x  at x = (1, 2, 3)'.
# The true value of the derivative is
# [ 3   3cos(3)    0
#  -2      0      0.2
#   0    cos(3)   0.2] .

f.fct <- function(x) {
  c(x[1]^3 - 2 * x[2] + 1,
    sin(x[1] * x[3]),
    log(x[2] + x[3]))
}
num.deriv.fct(f.fct, c(1, 2, 3))
}
\keyword{math}% use one of  RShowDoc("KEYWORDS")
