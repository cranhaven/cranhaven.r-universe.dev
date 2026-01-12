\name{hypot}
\alias{hypot}
\title{
Hypotenuse
}
\description{
Compute the length of the hypotenuse.
}
\usage{
hypot(x, y)
}
\arguments{
  \item{x, y}{
numeric vectors; the lengths of non-hypotenuse sides, the sides adjacent to the
right angle.
}
}
\details{
The hypotenuse is the longest side of a right-angled triangle, the side opposite
the right angle. The length of the hypotenuse is defined as:

\deqn{\sqrt(x^2 + y^2)}{
      \sqrt(x^2 + y^2)}

If \code{x[i]} or \code{y[i]} is infinite, the result in the \code{i}-th
position will always be \code{Inf}. Otherwise, if \code{x[i]} or \code{y[i]} is
\code{NA} or \code{NaN}, the result in the \code{i}-th position will be
\code{NaN}. Otherwise, if the absolute value of \code{x[i]} is considerably
larger than the absolute value of \code{y[i]}, the result in the \code{i}-th
position will be the absolute value of \code{x[i]} (and vice versa). Otherwise,
the value will be calculated using the above definition.
}
\value{
A numeric vector. If \code{x} or \code{y} is a zero-length vector the result has
length zero. Otherwise, the result has length of the maximum of the lengths of
\code{x} and \code{y}.
}
\examples{
hypot(Inf, NaN)     # still positive infinity
hypot(NaN, 0)       # NaN
hypot(NA_real_, 0)  # NaN


## numbers whose squares would overflow normally are handled well
hypot(.Machine$double.xmax, 5     )
hypot(1e+300              , 1e+300)


hypot(3, 4)         # 5
}
