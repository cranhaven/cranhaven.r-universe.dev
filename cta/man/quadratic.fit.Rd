\name{quadratic.fit}
\alias{quadratic.fit}
\title{
Quadratic Fit
}
\description{
Fits a quadratic curve that passes all three points on the two-dimensional Euclidean space \eqn{R^2}.

If the design matrix \eqn{X} of the quadratic fit has a condition number which is greater than \eqn{10^8}, a linear regression line is fitted to the three points instead.
}
\usage{
quadratic.fit(x, y)
}
\arguments{
  \item{x}{A vector of length three, which represents the \eqn{x}-values of the three points.}
  \item{y}{A vector of length three, which represents the \eqn{y}-values of the three points.}
}
\value{
\code{quadratic.fit} returns a vector of length three. The first, second, and third elements of the returned vector are the second degree, first degree, and zero-th degree coefficients, respectively, of the fitted quadratic curve, or of the fitted linear regression line.
}
\author{
Qiansheng Zhu
}
\seealso{
\code{\link{solve_quadratic}}
}
\examples{
# Three points: (0, 1), (1, 0), (3, 4).
quadratic.fit(c(0, 1, 3), c(1, 0, 4))
}
\keyword{math}% use one of  RShowDoc("KEYWORDS")
