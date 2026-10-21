\name{rbpspline}
\alias{rbpspline}
\title{Fits a penalized spline with radial basis functions to data}
\description{
  Fits a penalized spline with radial basis functions to data.
}
\usage{
rbpspline(y, x, knots, degree, penalty = "gcv", \dots)
}
\arguments{
  \item{y}{The response vector.}
  \item{x}{A vector/matrix giving the values of the predictor
    variable(s). If \code{x} is a matrix, each row corresponds to one
    observation.}
  \item{knots}{A vector givint the coordinates of the knots.}
  \item{degree}{The degree of the penalized smoothing spline.}
  \item{penalty}{A numeric giving the penalty coefficient for the
    penalization term. Alternatively, it could be either 'cv' or 'gcv'
    to choose the \code{penalty} using the (generalized)
    cross-validation criterion.} 
  \item{\dots}{Additional options to be passed to the \code{\link{cv}}
    or \code{\link{gcv}} function.}
}
\value{
  An object of class \code{pspline}. 
}
\details{
  The penalized spline with radial basis is defined by:

  \deqn{f(x) = \beta_0 + \beta_1 x + \ldots + \beta_{m-1} x^{m-1} +
    \sum_{k=0}^{K-1} \beta_{m+k} || x - \kappa_k ||^{2m - 1}}{f(x) =
    \beta_0 + \beta_1 x + \ldots + \beta_{m-1} x^{m-1} +
    \sum_{k=0}^{K-1} \beta_{m+k} || x - \kappa_k ||^{2m - 1}}
  where \eqn{\beta_i}{\beta_i} are the coefficients to be estimated,
    \eqn{\kappa_i}{\kappa_i} are the coordinates of the i-th knot and
    \eqn{m = \frac{d+1}{2}}{m = (d+1)/2} where \eqn{d}{d} corresponds to
    the \code{degree} of the spline.

    The fitting criterion is to minimize
    \deqn{||y - X\beta||^2 + \lambda^{2m-1} \beta^T K \beta}{||y - X
      \beta||^2 + \lambda^{2m-1} \beta^T K \beta}
    where \eqn{\lambda}{\lambda} is the penalty coefficient and
    \eqn{K}{K} the penalty matrix.  
}
\references{
  Ruppert, D. Wand, M.P. and Carrol, R.J. (2003) \emph{Semiparametric
    Regression} Cambridge Series in Statistical and Probabilistic
    Mathematics.
}
\seealso{
  \code{\link{cv}}, \code{\link{gcv}}
}
\examples{
n <- 200
x <- runif(n)
fun <- function(x) sin(3 * pi * x)
y <- fun(x) + rnorm(n, 0, sqrt(0.4))
knots <- quantile(x, prob = 1:(n/4) / (n/4 + 1))
fitted <- rbpspline(y, x, knots = knots, degree = 3)
fitted

plot(x, y)
lines(fitted, col = 2)
}
\author{Mathieu Ribatet}
\keyword{htest}