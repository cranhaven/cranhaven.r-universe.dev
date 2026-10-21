\name{rb}
\alias{rb}
\title{Creates a model using penalized smoothing splines}
\description{
  Creates a model using penalized smoothing splines using radial
  basis functions  
}
\usage{
rb(\dots, knots, degree, penalty)
}
\arguments{
  \item{\dots}{The explicative variables for which the spline is based
    on.}
  \item{knots}{The coordinates of knots. See section details.}
  \item{degree}{Numeric. The degree of the spline.}
  \item{penalty}{Numeric. The penalty coefficient.}
}
\value{
  A list giving all the required information to fit a penalized
  smoothing spline:
  \item{dsgn.mat}{The design matrix.}
  \item{pen.mat}{The penalization matrix.}
  \item{degree}{The degree of the smoothing spline.}
  \item{penalty}{The penalty of the smoothing spline.}
  \item{knots}{The knots of the smoothing spline.}
  \item{data}{The explicative variables (e.g. covariates).}
  \item{call}{How was the \code{\link{rb}} function was called?}
}
\section{Warning}{
This function is not supposed to be called directly. \code{\link{rb}} is
supposed to be embedded in a R formula.
}
\details{
  If one explicative variable is given in "\dots", the \code{knots}
  should be a numeric vector. Otherwise, \code{knots} should be a matrix
  with the same number of column and covariates.
}
\seealso{
  \code{\link{fitmaxstab}}
}
\examples{
n.site <- 30
locations <- matrix(runif(2*n.site, 0, 10), ncol = 2)
colnames(locations) <- c("lon", "lat")
knots <- quantile(locations[,2], 1:5/6)

form <- y ~ rb(lat, knots = knots, degree = 3, penalty = .5)
}
\author{Mathieu Ribatet}
\keyword{htest}
