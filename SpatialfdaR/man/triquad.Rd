\name{triquad}
\alias{triquad}
\title{
  Set up Gaussian quadrature points and weights for a triangular domain.
}
\description{
  The integral of a function over a triangle is approximated by a weighted sum
  of the values of the function at a set of points.
}
\usage{
  triquad(nquad, v)
}
\arguments{
  \item{nquad}{The number of quadrature points and weights is $N^2$.}
  \item{v}{A matrix with three rows and two columns containing the locations
  of the vertices of the triangle.
  }
}
\details{
    Gaussian quadrature approximates an integral of a function $f$ over a
    triangle by a weighted sum of $N^2$ values of $f$ at specified points
    within the triangle.  The larger \code{N}, the more accurate the
    approximation, but course the longer it takes to compute.  For many
    purposes, including  most uses of finite element methods, the accuracy
    does not have to be great, and \code{N=5} may well suffice
}
\value{A list object containing these named fields:
  \item{X}{The X-coordinates of the quadrature points.}
  \item{Y}{The Y-coordinates of the quadrature points.}
  \item{Wx}{The weights for the X-coordinates of the quadrature points.}
  \item{Wy}{The weights for the Y-coordinates of the quadrature points.}
}
\references{
  Sangalli, Laura M., Ramsay, James O., Ramsay, Timothy O. (2013),
    Spatial spline regression models,
    \emph{Journal of the Royal Statistical Society, Series B}, 75, 681-703.
}
\author{
  J. O. Ramsay
}
