\name{eval.FEM.basis}
\alias{eval.FEM.basis}
\title{
  Values of a Finite Element Functional Data Object
}
\description{
  Evaluate a finite element (FEM) functional data object at specified locations,
  or evaluate a pair of partial derivatives of the functional object.
}
\usage{
eval.FEM.basis(obspts, FEMbasis, nderivs=rep(0,2))
}
\arguments{
  \item{obspts}{
    a two-column matrix of x- and Y-coordinate values at which the functional data
    object is to be evaluated.  
  }
  \item{FEMbasis}{
    an FEM functional basis object to be evaluated.
  }
  \item{nderivs}{
    a vector length 2 containing a pair of orders of derivatives.  If the FEM
    basis is piecewise linear  (of order 1), the only admissible pairs are
    (0,0), (0,1) and (1,0).
    If the FEM basis is piecewise quadratic (of order 2), the pairs (1,1),
    (0,2) and (2,0) are also admissible.
  }
}
\details{
  Two common uses of this function are (1) to evaluate the surface at one or
  more significant points, and (2) evaluate the surface over a bounding
  rectangle so that the surface can be plotted as either a contour plot are
  as a persp type plot.  Points that are outside of polygon defining the
  surface are given the value NA, and are therefore not plotted.
}
\value{
  an array of 2 or 3 dimensions containing the surface
  values.  The first two dimension correspond to the arguments \code{obspts} and,      
  if present, the third dimension corresponds to the surface
  in the situation where the coefficient matrix has multiple columns.
}
\seealso{
  \code{\link{eval.FEM.fd}},
  \code{\link{smooth.FEM.basis}}
}
\keyword{smooth}
