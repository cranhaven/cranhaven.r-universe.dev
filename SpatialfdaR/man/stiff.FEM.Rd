\name{stiff.FEM}
\alias{stiff.FEM}
\title{
  Compute the stiffness matrix for a finite element basis.
}
\description{
  stiff.FEM produces the stiffness matrix containing integrals of products of
  second partial derivatives of the nodal functions.
}
\usage{
  stiff.FEM(FEMbasis)
}
\arguments{
  \item{FEMbasis}{A List object produced by function \code{create.FEM.basis}.
    It contains:
    \itemize{
        \item{order}{The order of the element (1 or 2).}
        \item{nodes}{Coordinates of node points.}
        \item{nodeindex}{Indices of node points for each element.}
        \item{jvec}{jacobian of the affine transformation of each
                      element to the master element.}
    }
  }
}
\value{
  A matrix \code{K0}: the \code{NNOD} by \code{NNOD} matrix of sums
  of products of nodal basis functions.
  For each element i, the integral of the product
  of the j'th and k'th second partial derivatives of the
  shape functions over the i'th element is computed.  Then that value 
  is the \code{(nodeindex[i,j],nodeindex[i,k])}th entry of the i'th 
  elemental stiff matrix.
}
\references{
  Sangalli, Laura M., Ramsay, James O., Ramsay, Timothy O. (2013),
    Spatial spline regression models,
    Journal of the Royal Statistical Society, Series B, 75, 681-703.
}
\author{
Jim Ramsay
}
\seealso{
  \code{\link{mass.FEM}}
}
\keyword{stiff.FEM}
