\name{mass.FEM}
\alias{mass.FEM}
\title{
  Compute the mass matrix for a finite element basis.
}
\description{
  mass.FEM produces the mass matrix containing integrals of products of
  the nodal functions.
}
\usage{
  mass.FEM(FEMbasis)
}
\arguments{
  \item{FEMbasis}{A List object produced by function \code{create.FEM.basis}.
    It contains:
    \itemize{
        \item{order}{The order of the element (1 or 2).}
        \item{nodes}{Coordinates of node points.}
        \item{nodeindex}{Indices of node points for each element.}
        \item{jvec}{Jacobian of the affine transformation of each
                      element to the master element.}
    }
  }
}
\value{
  A matrix \code{k0}: the \code{nnod} by \code{nnod} matrix of sums
  of products of nodal basis functions.
  For each element i, the integral of the product
  of the j'th and k'th shape functions over the i'th element is
  computed.  Then that value is the 
  \code{(nodeindex[i,j],nodeindex[i,k])} entry of the i'th elemental 
  mass matrix.
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
  \code{\link{stiff.FEM}}
}
\keyword{mass.FEM}

