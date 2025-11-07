\name{create.FEM.basis}
\alias{create.FEM.basis}
\title{
  Create a FEM Basis with Triangular Finite Element Basis Functions
}
\description{
  Functional data objects are constructed by specifying a set of basis
  functions and a set of coefficients defining a linear combination of
  these basis functions.

  The FEM basis is used for functions defined over spatial regions with
  complicated boundaries.  There is an outer boundary outside of which
  no spatial location is found and there may be, in addition, one or more
  interior boundaries defining holes inside of which no data are found as well.
  For example, the outer boundary may define a geographical region, and an inner
  boundary can define a lake within which no data of interest are found.

  The interior of the region not within the holes is subdivided into a set of
  triangles by a mesh generation procedure. See function
  \code{create.mesh.object} for a description of this process.

  FEM basis functions are centered on points called nodes that are on a boundary
  or within the interior region not within holes at points called nodes.  Some
  or all of these nodes are also vertices of the triangles.  FEM basis functions
  are the two-dimensional analogues of B-spline basis functions defined over
  one dimension.  Like splines, each FEM basis function is zero everywhere
  except in the immediate neighborhood defined by the triangles that share a
  node, and the nodes play the role of knots for splines.

  Like splines, FEM functions are piecewise bivariate polynomials, with a loss
  of smoothness across edges of triangles.  Linear FEM functions are
  once-differentiable within triangles, and quadratic functions are
  twice-differentiable.  But both types of function are only continuous across
  edges.  The second example below will highlight these features.

  Function \code{create.FEM.basis} supports only linear or quadratic functions.
  All nodes for linear basis functions correspond to triangle vertices, and
  consequently there are three nodes per triangle.  For quadratic  basis
  functions, vertices are nodes, but mid-points of edges are also nodes, so that
  there are six nodes per triangle.
}
\usage{
create.FEM.basis(pts, edg=NULL, tri, order=1, nquad=0)
}
\arguments{
  \item{pts}{
    The \code{nbasis} by 2 matrix of vertices of triangles containing
    the X- and Y-coordinates of the vertices.
  }
  \item{edg}{
  	 The number of edges by 2 matrix defining the segments of the
     boundary of the region which are also edges of the triangles
     adjacent to the boundary.
     The values in matrix \code{edg} are the indices of the vertices
     in matrix \code{pts} of the starting and ending points of the edges.
  }
  \item{tri}{
  	The no. of triangles by 3 matrix specifying triangles and
    their properties.  The indices in \code{pts} of the vertices of each
    triangle are in counter-clockwise order.
  }
  \item{order}{
    The order of the finite element basis functions.  This may be either one
    or two.  Order one functions are piecewise linear, and order two functions
    are piecewise quadratic.
  }
  \item{nquad}{
     An integer determining the number of quadrature points and weights used to
     estimate the value of an integral if a function over a triangle  using
     Gaussian quadrature. The number of quadrature points and weights is equal
     to the square of \code{nquad}.  The \code{nquad} <- 4 usually provides
     sufficient accuracy for statistical purposes, but the default value of zero
     implies that no such integration will be needed.  Data smoothing does not
     require integration, but data density estimation does.
  }
}
\details{
  The mesh generation step is critical, and if done carelessly or naively, can
  easily cause the smoothing process to fail, usually due to a singular set
  of coefficient equations.  Careful attention to generating a mesh is required
  in order to obtain a mesh that is both well-conditioned and has sufficient
  density to permit a faithful rendition of the smoothing surface required by
  the data.

  Well-conditioned meshes do not have triangles with small angles, and the
  two mesh-generations functions can allow the user to specify the minimal
  angle in the mesh.

  On the other hand, the finer the mesh, the greater the number of basis
  functions, so that, as in all smoothing procedures, one wants to avoid
  meshes so fine that the possibly noisy observations are interpolated rather
  than smoothed.

  Triangles with no data in them are in general to be avoided.  In fact, it is
  common but not necessary that the mesh be constructed so that data observation
  points are at the vertices of the triangles in the mesh.  This special case
  also leads to very fast computation of the smoothing surface.  In some cases,
  it may make sense to interpolate data to prespecified mesh points before
  undertaking smoothing.

  Piecewise linear basis functions are generally preferable unless estimates
  of the second partial derivatives of the surface are required because
  quadratic elements require twice as many basis functions.  First order
  partial derivatives of surfaces are piecewise constant over each  triangle.
  Even though the estimated surface is piecewise linear, its total
  curvature is still controlled by the size of the smoothing parameter
  specified in function \code{smooth.FEM.basis}.  The larger the smoothing
  parameter, the more flat the surface will become.

}
\value{
  A \code{basisfd} object of the type \code{FEM}.  See the help file for
  \code{basisfd} for further details about this class.

  The \code{params} slot or member for an FEM basis contains a good deal of
  information that is used in other functions, such as \code{smooth.FEM.basis}.
  Consequently, \code{basisfd$params} is itself a list with named members, their
  contents are:
  \item{mesh}{
    an object of class \code{MESH} specifying the structure of the triangular
    mesh.  See the help file for this class for further details
  }
  \item{order}{
    either 1 for linear elements or 2 for quadratic elements
  }
  \item{nodeList}{
    a \code{list} object with named members.  The list contains information
    that is required for other functions that may be repeatedly called, such as
    \code{smooth.FEM.basis}.  The names and their contents are:
    \enumerate{
      \item{nodes}{a K by 2 matrix of coordinates for the K nodes in the mesh.}
      \item{nodesindex}{the index of each node}
      \item{J}{the Jacobian for the transformation of each triangle to the
      standard right triangle}
      \item{metric}{a three-dimensional array with the length of the leading
      dimension equal to the number of triangles, and the next two dimensions of
      length 2.  The 2 by 2 matrices are the transformation matrices for mapping
      each triangle into the standard triangle}
      \item{quadmat}{NULL if argument \code{nquad} is zero, or otherwise, a list
      with a member for each triangle containing the quadrature points and weights
      for approximating the integral of a function over that triangle.  See the
      help file for function \code{triquad} for further details.}
      \item{Cart2Bary}{A three-dimensional array with the leading dimension equal
      to the number of triangles, and the remaining dimensions of length 3.
      Each order 3 matrix maps the cartesian coordinate vector
    }
    \code{c(1, p(1), p(2))} into the corresponding  barycentric coordinates for
    a triangle, where \code{p} contains the Cartesian coordinates of a point.}
  }
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
  \code{\link{plotFEM.mesh}},
  \code{\link{smooth.FEM.basis}}
}
\examples{
#  ----------------------------------------------------------------------
#  Example 1:  Set up the simplest possible mesh by hand so as to see
#  the essential elements of a finite element basis
#  Specify the three vertices, in counter-clockwise order, for a single
#  right triangle
#  ----------------------------------------------------------------------
pts <- matrix(c(0, 0, 1, 0, 0, 1),3,2,byrow=TRUE)
#  These points also specify the boundary edges by a 3 by 2 matrix
#  with each row containing the index of the starting point and of the
#  corresponding ending point
edg <- matrix(c(1, 2, 2, 3, 3, 1), 3, 2, byrow=TRUE)
#  The triangles are defined by a 1 by 3 matrix specifying the indices
# of the vertices in counter-clockwise order
tri <- matrix(c(1, 2, 3), 1, 3)
#  FEM basis objects can be either linear (order 1) or quadratice (order 2)
#  both are illustrated here:
#  Set up a FEM basis object of order 1 (piecewise linear) basis functions
order <- 1
FEMbasis1 <- create.FEM.basis(pts, edg, tri, order)
#  display the number of basis functions
print(paste("Number of basis functions =",FEMbasis1$nbasis))
#  plot the basis, the boundary being plotted in red
plotFEM.mesh(pts,tri)
#  Set up an FEM basis object of order 2 (piecewise quadratic) basis functions
order <- 2
FEMbasis2 <- create.FEM.basis(pts, edg, tri, order)
#  display the number of basis functions
print(paste("Number of basis functions =",FEMbasis2$nbasis))
#  plot the basis, the boundary being plotted in red
plotFEM.mesh(pts,tri)
#  ----------------------------------------------------------------------
#  Example 2:  Set up an FEM object with a hexagonal boundary, and a single
#  interior point
#  ----------------------------------------------------------------------
angle <- seq(0,2*pi,len=7)
x     <- cos(angle); y <- sin(angle)
pts   <- rbind(cbind(x[1:6], y[1:6]), c(0, 0))
edg   <- cbind((1:6),c((2:6), 1))
tri   <- matrix(c(7*rep(1,6), 1:6, 2:6, 1),6,3)
hexFEMbasis <- create.FEM.basis(pts, edg, tri)
#  display the number of basis functions
print(paste("Number of basis functions =",hexFEMbasis$nbasis))
#  plot the basis, the boundary being plotted in red
plotFEM.mesh(pts,tri)
}
\keyword{create}
