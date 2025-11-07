\name{insideIndex}
\alias{insideIndex}
\title{
  Index of the triangle containing a point.
}
\description{
  Evaluate the index of the triangle containing the point
  \code{(X,Y)} if such a triangle exists, and \code{NaN} otherwise.
}
\usage{
insideIndex(obspts, pts, tri, tricoef)
}
\arguments{
  \item{obspts}{A two-column matrix of observation location points.
  }
  \item{pts}{
    A two-column matrix of the locations of the vertices of the mesh.
  }
  \item{tri}{
    A three-column matrix of the indices in \code{pts} of the triangle vertices.
  }
  \item{tricoef}{Four-column matrix of coefficients for computing barycentric coordinates.}
}
\details{
  Each triangle is checked to see if the point is contained within it or on its boundary.  
  This is verified if all three of the barycentric coordinates are non-negative.
}
\value{
  A vector of integers indicating which triangle in \code{tri} contains a point.
}
\seealso{
  \code{\link{FEMdensity}},
  \code{\link{eval.FEM.basis}}
}
\examples{
#  ---------- density on a unit square,  4 triangles, 5 vertices  ----------
#  Generate a unit square with a node in its center defining four
#  triangles and five nodes.
result <- squareMesh(1)
pts  <- result$p
edg  <- result$e
tri  <- result$t
npts <- dim(pts)[1]
ntri <- dim(tri)[1]
#  define function for sine*cosine function
SinCosIntensFn <- function(x,y) {
  return(sin(2*pi*x)*cos(2*pi*y))
}
#  Compute a sine*cosine intensity surface.
intDensityVec <- triDensity(pts, tri, SinCosIntensFn)
#  Set up and plot an FEM basis object
SquareBasis1 <- create.FEM.basis(pts, edg, tri)
oldpar <- par(cex.lab=2)
plotFEM.mesh(pts, tri)
#  generate random points
N = 1000
obspts <- randomFEMpts(N, pts, tri, intDensityVec)
#  plot the random points
points(obspts[,1],obspts[,2])
#  find the triangle number containing each point
triIndex <- insideIndex(obspts, pts, tri)
par(oldpar)
}
\keyword{insideIndex}
