\name{triDensity}
\alias{triDensity}
\title{
Compute the probabilities that a random location will be within one of the triangles of an
FEM mesh.
}
\description{
Given a function defining a log-density surface for an FEM density, compute the
probabilities that a random location will be within each of the triangles of an
FEM mesh.  This involves numerical quadrature over each rectangle, and
\code{nquad} is the order of approximation.
}
\usage{
triDensity(pts, tri, logIntensFn, nquad=5)
}
\arguments{
  \item{pts}{A two-column matrix with each row corresponding to a location within
  a two-dimensional region bounded by line segments and containing a triangular
  mesh.}
  \item{tri}{A three-column matrix indexing for each triangle its vertices.}
  \item{logIntensFn}{A function of \code{x} and \code{y} giving the surface height
  for an unnormalized log density function.  It must return a square matrix
  of order equal to \code{nquad}.  This implies that any multiplications
  must be pointwise.}
  \item{nquad}{The order of the quadrature over a triangle.  The default order
  of five is good for most applications.}
}
\value{
A vector of length equal to the number of triangles in a mesh containing the
probabilities that a random observation will fall within the triangles.}
\references{
  Sangalli, Laura M., Ramsay, James O., Ramsay, Timothy O. (2013),
    Spatial spline regression models,
    Journal of the Royal Statistical Society, Series B, 75, 681-703.
}
\author{
Jim Ramsay
}
\seealso{
  \code{\link{smooth.FEM.density}}
}
\examples{
#  ---------  right triangle mesh  with zero log density --------------
#  Define the locations of the vertices of the right triangle
pts  <- matrix(c(0,0, 1,0, 0,1), 3, 2, byrow=TRUE)
npts <- dim(pts)[1]
#  Specify the single triangle defined by these vertices.
#  The vertices are listed in counter-clockwise order
tri  <- matrix(c(1,2,3),1,3)
ntri <- dim(tri)[1]
#  Set up the FEM basis object for this region
edg <- NULL
RtTriBasis <- create.FEM.basis(pts, edg, tri, 1)
#  set up function to provide log intensity zero
ZeroFn <- function(x,y) {
  xdim <- dim(x)
  ZeroIntens <- matrix(0,xdim[1],xdim[1])
  return(ZeroIntens)
}
#  Define the true log density, which is flat with height 0
intDensityVec <- triDensity(pts, tri, ZeroFn)
#  ---------  square mesh with sin*cos log density  --------------
nsquare <- 3
result <- squareMesh(nsquare)
pts <- result$p
tri <- result$t
pts <- pts/nsquare
#  Set up and plot an FEM basis object
edge <- NULL
SquareBasis3 <- create.FEM.basis(pts, edg, tri)
plotFEM.mesh(pts, tri)
#  set up function to provide log intensity sine*cosine
SinCosIntensFn <- function(x,y) {
  scale <- 1
  SinCosIntens <- scale*sin(2*pi*x)*cos(2*pi*y)
  return(SinCosIntens)
}
#  Computation of probabilities for each triangle of having a
#  location within it.
intDensityVec <- triDensity(pts, tri, SinCosIntensFn)
#  Display triangle numbers with probabilities
print(cbind(matrix(1:ntri,ntri,1), round(intDensityVec,3)))
}
