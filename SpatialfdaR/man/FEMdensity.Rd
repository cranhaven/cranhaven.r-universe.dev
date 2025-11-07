\name{FEMdensity}
\alias{FEMdensity}
\title{
Evaluate the function value and gradient for a penalized likelihood estimate
of spatial density.
}
\description{
A spatial density defined over a region with complicated a boundary that is
defined by a triangulation is estimated.  The basis functions are linear
finite elements (FEM) defined at each vertex.
The data are the spatial coordinates of a sample of a defined spatial event.
The density surface is the logarithm of the density, and the smoothness of
the log surface is controlled by a smoothing parameter \code{lambda}
multiplying the stiffness matrix.}
\usage{
FEMdensity(cvec, obspts, FEMbasis, K1=NULL, lambda=0)
}
\arguments{
  \item{cvec}{A matrix each column of which contains the coefficients
  for an FEM basis function expansion of a surface.
  The number of coefficients equals the number of vertices in the
  triangulation, also called the nodes of the FEM expansion.}
  \item{obspts}{A two-column matrix containing the locations at which the
  logarithm of the density is to be evaluated.}
  \item{FEMbasis}{This argument provides the FEM basis (class \code{basisfd}),
  and may be in the form of an FEM basis object, an FEM function object
  (class \code{fd}), or an FEM functional parameter object
  (class \code{fdPar}.}
  \item{K1}{The stiffness matrix associated with the triangulation.
  It is computed using function \code{}.}
  \item{lambda}{A non-negative real number controlling the smoothness
  of the surface by a roughness penalty consisting of \code{lambda}
  multiplying the stiffness matrix \code{K1}.}
}
\details{
A probability density surface of a two-dimensional region bounded by line
segments is defined by a set of linear or planar triangular surfaces.
Each triangular segment is associated with three vertices, and with
with three basis functions, each associated with a local linear surface
that has height one at one of the vertices and height zero along
the two opposing edges.  The basis function expansion is actually of
the logarithm of the density surface, and has one set of two partial
derivatives everywhere except along edges and at vertices of the
triangles.  The surface is continuous over edges and vertices.  The
density surface is computed by dividing the exponential of the log
surface by the integral of the density over the region, which
is returned along with the coefficients of the expansion within
a list object.
Multiple density surfaces may be estimated, in which case each
column of argument \code{cvec} is associated with a surface.}
\value{
  \item{F}{The penalized log likelihood value associated with the coefficient
  vector(s).}
  \item{grad}{The gradient vector(s) associated with the coefficient
  vector(s).}
  \item{norm}{The L2 norm(s) of the gradient vector(s).}
  \item{Pvec}{A matrix, each column of which contains density function values
  at the nodes or vertices.}
  \item{Pden}{A vector of values of the normalizing constants defined for
  each surface defined by integrating the exponential of a log surface
  over the triangular region.}
}
\references{
  Sangalli, Laura M., Ramsay, James O., Ramsay, Timothy O. (2013),
    Spatial spline regression models,
    Journal of the Royal Statistical Society, Series B, 75, 681-703.
}
\author{
Jim Ramsay}
\seealso{
  \code{\link{basisfd}},
  \code{\link{smooth.FEM.density}}
}
\examples{
#  ---------  density on right triangle  --------------
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
#  List object containing details of nodes.
nodeList <- makenodes(pts,tri,1)
#  number of random points required
Nsample <- 100
#  generate random points ... define the function
obspts <- randomFEMpts(Nsample, pts, tri)
#  Define a starting value for the coefficient vector of length 3
cvec0 <- matrix(rnorm(npts),npts,1)
#  Evaluate the function and gradient vector
result <- FEMdensity(cvec0, obspts, RtTriBasis)
print(paste("Function value =",result$F))
print("gradient:")
print(result$grad)
#  ---------- density on a unit square,  4 triangles, 5 nodes  ----------
#  Generate a unit square with a node in its center defining four
#  triangles and five nodes.
result <- squareMesh(1)
pts <- result$p
edg <- result$e
tri <- result$t
#  plot the mesh
plotFEM.mesh(pts, tri)
npts <- dim(pts)[1]
ntri <- dim(tri)[1]
#  Define the true log density function
SinCosIntensFn <- function(x,y) {
  return(sin(2*pi*x)*cos(2*pi*y))
}
#  Compute a sine*cosine intensity surface.
intDensityVec <- triDensity(pts, tri, SinCosIntensFn)
#  Set up and plot an FEM basis object
SquareBasis1 <- create.FEM.basis(pts, edg, tri)
N <- 100
obspts <- randomFEMpts(N, pts, tri, intDensityVec)
#  plot the random points
points(obspts[,1],obspts[,2])
#  Estimate the smooth density surface with light smoothing
order    <- 1
nodeList <- makenodes(pts,tri,order)
K1       <- stiff.FEM(SquareBasis1)
lambda   <- 1e-4
#  define a random coefficient vector
cvec     <- rnorm(npts,1)
#  display function value and gradient
result <- FEMdensity(cvec, obspts, SquareBasis1, K1, lambda)
print(paste("Function value =",round(result$F,3)))
print("gradient:")
print(round(result$grad,3))
}
