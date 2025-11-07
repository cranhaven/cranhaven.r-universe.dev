\name{randomFEMpts}
\alias{randomFEMpts}
\title{
  Generate Random Locations in a Mesh with a Specified Density 
}
\description{
  The probability that a random location is assigned to a triangle
  is defined by an input vector of probabilities, one per triangle.
  But if DensityVec = 1, any location within the mesh is accepted.
}
\usage{
  randomFEMpts(Nsample, pts, tri, logDensityVec=rep(0,ntri))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Nsample}{The number of random locations to be generated.}
  \item{pts}{A two-column matrix of locations of nodes.}
  \item{tri}{A three-column matrix of integers specifying the
  vertices of triangles in anti-clockwise order.}
  \item{logDensityVec}{A one-column matrix of log probability density values, 
  one for each triangle.}
}
\details{
  Within triangles, the locations are uniformly distributed.
  If DensityVec = 1, they are uniformly distributed over the whole mesh.
}
\value{
  A two-column matrix with \code{N} rows specifying the locations
  of the random points.
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
  \code{link{plotFEM.mesh}}
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
#  plot the random points
plotFEM.mesh(pts, tri)
points(obspts[,1],obspts[,2])
#  -----------------  density on a circle  ---------------
angle = seq(0,11*pi/6,len=12)
#  define the 24 point locations
rad = 2
ctr = c(0,0)
pts  = matrix(0,25,2);
pts[ 1:12,1] = rad*cos(angle)   + ctr[1]
pts[ 1:12,2] = rad*sin(angle)   + ctr[2]
pts[13:24,1] = rad*cos(angle)/2 + ctr[1]
pts[13:24,2] = rad*sin(angle)/2 + ctr[2]
#  define the edge indices
edg <- matrix(0,12,2)
for (i in 1:11) {
  edg[i,1] <- i
  edg[i,2] <- i+1
}
edg[12,] <- c(12,1)
#  use delaunay triangulation to define the triangles
tri = delaunayn(pts)
#  plot the triangulation of the circle
plotFEM.mesh(pts, tri, xlim=c(-2,2), ylim=c(-2,2))
#  Define the true log density function
SinCosIntensFn <- function(x,y) {
  return(sin(pi*x/2)*cos(pi*y/2))
}
#  locate observation points with sin*cos log density
intDensityVec <- triDensity(pts, tri, SinCosIntensFn)
#  number of random points required
Nsample <- 100
#  generate random points ... define the function
obspts <- randomFEMpts(Nsample, pts, tri, intDensityVec)
points(obspts[,1], obspts[,2], pch="o")
}
