\name{eval.FEM.fd}
\alias{eval.FEM.fd}
\title{Evaluate a functional data object with an FEM basis.}
\description{
A set of points is supplied, and the height of the surface is evaluated at these points.`
}
\usage{
eval.FEM.fd(pts, fdobj, nderivs=rep(0,2))
}
\arguments{
  \item{pts}{A two column matrix of pts within a triangular mesh.}
  \item{fdobj}{A functional data object with an FEM basis.}``1   
  \item{nderivs}{A vector of length 2 containing derivative orders with respect 
  to the X and Y coordinates of the triangular mesh.  The derivative orders are
  restricted to 0, 1 or 2, and 2 can only be used for a quadratic basis system.
  The default is both orders being 0.}
}
\value{
A matrix with number of rows equal to the number of rows of \code{pts}
containing the values of of one or more surfaces at these points.
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
  \code{\link{eval.FEM.basis}}
}
\examples{
#  ----------------------------------------------------------------------------
#  Example 1:  smoothing data over a circular region
#  ----------------------------------------------------------------------------
#  Set up a FEM object with a approximatedly circular boundary with 12 sides,
#  and two rings of 12 points plus a point at the centre.
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
plotFEM.mesh(pts, tri, shift = 0.02)
title("A 25-point curcular mesh")
#  Create an order 1 basis
hexFEMbasis <- create.FEM.basis(pts, edg, tri, 1)
#  locations of data
obspts <- pts
npts <- dim(obspts)[1]
points(obspts[,1], obspts[,2], col=2, cex=2, lwd=2)
hexfd = fd(matrix(1-c(obspts[,1]^2+obspts[,2]^2),npts,1),hexFEMbasis)
#  heights of a hemisphere at these locations
hexhgts <- round(eval.FEM.fd(obspts,hexfd),2)
}

