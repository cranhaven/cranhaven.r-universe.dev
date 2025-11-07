\name{smooth.FEM.density}
\alias{smooth.FEM.density}
\title{Compute a smooth FEM density surface of a triangulated region.}
\description{
A spatial density defined over a region with complicated a boundary that is
defined by a triangulation is estimated.  The basis functions are linear
finite elements (FEM) defined at each vertex.
The data are the spatial coordinates of a sample of a defined spatial event.
The density surface is the logarithm of the density, and the smoothness of
the log surface is controlled by a smoothing parameter \code{lambda}
multiplying the stiffness matrix.
}
\usage{
smooth.FEM.density(obspts, cvec, FEMbasis, K1=NULL, lambda=0,
                 conv=1e-4, iterlim=50, dbglev=FALSE)
}
\arguments{
  \item{obspts}{A two-column matrix with each row corresponding to a location within
  a two-dimensional region bounded by line segments and containing a triangular
  mesh.}
  \item{cvec}{A matrix each column of which contains the coefficients
  for an FEM basis function expansion of a surface.
  The number of coefficients equals the number of vertices in the
  triangulation, also called the nodes of the FEM expansion.}
  \item{FEMbasis}{This argument provides the FEM basis (class \code{basisfd}),
  and may be in the form of an FEM basis object, an FEM function object
  (class \code{fd}), or an FEM functional parameter object
  (class \code{fdPar}.}
  \item{K1}{The stiffness matrix associated with the triangulation.
  It is computed using function \code{stiff.FEM}.}
  \item{lambda}{A non-negative real number controlling the smoothness
  of the surface by a roughness penalty consisting of \code{lambda}.}
  \item{conv}{The convergence criterion using the function value.}
  \item{iterlim}{The maximum number of permitted iterations.}
  \item{dbglev}{print debugging output.}
}
\details{
The penalized log likelihood of a density surface is minimized with  respect to
the coefficient vector. with the initial value in \code{cvec}.
The spatial event locations are in the two-column matrix \code{pts}.
The FEM basis object is extracted from \code{IntensityfdPar}, which may
also be an FEM functional data object or an FEM basis object.
The roughness penalty, if required, is the stiff matrix \code{K1}
multiplied by the roughness parameter \code{lambda.}}
\value{
\item{cvec:}{The final coefficient vector or matrix.}
\item{Intensityfd:}{An FEM functional data object for the log density.}
\item{Flist:}{A list object with \code{F} the final penalized log likelihood
value, \code{grad} the final gradient and \code{norm} the final norm of the
gradient.}
\item{iterhist:}{A matrix with three columns displaying the iteration number,
the function value and the norm of the gradient vector for the initial
iteration and each subsequent iteration.}
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
  \code{link{FEMdensity}}
}
\examples{

#  ---------  Density on right triangle  --------------

#  Define the locations of the vertices of the right triangle
pts <- matrix(c(0,0, 1,0, 0,1), 3, 2, byrow=TRUE)
npts <- dim(pts)[1]
#  Specify the single triangle defined by these vertices.
#  The vertices are listed in counter-clockwise order
tri  <- matrix(c(1,2,3),1,3)
ntri <- dim(tri)[1]
#  Set up the FEM basis object for this region
RtTriBasis <- create.FEM.basis(pts, NULL, tri, 1)
plotFEM.mesh(pts, tri, RtTriBasis)
#  List object containing details of nodes.
nodeList <- makenodes(pts,tri,1)
K1 <- stiff.FEM(RtTriBasis)
#  Define the true log density, which is flat with height 0
ZeroFn <- function(x,y) {
  xdim <- dim(x)
  ZeroIntens <- matrix(0,xdim[1],xdim[1])
  return(ZeroIntens)
}
#  Compute of probabilities for each triangle of having a
#  location withinx it.
intDensityVec <- triDensity(pts, tri, ZeroFn)
#  number of random points required
N <- 100
#  generate random points ... define the function
obspts <- randomFEMpts(N, pts, tri, intDensityVec)
#  plot the random points
points(obspts[,1],obspts[,2])
#  Define a starting value for the coefficient vector of length 3
cvec <- matrix(0,npts,1)
#  Estimate the smooth density surface with no smoothing
densityList <- smooth.FEM.density(obspts, cvec, RtTriBasis, dbglev=2, iterlim=10)
#  The estimate of the coefficient vector
cvec <- densityList$cvec
#  Define the log density FEM functional data object
lnlamfd  <- fd(cvec,  RtTriBasis)
#  Plot the log density surface 
Xgrid <- seq(0,1,len=51)
Ygrid <- Xgrid
plotFEM.fd(lnlamfd, Xgrid, Ygrid)
#  Plot the log density surface
plotFEM.fd(lnlamfd, Xgrid, Ygrid)
#  Plot the log density surface using function contour
logdensitymat <- matrix(0,51,51)
for (i in 1:51) {
  for (j in 1:51) {
    logdensitymat[i,j] <- eval.FEM.fd(matrix(c(Xgrid[i],Ygrid[j]),1,2),lnlamfd)
  }
}
contour(Xgrid, Ygrid, logdensitymat)
#  Plot the density surface using function contour
densitymat <- matrix(0,51,51)
for (i in 1:51) {
  for (j in 1:51) {
    densitymat[i,j] <- exp(eval.FEM.fd(matrix(c(Xgrid[i],Ygrid[j]),1,2),lnlamfd))
  }
}
contour(Xgrid, Ygrid, densitymat)

#  ---------- density on a unit square,  4 triangles, 5 vertices  ----------

#  Generate a unit square with a node in its center defining four
#  triangles and five nodes.
result <- squareMesh(1)
pts    <- result$p
edg    <- result$e
tri    <- result$t
npts   <- dim(pts)[1]
ntri   <- dim(tri)[1]
#  Compute a sine*cosine intensity surface.
SinCosIntensFn <- function(x,y) {
  return(sin(2*pi*x)*cos(2*pi*y))
}
logDensityVec <- triDensity(pts, tri, SinCosIntensFn)
#  Set up and plot an FEM basis object
par(cex.lab=2)
SquareBasis1 <- create.FEM.basis(pts, edg, tri)
plotFEM.mesh(pts, tri)
#  generate random points
N = 100
obspts <- randomFEMpts(N, pts, tri, logDensityVec)
#  plot the random points
points(obspts[,1],obspts[,2])
#  Estimate the smooth density surface with light smoothing
order <- 1
nodeList <- makenodes(pts,tri,order)
K1 <- stiff.FEM(SquareBasis1)
lambda <- 1e-4
#  define a random coefficient vector
cvec  <- matrix(0,npts,1)
#  Estimate the smooth density surface with no smoothing
densityList <- smooth.FEM.density(obspts, cvec, SquareBasis1, dbglev=2, iterlim=10)
#  The estimate of the coefficient vector
cvec <- densityList$cvec
#  Define the log density FEM functional data object
lnlamfd  <- fd(cvec,  SquareBasis1)
#  Plot the log density surface 
Xgrid <- seq(0,1,len=51)
Ygrid <- Xgrid
plotFEM.fd(lnlamfd, Xgrid, Ygrid)
#  Plot the log density surface
plotFEM.fd(lnlamfd, Xgrid, Ygrid)
#  Plot the log density surface using function contour
logdensitymat <- matrix(0,51,51)
for (i in 1:51) {
  for (j in 1:51) {
    logdensitymat[i,j] <- eval.FEM.fd(matrix(c(Xgrid[i],Ygrid[j]),1,2),lnlamfd)
  }
}
contour(Xgrid, Ygrid, logdensitymat)
#  Plot the density surface using function contour
densitymat <- matrix(0,51,51)
for (i in 1:51) {
  for (j in 1:51) {
    densitymat[i,j] <- exp(eval.FEM.fd(matrix(c(Xgrid[i],Ygrid[j]),1,2),lnlamfd))
  }
}
contour(Xgrid, Ygrid, densitymat)
}



