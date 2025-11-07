\name{smooth.FEM.basis}
\alias{smooth.FEM.basis}
\title{
  Construct a functional data object by smoothing spatial data distributed over
  a region with a complicated boundary using a roughness penalty.
}
\description{
  Discrete observations of a surface are fit with a smooth  surface defined by
  an expansion in a set of FEM type basis functions.  The fitting criterion is
  weighted least squares, and smoothness can be defined in terms of a roughness
  penalty.

  Data smoothing requires at a bare minimum three elements: (1) a set of
  observed noisy values, (b) a set of argument values associated with these
  data, and (c) a basis function system used to define the surfaces.

  Optionally, a set covariates may be also used to take account of
  various non-smooth contributions to the data.  Smoothing without
  covariates is often called nonparametric regression, and with
  covariates is termed semiparametric regression.
}
\usage{
  smooth.FEM.basis(FEMloc, FEMdata, FEMbasis, lambda=1e-12, wtvec=NULL,
                  covariates=NULL, Laplace=NULL)
}
\arguments{
  \item{FEMloc}{
    A matrix with two columns containing the coordinates of the points
    where the data are observed.
  }
  \item{FEMdata}{If a single surface, column vector containing the values to be 
  smoothed.  Otherwise, a matrix of observation values that is number of pts by    
  number of surfaces.}
  \item{FEMbasis}{
    Either (1) a functional basis object of the \code{FEM} type if the first
    argument contains the X-coordinates, or, if the first argument is a matrix
    with three columns, an \code{N} by \code{q} matrix of covariate values,
    where \code{N} is the number of observations and \code{q} is the number of
    covariates.
  }
  \item{lambda}{
    Either (1) a nonnegative smoothing parameter value that controls the amount
    of penalty on the curvature of the fitted surface, or (2) a vector of
    weights for the observations.  The default value for \code{lambda} is
    1e-12, a value too small to noticeably affect the curvature of the fitted
    surface, but large enough to ensure nonsingularity in the equations
    defining the coefficients of the basis function expansion.
  }
  \item{wtvec}{
    a vector of length \code{N} that is the length of \code{Xvec}
    containing weights for the values to be smoothed, However, it may also
    be a symmetric matrix of order \code{n}.   \code{wt} defaults to all weights
    equal to 1.
  }
  \item{covariates}{
    An \code{N} by \code{q} matrix of covariate values, one for each covariate
    and each observed value.  By default this argument is NULL and no covariates
    are used to define the surface.
  }
  \item{Laplace}{If TRUE, the Laplacian surface is computed, otherwise not.}
}
\details{
  The surface fitted to the data by \code{smooth.FEM.basis} is an expansion
  in terms of finite element basis functions defined by a triangular mesh of
  points defining the region over which the smooth is defined.  The mesh also
  determines the location and shape of the basis functions.  The mesh therefore
  plays a role that is like that of knots for B-spline basis functions, but
  is perhaps even more critical to the success of the smoothing process.

  Designing and refining the mesh is therefore a preliminary step in finite
  element smoothing, and requires considerable care and effort.  Consult the
  help pages for \code{MESH} and \code{create.FEM.basis} for more details.

  A roughness penalty is a quantitative measure of the roughness of a
  surface that is designed to fit the data.  For this function, this penalty
  consists of the product of two parts.  The first is an approximate integral
  over the argument range of the square of a derivative of the surface.  A
  typical choice of derivative order is 2, whose square is often called
  the local curvature of the function.  Since a rough function has high
  curvature over most of the function's range, the integrated square of
  of the second derivative quantifies the total curvature of the function,
  and hence its roughness.  The second factor is a positive constant called
  the bandwidth of smoothing parameter, and given the variable name
  \code{lambda} here.

  The roughness penalty is added to the weighted error sum of squares
  and the composite is minimized, usually in conjunction with a
  high dimensional basis expansion such as a spline function defined by
  placing a knot at every observation point.  Consequently, the
  smoothing parameter controls the relative emphasis placed on fitting
  the data versus smoothness; when large, the fitted surface is more smooth,
  but the data fit worse, and when small, the fitted surface is more rough,
  but the data fit much better. Typically smoothing parameter \code{lambda}
  is manipulated on a logarithmic scale by, for example, defining it
  as a power of 10.

  A good compromise \code{lambda} value can be difficult to
  define, and minimizing the generalized cross-validation or "gcv"
  criterion that is output by \code{smooth.FEM.basis} is a popular strategy
  for making this choice, although by no means foolproof.  One may also
  explore \code{lambda} values for a few log units up and down from this
  minimizing value to see what the smoothing function and its derivatives
  look like.  The function \code{plotfit.fd} was designed for this purpose.

  The size of common logarithm of the minimizing value of \code{lambda}
  can vary widely, and spline functions depends critically on the typical
  spacing between knots.  While there is typically a "natural" measurement
  scale for the argument, such as time in milliseconds, seconds, and so
  forth, it is better from a computational perspective to choose an
  argument scaling that gives knot spacings not too different from one.
}
\value{
  an object of class \code{fdSmooth}, which is a named list of length 8
  with the following components:

  \item{fd}{
    an FEM functional data object containing a smooth of the data
  }
  \item{df}{
    a degrees of freedom measure of the smooth
  }
  \item{gcv}{
    the value of the generalized cross-validation or GCV criterion.  If
    there are multiple surfaces, this is a vector of values, one per
    surface.  If the smooth is multivariate, the result is a matrix of gcv
    values, with columns corresponding to variables.

    \deqn{gcv = n*SSE/((n-df)^2)}
  }
  \item{beta}{
    the regression coefficients associated with covariate variables.
    These are vector, matrix or array objects depending on whether
    there is a single surface, multiple surfaces or multiple surfaces and
    variables, respectively.
  }
  \item{SSE}{
    the error sums of squares.  SSE is a vector or a matrix of the same
    size as GCV.
  }
  \item{penmat}{
    the penalty matrix
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
  \code{\link{create.FEM.basis}},
  \code{\link{lambda2df}},
  \code{\link{lambda2gcv}},
  \code{\link{df2lambda}},
  \code{\link{plotFEM.fd}},
   \code{\link{plotFEM.mesh}},
 \code{\link{smooth.FEM.basis}}
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
pts  = matrix(0,25,2)
pts[ 1:12,1] = rad*cos(angle)   + ctr[1]
pts[ 1:12,2] = rad*sin(angle)   + ctr[2]
pts[13:24,1] = rad*cos(angle)/2 + ctr[1]
pts[13:24,2] = rad*sin(angle)/2 + ctr[2]
npts = nrow(pts)
#  define the edge indices
edg <- matrix(0,12,2)
for (i in 1:11) {
  edg[i,1] <- i
  edg[i,2] <- i+1
}
edg[12,] <- c(12,1)
#  use delaunay triangulation to define the triangles
#  These geometry and RTriangle packages give different but legitimate answers
tri_GM <- geometry::delaunayn(pts)
plotFEM.mesh(pts, tri_GM, nonum=FALSE, shift = 0.1)
FEMbasis_GM <- create.FEM.basis(pts, edg, tri_GM, 1)
ntri <- nrow(tri_GM)
#  plot the two meshes
plotFEM.mesh(pts, tri_GM, nonum=FALSE, shift = 0.1)
title("A 25-point circular mesh from geometry")
#  set up the FEM basis objects
FEMbasis_GM <- create.FEM.basis(pts, edg, tri_GM, 1)
#  locations of locations and data  (kept same for both triangulations)
nobs <- 201
FEMloc <- randomFEMpts(nobs, pts, tri_GM)
#  heights of a hemisphere at these locations (kept same for both triangulations)
sig <- 0.2
FEMdata <- sqrt(4 - FEMloc[,1]^2 - FEMloc[,2]^2) + 
  matrix(rnorm(nobs),nobs,1)*sig
#  Smooth the data 
FEMList_GM <- smooth.FEM.basis(FEMloc, FEMdata, FEMbasis_GM, lambda=1)
FEMfd_GM <- FEMList_GM$fd
round(FEMList_GM$SSE,3)
round(FEMList_GM$df,3)
Xgrid = seq(-2,2,len=21)
Ygrid = seq(-2,2,len=21)
#  plot the result
plotFEM.fd(FEMfd_GM, Xgrid, Ygrid, 
          main="A hemisphere over a 25-point circle")
}
