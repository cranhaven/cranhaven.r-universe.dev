\name{estimateAnisotropy}
\alias{estimateAnisotropy}

\title{estimateAnisotropy}

\description{ This function estimates geometric anisotropy parameters for 2-D scattered data using the 
CTI method.}

\usage{estimateAnisotropy(object,depVar, formulaString)}

\arguments{
\item{object}{ (i) An Intamap type  object (see \code{\link{intamap-package}}) containing one 
\code{\link[sp:SpatialPoints]{SpatialPointsDataFrame}} data frame named 
 \code{observations} which includes the observed values (ii) or a 
 \code{\link[sp:SpatialPixelsDataFrame]{SpatialPointsDataFrame}} which includes both coordinates and observations.}

\item{depVar}{name of the dependent variable; this is  used only in case (ii).}
\item{formulaString}{formula that defines the dependent variable as a linear model 
      of independent variables, only used for case (ii); suppose the dependent variable has name \code{z}, 
      for ordinary and simple kriging use the formula \code{z~1}; 
      for universal kriging, suppose \code{z} is linearly dependent on 
      \code{x} and \code{y}, use the formula \code{z~x+y}. The formulaString defaults
      to \code{"value~1"} if \code{value} is a part of the data set. 
      If not, the first column of the data set is used.}
} 

\details{
Given the input object that defines  N  coordinate pairs (x,y) and observed values (z), 
this method estimates of the geometric anisotropy parameters.  
Geometric anisotropy is a statistical property, which implies that the 
iso-level contours of the covariance function are elliptical. In this case the anisotropy is determined from 
 the anisotropic ratio (R) and the orientation angle (\eqn{\theta}) of the ellipse. 

Assuming a Cartesian coordinate system of axes x and y, \eqn{\theta} represents the angle 
between the horizontal axis and PA1, where PA1 is one of the principal axes  of the ellipse, 
arbitrarily selected (PA2 will denote the other axis). R represents the ratio of the correlation along PA1 divided by
the correlation length PA2. Note that the returned value of R is always greater than one (see \code{value} below.)

The estimation is based on the Covariance Tensor Identity (CTI) method. In CTI, the 
Hessian matrix of the covariance function is estimated from sample derivatives. The 
anisotropy parameters are estimated by explicit solutions of nonlinear equations that link
(R,\eqn{\theta}) with ratios of the covariance Hessian matrix elements. 

To estimate the sample derivatives from scattered data, a background square lattice is used.
The lattice extends in the horizontal direction from x.min to x.max where x.min (x.max) is equal 
to the minimum (maximum) x-coordinate of the data, and similarly in the vertical direction.
The cell step in each direction is equal to the length of the lattice to the respective direction 
divided by the square root of N.

BiLinear interpolation, as implemented in \code{akima} package, is used to interpolate the 
field's z  values at the nodes of the lattice.

The CTI method is described in detail in (Chorti and Hristopulos, 2008). 

Note that to be compatible with \code{gstat} the returned estimate of the anisotropy ratio is always
greater than 1. 

For observations assumed to have a trend, the trend is first subtracted from the data using
universal kriging. This is an approximation, as the trend subtraction does not 
take anisotropy into account.

}
\value{(i) If the input is an Intamap object, the value is a modification of the input object, 
containing a list element \code{anisPar} with  the estimated anisotropy parameters.
(ii)if the input is a \code{\link[sp:SpatialPixelsDataFrame]{SpatialPointsDataFrame}}, then only the list \code{anisPar} is returned.
The list \code{anisPar} contains the following elements:

\item{ratio}{The estimate of the anisotropy ratio parameter. Using the degeneracy of the anisotropy under simultaneous
ratio inversion and axis rotation transformations, the returned value of the ratio is always greater than 1.}
\item{direction}{The estimate of the anisotropy orientation angle. It returns the angle between the major 
anisotropy axis and the horizontal axis, and its value is in the interval (-90,90) degrees.}
\item{Q}{A 3x1 array containing the sample estimates of the diagonal and off-diagonal elements (Q11,Q22,Q12) of the  
covariance Hessian matrix evaluated at zero lag.}
\item{doRotation}{Boolean value indicating if the estimated anisotropy is statistically significant. This value is based on
a statistical test of the isotropic (R= 1) hypothesis using a non-parametric approximation for the 95 percent confidence 
interval for R. This approximation leads to conservative (wider than the true) estimates of the confidence interval. 
If doRotation==TRUE then an isotropy restoring transformation (rotation and rescaling) is performed on the coordinates. 
If doRotation==FALSE no action is taken. 
}
}
\references{
[1] Pebesma, E., Cornford, D., Dubois, G., Heuvelink, G.B.M., Hristopulos, D., Pilz, J., Stohlker, U., Morin, G., Skoien, J.O. INTAMAP: The design and implementation f an interoperable automated interpolation Web Service. Computers and Geosciences 37 (3), 2011. 

[2] A. Chorti and D. T. Hristopulos (2008). Non-parametric Identification of 
Anisotropic (Elliptic) Correlations in Spatially Distributed Data Sets, 
IEEE Transactions on Signal Processing, 56(10), 4738-4751 (2008).

[3] Em.Petrakis and D. T. Hristopulos (2009). A non-parametric test of statistical isotropy for Differentiable 
Spatial Random Fields in Two Dimensions.
Work in progress. email: dionisi@mred.tuc.gr
}
\author{ A.Chorti, D.T.Hristopulos,G. Spiliopoulos}

\note{ This function uses \code{akima} package to perform  "bilinear" interpolation. The source code also allows 
other interpolation methods, but this option is not available when the function is called from within INTAMAP.

In the \code{gstat} package, the anisotropy ratio is defined in the interval (0,1) and the orientation 
angle is the angle between the 
vertical axis and the major anisotropy axis, measured in the clockwise direction. 
If one wants to use ordinary kriging inside INTAMAP 
the necessary transformations are performed in the function \code{estimateParameters.automap}. 
If one wants to use ordinary kriging 
in the \code{gstat} package (but outside INTAMAP) the required transformations can be found in the 
source code of the \code{estimateParameters.automap}
function.

}
\examples{
    library(gstat)
    data(sic2004)
    coordinates(sic.val)=~x+y
    sic.val$value=sic.val$dayx

    params=NULL
    
    estimateAnisotropy(sic.val,depVar = "joker")

}
\keyword{spatial}
\keyword{nonparametric}
\keyword{htest}
