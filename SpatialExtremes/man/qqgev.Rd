\name{qqgev}
\alias{qqgev}
\title{QQ-plot for the GEV parameters}
\description{
  This function compares the GEV parameters estimated separately for
  each location to the ones estimated from a fitted spatial model.
}
\usage{
qqgev(fitted, xlab, ylab, ...)
}
\arguments{
  \item{fitted}{An object of class \code{maxstab} or
    \code{spatgev}. Most often, this will be the output of
    \code{\link{fitmaxstab}}, \code{\link{fitcovmat}},
    \code{\link{fitcovariance}} or \code{\link{fitspatgev}}.}
  \item{xlab,ylab}{The x and y-axis labels. May be missing.}
  \item{\dots}{Optional arguments to be passed to the \code{\link{plot}}
  function.}
}
\value{
  A QQ-plot.
}
\references{
  Schlather, M. (2002) Models for Stationary Max-Stable Random
  Fields. \emph{Extremes} \bold{5}:1,  33--44.
  
  Schlather, M. and Tawn, J. A. (2003) A dependence measure for
  multivariate and spatial extreme values: Properties and
  inference. Biometrika \bold{90}(1):139--156.

  Smith, R. L. (1990) Max-stable processes and spatial
  extremes. Unpublished manuscript.
}
\author{Mathieu Ribatet}
\seealso{\code{\link{qqextcoeff}}}
\examples{
##Define the coordinate of each location
n.site <- 30
locations <- matrix(runif(2*n.site, 0, 10), ncol = 2)
colnames(locations) <- c("lon", "lat")

##Simulate a max-stable process - with unit Frechet margins
data <- rmaxstab(50, locations, cov.mod = "gauss", cov11 = 100, cov12 =
25, cov22 = 220)

##Now define the spatial model for the GEV parameters
param.loc <- -10 + 2 * locations[,2]
param.scale <- 5 + 2 * locations[,1] + locations[,2]^2
param.shape <- rep(0.2, n.site)

##Transform the unit Frechet margins to GEV 
for (i in 1:n.site)
  data[,i] <- frech2gev(data[,i], param.loc[i], param.scale[i], param.shape[i])

##Define a model for the GEV margins to be fitted
##shape ~ 1 stands for the GEV shape parameter is constant
##over the region
loc.form <- loc ~ lat
scale.form <- scale ~ lon + I(lat^2)
shape.form <- shape ~ 1

fitted <- fitspatgev(data, locations, loc.form = loc.form, scale.form =
scale.form, shape.form = shape.form)

qqgev(fitted)
}
\keyword{hplot}
