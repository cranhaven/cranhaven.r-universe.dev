\name{profile2d}
\alias{profile2d}
\alias{profile2d.maxstab}
\title{Method for profiling (in 2d) fitted max-stable objects}
\description{Computes profile surfaces for fitted max-stable models.
}
\usage{
\method{profile2d}{maxstab}(fitted, params, ranges, n = 10, plot = TRUE,
\dots)
}
\arguments{
  \item{fitted}{An object of class ``maxstab''. Most often, it will be
    the output of the function \code{\link{fitmaxstab}}.}
  \item{params}{A character vector giving the two model parameters that
    are to be profiled.}
  \item{ranges}{A matrix corresponding to the ranges for the profiled
    model parameters that must be explored. Each row corresponds to
    one model parameter range.}
  \item{n}{Integer. The number of profiled model parameter that must be
    considered.}
  \item{plot}{Logical. If \code{TRUE} (default), the profile surface is
    plotted.}
  \item{\dots}{Extra options that must be passed to the
    \code{\link{plot}} function.}
}
\section{Warnings}{
  This function can be \bold{really} time consuming!
}
\value{
  A list with two arguments: \code{coord} and \code{llik}. \code{coord}
  is a matrix representing the grid where the profiled model parameters
  are fixed. \code{llik} the corresponding pairwise log-likelihood.
}
\examples{
\dontrun{
##Define the coordinates of each location
n.site <- 30
locations <- matrix(rnorm(2*n.site, sd = sqrt(.2)), ncol = 2)
colnames(locations) <- c("lon", "lat")

##Simulate a max-stable process - with unit Frechet margins
data <- rmaxstab(30, locations, cov.mod = "whitmat", nugget = 0, range = 30,
smooth = 0.5)

##Now define the spatial model for the GEV parameters
param.loc <- -10 + 2 * locations[,2]
param.scale <- 5 + 2 * locations[,1] + locations[,2]^2
param.shape <- rep(0.2, n.site)

##Transform the unit Frechet margins to GEV
for (i in 1:n.site)
  data[,i] <- frech2gev(data[,i], param.loc[i], param.scale[i],
param.shape[i]) 

##Define a model for the GEV margins to be fitted
##shape ~ 1 stands for the GEV shape parameter is constant
##over the region
loc.form <- loc ~ lat
scale.form <- scale ~ lon + (lat^2)
shape.form <- shape ~ 1

##Fit a max-stable process
##  1- using the Schlather representation
fitted <- fitmaxstab(data, locations, "whitmat", loc.form, scale.form,
                     shape.form)

##Plot the profile pairwise log-likelihood for the smooth parameter
ranges <- rbind(c(9,11), c(.3, .8))
profile2d(fitted, c("range", "smooth"), ranges = ranges)
}
}
\author{Mathieu Ribatet}
\keyword{models}