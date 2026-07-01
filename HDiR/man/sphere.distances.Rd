\encoding{utf8}
\name{sphere.distances}
\alias{sphere.distances}
 \title{
Euclidean and Hausdorff distances between two sets of points on the unit sphere
}
\description{
This function determines the Euclidean and Hausdorff distances between two sets of points on the unit sphere.
}
\usage{
sphere.distances(x, y)
}
 \arguments{
  \item{x}{
A matrix whose rows represent points on the unit sphere in Cartesian coordinates. If a row norm is different from one, a message appears indicating that they must be standardized.
}
  \item{y}{
A matrix whose rows represent points on the unit sphere in Cartesian coordinates. If a row norm is different from one, a message appears indicating that they must be standardized.
}
}
\details{
If \code{x} and \code{y} correspond to two HDRs boundaries, this function returns the Euclidean and Hausdorff distances between the HDR frontiers, but the function computes the Euclidean and Hausdorff distance for two sets of points on the sphere, no matter their nature. See Saavedra-Nieves and Crujeiras (2021) for more details on these two distances.
}
\value{
A list with two components:
\item{dE}{Euclidean distance.}
\item{dH }{Hausdorff distance.}
}
\references{
Saavedra-Nieves, P. and Crujeiras, R. M. (2021). Nonparametric estimation of directional highest density regions. Advances in Data Analysis and Classification, 1-36.}
\author{
Paula Saavedra-Nieves and Rosa M. Crujeiras.
}

\examples{
\donttest{
# Distances between boundaries of two plug-in HDR estimators for spherical model 9 in HDiR package
set.seed(1)
sample=rspheremix(1000, model =9)
x<-sphere.plugin.hdr(sample,tau=0.8,plot.hdr=FALSE)$hdr
y<-sphere.plugin.hdr(sample,tau=0.5,plot.hdr=FALSE)$hdr
sphere.distances(x, y)
}
}
