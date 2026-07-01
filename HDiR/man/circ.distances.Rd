\encoding{utf8}
\name{circ.distances}
\alias{circ.distances}

\title{Euclidean and Hausdorff distances between two sets of points on the unit circle}
\description{This function determines the Euclidean and Hausdorff distances between two sets of points on the unit circle.}
\usage{circ.distances(x, y)}
\arguments{
\item{x}{Numeric vector of angles in radians determining a set of points on the unit circle.}
  \item{y}{Numeric vector of angles in radians determining a set of points on the unit circle.}
}
\details{If \code{x} and \code{y} corresponds to two HDRs boundaries, this function returns the Euclidean and Hausdorff distances between the HDRs frontiers, but the function computes the Euclidean and Hausdorff distance for two sets of points on the circle, no matter their nature. See Saavedra-Nieves and Crujeiras (2021) for more details on these two distances.}
\value{
A list with two components:
\item{dE}{Euclidean distance.}
\item{dH }{Hausdorff distance.}
}
\references{Saavedra-Nieves, P. and Crujeiras, R. M. (2021). Nonparametric estimation of directional highest density regions. Advances in Data Analysis and Classification, 1-36.}
\author{Paula Saavedra-Nieves and Rosa M. Crujeiras.}

\examples{
# Distances between boundaries of two plug-in HDR estimators for orientations of saltator specie
data(sandhoppers)
attach(sandhoppers)
#Orientations in October
saltatorO<-angle[(species=="salt")&(time=="afternoon")&(sex=="M")&(month=="October")]
hdr1<-circ.plugin.hdr(sample=saltatorO,tau=0.8,plot.hdrconf=FALSE)$hdr
#Orientations in April
saltatorA<-angle[(species=="salt")&(time=="afternoon")&(sex=="M")&(month=="April")]
hdr2<-circ.plugin.hdr(sample=saltatorA,tau=0.8,plot.hdrconf=FALSE)$hdr
circ.distances(hdr1,hdr2)
}
