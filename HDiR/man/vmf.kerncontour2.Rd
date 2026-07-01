\name{vmf.kerncontour2}
\alias{vmf.kerncontour2}
 \title{
Von Mises-Fisher kernel density estimator with numerical smoothing parameter}
\description{
This function calculates the Von Mises-Fisher kernel density estimator is a slight modification of function vmf.kerncontour
in \code{Directional} package. In this new version, it is possible to provide a numerical value for the bandwidth.}
\usage{
vmf.kerncontour2(u, h, full = FALSE, ngrid = 100)
}
 \arguments{
  \item{u}{
A two column matrix. The first column is the latitude and the second is the longitude.}
  \item{h}{
Numeric value for bandwidth ot smoothing parameter.}
  \item{full}{
If \code{FALSE} (default), uses the range of positions from \code{u} to calculate and optionally plot densities. If \code{TRUE}, calculates densities covering the entire sphere.}
  \item{ngrid}{
Sets the resolution of the density calculation.}
}

\value{
A list with kernel density estimation. For more details, see function \code{vmf.kerncontour} in \code{Directional} package.
}

\author{
Paula Saavedra-Nieves and Rosa M. Crujeiras.
}
\keyword{internal}
