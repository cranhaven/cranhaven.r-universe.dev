\name{find.circ.hdr}
\alias{find.circ.hdr}
\title{
Calculation of the boundaries of HDRs in the circular setting
}
\description{
This function calculates the boundaries of HDRs in the circular setting.}
\usage{
find.circ.hdr(x, f, level)
}
\arguments{
  \item{x}{
Numeric vector of sorted angles in radians.}
  \item{f}{
Evaluation on a circular density \code{f} on \code{x}.}
  \item{level}{
Threshold of the HDR.}
}
\value{
A numeric vector of angles in radians corresponding to the frontiers of the HDR.
}

\author{
Paula Saavedra-Nieves and Rosa M. Crujeiras.
}
\keyword{internal}
