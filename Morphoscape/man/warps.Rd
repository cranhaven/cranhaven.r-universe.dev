\name{warps}
\alias{warps}
\docType{data}
\title{
Simulated Shape Warps
}
\description{
Trait data for simulated shape warps of turtle humeri used to study the morphological evolution of turtles in Dickson and Pierce (2019). The morphospace was defined by a geometric morphometric analyises of 1028 psuedolandmarks on 40 turtle humeri. Hypothetical shape warps were then produced on a 4x6 grid accross morphospace. For each shape warp, four functional traits were measured, corresponding to a locomotory performance trait: stress under simulated load (strength), bone curvature (stride length), muscular mechanical advantage (mechanical advantage), and frontal area (hydrodynamics).
}
\usage{data("warps")}
\format{
  A data frame with 24 observations on the following 6 variables.
  \describe{
    \item{\code{x}}{the first axis of shape variation as determiend by a between-groups principal components analysis of the \code{\link{turtles}} dataset}
    \item{\code{y}}{the second axis of shape variation as determiend by a between-groups principal components analysis of the \code{\link{turtles}} dataset}
    \item{\code{hydro}}{hydrodynamics}
    \item{\code{curve}}{stride length}
    \item{\code{mech}}{mechanical advantage}
    \item{\code{fea}}{strength (assessed using finite element analysis)}
  }
}
\source{
Dickson, B.V. and Pierce, S.E. (2019), Functional performance of turtle humerus shape across an ecological adaptive landscape. Evolution, 73: 1265-1277. \doi{10.1111/evo.13747}
}
\keyword{datasets}
