\name{SpatialExtremes}
\docType{package}
\alias{SpatialExtremes}
\title{Analysis of Spatial Extremes}
\description{
  The package \pkg{SpatialExtremes} aims to provide tools for the
  analysis of spatial extremes. Currently, the package uses the
  max-stable processes framework for the modelling of spatial extremes.

  Max-stable processes are the extension of the extreme value theory to
  random fields. Consequently, they are good candidate to the analysis
  of spatial extremes. The strategy used in this package is to fit
  max-stable processes to data using composite likelihood.

  In the future, the package will allow for non-stationarity as well as
  other approaches to model spatial extremes; namely latent variable and
  copula based approaches.

  A package vignette has been writen to help new users. It can be
  viewed, from the R console, by invoking
  \command{vignette("SpatialExtremesGuide")}.
}
\details{
  The package provides the following main tools:
  \enumerate{
    \item \code{\link{rgp}, \link{rmaxstab}, \link{rmaxlin},
      \link{rcopula}}: simulates gaussian, max-stable, max-linear and copula
    based random fields,
    \item \code{\link{condrgp}, \link{condrmaxlin}}: conditional simulations for
    gaussian, max-linear processes,
    \item \code{\link{fitspatgev}}: fits a spatial GEV model to data,
    \item \code{\link{fitmaxstab}}, \code{\link{lsmaxstab}}: fits
    max-stable processes to data,
    \item \code{\link{latent}}: draws a Markov chain from a Bayesian
    hierarchical model for spatial extremes,
    \item \code{\link[=predict.maxstab]{predict}}: allows predictions
    for fitted max-stable processes,
    \item \code{\link{map}}, \code{\link{condmap}}: plot a map for GEV
    parameter as well as return levels - or conditional return levels
    \item \code{\link[=anova.maxstab]{anova}}, \code{\link{TIC}},
  \code{\link{DIC}}: help users in model selection,
    \item \code{\link{madogram}}, \code{\link{fmadogram}},
  \code{\link{lmadogram}}: are (kind of) variograms devoted to extremes,
  \item \code{\link{fitextcoeff}}: estimates semi-parametrically the
  extremal coefficient,
    \item \code{\link{extcoeff}}: plots the evolution of the extremal
    coefficient from a fitted max-stable process,
    \item \code{\link{rbpspline}}: fits a penalized spline with radial
    basis function,
    \item \code{\link{gev2frech}}, \code{\link{frech2gev}}: transform
    GEV (resp. Frechet) observation to unit Frechet (resp. GEV) ones
    \item \code{\link{gevmle}}, \code{\link{gpdmle}}: fit the GEV/GPD
    distributions to data,
    \item \code{\link{distance}}: computes the distance between each
    pair of locations,
    \item \code{\link[=profile.maxstab]{profile}},
    \code{\link[=profile2d.maxstab]{profile2d}}: computes the profile
    composite likelihood,
    \item \code{\link{covariance}}, \code{\link{variogram}}: computes
  the covariance/semivariogram function.
  }
}
\section{Acknowledgement}{
  The development of the package has been financially supported by the
  Competence Center Environment and Sustainability (CCES) and more
  precisely within the EXTREMES project.
}
\author{Mathieu Ribatet}
\keyword{spatial}
