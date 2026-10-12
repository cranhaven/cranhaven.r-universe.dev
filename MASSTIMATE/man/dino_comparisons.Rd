\name{dino_comparisons}
\alias{dino_comparisons}
\docType{data}
\title{
Reconstruction body masses and associated limb circumference data from Campione & Evans (2020)
}
\description{
Metadata combining volumetric-density reconstruction mass estimates with their associated limb circumference measurements.
}
\usage{data("dino_comparisons")}
\format{
  A data frame with 447 observations on the following 8 variables.
  \describe{
    \item{\code{Clade}}{a character vector specifying the clade to which the \code{Taxon} belongs.}
    \item{\code{Taxon}}{a character vector specifying the genus or species name.}
    \item{\code{Recon.Method}}{a character vector specifying the reconstruction method used to generate \code{Recon.BM}.}
    \item{\code{Recon.BM}}{a numeric vector of the reconstruction body mass, in g.}
    \item{\code{Gait}}{a character vector specifying whether the \code{Taxon} is bipedal or quadrupedal.}
    \item{\code{HCFC}}{a numeric vector of the combined humeral and femoral circumferences, in mm (if \code{Gait = "quad"}).}
    \item{\code{HC}}{a numeric vector of the humeral circumferences, in mm (if \code{Gait = "quad"}).}
    \item{\code{FC}}{a numeric vector of the femoral circumferences, in mm.}
  }
}
\source{
  Campione, N. E. and Evans, D. C. (2020) The accuracy and precisions of body mass estimation in non-avian dinosaurs. Biological Reviews.
}