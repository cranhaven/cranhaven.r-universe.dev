\name{extants}
\alias{extants}
\docType{data}
\title{Extant limb data from Campione and Evans 2012}
\description{Humeral and Femoral data for 245 living tetrapods, including mammals and reptiles}
\usage{data(extants)}
\format{
  A data frame with 245 observations and the following 13 variables.
  \describe{
    \item{\code{Mon.Groups}}{a character vector representing the inclusive clade}
    \item{\code{Family}}{a character vector representing the family level taxonomy}
    \item{\code{Species}}{a character vector identifying the mammal or reptile species}
    \item{\code{Common.Name}}{a character vector identifying the common species name}
    \item{\code{SP.}}{a character vector specifying the specimen number (see Campione and Evans 2012 for more details)}
    \item{\code{BM}}{a numeric vector of body masses (in g)}
    \item{\code{Humerus.Length}}{a numeric vector of humeral lengths (in mm)}
    \item{\code{HC}}{a numeric vector of humeral circumferences (in mm)}
    \item{\code{Femur.Length}}{a numeric vector of femoral lengths (in mm)}
    \item{\code{FC}}{a numeric vector of femoral circumferences (in mm)}
    \item{\code{HCFC}}{a numeric vector of combined humeral and femoral circumferences (in mm)}
    \item{\code{log.BM}}{a numeric vector of the base-10 logarithms of body masses}
    \item{\code{log.HCFC}}{a numeric vector of the base-10 logarithms of combined humeral and femoral circumferences}
  }
}
\source{
Campione, N. E. and Evans, D. C. (2012) A universal scaling relationship between body mass and proximal limb bone dimensions in quadrupedal terrestrial tetrapods. BMC Biology, 10, 60.
}