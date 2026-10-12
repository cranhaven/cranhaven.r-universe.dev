\name{dinosbip}
\alias{dinosbip}
\docType{data}
\title{Dinosaur data from Campione et al. (2014)}
\description{Femoral circumference data for 34 bipedal dinosaurs}
\usage{data(dinosbip)}
\format{
  A data frame with 34 observations and the following 3 variables.
  \describe{
    \item{\code{Taxon}}{a factor with the taxon names}
    \item{\code{FC}}{a numeric vector of femoral circumferences}
    \item{\code{cor}}{a numeric vector with the correction factor (\eqn{\alpha{^2}}{\alpha^2}) values to be used in \code{cQE}}
  }
}
\source{
  	Campione, N. E., Evans, D. C., Brown, C. M. and Carrano, M. T. (2014) Body mass estimation in non-avian bipeds using a theoretical conversion to quadruped stylopodial proportions. Methods in Ecology and Evolution, 5(9), 913-923.
}