\name{fev}
\alias{fev}
\docType{data}
\title{ Data on lung capacity of 654 children and adolescents. }
\description{
  These data come from Rosner (2006), and represent forced expiratory 
  volume (FEV) in l/s and several covariates. 
}
\usage{data(fev)}
\format{
  A data frame with 654 observations on the following 6 variables:
  \describe{
    \item{\code{Id}}{an identification code}
    \item{\code{Age}}{age in years}
    \item{\code{FEV}}{forced expiratory volume in l/s}
    \item{\code{Hgt}}{height in inches}
    \item{\code{Sex}}{gender: 0 for female, 1 for male}
    \item{\code{Smoke}}{smokes (1) or not (0)}
  }
}
\source{
 Rosner, B. 2006. Fundamentals of Biostatistics. 6th ed. Duxbury Press.
}
\examples{
data(fev)
splom(fev[c(3, 2, 4, 5, 6)], main="fev data")
}
\keyword{datasets}
