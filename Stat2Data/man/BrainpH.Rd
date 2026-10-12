\name{BrainpH}
\alias{BrainpH}
\docType{data}
\title{Brain pH Measurements}
\description{
Brain tissue pH at time of death
}
\format{
  A data frame with 54 observations on the following 5 variables.
  \describe{
    \item{\code{pH}}{Brain tissue pH}
    \item{\code{Sex}}{\code{F} or \code{M}}
    \item{\code{Ethnicity}}{\code{AfricanAmerican}, \code{Asian}, \code{Caucasian}, or \code{PacificIslander}}
    \item{\code{Age}}{Age at death}
    \item{\code{DeathType}}{Cause of death (\code{Cardiac}, \code{Other}, or \code{Suicide})}
  }
}
\details{
These are data from a PNAS article (supplemental file) on pH in brain tissue samples for controls and for people who had Major Depressive Disorder. We extracted just the controls (roughly 3/4 of whom died of cardiac arrest).
}
\source{
Jun Z. Li et al. (2013), "Circadian patterns of gene expression in the human brain and disruption in major depressive disorder,"  PNAS, vol 110, no. 24, www.pnas.org/cgi/doi/10.1073/pnas.1305814110

Data extracted from Supporting Information, Table S4:
Li et al. www.pnas.org/cgi/content/short/1305814110

}
\keyword{datasets}
