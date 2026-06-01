\name{Loaloa}
\alias{Loaloa}
\docType{data}
\title{Loaloa Data}
\description{
Rousset et al. (2016) presented the loaloa data set which describes prevalence
of infection by the nematode Loa loa in North Cameroon, 1991-
2001. The study investigated the relationship between altitude, vegetation
indices, and prevalence of the parasite.
}
\usage{data("Loaloa")}
\format{
  A data frame with 197 observations on the following 11 variables.
  \describe{
    \item{\code{longitude}}{longitude, in degrees}
    \item{\code{latitude}}{latitude, in degrees}
    \item{\code{ntot}}{sample size per location}
    \item{\code{npos}}{number of infected individuals per location}
    \item{\code{maxNDVI}}{maximum normalised-difference vegetation index (NDVI) from repeated
satellite scans}
    \item{\code{seNDVI}}{standard error of NDVI}
    \item{\code{elev1}}{altitude, in m}
    \item{\code{elev2}}{additional altitude variables derived
from the previous one, provided for convenience respectively, positive values of altitude-650,}
    \item{\code{elev3}}{positive values of altitude-1000}
    \item{\code{elev4}}{positive values of altitude-1300}
    \item{\code{maxNDVI1}}{a copy of maxNDVI modified as maxNDVI1 (maxNDVI1=0.8
if maxNDVI>0.8, maxNDVI1=maxNDVI otherwise)}
  }
}
\references{
Rousset, F., Ferdy, J.B. and Courtiol, A. (2016). spaMM: Mixed Models,
Particularly Spatial GLMMs. R package version 1.7.2.
}

