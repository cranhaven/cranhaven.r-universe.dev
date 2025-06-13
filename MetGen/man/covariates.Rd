\name{mycovariates}
\alias{mycovariates}
\docType{data}
\title{
Covariates data
}
\description{
Covariates data that contains time series, geographical coordinates and different climatic variables for a subdaily resolution. Each row corresponds for a time step, and each time is available for a site.
}
\usage{mycovariates}
\format{
  A data frame with 4320 observations on the following 18 variables.
  \describe{
    \item{\code{dates}}{a numeric vector for the time steps}
    \item{\code{coord.x}}{a numeric vector to define the x coodinates of different sites}
    \item{\code{coord.y}}{a numeric vector to define the y coodinates of different sites}
    \item{\code{t2m}}{a numeric vector for a large scale variable obtained from reanalysis}
    \item{\code{cos.365d}}{a numeric vector for the seasonal variabiliy throughout 365 days}
    \item{\code{sin.365d}}{a numeric vector for the seasonal variabiliy throughout 365 days}
    \item{\code{cos.183d}}{a numeric vector for the seasonal variabiliy throughout 183 days}
    \item{\code{sin.183d}}{a numeric vector for the seasonal variabiliy throughout 183 days}
    \item{\code{cos.91d}}{a numeric vector for the seasonal variabiliy throughout 91 days}
    \item{\code{sin.91d}}{a numeric vector for the seasonal variabiliy throughout 91 days}
    \item{\code{cos.30d}}{a numeric vector for the seasonal variabiliy throughout 30 days}
    \item{\code{sin.30d}}{a numeric vector for the seasonal variabiliy throughout 30 days}
    \item{\code{cos.24h}}{a numeric vector for diurnal variabiliy during a day}
    \item{\code{sin.24h}}{a numeric vector for diurnal variabiliy during a day}
    \item{\code{cos.12h}}{a numeric vector for diurnal variabiliy during half a day}
    \item{\code{sin.12h}}{a numeric vector for diurnal variabiliy during half a day}
    \item{\code{cos.6h}}{a numeric vector for diurnal variabiliy during 6 hours}
    \item{\code{sin.6h}}{a numeric vector for diurnal variabiliy during 6 hours}
  }
}
