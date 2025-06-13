\name{myclimatic_data}
\alias{myclimatic_data}
\docType{data}
\title{
climatic data
}
\description{
A synthetic climatic data that contains time series, geographical coordinates and different climatic variables for a subdaily resolution. Each row corresponds for a time step, and each time is available for a site.
}
\usage{myclimatic_data}
\format{
  A data frame with 4320 observations on the following 6 variables.
  \describe{
    \item{\code{dates}}{a chron object to define time steps}
    \item{\code{JD}}{a numeric vector sepcifying julian days}
    \item{\code{coord.x}}{a numeric vector to define the x coodinates of different sites}
    \item{\code{coord.y}}{a numeric vector to define the y coodinates of different sites}
    \item{\code{temp}}{a numeric vector for a climatic variable (temperature)}
    \item{\code{Rh}}{a numeric vector for a climatic variable (relative humidity)}
  }
}

