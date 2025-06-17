\name{excalibrII}
\alias{excalibrII}
\docType{data}
\title{excalibrII}
\description{
    Example data for Tiburon Albite from 
    Bartelmehs, K. L., et al. "Excalibr II." Zeitschrift fuer Kristallographie 199.3-4 (1992): 185-196.
    }
\usage{data("excalibrII")}
\format{
  A data frame with 19 observations on the following 2 variables.
  \describe{
    \item{\code{S}}{a numeric vector}
    \item{\code{MS}}{a numeric vector}
  }
}
\examples{
res<-fit.joel(excalibrII,MR=180.15,cw="ccw",optimMR=FALSE)
}


