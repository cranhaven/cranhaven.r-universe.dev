\name{Gunter}
\alias{Gunter}
\docType{data}
\title{ Data from Gunter et al. 
}
\description{
    Gunter, Mickey E., et al. "Results from a McCrone spindle stage short course, a new version of EXCALIBR, and how to build a spindle stage." MICROSCOPE-LONDON THEN CHICAGO-. 52.1 (2004): 23-39.
}
\usage{data("Gunter")}
\format{
  A data frame with 19 observations on the following 2 variables.
  \describe{
    \item{\code{S}}{a numeric vector}
    \item{\code{MS}}{a numeric vector}
  }
}
\examples{
res<-fit.joel(Gunter,MR=-0.89,cw="cw",optimMR=FALSE)
}

