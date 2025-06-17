\name{Bloss73}
\alias{Bloss73}
\docType{data}
\title{Bloss73}
\description{
    Adularia data from: Bloss, F. D., and D. Riess. "Computer determination of 2V and indicatrix orientation from extinction data." American Mineralogist 58 (1973): 1052-1061.
}
\usage{data("Bloss73")}
\format{
  A data frame with 19 observations on the following 2 variables.
  \describe{
    \item{\code{S}}{a numeric vector}
    \item{\code{MS}}{a numeric vector}
  }
}
\examples{
res<-fit.joel(Bloss73,MR=180.95,cw="ccw",optimMR=FALSE)
}


