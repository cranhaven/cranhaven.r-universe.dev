\name{Carman}
\alias{Carman}
\docType{data}
\title{
    Data for Topaz by Carman
}
\description{
    Data from F. Donald Bloss, "The spindle stage, principles and practice", Cambridge UP, Cambridge, 1981, p. 226, for Topaz provided by Prof. Carman.
}
\usage{data("Carman")}
\format{
  A data frame with 36 observations of the following 2 variables.
  \describe{
    \item{\code{S}}{a numeric vector}
    \item{\code{MS}}{a numeric vector}
  }
}
\examples{
res<-fit.joel(Carman,cw="ccw",optimMR=TRUE)
}

