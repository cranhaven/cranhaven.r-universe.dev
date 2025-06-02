\name{ariprec}
\alias{ariprec}
\docType{data}
\title{
Data from climatic stations of the Ariari River (Meta-Colombia Department)
}
\description{
Data from climatic stations of the Ariari River (Meta-Colombia Department) associated with the rainfall variable
}
\usage{data(ariprec)}
\format{
  A data frame with 18 observations on the following 6 variables:
  \describe{
    \item{\code{Obs}}{a numeric vector; observation number}
    \item{\code{Nombre}}{a character vector; station name}
    \item{\code{x}}{a numeric vector; x-coordinate}
    \item{\code{y}}{a numeric vector; y-coordinate}
    \item{\code{ELEV}}{a numeric vector; Elevation above sea level}
    \item{\code{PRECI_TOT}}{a numeric vector; the target variable}
  }
}
\examples{
data(ariprec)
summary(ariprec)
}
\keyword{datasets}
