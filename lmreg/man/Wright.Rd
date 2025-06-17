\name{Wright}
\alias{Wright}
\docType{data}
\title{
Wright brothers' wind tunnel data 
}
\description{
Wright brothers' 1901 wind tunnel data on pressure over different types of wings at different angles.
}
\usage{data(Wright)}
\format{
  A data frame with 222 observations on the following 3 variables.
  \describe{
    \item{\code{Pressure}}{Air pressure (in psi)}
    \item{\code{Angle}}{Angle of wing (in degrees)}
    \item{\code{Wing}}{Wing type}
  }
}

\source{
Dataplot webpage of the National Institute of Standards and Technology (NIST),\cr USA (https://www.itl.nist.gov/div898/software/dataplot/data/WRIGHT11.DAT)
}

\examples{
data(Wright)
head(Wright)
}
\keyword{datasets}
