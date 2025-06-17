\name{hoop}
\alias{hoop}
\docType{data}
\title{
Hoop tree data
}
\description{
Compressive strength and moisture content of wood in hoop
trees (Williams, 1959).
}
\usage{data(hoop)}
\format{
  A data frame with 50 observations on the following 4 variables.
  \describe{
    \item{\code{Temp}}{Temperature (in Celsius)}
    \item{\code{Tree}}{Hoop tree number}
    \item{\code{Strength}}{Maximum compressive strength parallel to the grain (in MPa)}
    \item{\code{Moisture}}{Moisture content (100 times water mass/dry wood mass)}
  }
}

\source{
Williams, E.J. (1959) Regression Analysis, Wiley, New York.
}

\examples{
data(hoop)
head(hoop)
}
\keyword{datasets}
