\name{stars2}
\alias{stars2}
\docType{data}
\title{
Stars data 2
}
\description{
Distance of additional galactic objects from Earth and their velocities (Humason, 1936).
}
\usage{data(stars2)}
\format{
  A data frame with 21 observations on the following 2 variables.
  \describe{
    \item{\code{Distance}}{Distance from Earth (in million parsec; 1 parsec = 3.26 light years)}
    \item{\code{Velocity}}{Velocity of Galaxy (in km/s)}
  }
}

\details{The galactic objects in this data set are much further away from Earth than those in the data set \code{stars1.txt}. These became available within a few years of the publication of Hubble's original work, through rapid advancesment in technology. Although the new data cemented Hubble's hypothesis that distant objects have proportionately higher velocity (as they should in a universe expanding with constant acceleration), the constant of proportionality turned out to be somewhat different from Hubble's original estimate.}

\source{
Humason, M.L. (1936) The apparent radial velocities of 100 extra galactic nebula. Astrophys. J. 83, pp.10-22.
}

\examples{
data(stars2)
head(stars2)
}
\keyword{datasets}
