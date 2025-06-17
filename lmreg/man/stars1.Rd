\name{stars1}
\alias{stars1}
\docType{data}
\title{
Stars data 1
}
\description{
Distance of galactic objects from Earth and their velocities (Hubble, 1929).
}
\usage{data(stars1)}
\format{
  A data frame with 24 observations on the following 2 variables.
  \describe{
    \item{\code{Distance}}{Distance from Earth (in million parsec; 1 parsec = 3.26 light years)}
    \item{\code{Velocity}}{Velocity of galaxy (in km/s)}
  }
}

\source{
Hubble, E. (1929) A relation between distance and radial velocity among extra galactic nebulae. Proc. Nat. Acad. Sc. 15, pp.168-73.
}

\examples{
data(stars1)
head(stars1)
}
\keyword{datasets}
