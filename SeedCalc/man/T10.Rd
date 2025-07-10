\name{T10}
\alias{T10}
\title{Time spent to 10 percent germination}
\usage{
T10(time,nger)
}

\arguments{
  \item{time}{ A \code{vector} object containing the time for germination. }
  \item{nger}{ A \code{vector} object containing the accumulated number of seeds germinated at each time. }
}


\description{
Calculates the time spent to 10 percent germination (Adapted from Farooq et al., 2005).
}

\references{
FAROOQ, M.; BASRA, S. M. A.; AHMAD, N.; HAFEEZ, K. Thermal Hardening: A New Seed Vigor Enhancement Tool in Rice. Journal of Integrative Plant Biology, v. 47, n. 2, p. 187-193, 2005.
}

\examples{
time <- c(1,2,3,4,5,6,7,8,9,10)
nger <- c(0,2,4,15,25,38,45,50,50,50)
T10(time,nger)
}

