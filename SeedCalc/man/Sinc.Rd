\name{Sinc}
\alias{Sinc}
\title{Germination Synchrony}
\usage{
Sinc(time,nger)
}

\arguments{
  \item{time}{ A \code{vector} object containing the time for germination. }
  \item{nger}{ A \code{vector} object containing the accumulated number of seeds germinated at each time. }
}


\description{
Calculates the germination synchrony (Primack, 1980).
}

\references{
PRIMACK, R.B. 1980. Variation in the phenology of natural populations of montane shrubs in New Zealand. Journal of  Ecology, v.68, p.849-862.
}

\examples{
time <- c(1,2,3,4,5,6,7,8,9,10)
nger <- c(0,2,4,15,25,38,45,50,50,50)
Sinc(time,nger)
}
