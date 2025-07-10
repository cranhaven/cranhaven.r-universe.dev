\name{CVG}
\alias{CVG}
\title{Velocity of Germination Coefficient}
\usage{
CVG(time,nger)
}

\arguments{
  \item{time}{ A \code{vector} object containing the time for germination. }
  \item{nger}{ A \code{vector} object containing the accumulated number of seeds germinated at each time. }
}


\description{
Calculates the Velocity of Germination Coefficient (Nichols & Heydecker, 1968).
}

\references{
NICHOLS, M. A.; HEYDECKER, W. Two approaches to the study of germination data. Proceedings of the International Seed Testing Association, v. 33, p. 531–540, 1968.
}

\examples{
time <- c(1,2,3,4,5,6,7,8,9,10)
nger <- c(0,2,4,15,25,38,45,50,50,50)
CVG(time,nger)
}
