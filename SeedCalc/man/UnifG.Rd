\name{UnifG}
\alias{UnifG}
\title{Germination Uniformity Index}
\usage{
UnifG(time,nger)
}

\arguments{
  \item{time}{ A \code{vector} object containing the time for germination. }
  \item{nger}{ A \code{vector} object containing the accumulated number of seeds germinated at each time. }
}


\description{
Calculates the Germination Uniformity Index (Sako, 2001).
}

\references{
SAKO, Y.; MCDONALD, M. B.; FUJIMURA, K.; EVANS, A. F.; BENNETT, M. A. A system for automated seed vigour assessment. Seed Science and Technology, v. 29, n. 3, p. 625-636, 2001.
}

\examples{
time <- c(1,2,3,4,5,6,7,8,9,10)
nger <- c(0,2,4,15,25,38,45,50,50,50)
UnifG(time,nger)
}

