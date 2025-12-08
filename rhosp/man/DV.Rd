\name{DV}
\alias{DV}
\title{ implements the De Vielder approximation  }
\description{
   DV computes the De Vielder approximation whose goal is to identify the first
   two moments of a general case of the first model where p(x) is unknown to the first two moments
   of the specific case of the first model where \eqn{p(x) = 1- exp(-mu *x)}
}
\usage{
DV(T)
}
\arguments{
  \item{T}{a vector of the observations of the random variable T associated with
the first model}
}
\value{
  a vector of lambda and mu of the De Vielder method
}
\author{ Christophe Dutang and Julie Barthes }
\examples{
T<-c(12.9622796,  1.4146460,  1.3146761, 14.9147353,  7.5131105,  
8.5130874, 6.5943351, 10.6954653, 14.1000977, 12.4673316,  2.7185478,  
9.6297777, 10.0930441,  0.6270543, 26.7937074,  7.6082447)
res<-DV(T)
}
\keyword{ utilities }

