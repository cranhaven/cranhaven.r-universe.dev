\name{power.target.Nclasses}
\alias{power.target.Nclasses}
\title{
Compute the alternative hypothesis' PDs by increasing the PDs under the null hypothesis towards 1. 
}
\description{
Compute the alternative hypothesis' PDs by increasing the PDs under the null hypothesis towards 1. 

One can either increase all the PDs (i.e. altenative H1A in the ECB Working series' paper) or one at a time (scenario H1B). 

The alternative hypothesis H1C can be computed using \code{\link{sample.knowledge.H1}}. 
}
\usage{
power.target.Nclasses(p.0, size, N , target = 0.5, alpha = 0.05, precision = 1e-07)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{p.0}{A Vector with the PD values under H0
}
  \item{size}{A Vector with the size of the rating classes
}
  \item{N}{The number of PDs in which H1 should be different from the PDs under H0. For the alternative hypothesis H1A of the ECB Working paper, N should be set to length(p.0), for the alternative hypothesis H1B N should be 1. 
}
  \item{target}{
  The target power for the benchmark test
}
  \item{alpha}{The significance level chosen
}
  \item{precision}{The precision for the PDs under H1
}
}
\value{A vector with PD values under H1, such that the power of the benchmark equals the target power. 
}
\seealso{
\code{\link{sample.knowledge.H1}} 
}
\examples{
data(ratingData)

p.0<-ratingData$p.0
sizes<-ratingData$sizes

h1A<-power.target.Nclasses(p.0=p.0, size=sizes[1,], N=length(p.0), 
                                 target=0.50)
h1A                                 

h1B<-power.target.Nclasses(p.0=p.0, size=sizes[,], N=1, 
                                 target=0.30)
h1B                                 

}
