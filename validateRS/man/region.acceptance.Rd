\name{region.acceptance}

\alias{region.acceptance}

\title{
Determine acceptance region for a test
}
\description{
The acceptance region for a particular test is determined. 
}

\usage{
region.acceptance(hypo.test, p.0, size, alpha)
}

\arguments{
  \item{hypo.test}{
A string containing the name of the test "minP" for the multiple test, "minPp" for the enhanced multiple test or "sterneHull" for the envelope test. 
}
  \item{p.0}{
A vector containing the PD values under H0
}
  \item{size}{
A vector with the sizes of the rating classes
}
  \item{alpha}{
The desired significance level
}
}
\details{
The function computes the acceptance region, the observed significance level and the cardinality of the acceptance region. 
}
\value{
A list containing
\item{par }{The parameters that were passed to the function}
\item{acc.reg }{The name of the test and a description of the acceptance region. For the multiple test this is the lower left point of the critical region, for the enhanced multiple test this is the lower left point of the critical region and the equation of the cutting hyperplane. For the envelope test it contains the coordinates of all the points. }
\item{optim.data }{Data needed for optimal computations in other functions}
\item{observedAlpha }{The observed significance level}
\item{cardinality }{The cardinality of the acceptance region}
}

\examples{
mp<-region.acceptance(hypo.test="minP", p.0=c(0.001, 0.004), size=c(1000, 1000), alpha=0.05)
mpp<-region.acceptance(hypo.test="minPp", p.0=c(0.001, 0.004), size=c(1000, 1000), alpha=0.05)

}