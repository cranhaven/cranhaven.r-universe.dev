\name{sigtestCI}
\alias{sigtestCI}
\title{
Universal Fisherian significance test with confidence interval.
}
\description{
Given a critical value alpha, this function performs a Fisherian
significance test of the null hypothesis at level p, reports the result
of the test, as well as the lower and upper values of the corresponding
confidence interval. See Kadane(2016) for the idea for this.
}
\usage{
sigtestCI(p)
}

\arguments{
  \item{p}{
the desired significance level
}
}
\details{
Note that this function does not require any data: if a rare (as long as p
  is sufficiently small) event occurs, H[0] is deemed to be implausible,
  and rejected. If such an event does not occur, we can simply try to do
  the experiment again. A Neyman-Pearson hypothesis test does require
  data and also an alternative hypothesis. For a NP hypothesis test we
  can (and should) consider the power of the test (the probability of
  rejecting H[0] when   H[a] is true).  
  }
\value{
A message informing the user if H0 was rejected or not and the lower and
  upper boundaries of the corresponding confidence interval.
}
\references{
  Kadane, J.B. 2016. Beyond hypothesis testing. Entropy 18, 199.
  
  van Hulst, R. 2018. Evaluating Scientific Evidence. ms.
}
\author{
Robert van Hulst
}

\examples{
sigtestCI(p=0.05)
}

\keyword{ htest }

