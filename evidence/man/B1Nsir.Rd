\name{B1Nsir}
\alias{B1Nsir}
\title{
Bayesian analysis of a Normal sample using a SIR prior.
}
\description{
This function performs a standard Bayesian analysis of a single sample
of a population assumed to follow a Normal distribution. A Standard
Improper Reference prior is assumed.
}
\usage{
B1Nsir(x, r = 10000, alpha = 0.05)
}
\arguments{
  \item{x}{
a vector of sample values
}
  \item{r}{
the number of samples to be taken from the posterior distribution
(defaults to 10000)
}
  \item{alpha}{
1 - level of credibility, so that for alpha = 0.05 (the default)
credible intervals will have 95\% credibility
}
}
\value{
none returned; the function produces a plot of the posterior 
distribution and prints some statistics.
}
\references{
van Hulst, R. 2018. Evaluating Scientific Evidence. ms.
}
\author{
Robert van Hulst
}
\seealso{
\code{\link{B1Nmean}}
}
\examples{
data(darwin)
B1Nsir(darwin$difference)
}

\keyword{ htest }

\keyword{ models }
