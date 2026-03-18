\name{L2prop}
\alias{L2prop}
\title{
Likelihood analysis of the binomial parameters for two samples.
}
\description{
  When given the numbers of ``successes'' and the sample sizes for the two
  samples, this function plots the normed likelihoods of the two samples
  and calculates the likelihood ratio for two different models, one
  fitting two binomial parameters, and one fitting only one.
}
\usage{
L2prop(x, n)
}

\arguments{
  \item{x}{
a vector containing the 2 numbers of sampling units with the feature
}
  \item{n}{
a vector containing the 2 numbers of sampling units examined
}
}
\value{
none, the inferred difference between the probabilities and its 95\% credible interval are calculated and a plot is produced
}
\references{
van Hulst, R. 2018. Evaluating Scientific Evidence. ms.
}
\author{
Robert van Hulst
}

\seealso{
  \code{\link{B1prop}}
  
  \code{\link{prop.test}}
}
\examples{
L2prop(c(13, 22), c(78, 92))
}

\keyword{ htest }

