\name{L1prop}
\alias{L1prop}
\title{
Likelihood analysis of the binomial parameter for one sample.
}
\description{
  When given the number of ``successes'' and the sample size this
  function plots the normed likelihood of values of the binomial
  parameter \eqn{\pi} and calculates the likelihood ratio for a hypothesized
  value and the maximum likelihood value for the sample, as well as an
  approximate frequentist p-value.
}
\usage{
L1prop(x, n, p.hypoth, pLset=0.05)
}
\arguments{
  \item{x}{
the number of sampling units with the feature
}
  \item{n}{
the number of sampling units examined
}
  \item{p.hypoth}{
the hypothesized probability
}
\item{pLset}{
the desired likelihood for the likelihood interval
}
}
\value{
none, the normed likelihood for different values of the binomial
  probability is plotted with the likelihood interval, and some
  information is printed
}
\references{
  van Hulst, R. 2018. Evaluating Scientific Evidence. ms.

  Pawitan, Y. 2001. In All Likelihood. Oxford University Press, Oxford.
}
\author{
Robert van Hulst
}

\seealso{
  \code{\link{B1prop}}
  
  \code{\link{prop.test}}
}
\examples{
L1prop(13, 78, 0.02)
}

\keyword{ htest }

