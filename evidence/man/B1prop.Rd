\name{B1prop}
\alias{B1prop}
\title{
Bayesian analysis of the binomial parameter for one sample.
}
\description{
This function computes the posterior distribution of the binomial
probability \eqn{\pi} when given the number of ``successes'' and the sample
size, as well as one of a choice of priors. A plot of the posterior
distribution is produced with the 95\% credible interval of \eqn{\pi}.
}
\usage{
B1prop(s, n, p = 0.5, alpha = 0.05, prior = c("uniform", "near_0.5",
  "not_near_0.5", "near_0", "near_1", "custom"), params = NULL)
}

\arguments{
  \item{s}{
  the number of sampling units with the feature
}
  \item{n}{
the number of sampling units examined
}
  \item{p}{
   an optional hypothesized probability
}
\item{alpha}{
  1 - alpha is the desired level of credibility of a credible interval
  }
  \item{prior}{
    one of: "uniform", "near_0.5", "not_near_0.5", "near_0", "near_1",
    "custom", which are all beta distributions with appropriate parameter
    values. Note that if prior="custom" the following argument has to
    be supplied:
}
  \item{params}{a vector with the a and b parameters of the custom beta prior
}
}
\value{
the posterior probability
}
\references{
van Hulst, R. 2018. Evaluating Scientific Evidence. ms.
}
\author{
Robert van Hulst
}
\seealso{
  \code{\link{B2props}}
  
  \code{\link{prop.test}}
}
\examples{
B1prop(13, 100, .1, prior="near_0")
}

\keyword{ htest }

