\name{B2props}
\alias{B2props}
\title{
Bayesian analysis of the binomial parameters for two samples.
}
\description{
This function computes the posterior distributions of the binomial
parameters \eqn{\pi[1]} and \eqn{\pi[2]} when given the numbers of
``successes'' and the sample sizes for the two samples. It uses uniform
priors. A plot of the posterior distributions of the two \eqn{\pi}'s is
produced, and a plot of the posterior distribution of \eqn{\pi[1] - \pi[2]}
with its 95\% credible interval.
}
\usage{
B2props(s, n, alpha = 0.05)
}

\arguments{
  \item{s}{
a vector containing the 2 numbers of sampling units with the feature ("success")
}
  \item{n}{
a vector containing the 2 numbers of sampling units examined
}
  \item{alpha}{
1 - level of credibility, so that for alpha = 0.05 (the default)
credible intervals will have 95\% credibility
}
}
\value{
None, the inferred difference between the probabilities and its 95\% credible interval is calculated and several plots are produced
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
B2props(c(13, 22), c(78, 92))
}

\keyword{ htest }

