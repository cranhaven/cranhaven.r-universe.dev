% This is a template R document file
% Distributed under GPL 3 or later

\name{drsgld}
\alias{drsgld}
\alias{prsgld}
\alias{qrsgld}
\alias{rrsgld}
\alias{gld}
\title{Basic functions for RS-GLD}
\description{
 To compute the density, distribution, quantile, and to generate random sample for RS-GLD.
}
\usage{
## Default S3 method:
drsgld(x,lambda)
prsgld(x,lambda)
qrsgld(p,lambda)
rrsgld(n,lambda)
}
\arguments{
  \item{x}{a numeric value or a vector.}
  \item{p}{a probability or a vector of probabilities.}
  \item{n}{sample size.}
  \item{lambda}{a vector of four parameters for RS-GLD.}
}
\author{
  B. Wang  \email{bwang@jaguar1.usouthal.edu}
}
\references{

Karian, Z.A.,
  Dudewicz, E.J., McDonald, P., 1996. The Extended Generalized Lambda
  Distribution System for Fitting Distributions to Data:
  history,completion of theory, tables, applications, the ``final
  word'' on moment fits, \emph{Comm. in Statist.- Simul. \& Comput.}
  \bold{25}(3), 611-642.

Karian, Z.A., Dudewicz,
  E.J., 2000. \emph{Fitting Statistical Distributions: The Generalized
  Lambda Distribution and Generalized Bootstrap Methods}, Chapman and
  Hall/CRC.

}

\seealso{
 \code{\link{fit.gld}},
 \code{\link{qegld}},\code{\link{pegld}},
 \code{\link{regld}},\code{\link{degld}}.
}

\examples{

lambdas = c(0, 0.1975, 0.1349,0.1349)
qrsgld(c(0,.1,.5,.7,1),lambdas)
prsgld(c(-10,0,1,3,20),lambdas)
drsgld(c(-10,0,1,3,20),lambdas)
x = sort(rrsgld(100,lambdas))
plot(dnorm(x)~x,type='l')
lines(drsgld(x,lambdas)~x,lty=2,col=2)
lines(density(x),col=4,lty=3)

}
\keyword{stats}

