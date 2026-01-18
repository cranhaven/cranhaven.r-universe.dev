% This is a template R document file
% Distributed under GPL 3 or later

\name{degld}
\alias{degld}
\alias{pegld}
\alias{qegld}
\alias{regld}
\alias{egld}
\alias{gbd}
\title{Basic functions for RS-GLD}
\description{
 To compute the density, distribution, quantile, and to generate random sample for RS-GLD.
}
\usage{
## Default S3 method:
degld(x,lambda)
pegld(x,lambda)
qegld(p,lambda)
regld(n,lambda)
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
\seealso{\code{\link{fit.egld}},
 \code{\link{qrsgld}},\code{\link{prsgld}},
 \code{\link{rrsgld}},\code{\link{drsgld}}.
}
\examples{

lambdas = c(2,4,3,4)
shape=3;scale=4
x0 = rbeta(5,shape,scale)
x1 = x0*lambdas[2]+lambdas[1]
qegld(c(0,.1,.5,.7,1),lambdas)
qbeta(c(0,.1,.5,.7,1),shape,scale)*lambdas[2]+lambdas[1]

pegld(x1,lambdas)
pbeta(x0,shape,scale)

degld(x1,lambdas)
dbeta(x0,shape,scale)/lambdas[2]

x0 = sort(rbeta(1000,shape,scale))
y = x0*lambdas[2]+lambdas[1]
plot(dbeta(x0,shape,scale)/lambdas[2]~y,type='l')
lines(degld(y,lambdas)~y,lty=2,col=2)
lines(density(y),col=4,lty=3)

}
\keyword{stats}

