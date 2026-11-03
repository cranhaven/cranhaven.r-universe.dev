\name{rmvhyper}
\Rdversion{1.7}
\alias{rmvhyper}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Random generation for the multivariate hypergeometric distribution
}
\description{
Generates a single random deviate from a multivariate hypergeometric distribution.
}
\usage{
rmvhyper(Mk, m)
}
\arguments{
  \item{Mk}{ A numeric vector describing the population from which the sub-sample will be drawn.}
  \item{m}{ Number of elements to be drawn from the population}
}
\details{
The multivariate hypergeometric distribution is for sampling without replacement from a population with a finite number of element types. The number of element types is given by the length of the vector \code{Mk}.
}
\value{
A numeric vector of elements, totally to \code{m}, drawn without replacement from the population described by \code{Mk}.
}
\author{
Sebastien Haneuse
}
\seealso{
\code{\link{rhyper}}.
}
\examples{
##
rmvhyper(c(1000, 500, 200, 50), 200)

## Check the properties (first two moments) of the generated deviates
##
M  <- 100
Qx <- c(0.7, 0.15, 0.1, 0.05)
temp <- matrix(NA, nrow=10000, ncol=length(Qx))
for(i in 1:nrow(temp)) temp[i,] <- rmvhyper(M*Qx, 1)

##
rbind(Qx, apply(temp, 2,  mean))
rbind(sqrt(Qx * (1-Qx)), apply(temp, 2, sd))
}
