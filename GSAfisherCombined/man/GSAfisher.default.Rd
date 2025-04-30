\name{GSAfisher.default}
\alias{GSAfisher.default}
\title{GSAfisher.default}
\description{
Default method of GSAfisher function that computes the p-value for a joint test of association between a phenotype and a set of genetic variants (SNPs) by combining marginal p-values using the Fisher method.
}
\usage{
\method{GSAfisher}{default}(x,\dots)
}
\arguments{
  \item{x}{Numeric set of marginal p-values (one vector or a sinlge p-value).}
  \item{\dots}{Further arguments passed to or from other methods.}
}
\value{
  \item{p}{Fisher method combined p-value.}
}
\references{
Fisher, R.A. (1925). Statistical Methods for Research Workers. ISBN 0-05-002170-2.
}
\author{Carlos Garcia Prieto}
\examples{
#Generate 1000 random values from an Uniform distribution U (0, 0.25). 
x<-runif(1000, 0, 0.25)
#Apply the GSAfisher.default function to this data vector.
GSAfisher(x)
}
\keyword{ pchisq }