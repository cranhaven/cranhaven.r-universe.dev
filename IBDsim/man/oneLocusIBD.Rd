\name{oneLocusIBD}
\alias{oneLocusIBD}
\title{
Estimating pairwise IBD coefficients
}
\description{
Estimates by simulation the IBD coefficients of a non-inbred pairwise relationship. 
}
\usage{
oneLocusIBD(x, ind1, ind2, Nsim, Xchrom=FALSE, verbose=TRUE, ...)
}
\arguments{
  \item{x}{
A pedigree in the form of a \code{\link{linkdat}} object.
}
  \item{ind1, ind2}{
Numeric ID labels of the two individuals.
}
  \item{Nsim}{
The number of simulations to be performed.
}
  \item{Xchrom}{
A logical indicating if the locus is X-linked (if TRUE) or autosomal (FALSE).
}
  \item{verbose}{
A logical.
}
  \item{\dots}{
Further arguments to be passed on to \code{\link{IBDsim}}.
}
}
\details{
For any pair of non-inbred individuals, the IBD coefficients \eqn{\kappa=(\kappa[0], \kappa[1], \kappa[2])} associated with the relationship, are defined as the probabilities
\deqn{\kappa[i] = Pr(i alleles shared identically by descent).}

For an X-chromosomal locus, and if at least one of the individuals is male, \eqn{\kappa[i]} is defined only for \eqn{i=0,1}.
}
\value{
A numeric of length 3 (autosomal) or 2 (X-linked), estimating \eqn{\kappa}.
}

\author{
Magnus Dehli Vigeland
}
\seealso{
\code{\link{twoLocusIBD}}, \code{\link{oneLocusJacquard}}, \code{\link{twoLocusJacquard}}
}
\examples{
### Example 1: Full siblings
x <- nuclearPed(2) 
Nsim <- 100 # Should be increased substantially

# Autosomal kappa estimate (exact = c(0.25, 0.5, 0.25))
oneLocusIBD(x, ind1=3, ind2=4, Nsim=Nsim)

# X-chromosomal kappa estimate (exact = c(0.5, 0.5))
oneLocusIBD(x, ind1=3, ind2=4, Nsim=Nsim, Xchrom=TRUE)

}
\keyword{ math }