\name{twoLocusJacquard}
\alias{twoLocusJacquard}
\title{
Estimating two-locus Jacquard coefficients
}
\description{
Estimates by simulation the two-locus version of Jacquard's condensed identity coefficients for a pairwise relationship.
}
\usage{
twoLocusJacquard(x, ind1, ind2, rho=NULL, cM=NULL, Nsim, verbose=TRUE,...)
}
\arguments{
  \item{x}{
A pedigree in the form of a \code{\link{linkdat}} object.
}
  \item{ind1, ind2}{
Numeric ID labels of the two individuals.
}
  \item{rho}{
NULL, or a number in the interval [0, 0.5]: the recombination fraction between the two loci. If non-NULL, it is converted to centiMorgan using Haldanes map function: \code{cM = -50*log(1-2*rho)}.
}
  \item{cM}{
NULL, or a non-negative number: the distance in centiMorgan between the two loci. The numeric \code{Inf} is allowed, and corresponds to unlinked loci.
}
  \item{Nsim}{
The number of simulations to be performed.
}
  \item{verbose}{
A logical.
}
  \item{\dots}{
Further arguments to be passed on to \code{\link{IBDsim}}.
}
}
\details{
As in the case of IBD coefficients (see \code{\link{twoLocusIBD}}), we can generalise Jacquard's identity coefficients to two loci: Given any pair of individuals, and any pair of autosomal loci, we define the two-locus Jacquard coefficient \eqn{\Delta[i,j]} (where \eqn{1 \le i,j \le 9}) by
\deqn{\Delta[i,j] = Pr(Jacquard state \Sigma[i] at locus 1, and state \Sigma[j] at locus 2).}
}
\value{
A numerical 9*9 matrix. The entry in row \eqn{a} and column \eqn{b} is the estimate of \eqn{\Delta[a-1, b-1]} defined above. 
}
\author{
Magnus Dehli Vigeland
}
\seealso{
\code{\link{jacquard}}, \code{\link{oneLocusIBD}}, \code{\link{twoLocusIBD}}, \code{\link{oneLocusJacquard}}
}
\examples{
### Siblings whose parents are full siblings.
x = fullSibMating(generations=2)
Nsim = 100 # (increase to improve accuracy) 

# Estimate of the 9 identity coefficients
j_est = oneLocusJacquard(x, ind1=5, ind2=6, Nsim=Nsim, seed=123)

### Two-locus Jacquard coefficients
# Completely linked loci
rho = 0 
j2_linked = twoLocusJacquard(x, ind1=5, ind2=6, rho=rho, Nsim=Nsim, seed=123)
stopifnot(identical(diag(j2_linked), j_est))

# Completely unlinked
rho = 0.5
j2_unlinked = twoLocusJacquard(x, ind1=5, ind2=6, rho=rho, Nsim=Nsim, seed=123)
stopifnot(identical(j2_unlinked, outer(j_est, j_est)))
}
\keyword{ math }