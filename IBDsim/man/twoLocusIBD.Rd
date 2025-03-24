\name{twoLocusIBD}
\alias{twoLocusIBD}

\title{
Estimating two-locus IBD coefficients
}
\description{
Estimates by simulation the two-locus IBD coefficients of a non-inbred pairwise relationship.
}
\usage{
twoLocusIBD(x, ind1, ind2, rho=NULL, cM=NULL, Nsim, Xchrom=FALSE, verbose=TRUE, ...)
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
NULL, or a non-negative number: the distance in centiMorgan between the two loci.
}
  \item{Nsim}{
The number of simulations to be performed.
}
  \item{Xchrom}{
A logical indicating if the loci are X-linked (if TRUE) or autosomal (FALSE).
}
  \item{verbose}{
A logical.
}
  \item{\dots}{
Further arguments to be passed on to \code{\link{IBDsim}}.
}
}
\details{
For a given pair of autosomal loci, the two-locus IBD coefficients of a non-inbred pairwise relationship are defined by
\deqn{\kappa[i,j] = Pr(sharing i alleles IBD at locus 1, and j alleles IBD at locus 2),}
where \eqn{0 \le i,j \le 2}. For X-chromosomal loci, and if at least one of the individuals is male, \eqn{\kappa[i,j]} is defined only for \eqn{0 \le i,j \le 1}.

While the single-locus IBD coefficients \eqn{\kappa=(\kappa[0], \kappa[1], \kappa[2])} (see \code{\link{oneLocusIBD}}) depend only on the genealogy relating the two individuals, the two-locus coefficients also depend on the genetic distance between the loci. In particular, if the loci are completely linked (rho=0; cM=0) we have \eqn{\kappa[i,j] = \kappa[i]} if i=j and 0 otherwise. If the loci are unlinked (rho=0.5; cM=Inf) we have \eqn{\kappa[i,j] = \kappa[i]*\kappa[j]}. (See examples.)
}
\value{
A numerical matrix, where the entry in row \eqn{a} and column \eqn{b} is the estimate of \eqn{\kappa[a-1, b-1]} defined above. 
}
\author{
Magnus Dehli Vigeland
}
\seealso{
\code{\link{oneLocusIBD}}, \code{\link{oneLocusJacquard}}, \code{\link{twoLocusJacquard}}
}
\examples{
### Example 1: Full siblings
x <- nuclearPed(2) 
Nsim <- 100 # Should be increased substantially

# One-locus kappa estimates (autosomal and X):
k_est = oneLocusIBD(x, ind1=3, ind2=4, Nsim=Nsim, seed=123)
k_est_X = oneLocusIBD(x, ind1=3, ind2=4, Nsim=Nsim, Xchrom=TRUE, seed=123)

### Two-locus IBD estimation
# Completely linked, autosomal
rho = 0 
k2_linked = twoLocusIBD(x, ind1=3, ind2=4, rho=rho, Nsim=Nsim, seed=123)
stopifnot(identical(diag(k2_linked), k_est))

# Completely unlinked, autosomal
rho = 0.5
k2_unlinked = twoLocusIBD(x, ind1=3, ind2=4, rho=rho, Nsim=Nsim, seed=123)
stopifnot(identical(k2_unlinked, outer(k_est, k_est)))

# Recombination rate 10%, autosomal
rho <- 0.1 
cM <- -50*log(1-2*rho)
r1 <- twoLocusIBD(x, ind1=3, ind2=4, rho=rho, Nsim=Nsim, seed=17)
r2 <- twoLocusIBD(x, ind1=3, ind2=4, cM=cM, Nsim=Nsim, seed=17)
stopifnot(identical(r1, r2))

### Two-locus IBD on X
# Completely linked, X chromosome
rho = 0 
k2_linked_X = twoLocusIBD(x, ind1=3, ind2=4, rho=rho, Nsim=Nsim, Xchrom=TRUE, seed=123)
stopifnot(identical(diag(k2_linked_X), k_est_X))

# Completely unlinked, X chromosome
rho = 0.5
k2_unlinked_X = twoLocusIBD(x, ind1=3, ind2=4, rho=rho, Nsim=Nsim, Xchrom=TRUE, seed=123)
stopifnot(identical(k2_unlinked_X, outer(k_est_X, k_est_X)))

# Recombination rate 10%, X chromosome
rho <- 0.1 
cM <- -50*log(1-2*rho)
r1_X <- twoLocusIBD(x, ind1=3, ind2=4, rho=rho, Nsim=Nsim, Xchrom=TRUE, seed=123)
r2_X <- twoLocusIBD(x, ind1=3, ind2=4, cM=cM, Nsim=Nsim, Xchrom=TRUE, seed=123)
stopifnot(identical(r1_X, r2_X))


### Example 2: Comparing half sib, grandparent and uncle
# These are indistinguishable with unlinked loci, see e.g. 
# pages 182-183 in Egeland, Kling and Mostad (2016).
# Each simulations followed by exact counterpart.
x <- addSon(addSon(nuclearPed(2, sex=1:2), 4), 5)
plot(x)
rho <- 0.25
Nsim <- 10 # Should be increased to at least 10000
twoLocusIBD(x, 1, 6, rho=rho, Nsim=Nsim, verbose=FALSE)[2,2]; .5*(1-rho)
twoLocusIBD(x, 8, 6, rho=rho, Nsim=Nsim, verbose=FALSE)[2,2]; .5*(rho^2+(1-rho)^2)
twoLocusIBD(x, 3, 6, rho=rho, Nsim=Nsim, verbose=FALSE)[2,2]; .5*((1-rho)*(rho^2+(1-rho)^2) + rho/2)

### Example 3: X chromosome, granddaughter vs maternal grandfather.
y <- addDaughter(nuclearPed(1, sex=2), 3)
plot(y)
rho <- 0.25
Nsim <-10
twoLocusIBD(y, 1, 5, rho=rho, Nsim=Nsim, Xchrom=TRUE)

# Exact
matrix(c(1-rho, rho, rho, 1-rho)/2, ncol=2) 

}
\keyword{ math }