\name{oneLocusJacquard}
\alias{oneLocusJacquard}
\title{
Estimating Jacquard's condensed identity coefficients
}
\description{
Estimates by simulation Jacquard's 9 condensed identity coefficients for a pairwise relationship. This function is rarely needed, as exact values can be obtained by using \code{\link{jacquard}}.
}
\usage{
oneLocusJacquard(x, ind1, ind2, Nsim, verbose=TRUE,...)
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
  \item{verbose}{
A logical.
}
  \item{\dots}{
Further arguments to be passed on to \code{\link{IBDsim}}.
}
}
\details{
For the definition and further details about these coefficients, see Jacquard (1970).
}
\value{
A numeric of length 9, estimating the condensed Jacquard identity coefficients \eqn{\Delta}.
}

\author{
Magnus Dehli Vigeland
}
\seealso{
\code{\link{jacquard}}, \code{\link{oneLocusIBD}}, \code{\link{twoLocusIBD}}, \code{\link{twoLocusJacquard}}
}
\examples{
### Siblings whose parents are full siblings.
x = fullSibMating(generations=2)
Nsim = 100 # (increase to improve accuracy) 

# Estimating the 9 identity coefficients
j_est = oneLocusJacquard(x, ind1=5, ind2=6, Nsim=Nsim)

# Exact: c(2,1,4,1,4,1,7,10,2)/32

# With the "identity" package:
\dontrun{
  j_exact = jacquard(x, 5:6)
}
}
\keyword{ math }