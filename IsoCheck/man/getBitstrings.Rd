\name{getBitstrings}
\alias{getBitstrings}
\title{Bitstring Representation of a Spread or Star}
\description{This function computes a bitstring representation for a spread or star of \code{PG(n-1,2)} }
\usage{getBitstrings(spr)}
\arguments{
  \item{spr}{A spread or star of \code{PG(n-1,2)} stored as a three dimensional binary array (see Details and Examples of \code{\link{checkSpreadEquivalence}}).}
}

\details{This code obtains the bitstring representation (as described in Spencer et al. 2019) for any given spread or star of \code{PG(n-1,2)}. The spread should be formatted as a 3-dimensional array with \code{spr[i,j,k]} indicating whether or not the \code{i}th basic factor is present in the \code{j}th effect of the \code{k}th flat of \code{spr}. This representation facilitates fast equivalence checking for spreads or stars.
}

\value{
A matrix with each row characterizing the elements of a distinct flat in spr.
}
\references{
Spencer, N.A., Ranjan, P., and Mendivil, F., (2019), "Isomorphism Check for \eqn{2^n} Factorial Designs with Randomization Restrictions", Journal of Statistical Theory and Practice, 13(60),1-24 [https://doi.org/10.1007/s42519-019-0064-5]
}
\seealso{
\code{\link{checkSpreadEquivalence}} for checking equivalence of spreads.\cr
\code{\link{checkStarEquivalence}} for checking equivalence of spreads.
}
\keyword{Collineation}
\author{Neil Spencer, Pritam Ranjan, Franklin Mendivil}

\examples{
## Example 1: The bitstring representation of a 1-spread of PG(3,2)
data(spreadn4t2a)
getBitstrings(spreadn4t2a)



## Example 2: The bitstring representation of a star of PG(4,2) consisting of 4-flats.
data(starn5t3a)
getBitstrings(starn5t3a)
}
