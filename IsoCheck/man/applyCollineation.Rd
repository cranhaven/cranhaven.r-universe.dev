\name{applyCollineation}
\alias{applyCollineation}
\title{Relabel a Spread or Star with a Collineation Matrix}
\description{This function relabels a balanced (t-1)-spread or a covering star \eqn{St(n, \mu, t, t_0)} of \code{PG(n-1,2)} according to the specified collineation matrix.}
\usage{applyCollineation(C, spr)}
\arguments{
  \item{C}{A binary n by n matrix representing a collineation of \code{PG(n-1,2)}.}
  \item{spr}{A balanced spread or star of \code{PG(n-1,2)} stored as a three dimensional binary array (see Details and Examples of \code{\link{checkSpreadEquivalence}}).}
}

\details{This code applies the relabelling corresponding to a collineation matrix C to any given balanced spread or star of PG(n-1, 2). The spread should be formatted as a 3-dimensional array with \code{spr[i,j,k]} indicating whether or not the \code{i}th basic factor is present in the \code{j}th effect of the \code{k}th flat of \code{spr}. The collineation is applied via a matrix multiplication modulo 2 (i.e., the calculations are done over GF(2)). See Spencer et al. (2019) for details.
}

\value{
A spread or star of the same dimensions as spr.
}

\references{
Spencer, N.A., Ranjan, P., and Mendivil, F., (2019), "Isomorphism Check for \eqn{2^n} Factorial Designs with Randomization Restrictions", Journal of Statistical Theory and Practice, 13(60),1-24 [https://doi.org/10.1007/s42519-019-0064-5]
}

\seealso{
\code{\link{checkSpreadIsomorphism}} for checking the isomorphism of balanced spreads.\cr
\code{\link{checkStarIsomorphism}} for checking the isomorphism of balanced covering stars.
}

\keyword{Collineation}
\author{Neil Spencer, Pritam Ranjan, Franklin Mendivil}

\examples{
## Example 1: relabelling a 1-spread of PG(3,2)
data(spreadn4t2a)
Collin <- cbind(c(1,0,0,1), c(0,0,1,1), c(1,1,1,1), c(0,1,1,1))
# Collin is the collineation matrix corresponding to
# A -> AD, B -> CD, C -> ABCD, D -> BCD
applyCollineation(Collin, spreadn4t2a)



## Example 2: Relabelling a star of PG(4,2) consisting of 4-flats.
data(starn5t3a)
Collin2 <- cbind(c(0,0,0,0,1), c(1,0,0,0,0), c(0,1,0,0,0), c(0,0,0,1,0), c(0,0,1,0,0))
# Collin2 is the collineation matrix corresponding to
# A -> E, B -> A, C -> B, D -> D, E -> C
applyCollineation(Collin2, starn5t3a)
}
