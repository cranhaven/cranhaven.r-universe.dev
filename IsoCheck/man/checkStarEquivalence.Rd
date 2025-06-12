\name{checkStarEquivalence}
\alias{checkStarEquivalence}
\title{Checking the Equivalence of Two Stars}
\description{This function checks the equivalence of two balanced covering stars of \code{PG(n-1,2)} by comparing sorted bitstring representations.}
\usage{checkStarEquivalence(star1, star2)}
\arguments{
  \item{star1}{A star of \code{PG(n-1,2)} stored as a three dimensional binary array (see Details and Examples of \code{\link{checkSpreadEquivalence}}).}
  \item{star2}{A star of \code{PG(n-1,2)} stored as a three dimensional binary array (see Details and Examples of \code{\link{checkSpreadEquivalence}}).}
}

\details{This code checks if two stars of \code{PG(n-1,2)} are equivalent using the bitstring representation of Spencer et al. (2019). Both input stars should be formatted as 3-dimensional arrays with \code{spread1[i,j,k]} indicating whether or not the \code{i}th basic factor is present in the \code{j}th effect of the \code{k}th flat of spread1.
}

\value{
A Boolean indicating whether or not the two stars are equivalent.
}

\references{
Spencer, N.A., Ranjan, P., and Mendivil, F., (2019), "Isomorphism Check for \eqn{2^n} Factorial Designs with Randomization Restrictions", Journal of Statistical Theory and Practice, 13(60),1-24 [https://doi.org/10.1007/s42519-019-0064-5]
}
\seealso{
\code{\link{checkStarIsomorphism}} for checking the isomorphism of stars.\cr
\code{\link{checkSpreadEquivalence}} for checking the equivalence of spreads.
}

\keyword{Spread Equivalence}
\author{Neil Spencer, Pritam Ranjan, Franklin Mendivil}

\examples{
## Example 1: two non-equivalent stars of PG(4,2)
data(starn5t3a)
data(starn5t3b)

# test their equivalence
(test1 <- checkStarEquivalence(starn5t3a, starn5t3b))





## Example 2: two equivalent stars of PG(7,2) consisting of 6-flats
data(starn8t5a)

#permute the flats and flat order of starn8t5a to create a second equivalent spread equiv_starn8t5a.
equiv_starn8t5a <- starn8t5a
dims <- dim(equiv_starn8t5a)
for(i in 1:(dims[3])){
   equiv_starn8t5a[,,i] <- equiv_starn8t5a[,sample(1:dims[2], dims[2]),i]
}
equiv_starn8t5a <- starn8t5a[,,sample(1:dims[3], dims[3])]

(test2 <- checkStarEquivalence(starn8t5a, equiv_starn8t5a))
}
