\name{checkSpreadEquivalence}
\alias{checkSpreadEquivalence}
\title{Checking the Equivalence of Two Spreads}
\description{This function checks the equivalence of two \code{(t-1)}-spreads of \code{PG(n-1,2)} by comparing their sorted bitstring representations.}
\usage{checkSpreadEquivalence(spread1, spread2)}
\arguments{
  \item{spread1}{A \code{(t-1)}-spread of \code{PG(n-1,2)} stored as a three dimensional binary array (see Details and Examples).}
  \item{spread2}{A \code{(t-1)}-spread of \code{PG(n-1,2)} stored as a three dimensional binary array (see Details and Examples).}
}

\details{This code checks if two \code{(t-1)}-spreads of \code{PG(n-1,2)} are equivalent using the bitstring representation of Spencer et al. (2019). Both input spreads should be formatted as 3-dimensional arrays, for example, \code{spread1[i,j,k]} indicates whether or not the \code{i}th basic factor is present in the \code{j}th effect of the \code{k}th flat of spread1.
}

\value{
A Boolean indicating whether or not the two spreads are equivalent.
}

\references{
Spencer, N.A., Ranjan, P., and Mendivil, F., (2019), "Isomorphism Check for \eqn{2^n} Factorial Designs with Randomization Restrictions", Journal of Statistical Theory and Practice, 13(60),1-24 [https://doi.org/10.1007/s42519-019-0064-5]
}
\seealso{
\code{\link{checkSpreadIsomorphism}} for checking the isomorphism of spreads.\cr
\code{\link{checkStarEquivalence}} for checking the equivalence of two stars.
}

\keyword{Spread Equivalence}
\author{Neil Spencer, Pritam Ranjan, Franklin Mendivil}

\examples{
## Example 1: two non-equivalent 1-spreads of PG(3,2)
data(spreadn4t2a)
data(spreadn4t2b)

# test their equivalence
(test1 <- checkSpreadEquivalence(spreadn4t2a, spreadn4t2b))


# direct instantiation of a spread
spreadn4t2c <- array(NA, c(4,3,5))

spreadn4t2c[,1,1] <- c(0, 0, 0, 1)
spreadn4t2c[,2,1] <- c(0, 1, 1, 0)
spreadn4t2c[,3,1] <- c(0, 1, 1, 1)
spreadn4t2c[,1,2] <- c(0, 0, 1, 0)
spreadn4t2c[,2,2] <- c(1, 1, 0, 0)
spreadn4t2c[,3,2] <- c(1, 1, 1, 0)
spreadn4t2c[,1,3] <- c(0, 1, 0, 0)
spreadn4t2c[,2,3] <- c(1, 0, 1, 1)
spreadn4t2c[,3,3] <- c(1, 1, 1, 1)
spreadn4t2c[,1,4] <- c(1, 0, 0, 0)
spreadn4t2c[,2,4] <- c(0, 1, 0, 1)
spreadn4t2c[,3,4] <- c(1, 1, 0, 1)
spreadn4t2c[,1,5] <- c(0, 0, 1, 1)
spreadn4t2c[,2,5] <- c(1, 0, 1, 0)
spreadn4t2c[,3,5] <- c(1, 0, 0, 1)

(test2 <- checkSpreadEquivalence(spreadn4t2a, spreadn4t2c))





## Example 2: two equivalent 2-spreads of PG(5,2)
data(spreadn6t3a)

# permute the flats and flat order of spreadn6t3a to create a 
# second equivalent spread equiv_spreadn6t3a.

equiv_spreadn6t3a <- spreadn6t3a
dims <- dim(equiv_spreadn6t3a)
for(i in 1:(dims[3])){
   equiv_spreadn6t3a[,,i] <- equiv_spreadn6t3a[,sample(1:dims[2], dims[2]),i]
}
equiv_spreadn6t3a <- equiv_spreadn6t3a[,,sample(1:dims[3], dims[3])]

(test3 <- checkSpreadEquivalence(spreadn6t3a, equiv_spreadn6t3a))
}
