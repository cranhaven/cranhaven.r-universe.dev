\name{vectortostring}
\alias{vectortostring}
\title{Converts a binary vector to character strings}
\description{Converts a binary vector or matrix in \code{PG(n-1,2)} into string-vector/matrix .}
\usage{vectortostring(arry)}
\arguments{
  \item{arry}{A binary vector or a matrix of binary vectors representing a flat or spread/star in \code{PG(n-1,2)} (see Details and Examples of \code{\link{checkSpreadEquivalence}}).}
}

\details{Takes an array (a binary vector or an array of binary vectors, upto three dimensions) and returns the character string representation of the vectors. This can be used for reporting spreads and stars in an easy-to-read format. Recall that a sprad/star should be formatted as a 3-dimensional array with \code{star[i,j,k]} indicating whether or not the \code{i}th basic factor is present in the \code{j}th effect of the \code{k}th flat of \code{star}. See Spencer et al. (2019) for details.
}

\value{
Character string representation of the input \code{arry}.
}

\references{
Spencer, N.A., Ranjan, P., and Mendivil, F., (2019), "Isomorphism Check for \eqn{2^n} Factorial Designs with Randomization Restrictions", Journal of Statistical Theory and Practice, 13(60),1-24 [https://doi.org/10.1007/s42519-019-0064-5]
}

\seealso{
\code{\link{checkSpreadIsomorphism}} for checking the isomorphism of balanced spreads.\cr
\code{\link{checkStarIsomorphism}} for checking the isomorphism of balanced covering stars.
}

\keyword{Miscellaneous}
\author{Neil Spencer, Pritam Ranjan, Franklin Mendivil}

\examples{
## Example 1: converts c(0,1,1,0) into "BC"
vec = c(0,1,1,0)
vectortostring(vec)



## Example 2: converts "spreadn6t3a" into character string representation
data(spreadn6t3a)
vectortostring(spreadn6t3a)



## Example 3: converts "starn5t3a" into character string representation
data(starn5t3a)
vectortostring(starn5t3a)
}
