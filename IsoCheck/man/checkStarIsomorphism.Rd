\name{checkStarIsomorphism}
\alias{checkStarIsomorphism}
\title{Checking the Isomorphism of Two Stars}
\description{This function checks the isomorphism of two balanced covering stars of \code{PG(n-1,2)}. If they are isomorphic, it returns the list of isomorphism establishing collineations. The option is provided to enumerate all isomorphism establishing collineations or to terminate after the first one is found.}
\usage{checkStarIsomorphism(star1, star2, returnfirstIEC = FALSE)}
\arguments{
  \item{star1}{A star of \code{PG(n-1,2)} stored as a three dimensional binary array (see Details and Examples of \code{\link{checkSpreadEquivalence}} ).}
  \item{star2}{A star of \code{PG(n-1,2)} stored as a three dimensional binary array (see Details and Examples of \code{\link{checkSpreadEquivalence}}).}
  \item{returnfirstIEC}{An indicator of whether all isomorphism establishing collineations (IECs) should be returned (default), or terminate only after the first one is found.}
}

\details{This code considers all possible collineations of \code{PG(n-1,2)} to search for isomorphism establishing collineations from star1 to star2. The search is conducted by first projecting onto a lower dimensional space described as Algorithm 2 in Spencer et al. (2019). Equivalence is assessed using the bitstring comparison method described in Spencer et al. (2019). Both input stars should be formatted as 3-dimensional arrays with \code{star1[i,j,k]} indicating whether or not the \code{i}th basic factor is present in the \code{j}th effect of the \code{k}th flat of star1.
}

\value{
A list containing two objects. The first object is a Boolean indicating whether or not star1 is isomorphic to star2. If isomorphic, the second object is a list of isomorphism establishing collineation matrices. If not isomorphic, the second object is NA.
}

\references{
Spencer, N.A., Ranjan, P., and Mendivil, F., (2019), "Isomorphism Check for \eqn{2^n} Factorial Designs with Randomization Restrictions", Journal of Statistical Theory and Practice, 13(60),1-24 [https://doi.org/10.1007/s42519-019-0064-5]
}
\seealso{
\code{\link{checkSpreadIsomorphism}} for checking the isomorphism of spreads.\cr
\code{\link{checkStarEquivalence}} for checking the equivalence of stars.
}
\keyword{Star Isomorphism}
\author{Neil Spencer, Pritam Ranjan, Franklin Mendivil}

\examples{

## Example 1: Two stars of PG(4,2) consisting of 4-flats
data(starn5t3a)
data(starn5t3b)
test1 <- checkStarIsomorphism(starn5t3a, starn5t3b, returnfirstIEC = TRUE)
test1$result # the test indicates that they are isomorphic
(IECstar <- test1$IECs[[1]]) # the first IEC




## Example 2: Two stars of PG(7,2) consisting of 6-flats
data(starn8t5a)
data(starn8t5b)
test2 <- checkStarIsomorphism(starn8t5a, starn8t5b, returnfirstIEC = TRUE)
test2$result
}
