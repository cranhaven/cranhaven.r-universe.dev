\name{checkSpreadIsomorphism}
\alias{checkSpreadIsomorphism}
\title{Checking the Isomorphism of Two Spreads}
\description{This function checks the isomorphism of two \code{(t-1)}-spreads of \code{PG(n-1,2)}. If they are isomorphic, it returns the list of isomorphism establishing collineations (IECs). The option is provided to enumerate all IECs or to terminate after the first one is found.}

\usage{checkSpreadIsomorphism(spread1, spread2, returnfirstIEC = FALSE, printstatement = TRUE)}

\arguments{
  \item{spread1}{A \code{(t-1)}-spread of \code{PG(n-1,2)} stored as a three dimensional binary array (see Details and Examples of \code{\link{checkSpreadEquivalence}}).}
  \item{spread2}{A \code{(t-1)}-spread of \code{PG(n-1,2)} stored as a three dimensional binary array (see Details and Examples of \code{\link{checkSpreadEquivalence}}).}
  \item{returnfirstIEC}{An indicator to indicate whether all isomorphism establishing collineations should be returned (default), or terminate only after the first one is found.}
  \item{printstatement}{If set to true (default), running the function also prints a sentence declaring the isomorphism of the spreads.}
}

\details{This code considers all possible collineations of \code{PG(n-1,2)} to search for isomorphism establishing collineations (IECs) from spread1 to spread2. The search is conducted over the reduced space described in Algorithm 1 of Spencer et al. (2019). Equivalence is assessed using the bitstring comparison method described in Spencer et al. (2019). 

Both input spreads should be formatted as 3-dimensional arrays with \code{spread1[i,j,k]} indicating whether or not the \code{i}th basic factor is present in the \code{j}th effect of the \code{k}th flat of spread1.
}

\value{
A list containing two objects. The first object is a Boolean indicating whether or not spread1 is isomorphic to spread2. If isomorphic, the second object is a list of isomorphism establishing collineation matrices. If not isomorphic, the second object is NA.
}

\references{
Spencer, N.A., Ranjan, P., and Mendivil, F., (2019), "Isomorphism Check for \eqn{2^n} Factorial Designs with Randomization Restrictions", Journal of Statistical Theory and Practice, 13(60),1-24 [https://doi.org/10.1007/s42519-019-0064-5]
}
\seealso{
\code{\link{checkStarIsomorphism}} for checking the isomorphism of balanced covering stars.\cr
\code{\link{checkSpreadEquivalence}} for checking the equivalence of balanced spreads.
}
\keyword{Spread Isomorphism}
\author{Neil Spencer, Pritam Ranjan, Franklin Mendivil}

\examples{
## Example 1: two 1-spreads of PG(3,2)
data(spreadn4t2a)
data(spreadn4t2b)
# test their isomorphism
test1 <- checkSpreadIsomorphism(spreadn4t2a, spreadn4t2b)
test1$result # the test indicates that they are isomorphic
(IEC1 <- (test1$IECs)[[1]])
# we store the first isomorphism establishing collineation as IEC1




## Example 2: two 2-spreads of PG(5,2) using returnfirstIEC to cut down on runtime
data(spreadn6t3a)
data(spreadn6t3b)
test2 <- checkSpreadIsomorphism(spreadn6t3a, spreadn6t3b, returnfirstIEC = TRUE)
test2$result # the test indicates that they are isomorphic




## Example 3: non-isomorphic 1-spreads of PG(5,2)
data(spreadn6t2a)
data(spreadn6t2c)

# A bit slow for official example
# test3 <- checkSpreadIsomorphism(spreadn6t2a, spreadn6t2c, returnfirstIE#C = TRUE)
# test3$result




## Example 4: isomorphic 1-spreads of PG(5,2)
data(spreadn6t2a)
data(spreadn6t2b)
test4 <- checkSpreadIsomorphism(spreadn6t2a, spreadn6t2b, returnfirstIEC = TRUE)
test4$result # the test indicates that they are isomorphic
}
