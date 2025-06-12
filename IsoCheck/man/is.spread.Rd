\name{is.spread}
\alias{is.spread}
\title{Boolean check for a proper Spread}
\description{Checks whether or not the input is a proper balanced (t-1)-spread of \code{PG(n-1,2)}.}
\usage{is.spread(spr, printstatements = TRUE)}
\arguments{
  \item{spr}{A balanced spread of \code{PG(n-1,2)} stored as a three dimensional binary array (see Details and Examples of \code{\link{checkSpreadEquivalence}}).}
  \item{printstatements}{A Boolean indicating whether or not to print possible reasons for not being a spread if the input is not a spread.}
}

\details{Checks whether or not the input is a proper balanced (t-1)-spread of \code{PG(n-1,2)}. The spread should be formatted as a 3-dimensional array with \code{spr[i,j,k]} indicating whether or not the \code{i}th basic factor is present in the \code{j}th effect of the \code{k}th flat of \code{spr}. See Spencer et al. (2019) for details.
}

\value{
A Boolean indicating whether or not the input is a spread.
}

\references{
Spencer, N.A., Ranjan, P., and Mendivil, F., (2019), "Isomorphism Check for \eqn{2^n} Factorial Designs with Randomization Restrictions", Journal of Statistical Theory and Practice, 13(60),1-24 [https://doi.org/10.1007/s42519-019-0064-5]
}

\seealso{
\code{\link{is.star}} for checking if the input is a balanced covering star.\cr 
\code{\link{checkSpreadIsomorphism}} for checking the isomorphism of balanced spreads.\cr
\code{\link{checkStarIsomorphism}} for checking the isomorphism of balanced covering stars.
}

\keyword{Spread}
\author{Neil Spencer, Pritam Ranjan, Franklin Mendivil}

\examples{
## Example 1: checking whether "spreadn4t2a" is a proper spread
data(spreadn4t2a)
is.spread(spreadn4t2a)



## Example 2: checking whether "starn5t3a" is a proper spread
data(starn5t3a)
is.spread(starn5t3a)
}
