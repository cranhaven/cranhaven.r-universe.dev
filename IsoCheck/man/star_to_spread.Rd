\name{star_to_spread}
\alias{star_to_spread}
\title{Converts a star to its corresponding spread}
\description{A function that converts a balanced covering star \code{St(n, mu, t, t_0)}  of \code{PG(n-1,2)} to its corresponding balanced \code{((t-t_0)-1)}-spread of \code{PG((n-t_0)-1,2)}.}
\usage{star_to_spread(star)}
\arguments{
  \item{star}{A balanced covering star of \code{PG(n-1,2)} stored as a three dimensional binary array (see Details and Examples of \code{\link{checkSpreadEquivalence}}).}
}

\details{Finds a balanced \code{((t-t_0)-1)}-spread of \code{PG((n-t_0)-1,2)} embedded in \code{PG(n-1,2)} as conformable with the geometry of a balanced covering star \code{St(n, mu, t, t_0)} of \code{PG(n-1,2)}. The star should be formatted as a 3-dimensional array with \code{star[i,j,k]} indicating whether or not the \code{i}th basic factor is present in the \code{j}th effect of the \code{k}th flat of \code{star}. See Spencer et al. (2019) for details.
}

\value{
A balanced \code{((t-t_0)-1)}-spread of \code{PG((n-t_0)-1,2)}
}

\references{
Spencer, N.A., Ranjan, P., and Mendivil, F., (2019), "Isomorphism Check for \eqn{2^n} Factorial Designs with Randomization Restrictions", Journal of Statistical Theory and Practice, 13(60),1-24 [https://doi.org/10.1007/s42519-019-0064-5]
}

\seealso{
\code{\link{is.spread}} for checking if the input is a balanced spread.\cr 
\code{\link{checkSpreadIsomorphism}} for checking the isomorphism of balanced spreads.\cr
\code{\link{checkStarIsomorphism}} for checking the isomorphism of balanced covering stars.
}

\keyword{Star}
\author{Neil Spencer, Pritam Ranjan, Franklin Mendivil}

\examples{
## Example 1: checking whether "starn8t5a" is a proper star
data(starn8t5a)
star_to_spread(starn8t5a)



## Example 2: checking whether "starn5t3a" is a proper star
data(starn5t3a)
star_to_spread(starn5t3a)
}
