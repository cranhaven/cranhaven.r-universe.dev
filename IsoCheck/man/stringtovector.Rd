\name{stringtovector}
\alias{stringtovector}
\title{Converts a character string representation of a factorial effect into the vector form}
\description{Converts a character string representation of a factorial effect in \code{PG(n-1,2)} into a binary vector of length n.}
\usage{stringtovector(string,n)}
\arguments{
  \item{string}{a character string representation of a factorial effect in \code{PG(n-1,2)} (see Details and Examples of \code{\link{checkSpreadEquivalence}}).}\cr
  \item{n}{the number of basic factors, or equivalently, the dimension of \code{PG(n-1,2)}}
}

\details{Takes a character string representation of a factorial effect in \code{PG(n-1,2)}, and returns a binary vector of length n. This can be used in defining a spread or a star. The spread/star should be formatted as a 3-dimensional array with \code{spread[i,j,k]}/\code{star[i,j,k]} indicating whether or not the \code{i}th basic factor is present in the \code{j}th effect of the \code{k}th flat of \code{spread}/\code{star}. See Spencer et al. (2019) for details.
}

\value{
A binary vector of length n.
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
## Example : Converts "AC" into a vector representation
stringtovector("AC",4)
stringtovector("AC",5)
stringtovector("CD",6)

}
