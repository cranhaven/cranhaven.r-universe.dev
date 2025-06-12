\name{starn8t5a}
\docType{data}
\alias{starn8t5a}
\title{Data: A 4-star of PG(7,2)}
\description{
   A balanced 4-star of PG(7,2)
}
\usage{data(starn8t5a)}
\format{
 The star is formatted as a 3-dimensional array, where the \code{[i,j,k]}-th element indicates whether or not the \code{i}th basic factor is present in the \code{j}th effect of the \code{k}th flat of the star.
}
\note{
  In R, the data must be loaded using the \code{\link{data}} function.
}

\details{
The star consists of nine subspaces \eqn{f_1, f_2, ,..., f_9} of size 31 each. Use \code{vectortostring(starn8t5a)} to see the elements of this star.
}

\references{
Spencer, N.A., Ranjan, P., and Mendivil, F., (2019), "Isomorphism Check for \eqn{2^n} Factorial Designs with Randomization Restrictions", Journal of Statistical Theory and Practice, 13(60),1-24 [https://doi.org/10.1007/s42519-019-0064-5]
}


\keyword{Datasets}
\author{Neil Spencer, Pritam Ranjan, Franklin Mendivil}

\seealso{
\code{\link{checkSpreadIsomorphism}} for checking the isomorphism of balanced spreads.\cr
\code{\link{checkStarIsomorphism}} for checking the isomorphism of balanced covering stars.
}
