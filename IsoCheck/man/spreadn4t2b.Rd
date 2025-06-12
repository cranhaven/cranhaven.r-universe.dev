\name{spreadn4t2b}
\docType{data}
\alias{spreadn4t2b}
\title{Data: A cyclic 1-spread of PG(3,2)}
\description{
   A balanced 1-spread of PG(3,2) obtained via cyclic construction
}
\usage{data(spreadn4t2b)}
\format{
 The spread is formatted as a 3-dimensional array, where the \code{[i,j,k]}-th element indicates whether or not the \code{i}th basic factor is present in the \code{j}th effect of the \code{k}th flat of the spread.
}
\note{
  In R, the data must be loaded using the \code{\link{data}} function.
}

\details{
The spread consists of five subspaces \eqn{f_1, f_2, f_3, f_4, f_5} given by 
\eqn{f_1=(A,CD,ACD)}, \eqn{f_2 = (C,ABCD, ABD)}, \eqn{f_3 = (D,B, BD)}, \eqn{f_4 = (ABC,AD, BCD)} and \eqn{f_5=(AC,AB,BC)}.

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
