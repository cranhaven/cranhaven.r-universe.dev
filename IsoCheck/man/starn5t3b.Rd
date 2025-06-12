\name{starn5t3b}
\docType{data}
\alias{starn5t3b}
\title{Data: A 2-star of PG(4,2)}
\description{
   A balanced 2-star of PG(4,2)
}
\usage{data(starn5t3b)}
\format{
 The star is formatted as a 3-dimensional array, where the \code{[i,j,k]}-th element indicates whether or not the \code{i}th basic factor is present in the \code{j}th effect of the \code{k}th flat of the star.
}
\note{
  In R, the data must be loaded using the \code{\link{data}} function.
}

\details{
The star consists of five subspaces \eqn{f_1, f_2, f_3, f_4, f_5} given by 
\eqn{f_1=(ABC,AC,CDE,B,BCDE,ABDE,ADE)}, \eqn{f_2 = (AE,DE,AD,BCE,ABCDE,BCD,ABC)}, \eqn{f_3 = (D,C,CD,ABCD,AB,ABD,ABC)}, \eqn{f_4 = (E,ACDE,ACD,ABCE,BD,ABC,BDE)} and \eqn{f_5=(CE,A,ACE,ABC,BC,BE,ABE)}.

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
