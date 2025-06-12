\name{spreadn6t3b}
\docType{data}
\alias{spreadn6t3b}
\title{Data: A cyclic 2-spread of PG(5,2)}
\description{
   A balanced 2-spread of PG(5,2) obtained via cyclic construction
}
\usage{data(spreadn6t3b)}
\format{
 The spread is formatted as a 3-dimensional array, where the \code{[i,j,k]}-th element indicates whether or not the \code{i}th basic factor is present in the \code{j}th effect of the \code{k}th flat of the spread.
}
\note{
  In R, the data must be loaded using the \code{\link{data}} function.
}

\details{
The spread consists of 9 subspaces \eqn{f_1, f_2, ..., f_9} each of size 7. The subspaces are \cr \eqn{f_1=(ABC,AEF,BCEF,ADEF,BCDEF,D,ABCD)}, \eqn{f_2 = (E,ABCEF,ABCF,BDE,BD,ACDF,ACDEF)}, \eqn{f_3 = (DF,ABCE,ABCDEF,C,CDF,ABE,ABDEF)}, \eqn{f_4 = (B,DEF,BDEF,AF,ABF,ADE,ABDE)}, \eqn{f_5=(A,BDF,ABDF,ABCDE,BCDE,ACEF,CEF)}, \eqn{f_6=(EF,AB,ABEF,ACE,ACF,BCE,BCF)}, \cr \eqn{f_7=(BE,F,BEF,BCDF,CDEF,BCD,CDE)},\cr \eqn{f_8=(ABCDF,ADF,BC,BF,ACD,ABD,CF)} and \cr \eqn{f_9=(AC,AE,CE,DE,ACDE,AD,CD)}.

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
