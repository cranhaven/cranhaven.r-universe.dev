\name{spreadn6t3a}
\docType{data}
\alias{spreadn6t3a}
\title{Data: A cyclic 2-spread of PG(5,2)}
\description{
   A balanced 2-spread of PG(5,2) obtained via cyclic construction
}
\usage{data(spreadn6t3a)}
\format{
 The spread is formatted as a 3-dimensional array, where the \code{[i,j,k]}-th element indicates whether or not the \code{i}th basic factor is present in the \code{j}th effect of the \code{k}th flat of the spread.
}
\note{
  In R, the data must be loaded using the \code{\link{data}} function.
}

\details{
The spread consists of 9 subspaces \eqn{f_1, f_2, ..., f_9} each of size 7. The subspaces are \cr \eqn{f_1=(A,EF,AEF,BCE,ABCE,BCF,ABCF)}, \eqn{f_2 = (B,AF,ABF,CDF,BCDF,ACD,ABCD)}, \eqn{f_3 = (C,AB,ABC,ADE,ACDE,BDE,BCDE)}, \eqn{f_4 = (D,BC,BCD,BEF,BDEF,CEF,CDEF)}, \eqn{f_5=(E,CD,CDE,ACF,ACEF,ADF,ADEF)}, \eqn{f_6=(F,DE,DEF,ABD,ABDF,ABE,ABEF)}, \eqn{f_7=(BD,BF,DF,ACE,ABCDE,ABCEF,ACDEF)},\cr \eqn{f_8=(AC,CE,AE,BDF,ABCDF,BCDEF,ABDEF)} \cr and \eqn{f_9=(AD,BE,ABDE,CF,ACDF,BCEF,ABCDEF)}.

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
