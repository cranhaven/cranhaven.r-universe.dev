\name{spreadn6t2b}
\docType{data}
\alias{spreadn6t2b}
\title{Data: A cyclic 1-spread of PG(5,2)}
\description{
   A balanced 1-spread of PG(5,2) obtained via cyclic construction
}
\usage{data(spreadn6t2b)}
\format{
 The spread is formatted as a 3-dimensional array, where the \code{[i,j,k]}-th element indicates whether or not the \code{i}th basic factor is present in the \code{j}th effect of the \code{k}th flat of the spread.
}
\note{
  In R, the data must be loaded using the \code{\link{data}} function.
}

\details{
The spread consists of 21 subspaces \eqn{f_1, f_2, ..., f_{21}} given by \cr 
\eqn{f_1=(EF,BCD,BCDEF)}, \eqn{f_2 = (A,ACF,CF)}, \eqn{f_3 = (B,ABCDE,ACDE)}, \eqn{f_4 = (DF,F,D)}, \eqn{f_5=(E,BC,BCE)}, \eqn{f_6=(ABC,ABEF,CEF)}, \eqn{f_7=(AEF,ABDF,BDE)}, \eqn{f_8=(AB,BDEF,ADEF)}, \eqn{f_9=(BDF,ABCDEF,ACE)}, \eqn{f_{10}=(DEF,BCF,BCDE)}, \eqn{f_{11}=(ABCE,ACEF,BF)}, \eqn{f_{12}=(BEF,AE,ABF)}, \eqn{f_{13}=(ADF,AC,CDF)}, \eqn{f_{14}=(ABCDF,ABE,CDEF)}, \eqn{f_{15}=(AF,ACDF,CD)}, \eqn{f_{16}=(C,AD,ACD)}, \eqn{f_{17}=(ABDE,CE,ABCD)}, \eqn{f_{18}=(ABDEF,CDE,ABCF)}, \eqn{f_{19}=(ACDEF,BD,ABCEF)}, \eqn{f_{20}=(BCEF,DE,BCDF)}, and \eqn{f_{21}=(BE,ADE,ABD)}.

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
