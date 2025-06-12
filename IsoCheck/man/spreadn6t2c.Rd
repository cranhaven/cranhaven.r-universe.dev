\name{spreadn6t2c}
\docType{data}
\alias{spreadn6t2c}
\title{Data: A cyclic 1-spread of PG(5,2)}
\description{
   A balanced 1-spread of PG(5,2) obtained via cyclic construction
}
\usage{data(spreadn6t2c)}
\format{
 The spread is formatted as a 3-dimensional array, where the \code{[i,j,k]}-th element indicates whether or not the \code{i}th basic factor is present in the \code{j}th effect of the \code{k}th flat of the spread.
}
\note{
  In R, the data must be loaded using the \code{\link{data}} function.
}

\details{
The spread consists of 21 subspaces \eqn{f_1, f_2, ..., f_{21}} given by 
\eqn{f_1=(F,ABCEF,ABCE)}, \eqn{f_2 = (E,ABDF, ABDEF)}, \eqn{f_3 = (D,ACF, ACDF)}, \eqn{f_4 = (C,BF, BCF)}, \eqn{f_5=(B,AE, ABE)}, \eqn{f_6=(A,DEF, ADEF)}, \eqn{f_7=(EF,CDE,CDF)}, \eqn{f_8=(DE,BCD,BCE)}, \eqn{f_9=(CD,ABC,ABD)}, \eqn{f_{10}=(BC,ABEF,ACEF)}, \eqn{f_{11}=(AB,ADF,BDF)}, \eqn{f_{12}=(DF,BE,BDEF)}, \eqn{f_{13}=(CE,AD,ACDE)}, \eqn{f_{14}=(AC,BDE,ABCDE)}, \eqn{f_{15}=(BEF,ACD,ABCDEF)}, \eqn{f_{16}=(ADE,BCEF,ABCDF)}, \eqn{f_{17}=(CDEF,ABDE,ABCF)}, \eqn{f_{18}=(BCDE,ACDEF,ABF)}, \eqn{f_{19}=(ACE,AF,CEF)}, \eqn{f_{20}=(CF,BD,BCDF)}, and \eqn{f_{21}=(ABCD,AEF,BCDEF)}.

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
