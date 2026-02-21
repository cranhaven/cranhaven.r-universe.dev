
\name{CalculateSignatureBurdens}
\alias{CalculateSignatureBurdens}
\title{Calculate signature burdens}
\description{
   Estimation of the expected number of mutations attributed 
   by TMB-based catalog signatures (signature burden) given 
   the panel size matrix, the catalog signature profile matrix 
   and the signature activities matrix.
}
\usage{ CalculateSignatureBurdens(L, W, H)
}
\arguments{
    \item{L}{Panel size matrix or data frame with samples in columns, see 
             \code{\link{GenerateLMatrix}}}
    \item{W}{Catalog signature profiles matrix or data frame with signatures in columns}
    \item{H}{Activity matrix or data frame with samples in columns, 
             see \code{\link{EstimateSigActivity}} }
}
\details{ 
The panel size matrix L is of size P (the mutation context) by N 
(the sample size). The catalog signature profile matrix has dimension 
of P by K (the number of signatures) and the activity matrix H is of
 size K by N. For single base substitutions (SBS), P is 96. 
If \code{K} is the number of signatures and \code{N} is
  the number of samples, then \code{H} must be of dimension \code{K X N},
  \code{ncol(L) = N}, and \code{ncol(W) = K}.
For the catalog signature profile matrix \code{W}, reference SBS TMB
signature profiles in \code{data(SimData)} can be used. 
}
\value{ 
  A matrix of dimension \code{K X N}, where \code{K} is the number
  of signatures and \code{N} is the number of samples.
}
%\references{}
\author{
Donghyuk Lee <dhyuklee@pusan.ac.kr> and Bin Zhu <bin.zhu@nih.gov>
}
\examples{
    data(SimData, package="SATS")

    CalculateSignatureBurdens(SimData$L, SimData$TrueW_TMB, SimData$TrueH)
    # For more detailed usage, please refer to README and the user manual
    # in https://github.com/binzhulab/SATS/tree/main.
}
\keyword{ mutational signatures } 
\seealso{ \code{\link{EstimateSigActivity}} }
