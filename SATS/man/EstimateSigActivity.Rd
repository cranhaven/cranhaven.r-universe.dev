
\name{EstimateSigActivity}
\alias{EstimateSigActivity}
\title{Estimate signature activity}
\description{
   Estimation of signature activities given the original 
   mutation type matrix, the panel size matrix, and 
   the catalog signature profile matrix.
}
\usage{ EstimateSigActivity(V, L, W, n.start=50, iter.max=5000, eps=1e-5)
}
\arguments{
  \item{V}{Mutation type matrix or data frame with samples in columns}
  \item{L}{Panel size matrix or data frame with samples in columns, see 
           \code{\link{GenerateLMatrix}}}
  \item{W}{Catalog signature profiles matrix or data frame with signatures in columns}
  \item{n.start}{Number of initializations. The default is 50.}
  \item{iter.max}{Maximum number iterations in the EM algorithm. The default is 5000.}
  \item{eps}{Stopping tolerance in the EM algorithm. The default is 1e-5.}
}
\details{ 
The panel size matrix L and mutation type matrix V are of size P 
(the mutation context) by N (the sample size). 
The catalog signature profile matrix has dimension of P by K 
(the number of signatures). For single base substitutions (SBS), 
P is 96. For the objects V, L, and W, we must have dim(V) = dim(L) 
and ncol(W) = K, where K is the number of signatures.
EstimateSigActivity() uses the EM algorithm to estimate signature 
n.start, iter.max and eps control EM part. Because the convergence 
to a local saddle point can be an issue of the EM algorithm, 
it would be good practice to try multiple initial values 
(n.start, the default is 50). For each initial value, the default 
value of the maximal iteration of the EM algorithm (iter.max) is 5000, 
and the stopping tolerance (eps) is set to 1e-5. For the catalog
signature profile matrix \code{W}, reference SBS TMB signature
profiles in \code{data(SimData)} can be used.
}
\value{ 
  A list containing the estimated activity matrix \code{H}, the
  log-likelihood \code{loglike}, and the logical value \code{converged}.
}
%\references{}
\author{
Donghyuk Lee <dhyuklee@pusan.ac.kr> and Bin Zhu <bin.zhu@nih.gov>
}
\examples{
    data(SimData, package="SATS")
  \donttest{
    EstimateSigActivity(SimData$V, SimData$L, SimData$TrueW_TMB)
  } 
  # For more detailed usage, please refer to README and the user manual
  # in https://github.com/binzhulab/SATS/tree/main.
}
\keyword{ mutational signatures } 
\seealso{ \code{\link{CalculateSignatureBurdens}} }
