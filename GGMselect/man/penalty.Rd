\name{penalty}
\alias{penalty}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Penalty function}
\description{
  Compute the penalty function of GGMselect.
}
\usage{
penalty(p,n, dmax=min(3,n-3,p-1), K=2.5) 
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{p}{ the number of variables. \code{p} should be greater than \code{1}.}
  \item{n}{ the sample size. \code{n} should be greater than
    \code{3}.}
 \item{dmax}{ integer or \code{p}-dimensional vector of integers
    smaller or equal to
    \code{min(n-3, p-1)}. When \code{dmax} is a scalar, it gives the maximum
    degree of the estimated graph. When  \code{dmax} is a vector,
    \code{dmax[a]} gives the maximum degree of the node \code{a}. Default
    value: \code{min(3,n-3,p-1)}.}
   \item{K}{scalar or vector of real numbers larger than 1. Tuning parameter of the penalty function.}
 }
\details{
  More details are available on \url{../doc/Notice.pdf}
}
\value{
   A matrix of dimension \code{(max(Dmax)+1) x length(K)}. 
   The entry \code{[d+1,k]} gives the value of the penalty for the dimension \code{d} and the parameter
   \code{K[k]}. 
}
\references{ Please use \code{citation("GGMselect")} }
\author{Bouvier A, Giraud C, Huet S, Verzelen N }

\seealso{ \code{\link{selectQE}}, \code{\link{selectMyFam}},
  \code{\link{selectFast}}, \code{\link{simulateGraph}},
\code{\link{convertGraph}}}
  
\examples{
p=30
n=30
pen <- penalty(p,n, 3)
}
\keyword{ models }
\keyword{ graphs }
\keyword{ multivariate }

