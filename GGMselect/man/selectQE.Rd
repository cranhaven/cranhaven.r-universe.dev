\name{selectQE}
\alias{selectQE}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{  Estimate a graph in a Gaussian  Graphical Model: Quasi Exhaustive search}
\description{
  Select a graph within the family of graphs \code{QE}
  }
  \usage{
selectQE(X, dmax=min(3,nrow(X)-3,ncol(X)-1), K=2.5,
   min.ev=10**(-8), max.iter=10**6, max.nG=10**8, max.size=10**8,
   verbose=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{ \code{n x p} matrix where \code{n} is the sample size and
    \code{p} the number of variables. \code{n} should be greater than
    \code{3} and \code{p} greater than \code{1}.}
  \item{dmax}{ integer or \code{p}-dimensional vector of integers
    smaller or equal to
    \code{min(n-3, p-1)}. When \code{dmax} is a scalar, it gives the maximum
    degree of the estimated graph. When  \code{dmax} is a vector,
    \code{dmax[a]} gives the maximum degree of the node \code{a}.}
  \item{K}{ scalar or vector with values greater than 1. Tuning
    parameter in the penalty 
    function.}
  \item{min.ev}{ minimum eigenvalue for matrix inversion. }
  \item{max.iter }{ integer. Maximum number of stepwise
              iterations.}
  \item{max.nG}{ integer.  Maximum number of graphs considered in the 
    exhaustive search. Stepwise procedure beyond. }
  \item{max.size}{ integer. Maximum number of calculations of the
    residuals sums of squares. Execution stopped beyond.}
  \item{verbose}{ logical. If \code{TRUE} a trace of the current process is displayed in real time.}
}
\details{
  More details are available on \url{../doc/Notice.pdf}
}
\value{
   \item{Neighb }{array of dimension  \code{p x max(dmax) x length(K)} or, when
    \code{length(K)} equals 1, matrix of dimension \code{p x max(dmax)}. \code{Neighb[a, , k ]} contains the indices of the nodes connected
  to node \code{a} for \code{K[k]}.}
  \item{crit.min }{vector of dimension \code{length(K)}.
           The minimal values of the selection criterion
           for each value of \code{K}.}
 \item{G}{array of dimension \code{p x p x length(K)} or, when
    \code{length(K)} equals 1, matrix of dimension \code{p x p}.
   \code{G[,,k]} gives the adjacency matrix for \code{K[k]}. }
       }
       \references{Please use \code{citation("GGMselect")}.}
\author{Bouvier A, Giraud C, Huet S, Verzelen N.}
\seealso{ \code{\link{selectFast}}, \code{\link{selectMyFam}},
  \code{\link{simulateGraph}}, \code{\link{penalty}},
\code{\link{convertGraph}}}
\examples{
p=30
n=30
# simulate graph
eta=0.11
Gr <- simulateGraph(p,eta)
# simulate data
X <- rmvnorm(n, mean=rep(0,p), sigma=Gr$C)
# estimate graph
\dontrun{GQE <- selectQE(X)}

# plot the result
\dontrun{library(network)}
\dontrun{par(mfrow=c(1,2))}
\dontrun{gV <- network(Gr$G)}
\dontrun{plot(gV,jitter=TRUE, usearrows = FALSE, label=1:p,displaylabels=TRUE)}
\dontrun{gQE <- network(GQE$G)}
\dontrun{plot(gQE, jitter=TRUE, usearrows = FALSE,  label=1:p,displaylabels=TRUE)}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ multivariate }
\keyword{ graphs }
\keyword{ models }

