\name{selectFast}
\alias{selectFast}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{  Estimate a graph in a Gaussian  Graphical Model: Fast procedure}
\description{
  Select a graph within the (data-driven) families of graphs \code{EW}, \code{C01}, and \code{LA}.
}
\usage{selectFast(X, dmax=min(floor(nrow(X)/3),nrow(X)-3,ncol(X)-1),
           K=2.5, family="EW",
           min.ev=10**(-8), max.iter=200, eps=0.01,
           beta=nrow(X)*nrow(X)/2, tau=1/sqrt(nrow(X)*(ncol(X)-1)), h=0.001, T0=10,
           verbose=FALSE )
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
  \item{K}{ scalar or vector  with values greater than 1. Tuning
    parameter of the penalty 
    function.}
  \item{family}{character string or vector of character strings, among \code{"EW"}, \code{"LA"}, \code{"C01"},
    \code{c("C01","LA")} or \code{c("C01","LA","EW")}.  
}
   \item{min.ev}{ minimum eigenvalue for matrix inversion. }
   \item{max.iter, eps, beta, tau, h, T0}{ tuning parameters for the
     Langevin Monte Carlo algorithm. Only used when
     \code{family} is \code{"EW"} or \code{c("C01","LA","EW")}.}
   \item{verbose}{ logical. If \code{TRUE} a trace of the current process is displayed in real time.}
}
\details{
  More details are available on \url{../doc/Notice.pdf}
}
\value{
  A list with components \code{"EW"}, \code{"LA"}, \code{"C01"},
    \code{"C01.LA"} and \code{"C01.LA.EW"}, according to the
    \code{family} argument, each one with components:
  \item{Neighb }{array of dimension  \code{p x max(dmax) x
      length(K)} or, when \code{length(K)} equals 1,
    matrix of dimension \code{p x max(dmax)}.
   \code{Neighb[a, , k ]} contains the indices
    of the nodes connected
  to node \code{a} for \code{K[k]}.}
  \item{crit.min}{vector of dimension \code{length(K)}.
           It gives the minimal values of the selection criterion
           for each value of \code{K}}
 \item{G}{array of dimension \code{p x p x length(K)} or, when
    \code{length(K)} equals 1, matrix of dimension \code{p x p}.
   \code{G[,,k]} gives the adjacency matrix for \code{K[k]}. }
        }
\references{Please use \code{citation("GGMselect")}.} 
\author{Bouvier A, Giraud C, Huet S, Verzelen N.}
\seealso{ \code{\link{selectQE}}, \code{\link{selectMyFam}},
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
GRest <- selectFast(X, family="C01")

# plot result
library(network)
par(mfrow=c(1,2))
gV <- network(Gr$G)
plot(gV,jitter=TRUE, usearrows = FALSE, label=1:p,displaylabels=TRUE)
g <- network(GRest$C01$G)
plot(g, jitter=TRUE, usearrows = FALSE,  label=1:p,displaylabels=TRUE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ multivariate } 
\keyword{ graphs }
\keyword{ models }

