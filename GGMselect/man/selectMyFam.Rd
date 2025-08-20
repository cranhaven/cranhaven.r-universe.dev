\name{selectMyFam}
\alias{selectMyFam}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Select a graph within a given family of graphs in  Gaussian Graphical Modeling.}
\description{
  Select a graph within a given family of graphs.
  }
  \usage{
selectMyFam(X, MyFamily, K=2.5, min.ev=10**(-8))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{ \code{n x p} matrix where \code{n} is the sample size and
    \code{p} the number of variables. \code{n} should be greater than
    \code{3} and \code{p} greater than \code{1}.}
  \item{MyFamily }{ list of pxp adjacency matrices corresponding to
    graphs with degree less or equal to \code{n-3}.}
  \item{K}{ scalar or vector with values larger than 1. Tuning
    parameter of the penalty 
    function.}
  \item{min.ev}{ minimum eigenvalue for matrix inversion. }
}
\note{
  Adjacency matrices can be generated from lists of connected nodes
  by using the function \code{\link{convertGraph}}}
\details{
  More details are available on \url{../doc/Notice.pdf}
}
\value{
   \item{Neighb }{array of dimension  \code{p x dmax x
       length(K)} where \code{dmax} is the maximum degree of the graphs
     in \code{MyFamily}. When \code{K} is of length 1, matrix of
     dimension \code{p x dmax}.
     \code{Neighb[a, , k ]} contains the indices
     of the nodes connected
  to node \code{a} for \code{K[k]}.}
  \item{crit.min }{vector of dimension \code{length(K)}.
           The minimal values of the selection criterion
           for each value of \code{K}.}
	 \item{ind.min}{vector of dimension \code{length(K)}.
	   Indices of the families for which the  criterion is minimum.} 	 
	 \item{G}{array of dimension \code{p x p x length(K)} or,
	   when \code{length(K)} equals 1, matrix of
   dimension \code{p x p}.
   \code{G[,,k]} gives the adjacency matrix for \code{K[k]}. }
       }
 \references{Please use \code{citation("GGMselect")}.} 
\author{Bouvier A, Giraud C, Huet S, Verzelen N.}
\seealso{ \code{\link{selectFast}}, \code{\link{selectQE}},
  \code{\link{simulateGraph}}, \code{\link{penalty}},
\code{\link{convertGraph}}}
\examples{
p=30
n=30
# generate graph
eta=0.11
Gr <- simulateGraph(p,eta)
# generate data
X <- rmvnorm(n, mean=rep(0,p), sigma=Gr$C)
# generate a family of candidate graphs with glasso
library("glasso")
MyFamily <- NULL
for (j in 1:3){
  MyFamily[[j]] <- abs(sign(glasso(cov(X),rho=j/5)$wi))
  diag(MyFamily[[j]]) <- 0
}
# select a graph within MyFamily
GMF <- selectMyFam(X,MyFamily)
# plot the result
library(network)
par(mfrow=c(1,2))
gV <- network(Gr$G)
plot(gV,jitter=TRUE, usearrows = FALSE, label=1:p,displaylabels=TRUE)
gMyFam <- network(GMF$G)
plot(gMyFam, jitter=TRUE, usearrows = FALSE,  label=1:p,displaylabels=TRUE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ models }
\keyword{ multivariate } 
\keyword{ graphs } 
