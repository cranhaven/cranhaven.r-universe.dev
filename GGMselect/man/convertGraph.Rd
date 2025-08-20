\name{convertGraph}
\alias{convertGraph}
\title{Convert graphs into adjacency matrices }
\description{
  Convert into adjacency matrices \code{NG} graphs (expressed as
  lists of connected nodes)
}
\usage{
convertGraph(Graph)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Graph}{array of dimension \code{p x Dmax x NG}, where
    \code{Dmax} is the degree of the graph and \code{NG}  the
    number of graphs. If \code{NG}  is equal to 1, \code{Graph}
    can be a matrix
    of dimension \code{p x Dmax}.
    
   \code{Graph[a,,iG]} should be the indices of the nodes connected to the node \code{a},   for the graph \code{iG};\cr
 \code{Graph[a,1,iG]} should be equal to 0 if there is no node connected to the node \code{a}. }
}
\value{
  An array of dimension \code{p x p x NG}, or,
  when \code{NG}  is equal to 1, a matrix
  of dimension \code{p x p}.
  
  The entry \code{[,,iG]} is a symmetric matrix, with diagonal equal to
  zero. The entry \code{[a,b,iG]} is equal to 1 if \code{a} is connected to \code{b},
  0 otherwise.
}
\references{ Please use \code{citation("GGMselect")} }
\author{Bouvier A, Giraud C, Huet S, Verzelen N }

\seealso{ \code{\link{selectQE}}, \code{\link{selectMyFam}},
  \code{\link{selectFast}}, \code{\link{simulateGraph}},
  \code{\link{penalty}}}

\note{
  This function is  useful to generate the entry
  \code{MyFamily} of the function \code{\link{selectMyFam}}.
  Actually, the list of adjacency matrices \code{MyFamily}
  can be generated from lists of connected nodes with
  \code{convertGraph}.}
\examples{
p=30
n=30
# simulate graph
eta=0.11
Gr <- simulateGraph(p,eta)
X <- rmvnorm(n, mean=rep(0,p), sigma=Gr$C)
# estimate graph
GRest <- selectFast(X, family="C01")
# Neighb and G are 2 forms of the same result
a <- convertGraph(GRest$C01$Neighb)
print(all.equal(a, GRest$C01$G)) # TRUE
# recalculate the graph with selectMyFam
GMF <- selectMyFam(X, list(a))
print(all.equal(a,GMF$G)) # TRUE
}
\keyword{ multivariate } 
\keyword{ graphs }

