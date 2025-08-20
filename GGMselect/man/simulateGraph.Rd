\name{simulateGraph}
\alias{simulateGraph}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Generate sparse Gaussian Graphical Models}
\description{
Generate random covariance matrices \code{C} with sparse inverse. The
Gaussian law \code{N(0,C)} is then a sparse
(non-uniform) Gaussian Graphical Model.
}
\usage{
simulateGraph(p, eta, extraeta = eta/5)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{p}{ integer. Number of rows and columns  of \code{C}. Should be
    greater than \code{1}.}
  \item{eta}{ real number in (0,1). Proportion of edges in
    subgroups. Small values of \code{eta} give  sparse graphs. }
  \item{extraeta}{ real number in (0,1). Proportion of edges inter groups. }
}
\details{
  More details are available on \url{../doc/Notice.pdf}
}
\value{
  \item{G }{ p x p matrix. Adjacency matrix of the graph.}
  \item{Dmax}{integer. Maximum degree of the graph.}
  \item{Neighb }{array of dimension  \code{p x Dmax}.
   \code{Neighb[a, ]} contains the indices
    of the nodes connected
  to node \code{a}.}
  \item{Nnodes}{ integer. Number of nodes.}
  \item{C}{ p x p matrix. Covariance matrix.}
  \item{PCor}{ p x p matrix. Partial correlation matrix.}
}
       \references{Please use \code{citation("GGMselect")}.}
\author{Bouvier A, Giraud C, Huet S, Verzelen N }

\seealso{ \code{\link{selectQE}}, \code{\link{selectMyFam}},
  \code{\link{selectFast}}, \code{\link{penalty}},
\code{\link{convertGraph}}}
  
\examples{
# simulate a graph
p=30
eta=0.13
Gr <- simulateGraph(p,eta)

# plot the graph
library(network)
par(mfrow=c(1,1))
gV <- network(Gr$G)
plot(gV,jitter=TRUE, usearrows = FALSE, label=1:p,displaylabels=TRUE)}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ multivariate }
\keyword{ graphs }
\keyword{ models }
\keyword{ datagen }
