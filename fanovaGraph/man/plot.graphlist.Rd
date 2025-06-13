\name{plot.graphlist}
\alias{plot.graphlist}
\alias{plot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Plot Graph via Package \code{\link[igraph:igraph-package]{igraph}}
}
\description{
Plot FANOVA graphs using functions from package \code{\link[igraph:igraph-package]{igraph}}.}
\usage{
\method{plot}{graphlist}(x, names = NULL, i2 = NULL, layout = NULL, plot.i1=TRUE, max.thickness=15, 
circle.diameter=40, ...)
}
\arguments{
  \item{x}{an object of class \code{graphlist} as obtained from \code{\link{estimateGraph}}
}
  \item{names}{optional character string, names of vertices, defaults to \code{1:d}
}
    \item{i2}{optional vector of second order interaction indices (thickness of inner edges)
}
    \item{plot.i1}{optional boolean, if TRUE main effects are drawn in the graph by vertices thicknesses, should be set to FALSE when only total interaction indices are of interest
}

  \item{layout}{optional layout for the graph as in \code{\link[igraph:igraph-package]{igraph}},
  default is \code{layout.fruchterman.reingold}
}
  \item{max.thickness}{optional value for the maximal line thickness, defaults to 20
}
  \item{circle.diameter}{optional value for the circle diameter, defaults to 40
}
  \item{...}{additional arguments, passed to \code{\link{plot}}
}
}
\references{
Muehlenstaedt, T.; Roustant, O.; Carraro, L.; Kuhnt, S. (2011) Data-driven Kriging models based on FANOVA-decomposition, \emph{Statistics and Computing}.

Csardi, G.; Nepusz, T. (2006) The igraph software package for complex network research, \emph{InterJournal Complex Systems}, \bold{Complex Systems}, 1695.
}
\author{J. Fruth, O. Roustant, S. Hess, S. Neumaerker
}

\seealso{
\code{\link{plotGraphChange}}
}
\examples{
op <- par(no.readonly=TRUE)
g1 <- estimateGraph(f.mat=ishigami.fun, d=3, q.arg=list(min=-pi,max=pi), n.tot=10000)
plot(g1)
plot(g1, names=c("A","B","C"))
plot(g1, names=c("A","B","C"), plot.i1 = FALSE)

# include pure second order indices
g2 <- estimateGraph(f.mat=function(x) x[,1]*x[,2]*x[,3]+x[,2]*x[,3], d=3, 
  q.arg=list(min=-1,max=1), n.tot=10000)

plot(g2)
plot(g2, i2 = c(0.001, 0.001, 0.05))

# equal layouts and different edge thicknesses and circle diameters
g3 <- estimateGraph(f.mat=function(x) x[,1]*x[,2]*x[,3]*x[,4]*x[,5], d=5, 
  q.arg=list(min=-1,max=1), n.tot=10000)

g4 <- estimateGraph(f.mat=function(x) x[,1]*x[,2]*x[,3]+x[,4]*x[,5], d=5, 
  q.arg=list(min=-1,max=1), n.tot=10000)

graphClassIgraph <- graph.full(n = 5, directed = FALSE)
layout <- layout.circle(graphClassIgraph)

plot(g3, max.thickness= 10, circle.diameter= 30, layout=layout)
plot(g4, max.thickness= 30, circle.diameter= 50, layout=layout)
}