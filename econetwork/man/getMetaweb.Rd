\name{getMetaweb}
\alias{getMetaweb}
\title{
Get metaweb 
}
\description{
Computation of the binary metaweb from a list of graph
}
\usage{
getMetaweb(gList)
}
\arguments{
  \item{gList}{A \code{list} of graph objects of class \code{igraph}.}
}
\details{
  This function computes the metaweb from a list of graph. It computes the union (in the sense of graph theory) of the set of graphs.
}
\value{
\code{getMetaweb} returns an object of class \code{igraph}.
}
\references{
Marc Ohlmann, Vincent Miele, Stephane Dray, Loic Chalmandrier, Louise O'Connor & Wilfried Thuiller, Diversity indices for ecological networks: a unifying framework using Hill numbers. Ecology Letters (2019) <doi:10.1111/ele.13221>
}
\author{
Authors: Stephane Dray, Vincent Miele, Marc Ohlmann, Wilfried Thuiller
Maintainer: Wilfried Thuiller <wilfried.thuiller@univ-grenoble-alpes.fr>
}
\examples{
# Generating a set of Erdos-Renyi graphs
library(igraph)
gList <- c()
for(i in 1:4){
  graphLocal <- erdos.renyi.game(60, type = 'gnp', p.or.m=0.1, directed=TRUE)
  V(graphLocal)$name <- as.character(1:60)
  gList <- c(gList, list(graphLocal))
}
names(gList) <- c("A","B","C","D")

## building the metaweb
graphMetaweb <- getMetaweb(gList)
}