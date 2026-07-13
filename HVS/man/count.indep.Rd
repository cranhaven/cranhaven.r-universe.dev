\name{count.indep}
\alias{count.indep}
\alias{count.vc}
\title{
Count the number of independent sets for a graph
}
\description{
Counts the number of independent sets (or equivalently vertex covers) in
a graph. 
}
\usage{
count.indep(G,cutoff,method="exact",samp.size=1000)
count.vc(G,cutoff,method="exact",samp.size=1000)
}
\arguments{
  \item{G}{
    The graph, represented as a square matrix. The edges are the values
    smaller than the cutoff parameter. 
  }
  \item{cutoff}{
    A value separating edges and non-edges of the graph.
  }
  \item{method}{
    A list of methods to use. The options are "exact" and
    "importance_sample". The "exact" method is computationally
    expensive, but counts the exact number of independent sets (or
    vertex covers). The "importance_sample" method uses a sequential
    importance sampling scheme to approximate the number of independent sets. 
  }
  \item{samp.size}{
    For the approximate methods, this is the number of samples used to
    approximate the number of independent sets. Larger values take
    proportionately longer, but produce more accurate values.
  }
}
\details{
  An independent set for a graph is a set that contains no edges. A
  vertex cover is a set that has non-empty intersection with every
  edge. The complement of an independent set is a vertex cover. These
  functions count the number of independent sets for a graph. The
  "exact" method exactly counts the number of independent sets by
  recursively partitioning into sets that contain a specific vertex and
  sets that do not. Since counting the number of independent sets in a
  graph is NP-hard, this is computationally expensive. The
  "importance_sample" method uses a random importance sampling scheme
  that sequentially samples independent sets starting from the empty set
  and adding one element each time. The samp.size parameter is the
  number of such sequences generated. 

  Both the exact and independent versions of this function are much
  faster than the corresponding hypergraph versions, due to a more
  efficient encoding of the graph structure.
}
\value{
  The number of independent sets (exact or approximate).
}
\references{
Hypergraph Variable Selection with False Discovery Rate Control

Sarah Organ, Toby Kenney, Hong Gu

http://arxiv.org/abs/2606.20514
}
\author{
Toby Kenney,  Sarah Organ
}
\examples{
set.seed(1)
G<-matrix(runif(400),20,20)
G<-pmin(G,t(G)) # make G symmetric 

ind.set<-count.indep(G,0.05,method=c("exact","importance_sample"),)
}
