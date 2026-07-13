\name{count.indep.hyper}
\alias{count.indep.hyper}
\alias{count.vc.hyper}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Count the number of independent sets for a hypergraph
}
\description{
Counts the number of independent sets (or equivalently vertex covers) in a hypergraph.
}
\usage{
count.indep.hyper(H,method="exact",samp.size=1000)
count.vc.hyper(H,method="exact",samp.size=1000)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{H}{
    The hypergraph, represented as a list with two components:
    \describe{
      \item{vertices}{The number of vertices in the hypergraph.}
      \item{edges}{a list of edges. Each edge is a vector of vertex numbers.}
    }
  }
  \item{method}{
    A list of methods to use. The options are "exact" and "approx". 
    The "exact" method is computationally expensive, but counts the
    exact number of independent sets (or vertex covers). The "approx"
    method uses a sequential importance sampling scheme to approximate
    the number of independent sets. 
  }
  \item{samp.size}{
    For the "approx" method, this is the number of samples used to
    estimate the number of independent sets. Larger values take
    proportionately longer, but produce more accurate values.
  }
}
\details{
  An independent set for a hypergraph is a set that contains no edges. A
  vertex cover is a set that has non-empty intersection with every
  edge. The complement of an independent set is a vertex cover, so there
  are the same number of independent sets as vertex covers. These
  functions count the number of independent sets for a hypergraph. The
  "exact" method exactly counts the number of independent sets by
  recursively partitioning into sets that contain a specific vertex and
  sets that do not. Since counting the number of independent sets in a
  hypergraph is NP-hard, this is computationally expensive. The
  "approx" method uses a random importance sampling scheme
  that sequentially samples independent sets starting from the empty set
  and adding one element each time. The samp.size parameter is the
  number of such sequences generated. 

  If the hypergraph has no edges with more than two vertices, it is
  usually much faster to convert to a matrix representation and use the
  count.indep (or count.vc) function on this graph.
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
edge.orders<-rpois(15,1.6)+1

H<-list("vertices"=10,lapply(edge.orders,function(s){sample(seq_len(10),s)}))

ind.set<-count.indep.hyper(H,method=c("exact","importance_sample"))
}
