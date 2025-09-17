\name{createGraph}
\alias{createGraph}
\title{Simulate Graph}
\description{Simulate domain knowledge graph.}
\usage{
createGraph(subtype1.feats = 1:5, subtype2.feats = 6:10, subtype3.feats = 11:15, 
n.interactions = 1e+06, n.features = 10000)
}
\arguments{
  \item{subtype1.feats}{Index of features used to define subtype 1.}
  \item{subtype2.feats}{Index of features used to define subtype 2.}
  \item{subtype3.feats}{Index of features used to define subtype 3.}
  \item{n.interactions}{Number of pairwise interactions to simulate.}
  \item{n.features}{Number of features to simulate}
}
\value{Returns a data frame representation of a graph. The first two columns represent graph nodes and the third column represents the edge weights between nodes. 

All pairwise combinations of \code{subtype1.feats} have an edge weight of 1.

All pairwise combinations of \code{subtype2.feats} have an edge weight of 1.

All pairwise combinations of \code{subtype3.feats} have an edge weight of 1.

All other pairwise combinations have an edge weight uniformly distributed between 0 and 1.}
\references{ADD REFERENCE}
\author{Michelle Saul}
\seealso{\code{\link{createData}}}
\examples{
# Toy Example 
graph <- createGraph(subtype1.feats = 1:5, subtype2.feats = 6:10, subtype3.feats = 11:15, 
n.interactions = 100, n.features = 20)
}