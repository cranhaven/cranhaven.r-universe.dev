\name{vertex.sort}

\alias{vertex.sort}
\alias{print.vertex.sort}
\alias{summary.vertex.sort}

\title{Elucidates the hierarchical structure of a directed network using
  the Vertex Sort algorithm}

\description{
  The \code{vertex.sort()} function applies a network decomposition
  algorithm called the Vertex Sort to a directed network (Jothi et
  al. 2009) and generates an object of class vertex.sort.  The Vertex
  Sort algorithm elucidates the hierarchical structure of a directed network
  by classifying its nodes into mainly four layers: the Top, Core,
  Bottom and Zero layers.  For more information on each of these layers
  see details below or refer to (Jothi et al. 2009).
}
\usage{
  vertex.sort(edges)
  \method{print}{vertex.sort}(x, \dots)
  \method{summary}{vertex.sort}(object, \dots)
}
\arguments{
  \item{edges}{a dataframe that contains a symbolic edge list of the graph
    to decompose.  It should contain two columns listing Actor and
    Target nodes, see details below.}
  \item{x, object}{the vertex.sort object of wich details or summary
  will be printed.}
  \item{...}{further arguments passed to or from other methods.}
}
\details{
  The Vertex Sort algorithm is a network decomposition method that
  elucidates the network hierarchy (Jothi et al. 2009).  It classifies
  network nodes into various levels.  First, it transforms the directed
  network into an acyclic directed graph.  Second, it applies the leaf
  removal (LR) algorithm on the network and on its transpose.  Third, it
  merges the results of the two LR applications into a global result, in
  which the level of each node is determined by the possibility of a
  node to span many levels.  Finally, it groups nodes into four
  non-overlapping layers: the Top, Core, Bottom and Zero layers. The
  Core layer is made of the nodes composing the biggest strongly
  connected component of the network and Top and Bottom layers contain
  nodes that regulate and are regulated by the Core layer respectively.
  Transforming a network from a cyclic to an acyclic graph is achieved
  by collapsing each strongly connected component into a super-node.

  The \code{print()} function prints the details of a vertex.sort object
  (i.e. all the elements of the vertex.sort object).

  The \code{summary()} function prints a summary of a vertex.sort object.
  It describes: the sorted graph (i.e. number of nodes, edges, layers and
  levels), the graph layers (i.e. number of nodes of type Actors in Top,
  Core and Bottom layers and the number of nodes of type Targets), and
  the number of excluded and disconnected nodes (i.e. number of excluded
  Actor nodes and the number of disconnected Actor and Target nodes).
}

\value{
  The \code{vertex.sort()} function returns an object of class vertex.sort.
  The functions \code{print()} and \code{summary()} are used to obtain the
  details and a summary of the contents of the vertex.sort objet.

  An object of class vertex.sort is a list containing the following elements:
  \item{graph}{a graph of class \code{igraph} containing the edges of
  the input network to be sorted.}
  \item{edges}{a dataframe containing the network edges}
  \item{traits}{a dataframe listing the network nodes and determining
    whether each of them is of type Actor (node with a non-zero
  out-degree) or of type Target (a node with a zero out-degree or a node
  that does not regulate an Actor node).}
  \item{actors}{a vector containing all Actor nodes.}
  \item{targets}{a vector containing all Target nodes.}
  \item{top.actors}{a vector containing Actor nodes that were classified
  in the Top layer.}
  \item{core.actors}{a vector containing Actor nodes that were classified
  in the Core layer.}
  \item{bottom.actors}{a vector containing Actor nodes that were
    classified in the Bottom layer.}
  \item{excluded.actors}{a vector containing Actor nodes that were
    excluded from the classification, because the algorithm was unable to
    classify them propertly.}
  \item{disconnected.actors}{a vector containing Actor nodes that were
    found to be disconnected from the input graph and hence were not
    subject to the Vertex Sort algorithm.}
  \item{disconnected.targets}{a vector containing Target nodes that were
    found to be disconnected from the input graph and hence were not
    subject to the decomposition algorithm.}
  \item{levels.no}{an integer indicating the number of levels of the
    sorted network.}
  \item{nodes.in.levels}{a list enumerating the nodes belonging to each level.}
}
\references{
  Jothi, R., Balaj, S., Wuster, A. et al. 2009 \emph{Molecular system
    biology} \bold{5}, --294-309.
}

\author{
  Diala Abd-Rabbo \email{diala.abd.rabbo@gmail.com}
}

\note{
  In the article (Jothi et al. 2009), the authors talked about only
  three layers (the Top, Core and Bottom layers). However, in the
  present package we talk about four layers, the three layers mentioned
  by Jothi et al. in addition to the Zero layer corresponding to the
  layer containg the target nodes that do not regulate other Actor nodes.
}

\examples{
  ## load the VertexSort library
  library(VertexSort)

  ## load interactions of the kinase-phosphatase network (kp-net)
  data("interactions")

  ## apply the Vertex Sort algorithm
  vs_kp_net <- vertex.sort(interactions)

  ## print the results (the contents of the vertex.sort object)
  vs_kp_net

  ## print a summary of the results
  summary(vs_kp_net)

  ## print levels.no, an element of the vertex.sort object
  vs_kp_net$levels.no
}

\keyword{ graphs }

