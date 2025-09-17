\name{getAdjacencyMatrix}
\alias{getAdjacencyMatrix}

\title{
A function that extracts valuable properties from a raw social network.
}
\description{
This function transforms a network, which is a data.frame type in a specified format, in to a resultant \eqn{n}{n} by \eqn{n}{n} adjacency matrix, where \eqn{a_{ij}}{a_{ij}} = 0 if vertex \eqn{i}{i} and \eqn{j}{j} (\eqn{i \neq j}{i \neq j}) are not adjacent i.e. vertex \eqn{i}{i} and \eqn{j}{j} are not the head/tail of an edge \eqn{e}{e} and \eqn{a_{ij}}{a_{ij}} = 1 if vertex \eqn{i}{i} and \eqn{j}{j} (\eqn{i \neq j}{i \neq j}) are adjacent i.e. vertex \eqn{i}{i} and \eqn{j}{j} are the head/tail of an edge \eqn{e}{e}. \eqn{a_{ij}}{a_{ij}} = 0 when \eqn{i = j}{i = j}. 
}
\usage{
getAdjacencyMatrix(rawNetwork)
}
\arguments{
  \item{rawNetwork}{
  The data.frame which encodes information about the network. The dimensions of the matrix are \eqn{n}{n} by \eqn{(l+1)}{(l+1)}.The data.frame contains one column corresponding to the labels for each of the \eqn{n}{n} vertices in the network, the column name for this should be `labels'. The other \eqn{l}{l} columns corresponds to the corresponds to the vertices which are adjacent to each of the \eqn{n}{n} vertices in the network. It is important to note that the label of a vertex should not be 0. The \eqn{n}{n}th vertex can be adjacent to a maximum of \eqn{l} other vertices.
  }
}
\value{
\item{adjacencyMatrix }{The resultant adjaceny matrix for the rawNetwork data.frame.}
\item{nonnominators }{The individuals in the social network who are nominees of at least one other individual but were not in the set of individuals who did the nominating.}
\item{vertexNoOutdegrees }{The individuals in the social network that have an outdegree of 0.} 
\item{vertexNoIndegrees }{The individuals in the social network that have an indegree of 0.} 
\item{vertexIsolates }{The individuals in the social network that have an outdegree and indegree of 0.} 
}
\author{
George Gerogiannis
}

\examples{

rawNetwork = matrix(NA, 4, 3)
rawNetwork = as.data.frame(rawNetwork)
colnames(rawNetwork)[1] = "labels"
rawNetwork[, 1] = c("A", "B", "C", "D")
rawNetwork[, 2] = c(0, "C", "D", 0)
rawNetwork[, 3] = c("B", 0, "A", "C")
getAdjacencyMatrix(rawNetwork)

rawNetwork = matrix(NA, 4, 3)
rawNetwork = as.data.frame(rawNetwork)
colnames(rawNetwork)[2] = "labels"
rawNetwork[, 1] = c(NA, "Charlie", "David", 0)
rawNetwork[, 2] = c("Alistar", "Bob", "Charlie", "David")
rawNetwork[, 3] = c("Bob", NA, "Alistar", "Charlie")
getAdjacencyMatrix(rawNetwork)

rawNetwork = matrix(NA, 4, 3)
rawNetwork = as.data.frame(rawNetwork)
colnames(rawNetwork)[1] = "labels"
rawNetwork[, 1] = c(245, 344, 234, 104)
rawNetwork[, 2] = c(NA, 234, 104, NA)
rawNetwork[, 3] = c(344, 0, 245, 234)
getAdjacencyMatrix(rawNetwork)

rawNetwork = matrix(NA, 4, 3)
rawNetwork = as.data.frame(rawNetwork)
colnames(rawNetwork)[1] = "labels"
rawNetwork[, 1] = c(245, 344, 234, 104)
rawNetwork[, 2] = c(32, 234, 104, 0)
rawNetwork[, 3] = c(344, 20, 245, 234)
getAdjacencyMatrix(rawNetwork)

rawNetwork = matrix(NA, 4, 3)
rawNetwork = as.data.frame(rawNetwork)
colnames(rawNetwork)[1] = "labels"
rawNetwork[, 1] = c("Alistar", "Bob", "Charlie", "David")
rawNetwork[, 2] = c(NA, "Charlie", "David", 0)
rawNetwork[, 3] = c("Bob", "Blaine", "Alistar", "Charlie")
getAdjacencyMatrix(rawNetwork)

rawNetwork = matrix(NA, 4, 3)
rawNetwork = as.data.frame(rawNetwork)
colnames(rawNetwork)[1] = "labels"
rawNetwork[, 1] = c("Alistar", "Bob", "Charlie", "David")
rawNetwork[, 2] = c(0, "Charlie", 0, 0)
rawNetwork[, 3] = c("Bob", "Blaine", "Alistar", 0)
getAdjacencyMatrix(rawNetwork)

rawNetwork = matrix(NA, 4, 3)
rawNetwork = as.data.frame(rawNetwork)
colnames(rawNetwork)[1] = "labels"
rawNetwork[, 1] = c(245, 344, 234, 104)
rawNetwork[, 2] = c(32, 0, 104, 0)
rawNetwork[, 3] = c(34, 0, 245, 234)
getAdjacencyMatrix(rawNetwork)
}

