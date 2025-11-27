\name{concept.indices}
\alias{concept.indices}

\title{
Concept indices
}
\description{
Concept-level indices are calculated, including the out-degree, in-degree, centrality, and whether it is a transmitter, reveiver, ordinary or unconnected concept.
}
\usage{
concept.indices(matrix, concept.names)
}
\arguments{
  \item{matrix}{
A quantitative fuzzy cognitive map.
}
  \item{concept.names}{
A character vector.
}
}
\details{
The fuzzy cognitive map should be in the form of a quantitative adjacency matrix. The concept.names input is the names of the concepts in the fuzzy cognitive map.
}
\value{
A dataframe containing the concept name, out-degree, in-degree, centrality, and whether it is a  transmitter, reveiver, ordinary or unconnected concept.
}

\author{
Shaun Turney
}


\examples{
matrix = matrix(nrow=7,ncol=7)
matrix[1,] = c(0,-0.5,0,0,1,0,1)
matrix[2,] = c(1,0,1,0.2,0,0,0.6)
matrix[3,] = c(0,1,0,0,0,0,0)
matrix[4,] = c(0.6,0,0,1,0,0,0.1)
matrix[5,] = c(0,0.5,0,0,1,0,-0.6) 
matrix[6,] = c(0,0,-1,0,0,0,0)
matrix[7,] = c(0,0,0,-0.5,0,0,1)
concept.names = c("A","B","C","D","E","F","G")

concept.indices(matrix,concept.names)

}
