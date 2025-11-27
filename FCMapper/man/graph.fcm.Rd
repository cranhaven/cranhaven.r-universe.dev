\name{graph.fcm}
\alias{graph.fcm}

\title{
Fuzzy cognitive map graph
}
\description{
Creates a visual representation (circles and arrows) of the fuzzy cognitive map by calling igraph. The thickness of the arrows is determined by the magnitude of the edge value. Negative edges are red while positive edges are black.The size of the circles (concepts) is defined by the user.
}
\usage{
graph.fcm(matrix, concept.sizes, concept.names)
}

\arguments{
  \item{matrix}{
A quantitative fuzzy cognitive map. 
}
  \item{concept.sizes}{
A numeric vector the same length as the number of concepts.
}
  \item{concept.names}{
A character vector.
}
}
\details{
The fuzzy cognitive maps should be in the form of quantitative adjacency matrices. The concept.names input is the names of the concepts in the fuzzy cognitive map. The concept.sizes is a vector of values between 0 and 1 and determines the size of the circles. The user may want to define concept.sizes as the equilibrium values of the concepts. 
}
\value{
An igraph plot with circles representing concepts and arrows representing edges. 
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

results = nochanges.scenario(matrix,iter=25,concept.names)

graph.fcm(matrix,concept.sizes=results$Equilibrium_value,concept.names)

}