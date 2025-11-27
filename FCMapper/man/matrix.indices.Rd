\name{matrix.indices}
\alias{matrix.indices}

\title{
Matrix indices
}
\description{
Matrix-level indices are calculated, including the number of connections, connection density, number of concepts, number of transmitters, number of receivers, number of no connections, number of ordinary, number of self-loops, connections per variable, complexity, and hierarchy.
}
\usage{
matrix.indices(matrix)
}
\arguments{
  \item{matrix}{
A quantitative fuzzy cognitive matrix.
}
}
\details{
The fuzzy cognitive maps should be in the form of a quantitative adjacency matrix.
}
\value{
A dataframe containing the number of connections, connection density, number of concepts, number of transmitters, number of receivers, number of no connections, number of ordinary, number of self-loops, connections per variable, complexity, and hierarchy.
}
\references{
Ozesmi, U., & Ozesmi, S. L. (2004). Ecological models based on people's knowledge: a multi-step fuzzy cognitive mapping approach. Ecological Modelling, 176(1), 43-64.
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

matrix.indices(matrix)
}

