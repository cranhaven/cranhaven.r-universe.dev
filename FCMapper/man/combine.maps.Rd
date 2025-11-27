\name{combine.maps}
\alias{combine.maps}

\title{
Map aggregator 
}
\description{
Aggregates two fuzzy cognitive maps into one.
}
\usage{
combine.maps(matrix1, matrix2, concept.names1, concept.names2)
}

\arguments{
  \item{matrix1}{
A quantitative fuzzy cognitive map.
}
  \item{matrix2}{
A quantitative fuzzy cognitive map.
}
  \item{concept.names1}{
A character vector.
}
  \item{concept.names2}{
A character vector.
}
}
\details{
The fuzzy cognitive maps should be in the form of quantitative adjacency matrices. The concept.names inputs are the names of the concepts in the first and second fuzzy cognitive maps, respectively. 
}
\value{
An aggregated fuzzy cognitive map in adjacency matrix format. It contains the edges and concepts of the two inputted fuzzy cognitive maps. Edges which are shared between the two fuzzy cognitive maps are averaged in the aggregated map.
}

\author{
Shaun Turney
}

\examples{
#Matrix 1
matrix = matrix(nrow=7,ncol=7)
matrix[1,] = c(0,-0.5,0,0,1,0,1)
matrix[2,] = c(1,0,1,0.2,0,0,0.6)
matrix[3,] = c(0,1,0,0,0,0,0)
matrix[4,] = c(0.6,0,0,1,0,0,0.1)
matrix[5,] = c(0,0.5,0,0,1,0,-0.6) 
matrix[6,] = c(0,0,-1,0,0,0,0)
matrix[7,] = c(0,0,0,-0.5,0,0,1)
concept.names = c("A","B","C","D","E","F","G")

#Matrix 2
matrix2 = matrix(nrow=6,ncol=6)
matrix2[1,] = c(0,1,0,1,0,0)
matrix2[2,] = c(-1,1,0,-1,0,1)
matrix2[3,] = c(0,0,1,0,0,1)
matrix2[4,] = c(-1,0,0,0,0,0)
matrix2[5,] = c(0,0.5,0,0,1,0) 
matrix2[6,] = c(1,0,0,-1,0,0)
concept.names2 = c("E","F","G","H","I","J")

#Aggregate
combine.maps(matrix1=matrix,matrix2=matrix2,concept.names1=
concept.names,concept.names2=concept.names2)

}


