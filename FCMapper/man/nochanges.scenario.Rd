\name{nochanges.scenario}
\alias{nochanges.scenario}
\title{
Scenario with no fixed values
}
\description{
The equilibrium values of the concepts in a fuzzy cognitive map are calculated. A plot of the concept values over the iterations is given. It activates the matrix with a vector of 1s and squeezes the resulting vector with a logic function. The function checks for convergence and gives a warning if convergence isn't reached. A plot of the concept values over the iterations is given.
}
\usage{
nochanges.scenario(matrix, concept.names, iter)
}
\arguments{
  \item{matrix}{
A quantitative fuzzy cognitive map.
}
  \item{concept.names}{
A character vector.
}
  \item{iter}{
The number of iterations.
}
}
\details{
The fuzzy cognitive map should be in the form of quantitative adjacency matrices. The concept.names input is the names of the concepts in thefuzzy cognitive map. 
}
\value{
A dataframe containing the equilibrium values of the concepts. If equilibrium has not been reached a warning will be printed. A plot of the concept values over the iterations is given.
}

\author{
Shaun Turney
}


\seealso{
\code{\link{nochanges.scenario}}
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

nochanges.scenario(matrix,iter=25,concept.names)

}
