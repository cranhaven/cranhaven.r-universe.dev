\name{changes.scenario}
\alias{changes.scenario}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Scenario with fixed values
}
\description{
The equilibrium values of the concepts in a fuzzy cognitive map are calculated. One or more concept values can be fixed by the user to be within 0 or 1. At each iteration (time step) the fixed concepts are set back to their chosen values. A plot of the concept values over the iterations is given.
}
\usage{
changes.scenario(matrix, concept.names, iter, set.concepts, set.values)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{matrix}{
A quantitative fuzzy cognitive map.
}
  \item{concept.names}{
A quantitative character vector.
}
  \item{iter}{
Number of iterations.
}
  \item{set.concepts}{
A character vector. 
}
  \item{set.values}{
A numeric vector.  
}
}
\details{
The fuzzy cognitive map should be in the form of quantitative adjacency matrices. The concept.names input is the names of the concepts in thefuzzy cognitive map. set.concepts is a character vector of the concepts the user wishes to fix, while set.values gives the desired values in the same order. 
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

changes.scenario(matrix,concept.names,iter=25,set.concepts=c("B","G"),set.values=c(0.5,0))

}
