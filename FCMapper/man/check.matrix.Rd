\name{check.matrix}
\alias{check.matrix}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Matrix validation
}
\description{
The fuzzy cognitive map is checked to confirm that it is square and all values are within -1 and 1. 
}
\usage{
check.matrix(matrix)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{matrix}{
A quantitative fuzzy cognitive matrix.
}
}
\details{
The fuzzy cognitive map should be in the form of a quantitative adjacency matrix. 
}
\value{
If the matrix is not square then a warning will be printed. If the matrix contains values not between -1 and 1 then a warning will be printed along with the identify of the offending values. If the matrix is square or has no values outside of -1 and 1 then a confirmation is printed. The function checks the diagonal to determine whether there are self-loops and gives a warning if there are.
}

\author{
Shaun Turney
}

\examples{

#Is not square and contains values outside of -1 and 1: 
matrix = matrix(nrow=7,ncol=7)
matrix[1,] = c(0,-0.5,0,0,1,0,1)
matrix[2,] = c(1,0,1,0.2,0,0,0.6)
matrix[3,] = c(0,1,0,0,0,2,0)
matrix[4,] = c(0.6,0,0,1,0,0,0.1)
matrix[5,] = c(-5,0.5,0,0,1,0,-0.6) 
matrix[6,] = c(0,0,-1,0,0,0,0)

#Check
check.matrix(matrix)

#Matrix without issues:
matrix2 = matrix(nrow=7,ncol=7)
matrix2[1,] = c(0,-0.5,0,0,1,0,1)
matrix2[2,] = c(1,0,1,0.2,0,0,0.6)
matrix2[3,] = c(0,1,0,0,0,0,0)
matrix2[4,] = c(0.6,0,0,1,0,0,0.1)
matrix2[5,] = c(0,0.5,0,0,1,0,-0.6) 
matrix2[6,] = c(0,0,-1,0,0,0,0)
matrix2[7,] = c(0,0,0,-0.5,0,0,1)

#Check
check.matrix(matrix2)
}

