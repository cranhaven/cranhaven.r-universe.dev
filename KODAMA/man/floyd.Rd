\name{floyd}
\alias{floyd}
\title{Find Shortest Paths Between All Nodes in a Graph}
\description{
The \code{floyd} function finds all shortest paths in a graph using Floyd's algorithm. 
}

\usage{
floyd(data)
}
\arguments{
\item{data}{matrix or distance object}
}
\value{
\code{floyd} returns a matrix with the total lengths of the shortest path between each pair of points.
}

\references{
Floyd, Robert W	\cr
Algorithm 97: Shortest Path.\cr
\emph{Communications of the ACM} 1962; 5 (6): 345. doi:10.1145/367766.368168. 
}


 


\examples{
# build a graph with 5 nodes
x=matrix(c(0,NA,NA,NA,NA,30,0,NA,NA,NA,10,NA,0,NA,NA,NA,70,50,0,10,NA,40,20,60,0),ncol=5)
print(x)

# compute all path lengths
z=floyd(x)
print(z)

}


