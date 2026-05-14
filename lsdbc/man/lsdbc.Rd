\name{lsdbc}
\alias{lsdbc}

\title{
Locally Scaled Density Based Clustering
}
\description{
Generate a locally scaled density based clustering as proposed by Bicici and Yuret (2007).
}
\usage{
lsdbc(data, k, alpha, jarak = c("euclidean", "manhattan", "canberra", "geodesic"))
}

\arguments{
  \item{data}{
Dataset consists of two variables (x,y) indicating coordinates of each data (point)
}
  \item{k}{
Number of neighbor to be considered
}
  \item{alpha}{
Parameter for determining local maximum
}
  \item{jarak}{
Type of distance to be used, the options are c("euclidean", "manhattan", "canberra", "geodesic")
}
}
\value{
This function returns a list with the following objects:
  \item{data}{
    a dataframe of the dataset used.
  }
  \item{cluster}{
    an integer vector coding cluster membership, 0 indicates a noise and cluster start at 1.
  }
  \item{parameter}{
    consist of parameter k and alpha.
  }
}
\references{
Bicici, E., & Yuret, D. (2007). Locally Scaled Density Based Clustering. International Conference on Adaptive and Natural Computing Algorithms (pp. 739-748). Berlin: Springer.
}
\author{
Fella Ulandari and Robert Kurniawan
}
\seealso{
https://doi.org/10.1007/978-3-540-71618-1_82
}
\examples{
x <- runif(20,-1,1)
y <- runif(20,-1,1)
dataset <- cbind(x,y)
l <- lsdbc(dataset, 7,3,"euclidean")
l
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{lsdbc}

