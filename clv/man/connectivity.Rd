\name{connectivity}
\title{Connectivity Index - Internal Measure}
\description{
	Function evaluates \emph{connectivity} index.
}
\usage{
connectivity(data,clust,neighbour.num, dist="euclidean")
connectivity.diss.mx(diss.mx,clust,neighbour.num)
}
\alias{connectivity}
\alias{connectivity.diss.mx}
\arguments{
  \item{data}{
		\code{numeric matrix} or \code{data.frame} where columns correspond to variables and rows to 
		observations
	}
  \item{diss.mx}{
		square, symetric \code{numeric matrix} or \code{data.frame}, representation of 
		dissimilarity matrix where infomartion about distances between objects is stored.
	}
  \item{clust}{
		integer \code{vector} with information about cluster id the object is assigned to.
		If vector is not integer type, it will be coerced with warning. 
	}
  \item{neighbour.num}{value which tells how many nearest neighbors for every object should be checked.}
  \item{dist}{chosen metric: "euclidean" (default value), "manhattan", "correlation" 
	(variable enable only in \code{connectivity} function).}
}
\details{
	For given data and its partitioning \emph{connectivity} index is computed.
	For choosen pattern \code{neighbour.num} nearest neighbours are found and sorted from closest 
	to most further. Alghorithm checks if those neighbours are 
	assigned to the same cluster. At the beggining \emph{connectivity} value is equal 0 and increase 
	with value:
	\tabular{ll}{
	1/i \tab when i-th nearest neighbour is not assigned to the same cluster, \cr
	0   \tab otherwise.
	}
	Procedure is repeated for all patterns which comming from our data set. All values received 
	for particular pattern are added and creates main \emph{connectivity} index.
}
\value{
  \code{connectivity} returns a \emph{connectivity} value.
}
\references{
	J. Handl, J. Knowles and D. B. Kell \emph{Sumplementary material to computational cluster validation in post-genomic data analysis}, 
	\url{http://dbkgroup.org/handl/clustervalidation/supplementary.pdf}
}
\examples{
# load and prepare data
library(clv)
data(iris)
iris.data <- iris[,1:4]

# cluster data
pam.mod <- pam(iris.data,5) # create five clusters
v.pred <- as.integer(pam.mod$clustering) # get cluster ids associated to gived data objects

# compute connectivity index using data and its clusterization
conn1 <- connectivity(iris.data, v.pred, 10)
conn2 <- connectivity(iris.data, v.pred, 10, dist="manhattan")
conn3 <- connectivity(iris.data, v.pred, 10, dist="correlation")

# the same using dissimilarity matrix
iris.diss.mx <- as.matrix(daisy(iris.data))
conn4 <- connectivity.diss.mx(iris.diss.mx, v.pred, 10)
}
\author{Lukasz Nieweglowski}
\keyword{cluster}
