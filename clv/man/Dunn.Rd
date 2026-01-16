\name{clv.Dunn}
\title{Dunn Index - Internal Measure}
\description{
	Function computes \emph{Dunn index} - internal measure for given data and its partitioning.
}
\usage{
clv.Dunn( index.list, intracls, intercls)
}
\alias{clv.Dunn}
\arguments{
  \item{index.list}{ 
		object returned by function \code{\link{cls.scatt.data}} or \code{\link{cls.scatt.diss.mx}}.
	}
  \item{intracls}{
		string \code{vector} containing one or more names of intra cluster distances. 
		Available are:
		\tabular{l}{
		1. if \code{index.list} is produced by \code{\link{cls.scatt.data}}:
			complete | average | centroid, \cr
		2. if \code{index.list} is produced by \code{\link{cls.scatt.diss.mx}}: 
			complete | average.
		}
	}
  \item{intercls}{
		string \code{vector} containing one or more names of inter cluster diameters. 
		Available are:
		\tabular{l}{
		1. if \code{index.list} is produced by \code{\link{cls.scatt.data}}: 
			single | complete | average | centroid | aveToCent | hausdorff. \cr
		2. if \code{index.list} is produced by \code{\link{cls.scatt.diss.mx}}:
			single | complete | average | hausdorff.
		}
	}
}
\details{
	Dunn index:

	D = [ min\{ k,l - numbers of clusters \} \emph{dist}(Ck, Cl) ]/[  max\{ m - cluster number \} \emph{diam}(Cm) ]
 
	\tabular{ll}{
	k,l,m  \tab - numbers of clusters which come from the same partitioning, \cr
	\emph{dist(Ck,Cl)} \tab - inter cluster distance between clusters Ck and Cl, \cr
	\emph{diam(Cm)} \tab - intra cluster diameter computed for cluster Cm. 
	}
	
}
\value{
	As output user gets matrix of \emph{Dunn} indices.
	Matrix dimension depends on how many \emph{diam} and \emph{dist} measures are chosen by the user, 
	normally \code{dim(D)=c(length(intercls),length(intracls))}.
	Each pair: (inter-cluster dist, intra-cluster diam) have its own position in result matrix. 
}
\seealso{
		Functions which produce \emph{index.list} input argument: \code{\link{cls.scatt.data}}, \code{\link{cls.scatt.diss.mx}}.
		Related functions: \link{clv.Davies.Bouldin}.
	}
\references{
	M. Halkidi, Y. Batistakis, M. Vazirgiannis \emph{Clustering Validity Checking Methods : Part II},
	\url{http://citeseer.ist.psu.edu/537304.html}
}
\examples{
# load and prepare data
library(clv)
data(iris)
iris.data <- iris[,1:4]

# cluster data
agnes.mod <- agnes(iris.data) # create cluster tree 
v.pred <- as.integer(cutree(agnes.mod,5)) # "cut" the tree 

intraclust = c("complete","average","centroid")
interclust = c("single", "complete", "average","centroid", "aveToCent", "hausdorff")

# compute Dunn indicies (also Davies-Bouldin indicies)
# 1. optimal solution:

# compute intercluster distances and intracluster diameters
cls.scatt <- cls.scatt.data(iris.data, v.pred, dist="manhattan")

# once computed valuse use in both functions
dunn1 <- clv.Dunn(cls.scatt, intraclust, interclust)
davies1 <- clv.Davies.Bouldin(cls.scatt, intraclust, interclust)

# 2. functional solution:

# define new Dunn and Davies.Bouldin functions
Dunn <- function(data,clust) 
  clv.Dunn( cls.scatt.data(data,clust),
     intracls = c("complete","average","centroid"), 
     intercls = c("single", "complete", "average","centroid", "aveToCent", "hausdorff")
  )
Davies.Bouldin <- function(data,clust) 
  clv.Davies.Bouldin( cls.scatt.data(data,clust),
    intracls = c("complete","average","centroid"),
    intercls = c("single", "complete", "average","centroid", "aveToCent", "hausdorff")
  )

# compute indicies
dunn2 <- Dunn(iris.data, v.pred)
davies2 <- Davies.Bouldin(iris.data, v.pred)
}
\author{Lukasz Nieweglowski}
\keyword{cluster}
