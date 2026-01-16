\name{cls.scatt.data}
\title{Intercluster distances and intracluster diameters - Internal Measures}
\description{
	Two functions which find most popular \emph{intercluster distances} and \emph{intracluster diameters}.
}
\usage{
cls.scatt.data(data, clust, dist="euclidean")
cls.scatt.diss.mx(diss.mx, clust)
}
\alias{cls.scatt.data}
\alias{cls.scatt.diss.mx}
\arguments{
 \item{data}{
		\code{numeric matrix} or \code{data.frame} where columns correspond to variables and rows to 
		observations
	}
  \item{diss.mx}{
		square, symmetric \code{numeric matrix} or \code{data.frame}, representation of 
		dissimilarity matrix where infomartion about distances between objects is stored.
	}
  \item{clust}{
		integer \code{vector} with information about cluster id the object is assigned to.
		If vector is not integer type, it will be coerced with warning.
	}
  \item{dist}{
		chosen metric: "euclidean" (default value), "manhattan", "correlation"
		(variable enable only in \code{cls.scatt.data} function). 
	}
}
\details{
	Six \emph{intercluster distances} and three \emph{intracluster diameters} can be used to 
	calculate such validity indices as \emph{Dunn} and \emph{Davies-Bouldin} like.
	Let \code{d(x,y)} be a distance function between two objects comming from our data set.
	
	\emph{Intracluster diameters}

	The \emph{complete diameter} represents the distance between two the most remote objects belonging 
	to the same cluster. 

	diam1(C) = max\{ d(x,y): x,y belongs to cluster C \}
	
	The \emph{average diameter} distance defines the average distance between all of the 
	samples belonging to the same cluster. 

	diam2(C) = 1/|C|(|C|-1) * sum\{ forall x,y belongs to cluster C and x != y \} d(x,y)

	The \emph{centroid diameter} distance reflects the double average distance between all of the 
	samples and the cluster's center (v(C) - cluster center).

	diam3(C) = 1/|C| * sum\{ forall x belonging to cluster C\} d(x,v(C))

	\emph{Intercluster distances}

	The \emph{single linkage} distance defines the closest distance between two samples 
	belonging to two different clusters. 

	dist1(Ci,Cj) = min\{ d(x,y): x belongs to Ci and y to Cj cluster \}

	The \emph{complete linkage} distance represents the distance between the most remote samples
	belonging to two different clusters. 

	dist2(Ci,Cj) = max\{ d(x,y): x belongs to Ci and y to Cj cluster \}

	The \emph{average linkage} distance defines the average distance between all of the samples 
	belonging to two different clusters. 

	dist3(Ci,Cj) = 1/(|Ci|*|Cj|) * sum\{ forall x belongs Ci and y to Cj \} d(x,y)

	The \emph{centroid linkage} distance reflects the distance between the centres of two clusters
	(v(i), v(j) - clusters' centers).

	dist4(Ci,Cj) = d(v(i), V(j))

	The \emph{average of centroids linkage} represents the distance between the centre of a cluster 
	and all of samples belonging to a different cluster. 

	dist5(Ci,Cj) = 1/(|Ci|+|Cj|) * 
				   ( sum\{ forall x belongs Ci \} d(x,v(j)) + sum\{ forall y belongs Cj \} d(y,v(i)) )

	\emph{Hausdorff metrics} are based on the discovery of a maximal distance from samples of one 
	cluster to the nearest sample of another cluster. 
 
	dist6(Ci,Cj) = max\{ distH(Ci,Cj), distH(Cj,Ci) \}

	where: distH(A,B) = max\{ min\{ d(x,y): y belongs to B\}: x belongs to A \}
	
}
\value{
  \code{cls.scatt.data} returns an object of class \code{"list"}.
	Intracluster diameters: 
	\code{intracls.complete},
	\code{intracls.average},
    \code{intracls.centroid},
	are stored in vectors and intercluster distances:
	\code{intercls.single}, 
	\code{intercls.complete},
	\code{intercls.average},
	\code{intercls.centroid},
	\code{intercls.ave_to_cent},
	\code{intercls.hausdorff}
	in symmetric matrices.
	Vectors' lengths and both dimensions of each matrix are equal to number of clusters.
	Additionally in result list \code{cluster.center} matrix (rows correspond to clusters centers) 
	and \code{cluster.size} vector is given (information about size of each cluster).
	\cr\cr
	\code{cls.scatt.diss.mx} returns an object of class \code{"list"}.
	Intracluster diameters: 
	\code{intracls.complete},
	\code{intracls.average},
	are stored in vectors and intercluster distances:
	\code{intercls.single}, 
	\code{intercls.complete},
	\code{intercls.average},
	\code{intercls.hausdorff}
	in symmetric matrices.
	Vectors' lengths and both dimensions of each matrix are equal to number of clusters.
	Additionally in result list \code{cluster.size} vector is given (information about size of each cluster).
}
\references{
	J. Handl, J. Knowles and D. B. Kell \emph{Computational cluster validation in post-genomic data analysis},
	\url{http://bioinformatics.oxfordjournals.org/cgi/reprint/21/15/3201?ijkey=VbTHU29vqzwkGs2&keytype=ref}

	N. Bolshakova, F. Azuajeb \emph{Cluster validation techniques for genome expression data},
	\url{http://citeseer.ist.psu.edu/552250.html}
}
\examples{
# load and prepare data
library(clv)
data(iris)
iris.data <- iris[,1:4]

# cluster data
pam.mod <- pam(iris.data,5) # create five clusters
v.pred <- as.integer(pam.mod$clustering) # get cluster ids associated to given data objects

# compute intercluster distances and intracluster diameters
cls.scatt1 <- cls.scatt.data(iris.data, v.pred)
cls.scatt2 <- cls.scatt.data(iris.data, v.pred, dist="manhattan")
cls.scatt3 <- cls.scatt.data(iris.data, v.pred, dist="correlation")

# the same using dissimilarity matrix
iris.diss.mx <- as.matrix(daisy(iris.data))
cls.scatt4 <- cls.scatt.diss.mx(iris.diss.mx, v.pred)
}
\seealso{
		Result used in: \code{\link{clv.Dunn}}, \code{\link{clv.Davies.Bouldin}}.
	}
\author{Lukasz Nieweglowski}
\keyword{cluster}
