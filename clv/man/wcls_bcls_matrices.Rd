\name{wcls/bcls.matrix}
\title{Matrix Cluster Scatter Measures}
\description{
	Functions compute two base matrix cluster scatter measures.
}
\usage{
wcls.matrix(data,clust,cluster.center)
bcls.matrix(cluster.center,cluster.size,mean)
}
\alias{wcls.matrix}
\alias{bcls.matrix}
\arguments{
  \item{data}{
		\code{numeric matrix} or \code{data.frame} where columns correspond to variables and 
		rows to observations
	}
  \item{clust}{
		integer \code{vector} with information about cluster id the object is assigned to.
		If vector is not integer type, it will be coerced with warning. 
	}
  \item{cluster.center}{
		\code{matrix} or \code{data.frame} where columns correspond to variables and rows to cluster centers 
		defined by \code{data} and \code{clust} parameters. 
	}
  \item{cluster.size}{
		integer \code{vector} with information about size of each cluster computed using \code{clust} 
		vector.  
	}
  \item{mean}{ \emph{mean} of all data objects. }
}
\details{
	There are two base matrix scatter measures.
	
	1. \emph{within-cluster scatter measure} defined as:

	W = sum(forall k in 1:cluster.num) W(k) 

	where W(k) = sum(forall x) (x - m(k))*(x - m(k))' 

	\tabular{ll}{
	x \tab - object belongs to cluster k, \cr
	m(k) \tab - center of cluster k.
	}
	2. \emph{between-cluster scatter measure} defined as:
	
	B = sum(forall k in 1:cluster.num) |C(k)|*( m(k) - m )*( m(k) - m )'

	\tabular{ll}{
	|C(k)| \tab - size of cluster k, \cr
	m(k) \tab - center of cluster k, \cr
	m \tab - center of all data objects.
	}
}
\value{
	\tabular{ll}{
	\code{wcls.matrix} \tab returns W matrix (\emph{within-cluster scatter measure}),\cr
	\code{bcls.matrix} \tab returns B matrix (\emph{between-cluster scatter measure}).
	}
}
\references{
	T. Hastie, R. Tibshirani, G. Walther \emph{Estimating the number of data clusters via the Gap statistic},
	\url{http://citeseer.ist.psu.edu/tibshirani00estimating.html}
}
\examples{
# load and prepare data
library(clv)
data(iris)
iris.data <- iris[,1:4]

# cluster data
pam.mod <- pam(iris.data,5) # create five clusters
v.pred <- as.integer(pam.mod$clustering) # get cluster ids associated to given data objects

# compute cluster sizes, center of each cluster 
# and mean from data objects
cls.attr <- cls.attrib(iris.data, v.pred)
center <- cls.attr$cluster.center
size <- cls.attr$cluster.size
iris.mean <- cls.attr$mean

# compute matrix scatter measures
W.matrix <- wcls.matrix(iris.data, v.pred, center)
B.matrix <- bcls.matrix(center, size, iris.mean)
T.matrix <- W.matrix + B.matrix

# example of indices based on W, B i T matrices
mx.scatt.crit1 = sum(diag(W.matrix))
mx.scatt.crit2 = sum(diag(B.matrix))/sum(diag(W.matrix))
mx.scatt.crit3 = det(W.matrix)/det(T.matrix)
}
\author{Lukasz Nieweglowski}
\keyword{cluster}
