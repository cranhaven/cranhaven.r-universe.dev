\name{clv.Scatt}
\title{Average scattering for clusters - Internal Measure}
\description{
	Function computes \emph{average scattering for clusters}.
}
\usage{
clv.Scatt(data, clust, dist="euclidean")
}
\alias{clv.Scatt}
\arguments{
  \item{data}{
		\code{numeric matrix} or \code{data.frame} where columns correspond to variables and 
		rows to observations
	}
  \item{clust}{
		integer \code{vector} with information about cluster id the object is assigned to.
		If vector is not integer type, it will be coerced with warning.
	}
  \item{dist}{choosen metric: "euclidean" (default value),  "manhattan", "correlation"}
}
\details{
	Let \emph{scatter for set} X assigned as \emph{sigma}(X) be defined as
	vector of variances computed for particular dimensions. 
	\emph{Average scattering for clusters} is defined as:

	\code{Scatt} = (1/|C|) * sum\{forall i in 1:|C|\} ||\emph{sigma}(Ci)||/||\emph{sigma}(X)||

	where:
	\tabular{ll}{
	|C| \tab - number of clusters, \cr
	i \tab - cluster id, \cr
	Ci \tab - cluster with id 'i', \cr
	X \tab - set with all objects, \cr
	||x|| \tab - sqrt(x*x').
	}

	\emph{Standard deviation} is defined as:

	\code{stdev} = (1/|C|) * sqrt( sum\{forall i in 1:|C|\} ||\emph{sigma}(Ci)||  )

}
\value{
	As result \code{list} with three values is returned.
	\tabular{ll}{
		\code{Scatt} \tab - \emph{average scattering for clusters} value, \cr
		\code{stdev} \tab - \emph{standard deviation} value, \cr
		\code{cluster.center} \tab - numeric \code{matrix} where columns 
		correspond to variables and rows to cluster centers.
	}
}
\references{
	M. Haldiki, Y. Batistakis, M. Vazirgiannis \emph{On Clustering Validation Techniques},
	\url{http://citeseer.ist.psu.edu/513619.html}
}
\examples{
# load and prepare data
library(clv)
data(iris)
iris.data <- iris[,1:4]

# cluster data
agnes.mod <- agnes(iris.data) # create cluster tree 
v.pred <- as.integer(cutree(agnes.mod,5)) # "cut" the tree 

# compute Scatt index
scatt <- clv.Scatt(iris.data, v.pred)
}
\seealso{\code{\link{clv.SD}} and \code{\link{clv.SDbw}}}
\author{Lukasz Nieweglowski}
\keyword{cluster}
