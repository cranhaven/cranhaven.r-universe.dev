\name{clv.Dis}
\title{Total separation between clusters - Internal Measure}
\description{
	Function computes \emph{total separation between clusters}.
}
\usage{
clv.Dis(cluster.center)
}
\alias{clv.Dis}
\arguments{
\item{cluster.center}{
		\code{numeric matrix} or \code{data.frame} where columns correspond to variables and rows 
		cluster centers.
	}
}
\details{
	The definition of total separation between clusters is given by equation: 

	\code{Dis} = (Dmax/Dmin) * sum\{forall i in 1:|C|\} 1 /( sum\{forall j in 1:|C|\} ||vi - vj|| )

	where:
	\tabular{ll}{
	|C| \tab - number of clusters, \cr
	vi, vj \tab - centers of clusters i and j, \cr
	Dmax \tab - defined as: max\{||vi - vj||: vi,vj - centers of clusters \}, \cr
	Dmin \tab - defined as: min\{||vi - vj||: vi,vj - centers of clusters \}, \cr
	||x|| \tab - means: sqrt(x*x').
	}

	This value is a part of \code{\link{clv.SD}} and \code{\link{clv.SDbw}}.
}
\value{
	As result \code{Dis} value is returned.
}
\seealso{\code{\link{clv.SD}} and \code{\link{clv.SDbw}}}
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

# compute Dis index
scatt <- clv.Scatt(iris.data, v.pred)
dis <- clv.Dis(scatt$cluster.center)
}
\author{Lukasz Nieweglowski}
\keyword{cluster}
