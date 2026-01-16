\name{clv.Dens_bw}
\title{Inter-cluster density - Internal Measure}
\description{
	Function computes \emph{inter-cluster density}.
}
\usage{
clv.DensBw(data, clust, scatt.obj, dist="euclidean")
}
\alias{clv.DensBw}
\arguments{
  \item{data}{\code{matrix} or \code{data.frame} where columns correspond to variables and rows to observations}
  \item{clust}{
	integer \code{vector} with information about cluster id the object is assigned to.
	If vector is not integer type, it will be coerced with warning.}
  \item{scatt.obj}{ object returned by \code{\link{clv.Scatt}} function. }
  \item{dist}{chosen metric: "euclidean" (default value), "manhattan", "correlation"}
}
\details{
	The definition of \emph{inter-cluster density} is given by equation: 

	\code{Dens_bw} = 1/(|C|*(|C|-1)) * sum\{forall i in 1:|C|\} sum\{forall j in 1:|C| and j != i\}
					\emph{density}(u(i,j))/max\{\emph{density}(v(i)), \emph{density}(v(j))\}

	where:
	\tabular{ll}{
		|C| \tab - number of clusters, \cr
		v(i), v(j) \tab - centers of clusters i and j, \cr
		u(i,j) \tab - middle point of the line segment defined by the clusters' centers v(i), v(j), \cr
		density(x) \tab - see below.
	}
	
	Let define function \emph{f}(x,u):
	\tabular{ll}{
		\emph{f}(x,u) = 0 \tab if dist(x,u) > \emph{stdev} (\emph{stdev} is defined in 
						\code{\link{clv.Scatt}}) \cr
		\emph{f}(x,u) = 1 \tab otherwise 
	}
	Function \emph{f} is used in definition of \emph{density}(u):

	\emph{density}(u) = sum\{forall i in 1:n(i,j)\} \emph{f}(xi,u)

	where n(i,j) is the number of objects which belongs to clusters i and j and xi is such object.

	This value is used by \code{\link{clv.SDbw}}.
}
\value{
	As result \code{Dens_bw} value is returned.
}
\examples{
# load and prepare data
library(clv)
data(iris)
iris.data <- iris[,1:4]

# cluster data
agnes.mod <- agnes(iris.data) # create cluster tree 
v.pred <- as.integer(cutree(agnes.mod,5)) # "cut" the tree 

# compute Dens_bw index
scatt <- clv.Scatt(iris.data, v.pred)
dens.bw <- clv.DensBw(iris.data, v.pred, scatt)
}
\seealso{ \code{\link{clv.SD}} and \code{\link{clv.SDbw}} }
\author{Lukasz Nieweglowski}
\keyword{cluster}
