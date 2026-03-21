\name{qkspeclust}
\alias{qkspeclust}
\alias{qkspeclust,qkspecc-method}

\title{qkernel spectral Clustering}
\description{This is also a qkernel spectral clustering algorithm which uses three ways to assign labels after the laplacian embedding: kmeans, hclust and dbscan.}


\usage{
\S4method{qkspeclust}{qkspecc}(x, clustmethod = "kmeans",
         Nocent=NULL,iterations=NULL, hmethod=NULL,eps = NULL, MinPts = NULL)}

\arguments{
  \item{x}{object of class \code{qkspecc}.}

  \item{clustmethod}{the strategy to use to assign labels in the embedding space.
  There are three ways to assign labels after the laplacian embedding: kmeans, hclust and dbscan. }

  \item{Nocent}{the number of clusters}

  \item{iterations}{the maximum number of iterations allowed for "kmeans". }

  \item{hmethod}{the agglomeration method for "hclust". This should be (an unambiguous abbreviation of) one of "ward.D", "ward.D2", "single", "complete", "average" (= UPGMA), "mcquitty" (= WPGMA), "median" (= WPGMC) or "centroid" (= UPGMC).}

  \item{eps}{Reachability distance for "dbscan".}

  \item{MinPts}{Reachability minimum no. of points for "dbscan".}

}
\details{
  The qkernel spectral clustering  works by embedding the data points of the
partitioning problem into the subspace of the eigenvectors corresponding to the
\eqn{k} smallest eigenvalues of the graph Laplacian matrix.
  Using the simple clustering methods like \code{kmeans}, \code{hclust} and \code{dbscan}
  on the embedded points usually leads to good performance. It can be shown that
  qkernel spectral clustering methods boil down to graph partitioning.\cr
}
\value{
 An S4 object of class \code{qkspecc} which extends the class \code{vector}
 containing integers indicating the cluster to which
 each point is allocated. The following slots contain useful information

  \item{clust}{The cluster assignments}
  \item{eVec}{The corresponding eigenvector}
  \item{eVal}{The corresponding eigenvalues}
  \item{xmatrix}{The original data matrix}
  \item{ymatrix}{The real valued matrix of eigenvectors corresponding to the k smallest eigenvalues of the graph Laplacian matrix}
  \item{cndkernf}{The kernel function used}
}
\references{
    Andrew Y. Ng, Michael I. Jordan, Yair Weiss\cr
  \emph{On Spectral Clustering: Analysis and an Algorithm}\cr
  Neural Information Processing Symposium 2001\cr
}
\author{Yusen Zhang \cr \email{yusenzhang@126.com}
}


\seealso{\code{\link{qkernmatrix}}, \code{\link{cndkernmatrix}}, \code{\link{qkspecc-class}}, \code{\link{qkspecc}}}
\examples{
data("iris")
x=as.matrix(iris[ ,-5])

qspe <- qkspecc(x,kernel = "rbfbase", qpar = list(sigma = 90, q = 0.9),
                Nocent=3, normalize="symmetric", maxk=15,iterations=1200)
plot(x, col = clust(qspe))

qspec <- qkspeclust(qspe,clustmethod = "hclust", Nocent=3, hmethod="ward.D2")
plot(x, col = clust(qspec))
plot(qspec)

}
\keyword{cluster}

