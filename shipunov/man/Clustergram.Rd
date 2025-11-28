\name{Clustergram}
\alias{Clustergram}
\title{Clustergram: visualize the cluster structure}

\description{Plot which shows cluster memberships and distances when clusters numbers increases}

\usage{

  Clustergram(data, maxcl=ncol(data)*2, nboot=FALSE, method="kmeans",
  m.dist="euclidean", m.hclust="complete", plot=TRUE, broom=4e-3,
  col="gray", ...)

}

\arguments{

  \item{data}{Data, typically data frame}

  \item{maxcl}{Maximal number of clusters, default is number of columns times 2; minimal number of clusters is 2}

  \item{nboot}{Either 'FALSE' (no bootstrap, default) or number of bootstrap runs}

  \item{method}{Either 'kmeans' or 'hclust'}

  \item{m.dist}{If method='hclust', method to calculate distances, see ?dist}

  \item{m.hclust}{If method='hclust', method to clusterize, see ?hclust}

  \item{plot}{Plot?}

  \item{broom}{Extent to which spread lines, default is 4e-3}

  \item{col}{Color of lines}

  \item{...}{Further arguments to plot()}

}

\details{

Clustergram shows how cluster members are assigned to clusters as the
number of clusters increases. This graph is useful in exploratory
analysis for non-hierarchical clustering algorithms like k-means and for
hierarchical cluster algorithms when the number of observations is large
enough to make dendrograms impractical (from Schonlau, 2004; see also
www.schonlau.net).

One application is to use clustergram to determine the optimal number of
clusters. Basic idea is that you look for the point (number of clusters)
where more clusters do not significanly change the picture (i.e., do not
add more information) The best number of clusters is _near_ that point
(see examples).

See also Martin Fleischmann (martinfleischmann.net) for practical
explanation and scikit-learn 'clustergram' Python package.

Clustergram() code based on simplified and optimized Tal Galili's github
'clustergram' code.

}

% \value{}

\references{

Schonlau M. 2004. Visualizing non-hierarchical and hierarchical cluster
analyses with clustergrams. Computational Statistics 19, 95-111.

}

\author{Alexey Shipunov}

\seealso{

 \code{\link{dist}}, \code{\link{hclust}}, \code{\link{kmeans}}

}

\examples{

set.seed(250)
aa <- matrix(rnorm(20000), nrow=100)
## maximal number of clusters is less than default
## line color is like in scikit-learn
## larger "broom" so lines are a bit broader
Clustergram(aa, maxcl=5, col="#3B6E8C", broom=2e-2, main="Artificial data, homogeneous")
aa[1:60, ] <- aa[1:60, ] + 0.7
aa[1:20, ] <- aa[1:20, ] + 0.4
Clustergram(aa, maxcl=5, col="#F29528", broom=2e-2,
 main="Artificial data, heterogeneous, 3 clusters")

## Clustergram() invisibly outputs matrix of clusters
ii <- Clustergram(iris[, -5], main=expression(bolditalic("Iris")*bold(" data")))
head(ii)
## Hierarchical clustering instead of kmeans
Clustergram(iris[, -5], method="hclust", m.hclust="average", main="Iris, UPGMA")
## Bootstrap. Resulted PDF could be opening slowly, use raster images in that case
Clustergram(iris[, -5], nboot=100, col=adjustcolor("darkgray", alpha.f=0.3),
 main="Iris, kmeans, nboot 100")

}

\keyword{hplot}
