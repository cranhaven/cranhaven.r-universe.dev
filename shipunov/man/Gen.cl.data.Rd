\name{Gen.cl.data}
\alias{Gen.cl.data}

\title{Generates datasets for clustering}
\description{Imitation of the Python \code{sklearn.datasets} functions.}

\usage{
Gen.cl.data(type=c("blobs", "moons", "circles"), N=100, noise=NULL,
 shuffle=TRUE, bdim=2, bcenters=3, bnoise=1, bbox=c(-10, 10), cfactor=0.8)
}

\arguments{
\item{type}{'blobs' are Gaussian blobs; 'moons' are two interleaving half-circles; 'circles' are two embedded circles}
\item{N}{Number of data points}
\item{shuffle}{Whether to randomize the output}
\item{noise}{Standard deviation of Gaussian noise applied to point positions}
\item{bdim}{Dimensionality of 'blobs' dataset}
\item{bcenters}{Number of 'blobs' centers}
\item{bnoise}{Standard deviation of 'blobs' Gaussian noise: vector of length one or length equal to the number of centers}
\item{bbox}{The bounding box within which blobs centers will be created}
\item{cfactor}{Scale factor between 'circles' (should be > 0 and < 1)}
}

\details{
Algorihms were taken partly from Python 'scikit-learn' and from Github 'elbamos/clusteringdatasets'.
}

% \value{}

% \references{}

\author{Alexey Shipunov}

% \seealso{}

\examples{
scikit.palette <- c("#377EB8", "#FF7F00", "#4DAF4A", "#F781BF", "#A65628", "#984EA3",
"#999999", "#E41A1C", "#DEDE00", "#000000")
palette(scikit.palette)
n.samples <- 500

## data
set.seed(21)
no.structure <- list(samples=cbind(runif(n.samples), runif(n.samples)),
 labels=rep(1, n.samples))
noisy.circles <- Gen.cl.data(type="circles", N=n.samples, cfactor=0.5, noise=0.05)
noisy.moons <- Gen.cl.data(type="moons", N=n.samples, noise=0.05)
blobs <- Gen.cl.data(type="blobs", N=n.samples, noise=1)
## anisotropically distributed data
aniso <- Gen.cl.data(type="blobs", N=n.samples)
aniso$samples <- aniso$samples \%*\% rbind(c(0.6, -0.6), c(-0.4, 0.8))
## blobs with varied variances
varied <- Gen.cl.data(type="blobs", N=n.samples, bnoise=c(1, 2.5, 0.5))
set.seed(NULL)

## single example
plot(aniso$samples, col=aniso$labels, pch=19)

## all data objects example
## old.X11.options <- X11.options(width=6, height=6) # to make square cells
oldpar <- par(mfrow=c(2, 3), mar=c(1, 1, 3, 1))
for (n in c("noisy.circles", "noisy.moons", "no.structure",
 "blobs", "aniso", "varied")) {
 plot(get(n)$samples, col=get(n)$labels, pch=19, main=n, xlab="", ylab="",
 xaxt="n", yaxt="n")
}
par(oldpar)
## X11.options <- old.X11.options

\donttest{
## comparison of clustering techniques example
## old.X11.options <- X11.options(width=10, height=6)  # to make square cells
oldpar <- par(mfrow=c(6, 10), mar=rep(0, 4), xaxt="n", yaxt="n")
COUNT <- 1
for (n in c("noisy.circles", "noisy.moons", "no.structure", "blobs", "aniso", "varied")) {
 K <- 3
 if (n \%in\% c("noisy.circles", "noisy.moons")) K <- 2
 TITLE <- function(x) if (COUNT==1) { legend("topleft", legend=x, cex=1.25, bty="n") }
##
 newlabels <- cutree(hclust(dist(get(n)$samples), method="ward.D2"), k=K)
 plot(get(n)$samples, col=newlabels, pch=19)
 TITLE("Ward")
##
 newlabels <- cutree(hclust(dist(get(n)$samples), method="average"), k=K)
 plot(get(n)$samples, col=newlabels, pch=19)
 TITLE("UPGMA")
##
 newlabels <- kmeans(round(get(n)$samples, 5), centers=K)$cluster
 plot(get(n)$samples, col=newlabels, pch=19)
 TITLE("K-means")
##
 newlabels <- cutree(as.hclust(cluster::diana(dist(get(n)$samples))), k=K) # slow
 plot(get(n)$samples, col=newlabels, pch=19)
 TITLE("DIANA")
##
 nn <- cluster::fanny(get(n)$samples, k=K) # a bit slow
 dunn <- apply(nn$membership, 1, function(.x) (sum(.x^2) - 1/K) / (1 - 1/K))
 fuzzy <- dunn < 0.05
 plot(get(n)$samples[!fuzzy, ], col=nn$clustering[!fuzzy], pch=19)
 points(get(n)$samples[fuzzy, ], col="black", pch=1)
 TITLE("FANNY")
##
 newlabels <- kernlab::specc(get(n)$samples, centers=K)
 plot(get(n)$samples, col=newlabels, pch=19)
 TITLE("spectral")
##
 nn <- apcluster::apclusterK(apcluster::negDistMat(), get(n)$samples, K=K) # very slow
 newlabes <- apply(sapply(nn@clusters,
  function(.y) 1:nrow(get(n)$samples) \%in\% .y), 1, which)
 plot(get(n)$samples, col=newlabels, pch=19)
 TITLE("AP") # affinity propagation
##
 ## eps values taken out of scikit and 'dbscan::kNNdistplot() "knee"', 'minPts' default
 EPS <- c(noisy.circles=0.3, noisy.moons=0.3, no.structure=0.3, blobs=1,
  aniso=0.5, varied=1)
 nn <- dbscan::dbscan(get(n)$samples, eps=EPS[n])
 outliers <- nn$cluster == 0
 plot(get(n)$samples[!outliers, ], col=nn$cluster[!outliers], pch=19)
 points(get(n)$samples[outliers, ], col="black", pch=1)
 TITLE("DBSCAN")
##
 newlabels <- meanShiftR::meanShift(get(n)$samples, nNeighbors=10)$assignment
 plot(get(n)$samples, col=newlabels, pch=19)
 TITLE("mean-shift")
##
 library(mclust)
 newlabels <- Mclust(get(n)$samples)$classification
 plot(get(n)$samples, col=newlabels, pch=19)
 TITLE("Gaussian")
 COUNT <- COUNT + 1
}
par(oldpar)
## X11.options <- old.X11.options
}

\donttest{
## comparison of linkages example
## old.X11.options <- X11.options(width=8, height=6) # to make square cells
oldpar <- par(mfrow=c(6, 8), mar=rep(0, 4), xaxt="n", yaxt="n")
COUNT <- 1
for (n in c("noisy.circles", "noisy.moons", "no.structure", "blobs", "aniso", "varied")) {
 K <- 3 ; if (n \%in\% c("noisy.circles", "noisy.moons")) K <- 2
 TITLE <- function(x) if (COUNT==1) { legend("topleft", legend=x, cex=1.25, bty="n") }
 newlabels <- cutree(hclust(dist(get(n)$samples), method="ward.D2"), k=K)
 plot(get(n)$samples, col=newlabels, pch=19)
 TITLE("Ward orig")
 newlabels <- cutree(hclust(dist(get(n)$samples), method="ward.D"), k=K)
 plot(get(n)$samples, col=newlabels, pch=19)
 TITLE("Ward")
 newlabels <- cutree(hclust(dist(get(n)$samples), method="average"), k=K)
 plot(get(n)$samples, col=newlabels, pch=19)
 TITLE("UPGMA")
 newlabels <- cutree(hclust(dist(get(n)$samples), method="single"), k=K)
 plot(get(n)$samples, col=newlabels, pch=19)
 TITLE("single")
 newlabels <- cutree(hclust(dist(get(n)$samples), method="complete"), k=K)
 plot(get(n)$samples, col=newlabels, pch=19)
 TITLE("complete")
 newlabels <- cutree(hclust(dist(get(n)$samples), method="mcquitty"), k=K)
 plot(get(n)$samples, col=newlabels, pch=19)
 TITLE("WPGMA")
 newlabels <- cutree(hclust(dist(get(n)$samples), method="median"), k=K)
 plot(get(n)$samples, col=newlabels, pch=19)
 TITLE("WPGMC")
 newlabels <- cutree(hclust(dist(get(n)$samples), method="centroid"), k=K)
 plot(get(n)$samples, col=newlabels, pch=19)
 TITLE("UPGMC")
 COUNT <- COUNT + 1
}
par(oldpar)
## X11.options <- old.X11.options
}
palette("default")
}

\keyword{datagen}
