\name{KM}
\alias{KM}
\title{
K-means over dense representation of data
}
\description{
Multithreaded weighted Minkowski and spherical K-means via Lloyd's algorithm over dense representation of data.
}
\usage{
KM(
  X,
  centroid,
  Xw = rep(1, ncol(X)),
  minkP = 2,
  maxIter = 100L,
  maxCore = 7L,
  verbose = TRUE
  )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{
A \code{d x N} numeric matrix where \code{N} is the number of data points --- each column is an observation, and \code{d} is the dimensionality. Column-observation representation promotes cache locality.
}
  \item{centroid}{
A \code{d x K} numeric matrix where \code{K} is the number of clusters. Each column represents a cluster center.
}
  \item{Xw}{
A numeric vector of size \code{N}. \code{Xw[i]} is the weight on observation \code{X[, i]}. Users should normalize \code{Xw} such that the elements sum up to \code{N}. Default uniform weights for all observations.
}
  \item{minkP}{
A numeric value or a character string. If numeric, \code{minkP} is the power \code{p} in the definition of Minkowski distance. If character string, \code{"max"} implies Chebyshev distance, \code{"cosine"} implies cosine dissimilarity. Default 2.
}
  \item{maxIter}{
An integer. The maximal number of iterations. Default 100.
}
  \item{maxCore}{
An integer. The maximal number of threads to invoke. No more than the total number of logical processors on machine. Default 7.
}
  \item{verbose}{
A boolean value. \code{TRUE} prints progress.
}
}
\details{
Implementation highlights include:

(i) In Minkowski distance calculation, integer power no greater than 30 uses multiplications. Fractional powers or powers above 30 call \code{std::pow()}.

(ii) Multithreaded observation-centroid distance calculations. Distances are memorized to avoid unnecessary recomputations if centroids did not change in the last iteration.

(iii) A lookup table is built for storing observation - centroid ID pairs during the assignment step. Observation IDs are then grouped by centroid IDs which allows parallel computing cluster means.

(iv) Function allows non-uniform weights on observations.

(v) Meta-template programming trims branches over different distance functions and other computing methods during compile time.
}


\value{
A list of size \code{K}, the number of clusters. Each element is a list of 3 vectors:
\item{centroid }{a numeric vector of size \code{d}.}
\item{clusterMember }{an integer vector of indexes of elements grouped to \code{centroid}.}
\item{member2centroidDistance }{a numeric vector of the same size of \code{clusterMember}. The \code{i}th element is the Minkowski distance or cosine dissimilarity from \code{centroid} to the \code{clusterMember[i]}th observation in \code{X}.}

Empty \code{clusterMember} implies empty cluster.
}


\note{
Although rarely happens, divergence of K-means with non-Euclidean distance \code{minkP != 2} measure is still a theoretical possibility.
}


\examples{
# ===========================================================================
# Play random numbers. See speed.
# ===========================================================================
N = 5000L # Number of points.
d = 500L # Dimensionality.
K = 50L # Number of clusters.
dat = matrix(rnorm(N * d) + runif(N * d), nrow = d)


# Use kmeans++ initialization.
centroidInd = GMKMcharlie::KMppIni(
  X = dat, K, firstSelection = 1L, minkP = 2, stochastic = FALSE,
  seed = sample(1e9L, 1), maxCore = 2L, verbose = TRUE)


centroid = dat[, centroidInd]


# Euclidean.
system.time({rst = GMKMcharlie::KM(
  X = dat, centroid = centroid, maxIter = 100,
  minkP = 2, maxCore = 2, verbose = TRUE)})


# Cosine dissimilarity.
dat = apply(dat, 2, function(x) x / sum(x ^ 2) ^ 0.5)
centroid = dat[, centroidInd]
system.time({rst2 = GMKMcharlie::KM(
  X = dat, centroid = centroid, maxIter = 100,
  minkP = "cosine", maxCore = 2, verbose = TRUE)})


# ===========================================================================
# Test against R's inbuilt km()
# ===========================================================================
dat = t(iris[1:4])
dimnames(dat) = NULL


# Use kmeans++ initialization.
centroidInd = GMKMcharlie::KMppIni(
  X = dat, K = 3, firstSelection = 1L, minkP = 2, stochastic = FALSE,
  seed = sample(1e9L, 1), maxCore = 2L, verbose = TRUE)
centroid = dat[, centroidInd]


rst = GMKMcharlie::KM(X = dat, centroid = centroid, maxIter = 100,
                      minkP = 2, maxCore = 2, verbose = TRUE)
rst = lapply(rst, function(x) sort(x$clusterMember))


rst2 = kmeans(x = t(dat), centers = t(centroid), algorithm = "Lloyd")
rst2 = aggregate(list(1L : length(rst2$cluster)),
                 list(rst2$cluster), function(x) sort(x))[[2]]


setdiff(rst, rst2)
}







