\name{KMconstrained}
\alias{KMconstrained}
\title{
K-means over dense data input with constraints on cluster weights
}
\description{
Multithreaded weighted Minkowski and spherical K-means via Lloyd's algorithm over dense representation of data given cluster size (weight) constraints.
}
\usage{
KMconstrained(
  X,
  centroid,
  Xw = rep(1, ncol(X)),
  clusterWeightUB = rep(ncol(X) + 1, ncol(centroid)),
  minkP = 2,
  convergenceTail = 5L,
  tailConvergedRelaErr = 1e-04,
  maxIter = 100L,
  maxCore = 7L,
  paraSortInplaceMerge = FALSE,
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
  \item{clusterWeightUB}{
An integer vector of size \code{K}. The upper bound of weight for each cluster. If \code{Xw} are all 1, \code{clusterWeightUB} upper-bound cluster sizes.
}
  \item{minkP}{
A numeric value or a character string. If numeric, \code{minkP} is the power \code{p} in the definition of Minkowski distance. If character string, \code{"max"} implies Chebyshev distance, \code{"cosine"} implies cosine dissimilarity. Default 2.
}
  \item{convergenceTail}{
An integer. The algorithm may end up with "cyclical convergence" due to the size / weight constraints, that is, every few iterations produce the same clustering. If the cost (total in-cluster distance) of each of the last \code{convergenceTail} iterations has a relative difference less than \code{tailConvergedRelaErr} against the cost from the prior iteration, the program stops.
}
  \item{tailConvergedRelaErr}{
A numeric value, explained in \code{convergenceTail}.
}
  \item{maxIter}{
An integer. The maximal number of iterations. Default 100.
}
  \item{maxCore}{
An integer. The maximal number of threads to invoke. No more than the total number of logical processors on machine. Default 7.
}
  \item{paraSortInplaceMerge}{
A boolean value. \code{TRUE} let the algorithm call \code{std::inplace_merge()} (\code{std} refers to C++ STL namespace) instead of \code{std::merge()} for parallel-sorting the observation-centroid distances. In-place merge is slower but requires no extra memory.
}
  \item{verbose}{
A boolean value. \code{TRUE} prints progress.
}
}
\details{
See details in \code{KM()} for common implementation highlights. Weight upper bounds are implemented as follows:

In each iteration, all the (observation ID, centroid ID, distance) tuples are sorted by distance. From the first to the last tuple, the algorithm puts observation in the cluster labeled by the centroid ID, if (i) the observation has not already been assigned and (ii) the cluster size has not exceeded its upper bound. The actual implementation is slightly different. A parallel merge sort is crafted for computing speed.
}
\value{
A list of size \code{K}, the number of clusters. Each element is a list of 3 vectors:
\item{centroid }{a numeric vector of size \code{d}.}
\item{clusterMember }{an integer vector of indexes of elements grouped to \code{centroid}.}
\item{member2centroidDistance }{a numeric vector of the same size of \code{clusterMember}. The \code{i}th element is the Minkowski distance or cosine dissimilarity from \code{centroid} to the \code{clusterMember[i]}th observation in \code{X}.}

Empty \code{clusterMember} implies empty cluster.
}

\note{
Although rarely happens, divergence of K-means with non-Euclidean distance \code{minkP != 2} measure is still a theoretical possibility. Bounding the cluster weights / sizes increases the chance of divergence.
}

\examples{
N = 3000L # Number of points.
d = 500L # Dimensionality.
K = 50L # Number of clusters.
dat = matrix(rnorm(N * d) + runif(N * d), nrow = d)


# Use kmeans++ initialization.
centroidInd = GMKMcharlie::KMppIni(
  X = dat, K, firstSelection = 1L, minkP = 2, stochastic = FALSE,
  seed = sample(1e9L, 1), maxCore = 2L, verbose = TRUE)


centroid = dat[, centroidInd]


# Each cluster size should not be greater than N / K * 2.
sizeConstraints = as.integer(rep(N / K * 2, K))
system.time({rst = GMKMcharlie::KMconstrained(
  X = dat, centroid = centroid, clusterWeightUB = sizeConstraints,
  maxCore = 2L, tailConvergedRelaErr = 1e-6, verbose = TRUE)})


# Size upper bounds vary in [N / K * 1.5, N / K * 2]
sizeConstraints = as.integer(round(runif(K, N / K * 1.5, N / K * 2)))
system.time({rst = GMKMcharlie::KMconstrained(
  X = dat, centroid = centroid, clusterWeightUB = sizeConstraints,
  maxCore = 2L, tailConvergedRelaErr = 1e-6, verbose = TRUE)})
}







































