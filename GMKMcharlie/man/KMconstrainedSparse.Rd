\name{KMconstrainedSparse}
\alias{KMconstrainedSparse}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
K-means over sparse data input with constraints on cluster weights
}
\description{
Multithreaded weighted Minkowski and spherical K-means via Lloyd's algorithm over sparse representation of data given cluster size (weight) constraints.
}
\usage{
KMconstrainedSparse(
  X,
  d,
  centroid,
  Xw = rep(1, length(X)),
  clusterWeightUB = rep(length(X) + 1, length(centroid)),
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
A list of size \code{N}, the number of observations. \code{X[[i]]} is a 2-column data frame. The 1st column is a sorted \strong{integer vector} of the indexes of nonzero dimensions. Values in these dimensions are stored in the 2nd column as a \strong{numeric vector}. Internally the algorithm sets a 32-bit \emph{int} pointer to the beginning of the 1st column and a 64-bit \emph{double} pointer to the beginning of the 2nd column, so it is critical that the input has the correct type.
}
  \item{d}{
An integer. The dimensionality of \code{X}. \code{d} MUST be no less than the maximum of all index vectors in \code{X}.
}
  \item{centroid}{
A list of size \code{K}, the number of clusters. \code{centroid[[i]]} can be in dense or sparse representation. If dense, a numeric vector of size \code{d}. If sparse, a 2-column data frame in the same sense as \code{X[[i]]}.
}
  \item{Xw}{
A numeric vector of size \code{N}. \code{Xw[i]} is the weight on observation \code{X[[i]]}. Users should normalize \code{Xw} such that the elements sum up to \code{N}. Default uniform weights for all observations.
}
  \item{clusterWeightUB}{
An integer vector of size \code{K}. The upper bound of weight for each cluster. If \code{Xw} are all 1s, \code{clusterWeightUB} upper-bound cluster sizes.
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
See details for \code{KMconstrained()} and \code{KM()}
}
\note{
Although rarely happens, divergence of K-means with non-Euclidean distance \code{minkP != 2} measure is still a theoretical possibility. Bounding the cluster weights / sizes increases the chance of divergence.
}
\value{
A list of size \code{K}, the number of clusters. Each element is a list of 3 vectors:
\item{centroid }{a numeric vector of size \code{d}.}
\item{clusterMember }{an integer vector of indexes of elements grouped to \code{centroid}.}
\item{member2centroidDistance }{a numeric vector of the same size of \code{clusterMember}. The \code{i}th element is the Minkowski distance or cosine dissimilarity from \code{centroid} to the \code{clusterMember[i]}th observation in \code{X}.}

Empty \code{clusterMember} implies empty cluster.
}

\examples{
N = 5000L # Number of points.
d = 500L # Dimensionality.
K = 50L # Number of clusters.


# Create a data matrix, about 95% of which are zeros.
dat = matrix(unlist(lapply(1L : N, function(x)
{
  tmp = numeric(d)
  # Nonzero entries.
  Nnz = as.integer(max(1, d * runif(1, 0, 0.05)))
  tmp[sample(d, Nnz)] = runif(Nnz) + rnorm(Nnz)
  tmp
})), nrow = d); gc()


# Convert to sparse representation.
# GMKMcharlie::d2s() is equivalent.
sparsedat = apply(dat, 2, function(x)
{
  nonz = which(x != 0)
  list(nonz, x[nonz])
}); gc()


centroidInd = sample(length(sparsedat), K)


# Test speed using sparse representation.
sparseCentroid = sparsedat[centroidInd]
# Size upper bounds vary in [N / K * 1.5, N / K * 2]
sizeConstraints = as.integer(round(runif(K, N / K * 1.5, N / K * 2)))
system.time({sparseRst = GMKMcharlie::KMconstrainedSparse(
  X = sparsedat, d = d, centroid = sparseCentroid,
  clusterWeightUB = sizeConstraints,
  tailConvergedRelaErr = 1e-6,
  maxIter = 100, minkP = 2, maxCore = 2, verbose = TRUE)})
}




