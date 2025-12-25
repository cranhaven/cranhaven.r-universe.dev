\name{KMsparse}
\alias{KMsparse}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
K-means over sparse representation of data
}
\description{
Multithreaded weighted Minkowski and spherical K-means via Lloyd's algorithm over sparse representation of data.
}
\usage{
KMsparse(
  X,
  d,
  centroid,
  Xw = rep(1, length(X)),
  minkP = 2,
  maxIter = 100L,
  maxCore = 7L,
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
See details in \code{KM()} for implementation highlights. There are some other optimizations such as, except for the maximum norm, cost of computing the distance between a dense centroid vector and a sparse observation is linear to the size of the sparse observation, which should be largely less than the size of the dense vector. This is done by letting every centroid memorize its before-root Minkowski norm. The full distance can then be inferred from adding the residual norm to the partial distance.
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
N = 10000L # Number of points.
d = 500L # Dimensionality.
K = 100L # Number of clusters.


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
# GMKMcharlie::d2s() acheives the same.
sparsedat = apply(dat, 2, function(x)
{
  nonz = which(x != 0)
  list(nonz, x[nonz])
}); gc()


centroidInd = sample(length(sparsedat), K)


# Test speed using dense representation.
centroid = dat[, centroidInd]
system.time({rst = GMKMcharlie::KM(
  X = dat, centroid = centroid, maxIter = 100,
  minkP = 2, maxCore = 2, verbose = TRUE)})


# Test speed using sparse representation.
sparseCentroid = sparsedat[centroidInd]
system.time({sparseRst = GMKMcharlie::KMsparse(
  X = sparsedat, d = d, centroid = sparseCentroid,
  maxIter = 100, minkP = 2, maxCore = 2, verbose = TRUE)})
}




