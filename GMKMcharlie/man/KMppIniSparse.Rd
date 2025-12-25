\name{KMppIniSparse}
\alias{KMppIniSparse}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Minkowski and spherical, deterministic and stochastic, multithreaded K-means++ initialization over sparse representation of data
}
\description{
Find suitable observations as initial centroids.
}
\usage{
KMppIniSparse(
  X,
  d,
  K,
  firstSelection = 1L,
  minkP = 2,
  stochastic = FALSE,
  seed = 123,
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
  \item{K}{
An integer, the number of centroids.
}
  \item{firstSelection}{
An integer, index of the observation selected as the first initial centroid in \code{X}. Should be no greater than \code{N}.
}
  \item{minkP}{
A numeric value or a character string. If numeric, \code{minkP} is the power \code{p} in the definition of Minkowski distance. If character string, \code{"max"} implies Chebyshev distance, \code{"cosine"} implies cosine dissimilarity. Default 2.
}
  \item{stochastic}{
A boolean value. \code{TRUE} runs the stochastic K-means++ initialization by Arthur and Vassilvitskii (2007). Roughly speaking, the algorithm is stochastic in the sense that each of the remaining observations has a probability of being selected as the next centroid, and the probability is an increasing function of the minimal distance between this observation and the existing centroids. In the same context, the deterministic version selects as the next centroid with probability 1 the observation that has the longest minimal distance to the existing centroids.
}
  \item{seed}{
Random seed if \code{stochastic}.
}
  \item{maxCore}{
An integer. The maximal number of threads to invoke. No more than the total number of logical processors on machine. Default 7.
}
  \item{verbose}{
A boolean value. \code{TRUE} prints progress.
}
}
\details{
In each iteration, the distances between the newly selected centroid and all the other observations are computed with multiple threads. Scheduling is homemade for minimizing the overhead of thread communication.
}
\value{
An integer vector of size \code{K}. The vector contains the indexes of observations selected as the initial centroids.
}

\examples{
N = 2000L
d = 3000L
X = matrix(rnorm(N * d) + 2, nrow = d)
# Fill many zeros in X:
X = apply(X, 2, function(x) {
  x[sort(sample(d, d * runif(1, 0.95, 0.99)))] = 0; x})
# Get the sparse version of X.
sparseX = GMKMcharlie::d2s(X)


K = 30L
seed = 123L
# Time cost of finding the centroids via dense representation.
# CRAN check allows only 2 threads. Increase `maxCore` for more speed.
system.time({kmppViaDense = GMKMcharlie::KMppIni(
  X, K, firstSelection = 1L, minkP = 2, stochastic = TRUE, seed = seed,
  maxCore = 2L)})


# Time cost of finding the initial centroids via sparse representation.
system.time({kmppViaSparse = GMKMcharlie::KMppIniSparse(
  sparseX, d, K, firstSelection = 1L, minkP = 2, stochastic = TRUE,
  seed = seed, maxCore = 2L)})


# Results should be identical.
sum(kmppViaSparse - kmppViaDense)
}















