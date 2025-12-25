\name{KMppIni}
\alias{KMppIni}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Minkowski and spherical, deterministic and stochastic, multithreaded K-means++ initialization over dense representation of data
}
\description{
Find suitable observations as initial centroids.
}
\usage{
KMppIni(
  X,
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
A \code{d x N} numeric matrix where \code{N} is the number of data points --- each column is an observation, and \code{d} is the dimensionality. Column-observation representation promotes cache locality.
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
N = 30000L
d = 300L
K = 30L
X = matrix(rnorm(N * d) + 2, nrow = d)
# CRAN check allows examples invoking 2 threads at most. Change `maxCore`
# for acceleration.
kmppSt = KMppIni(X, K, firstSelection = 1L, minkP = 2,
                 stochastic = TRUE, seed = sample(1e9L, 1), maxCore = 2L)
kmppDt = KMppIni(X, K, firstSelection = 1L, minkP = 2,
                 stochastic = FALSE, maxCore = 2L)
str(kmppSt)
str(kmppDt)
}



