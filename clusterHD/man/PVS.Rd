\name{PVS}
\alias{PVS}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Pooled variable scaling for cluster analysis
}
\description{
The function computes a scale for each variable in the data. 
The result can then be used to standardize a dataset before applying
a clustering algorithm (such as k-means). The scale estimation is based on pooled scale estimators, which result from clustering the individual variables in the data. The method is proposed in Raymaekers, and Zamar (2020) <doi:10.1093/bioinformatics/btaa243>.
}
\usage{
PVS(X, kmax = 3, dist = "euclidean",
    method = "gap", B = 1000,
    gapMethod = "firstSEmax",
    minSize = 0.05, rDist = runif,
    SE.factor = 1, refDist = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{ an \eqn{n} by \eqn{p} data matrix.}
  \item{kmax}{ maximum number of clusters in one variable. Default is \code{3}.}
  \item{dist}{\code{"euclidean"} for pooled standard deviation and \code{"manhattan"} for pooled mean absolute deviation. Default is \code{"euclidean"}.}
  \item{method}{either \code{"gap"} or \code{"jump"} to determine the number of clusters. Default is \code{"gap"}.}
  \item{B}{ number of bootstrap samples for the reference distribution of the gap statistic. Default is \code{1000}.}
  \item{gapMethod}{method to define number of clusters in the gap statistic. See \code{\link[cluster:maxSE]{cluster::maxSE}} for more info. Defaults to \code{"firstSEmax"}.}
  \item{minSize}{ minimum cluster size as a percentage of the total number of observations. Defaults to \code{0.05}.}
  \item{rDist}{Optional. Reference distribution (as a function) for the gap statistic. Defaults to \code{runif}, the uniform distribution.}
  \item{SE.factor}{factor for determining number of clusters when using the gap statistic.  See \code{\link[cluster:maxSE]{cluster::maxSE}} for more details. Defaults to \code{1}}
  \item{refDist}{ Optional. A \code{k} by \code{2} matrix with the mean and standard error of the reference distribution of the gap statistic in its columns. Can be used to avoid bootstrapping when repeatedly applying the function to same size data.}
}

\value{
A vector of length \code{p} containing the estimated scales for the variables.
}
\references{
Raymaekers, J, Zamar, R.H. (2020). Pooled variable scaling for cluster analysis. \emph{Bioinformatics}, \bold{36}(12), 3849-3855. \doi{10.1093/bioinformatics/btaa243}
}
\author{
Jakob Raymaekers
}


\examples{


X <- iris[, -5]
y <- unclass(iris[, 5])

# Compute scales using different scale estimators.
# the pooled standard deviation is considerably smaller for variable 3 and 4:
sds     <- apply(X, 2, sd); round(sds, 2)
ranges  <- apply(X, 2, function(y) diff(range(y))); round(ranges, 2)
psds    <- PVS(X); round(psds, 2)

# Now cluster using k-means after scaling the data

nbclus <- 3
kmeans.std <- kmeans(X, nbclus, nstart = 100) # no scaling
kmeans.sd  <- kmeans(scale(X), nbclus, nstart = 100)
kmeans.rg  <- kmeans(scale(X, scale = ranges), nbclus, nstart = 100)
kmeans.psd <- kmeans(scale(X, scale = psds), nbclus, nstart = 100)

# Calculate the Adjusted Rand Index for each of the clustering outcomes
round(mclust::adjustedRandIndex(y, kmeans.std$cluster), 2) 
round(mclust::adjustedRandIndex(y, kmeans.sd$cluster), 2) 
round(mclust::adjustedRandIndex(y, kmeans.rg$cluster), 2) 
round(mclust::adjustedRandIndex(y, kmeans.psd$cluster), 2)


}
