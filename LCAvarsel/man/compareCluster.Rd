\name{compareCluster}
\alias{compareCluster}
\title{Clustering comparison criteria}

\description{
Computes some criteria for comparing two classifications of the data points.
}

\usage{
compareCluster(class1, class2)
}

\arguments{
\item{class1}{A numeric or character vector of class labels.}

\item{class2}{A numeric or character vector of class labels. Must be same length of \code{class1}.}
}

\value{
A list containing:
\item{tab}{The confusion matrix between the two clusterings.}
\item{jaccard}{Jaccard index.}
\item{RI}{Rand index.}
\item{ARI}{Adjusted Rand index.}
\item{varInfo}{Variation of information between the two clusterings.}
}

\details{
The Jaccard, Rand and adjusted Rand indices measure the agreement between two partitions of the units. These indices vary in the interval \eqn{[0,1]} and a value of 1 corresponds to a perfect correspondence. Note that sometimes the adjusted Rand index could take negative values (see Hubert, Arabie, 1985). The variation of information is a measure of the distance between the two clusterings and a small value is indication of closeness.
}

\references{
Hubert, L. and Arabie, P. (1985). Comparing partitions. \emph{Journal of Classification}, 2193-218.

Meila, M. (2007). Comparing clusterings - an information based distance. \emph{Journal of Multivariate Analysis}, 98, 873-895.
}

\examples{
cl1 <- sample(1:3, 100, replace = TRUE)
cl2 <- sample(letters[1:4], 100, replace = TRUE)
compareCluster(cl1, cl2)
compareCluster(cl1, cl1)   # perfect matching
}
