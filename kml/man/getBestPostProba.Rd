\name{getBestPostProba}
\Rdversion{1.1}
\alias{getBestPostProba}

\title{
~ Function: getBestPostProba ~
}
\description{
  Given a \code{\linkS4class{ClusterLongData}} object that hold a
  \code{Partition}, this function extract the best
  posterior probability of each individual.
}

\usage{
getBestPostProba(xCld, nbCluster, clusterRank = 1)
}

\arguments{
  \item{xCld}{\code{[ClusterLongData]}: object from who a cluster should
    be extracted.}
  \item{nbCluster}{\code{[integer]}: number of cluster of the desired cluster.}
  \item{clusterRank}{\code{[integer]}: rank of the partition in the
    clusters list.}
}
\details{
  Given a \code{\linkS4class{ClusterLongData}} object that hold a
  \code{Partition}, this function extract the best
  posterior probability of each individual.
}

\value{A vector of numeric.}

\seealso{
  \code{\linkS4class{ClusterLongData}}
}
\examples{
### Move to tempdir
wd <- getwd()
setwd(tempdir()); getwd()

### Creation of an object ClusterLongData
myCld <- gald(20)

### Computation of some partition
kml(myCld,2:4,3)

### Extraction the best posterior probabilities
### form the list of partition with 3 clusters of the second clustering
getBestPostProba(myCld,3,2)

### Go back to current dir
setwd(wd)
}
