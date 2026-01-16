\name{getClusters}
\Rdversion{1.1}
\alias{getClusters}

\title{
~ Function: getClusters ~
}
\description{
This function extract a cluster affectation from an
\code{\linkS4class{ClusterLongData}} object.
}

\usage{
getClusters(xCld, nbCluster, clusterRank = 1, asInteger = FALSE)
}

\arguments{
  \item{xCld}{\code{[ClusterLongData]}: object from who a cluster should
    be extracted.}
  \item{nbCluster}{\code{[integer]}: number of cluster of the desired cluster.}
  \item{clusterRank}{\code{[integer]}: rank of the partition in the
    clusters list.}
  \item{asInteger}{\code{[logical]}: should the cluster be given as a
    vector of integer ? If FALSE, a vector of LETTERS is return.}
}
\details{
  This function extract a clusters from an object
  \code{\linkS4class{ClusterLongData}}.
  It is almost the same as
  \code{xCld[paste("c",nbCluster,sep="")][[clusterRank]]} except that
  the individual with too many missing value (and thus excludes from the
  analysis) will be noted by some NA values.
}

\value{A vector of numeric or a LETTER, according to the value of \code{asInteger}.}

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

### Extraction form the list of partition with 3 clusters
###   of the second clustering
getClusters(myCld,3,2)

### Go back to current dir
setwd(wd)
}
