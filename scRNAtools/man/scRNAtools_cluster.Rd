\name{scRNAtools_cluster}
\alias{scRNAtools_cluster}
\title{Cluster section}
\description{Do consistent clustering analysis use clusterProfiler method}
\usage{
scRNAtools_cluster(example1, k)
}
\arguments{
  \item{example1}{
scRNA sequencing data with header.
}
  \item{k}{
The number of class. If you set k is 6, you will obtain 6 results of cluster.
}
}
\details{
The results are presented in your occurrent path.
}
\references{
Guangchuang Yu, Li-Gen Wang, Yanyan Han and Qing-Yu He. clusterProfiler: an R package for comparing biological themes among gene clusters. OMICS: A Journal of Integrative Biology 2012, 16(5):284-287.
}
\author{
Qian Yang
}
\examples{
##setwd("")###Set the path your data in.
data(example1)##Example data in this package.
k<-6##set K based on your own requirement.
scRNAtools_cluster(example1,k)
}
