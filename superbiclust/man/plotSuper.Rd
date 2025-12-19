\name{plotSuper}
\alias{plotSuper}
\title{
Plot gene profiles within biclusters
}

\description{
Function for plotting gene profiles for compounds within constructed super-bicluster}
\usage{
plotSuper(x, data, BiclustSet)
}

\arguments{
  \item{x}{
	a vector, containing indices of biclusters, to be joined for obtaining the robust bicluster  
}
  \item{data}{
	matrix, dataset, from which the bicluster results are obtained  
}
  \item{BiclustSet}{
	a BiclustSet object containing bicluster output  
}
}

\details{
This function constructs a robust bicluster from a set of biclusters identified in a hierarchical tree and
plots gene profiles for columns in a robust bicluster. Each line represents a gene from a bicluster. 
The bicluster is saved as \code{Biclust} object which can be further plotetd by available functions from \code{biclust package}.
The information about the number of biclusters used to generate the resulting robust bicluster is saved in \code{Call} slot of the object.
This information is important to see how often the bicluster has been discovered under different parameter settings (e.g. initialization seeds) 
Indices used as an input can be obtained by identify function or by cutting the tree.
}
\value{
biclust object containing bicluster and the information about bicluster subset used to generate it
}
\author{
Tatsiana Khamiakova
}
\seealso{
\code{\link{HCLtree}}, \code{\link{plotSuperAll}}, \code{\link{plotProfilesWithinBicluster}}
}
