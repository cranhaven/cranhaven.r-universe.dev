\name{plotSuperAll}
\alias{plotSuperAll}
\title{
Plot gene profiles for all samples in the data
}

\description{
Function for plotting bicluster gene profiles for all samples in the data}
\usage{
plotSuperAll(x, data, BiclustSet)
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
This function constructs a robust bicluster from a subset of biclusters specified in x argument and plots the expression profiles 
}
\value{
biclust object
}
\author{
Tatsiana Khamiakova
}
\seealso{
\code{\link{HCLtree}}, \code{\link{plotProfilesAcrossAllSamples}}
}
