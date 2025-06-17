\name{progenyClust-package}
\alias{progenyClust-package}
\docType{package}
\title{
Finding the Optimal Cluster Number Using Progeny Clustering
}
\description{
Implementing the Progeny Clustering algorithm based on Hu, Chenyue W., et al. "Progeny Clustering: A Method to Identify Biological Phenotypes." Scientific Reports 5 (2015), the progenyClust package assesses the clustering stability and identifies the optimal clustering number for a given data matrix. It uses kmeans clustering as default, but can be customized to work with other clustering algorithms and different parameter settings. The package includes one main function progenyClust(), plot and summary methods for "progenyClust" object, and two example dataset ("test" and "cell") for testing. 
}
\details{
Package: progenyclust\cr
Version: 1.1\cr
Date: 2015-11-24\cr
License: AGPL-3\cr
Imports: Hmisc\cr
Depends: graphics, stats
}
\author{
C.W. Hu, Rice University\cr
Maintainer: C.W. Hu <wendyhu001@gmail.com>
}
\references{
Hu, C.W., et al. "Progeny Clustering: A Method to Identify Biological Phenotypes." Scientific reports 5 (2015). \cr
http://www.nature.com/articles/srep12894
}

\keyword{ package }


