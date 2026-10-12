\name{cluster.size}
\alias{cluster.size}
\title{
Get Summary Information of The Data.
}
\description{
Get necessary information (i.e., the number of clusters, cluster sizes) of the data set. 
}
\usage{
cluster.size(id)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
 \item{id}{
 The variable name of the cluster id in the data set. 
    }
 }
  
\details{
 This function will get the number of clusters and cluster sizes of the data set. 
}
\value{
  \item{n }{a vector of numbers denote the cluster sizes for different id.}
  \item{m}{the total number of clusters in the data set.}
}

\author{
Ming Wang <mwang@phs.psu.edu>
Maintainer: Zheng Li <zheng.li@outlook.com>
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{cluster}
\keyword{GEE}% __ONLY ONE__ keyword per line
