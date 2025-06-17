\name{hclust.progenyClust}
\alias{hclust.progenyClust}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Hierarchical Clustering
}
\description{
hierarchical clustering function for progeny clustering
}
\usage{
hclust.progenyClust(x,k,h.method='ward.D2',dist='euclidean',p=2,...)
}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
  a numeric matrix, data frame or \code{\link{dist}} object.
}
  \item{k}{
  an integer specifying the number of clusters.
}
  \item{h.method}{
  the agglomeration method to be used. This should be (an unambiguous abbreviation of) one of \code{"ward.D"}, \code{"ward.D2"} (default), \code{"single"}, \code{"complete"}, \code{"average"} (= UPGMA), \code{"mcquitty"} (= WPGMA), \code{"median"} (= WPGMC) or \code{"centroid"} (= UPGMC).
  }
  \item{dist}{
  the distance measure to be used. This must be one of \code{"euclidean"}, \code{"maximum"}, \code{"manhattan"}, \code{"canberra"}, \code{"binary"} or \code{"minkowski"}. Any unambiguous substring can be given.
}
  \item{p}{
  The power of the Minkowski distance, when \code{dist="minkowski"}.
}
  \item{\dots}{
  additional arguments in \code{\link{hclust}}(...).
  }
}
\details{
The function \code{hclust.progenyClust} mainly streamlines \code{\link{dist}}, \code{\link{hclust}} and \code{\link{cutree}} into one, and structures the output to be directly used by \code{\link{progenyClust}}. Most arguments and explanations were kept the same to ensure consistancy and avoid confusion. For more details, please check each individual function.
}
\value{
 \item{cluster}{A vector of integers (from 1:k) indicating the cluster membership for each sample.}
 \item{tree}{An object of class \code{\link{hclust}} which describes the tree produced by the clustering process.}
 \item{dist}{A dissimilarity structure as produced by \code{\link{dist}}.}
%%  If it is a LIST, use
   
%% ...
}
\references{
Hu, C.W., et al. "Progeny Clustering: A Method to Identify Biological Phenotypes." Scientific reports 5 (2015). \cr
\url{http://www.nature.com/articles/srep12894}
}
\author{
C.W. Hu, Rice University
}

\examples{
# a 3-cluster 2-dimensional example dataset
data('test')

# default progeny clsutering
progenyClust(test,FUNclust=hclust.progenyClust,ncluster=2:5)->pc

# plot the scores to select the optimal cluster number
plot(pc)

# plot the clustering results with the optimal cluster number
plot(pc,test)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{hierarchical}
\keyword{ cluster }
\keyword{ plot }

% __ONLY ONE__ keyword per line
