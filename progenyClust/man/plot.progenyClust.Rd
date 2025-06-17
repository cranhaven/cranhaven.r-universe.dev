\name{plot.progenyClust}
\alias{plot.progenyClust}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot Progeny Clustering Results
}
\description{
Plot the cluster number selection results and visualizes the clustering results.
}
\usage{
\method{plot}{progenyClust}(x,data=NULL,k=NULL,errorbar=FALSE,xlab='',ylab='',...)
}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
  a \code{\link{progenyClust}} object.
}
  \item{data}{
  the full or a subset of the oringal data matrix that was used for clustering. If unspecified, the function will plot stability scores for cluster number selection; If specified, the function will plot the data in scatter plots with colors annotated by clustering memberships (Please see details below).
}
  \item{k}{
  integer specifying the cluster number for visualizing the clustering results of original data: only takes into effect when argument \code{data} is provided, and needs to be a cluster number that was previously investigated in \code{\link{progenyClust}} to generate the \code{progenyClust} object \code{x}. The default is the optimal number of clusters.  
  }
  \item{errorbar}{
  logical flag: specifies whether the error bars should be drawn. The error bars can only be drawn when progeny clustering is repeated multiple times, i.e. input argument "repeats" in function \link{progenyClust} is greater than 1.
}
  \item{xlab}{
  character string specifying the name of the x axis.
}
  \item{ylab}{
  character string specifying the name of the y axis.
}
  \item{\dots}{
  additional graphical arguments in \code{\link{plot}}(...).
  }
}
\details{
The plot function provides two types of visualization: (1) visualizing stability scores, and (2) visualizing clustering results. 
To visualize the stability scores that are output from \code{\link{progenyClust}} function, please run the plot function without specifying the input argument \code{data}. The resulting plot visualizes the stability score at each cluster number. This plot can provide an overview of clustering stability, and can facilitate selecting the optimal cluster number.\cr\cr
The plot function can also visualize the clustering results in scatter plots by specifying the input argument \code{data}. Since the goal is to view how the original data is clustered with certain cluster number, \code{data} needs to contain exactly the same number of samples as in the original data that was used to run the \code{\link{progenyClust}} function. If \code{data} contains more than two features, a table of scatter plots will be created to show clustering results within each pair of dimensions. \code{data} with more than 20 features/columns will not be accepted, but a subset of \code{data} with selected features can be used in this case. The input argument \code{k} specifies the cluster number at which the clustering result is shown. Note that \code{k} needs to be a cluster number that was previously examined by \code{\link{progenyClust}} when generating the \code{progenyClust} object \code{x}. If \code{k} is not provided, the function will use the optimal cluster number determined by the Gap criterion only if \code{method='gap'}, and will use the optimal number determined by the Score criterion if \code{method='gap'} or \code{method='both'} when running \code{\link{progenyClust}}. 
}
\value{
returns plots as described in Details.
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
progenyClust(test,ncluster=2:5)->pc

# plot the scores to select the optimal cluster number
plot(pc)

# plot the clustering results with the optimal cluster number
plot(pc,test)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ cluster }
\keyword{ plot }
% __ONLY ONE__ keyword per line
