\name{memrob}
\alias{memrob}
\title{Calculate the membership robustness from consensus clustering results}

\description{
This function calculates the membership robustness from a \code{\link{consmatrix}} or \code{\link{mergematrix}}
class object. 
}
\usage{
memrob(x,rm)
}

\arguments{
  \item{x}{
		either a \code{\link{consmatrix}} or \code{\link{mergematrix}} object. 
}
  \item{rm}{
		(optional) if a \code{\link{mergematrix}} object is passed then you must provide a reference clustering
		structure to calculate cluster robustness against. These structures are stored with every \code{\link{consmatrix}}
		object in the 'rm' slot. You would normally select a reference matrix for a cluster number matching that of the
		\code{\link{mergematrix}} (see example below). 
	}
}

\value{
Returns a list of \code{\link{memroblist}} class objects, one for each cluster, and the full membership robustness matrix as a \code{\link{memrobmatrix}} class object.
}

\references{
Merged consensus clustering to assess and improve class discovery with microarray data. Simpson TI, Armstrong JD and Jarman AP. BMC Bioinformatics 2010, 11:590.
}

\author{
Dr. T. Ian Simpson
\email{ian.simpson@ed.ac.uk}
}

\seealso{
Also see \code{\link{cluscomp}}, \code{\link{consmatrix}} and \code{\link{mergematrix}}.
}
\examples{
#load cmr (consensus clustering result produced by cluscomp)
data(testcmr);

#calculate the cluster robustness of the consensus matrix for pam where k=4
mr1 <- memrob(testcmr$e1_kmeans_k4);

#show the membership robustness of cluster 1
mr1$cluster1;

#calculate the cluster robustness of the merge matrix in reference
#to the clustering structure of pam where k=4
mr2 <- memrob(testcmr$merge_k4,testcmr$e1_kmeans_k4@rm);

#plot a heatmap of the full membership robustness matrix
heatmap(mr2$resultmatrix@mrm)
}