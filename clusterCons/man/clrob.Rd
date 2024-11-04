\name{clrob}
\alias{clrob}
\title{Calculate the cluster robustness from consensus clustering results}

\description{
This function calculates the cluster robustness from a \code{\link{consmatrix}} or \code{\link{mergematrix}}
class object. 
}
\usage{
clrob(x,rm)
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
Returns a data.frame of the cluster robustness values indexed by cluster number.
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
clrob(testcmr$e1_kmeans_k4);

#calculate the cluster robustness of the merge matrix in reference
#to the clustering structrure of pam where k=4
clrob(testcmr$merge_k4,testcmr$e1_kmeans_k4@rm);
}