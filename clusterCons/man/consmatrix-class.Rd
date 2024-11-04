\name{consmatrix-class}
\docType{class}
\alias{consmatrix-class}
\alias{consmatrix}

\title{Class "consmatrix"}
\description{
Objects of class \code{'consmatrix'} are created to hold the results of a consensus clustering experiment along with the necessary ancillary
data to allow the subsequent downstream calculations such as cluster and membership robustness. In addition the object holds the original call 
made when running \code{\link{cluscomp}}.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("consmatrix", ...)}, but are normally created internally by the \code{\link{cluscomp}} function to store
consensus matrices and their associated meta-data.
}
\section{Slots}{
  \describe{
    	\item{\code{cm}:}{Object of class \code{"matrix"} - the consensus matrix itself}
    	\item{\code{rm}:}{Object of class \code{"data.frame"} - the cluster membership of the full (i.e. not consensus) clustering result when 
    	the current algorith is called with the same algorithm parameters as the consensus clustering run. This is needed to be able to work with
    	merge matrices that need a clustering structure on which to operate to produce cluster and membership robustness values.}
    	\item{\code{a}:}{Object of class \code{"character"} - the clustering algorithm name }
    	\item{\code{k}:}{Object of class \code{"numeric"} - the cluster number (k) used }
    	\item{\code{call}:}{Object of class \code{"call"} - the original parameters passed to \code{cluscomp} for provenance and reproducibility}
	}
}

\references{
Merged consensus clustering to assess and improve class discovery with microarray data. Simpson TI, Armstrong JD and Jarman AP. BMC Bioinformatics 2010, 11:590.
}

\author{
Dr. T. Ian Simpson
\email{ian.simpson@ed.ac.uk}
}

\seealso{
See Also \code{\link{cluscomp}}
}
\examples{
showClass("consmatrix");

#you can access the slots in useful ways

#load a cmr
data(testcmr);

#get a consensus clustering matrix via the 'cm' slot
cm <- testcmr$e1_kmeans_k4@cm;

#this can be used as a distance matrix, e.g. for a heatmap
heatmap(cm);

#or as a new distance matrix
dm <- data.frame(cm) #first convert to a data.frame
#make sure names are the same for rows and columns
names(dm) <- row.names(dm);

#you need to explicitly tell cluscomp that you are passing a distance matrix
cmr2 <- cluscomp(dm,diss=TRUE,clmin=2,clmax=4,rep=2);

#for merge consensus clustering you take advantage of the reference matrix (rm) slot
#cluster robustness for agnes with cluster number (k) = 3
clrob(testcmr$merge_k3,testcmr$e1_kmeans_k3@rm);
#membership robustness for cluster 1
memrob(testcmr$merge_k3,testcmr$e1_kmeans_k3@rm)$cluster1;
}

\keyword{classes}
