\name{auc}
\alias{auc}
\alias{aucs}
\title{
Calculate area under the curve statistics
}
\description{
These functions calculate the area under the curve (AUC) for cumulative density functions of a consensus matrix. The function \code{auc} operates on an indvidual consensus matrix whereas \code{aucs} operates on an entire \code{cluscomp} analysis result as described below.
}
\usage{
auc(x)
aucs(x)
}

\arguments{
	\item{x}{
	For \code{auc(x)}, provide a numeric square data matrix such as an individual consensus matrix. For \code{aucs(x)} provide a list of \code{"consmatrix"} class objects (see \code{\link{consmatrix-class} for details}) such as those produced directly by the \code{\link{cluscomp}} function.

	The functions will not allow any missing values (NAs).
	}
}

\value{
	\code{auc(x)} returns an individual AUC value.
	
	\code{aucs(x)} returns a data.frame with the following variables.
	\item{k}{
		cluster number as a factor
		}
	\item{a}{
		algorithm identifier as a factor
		}
	\item{aucs}{
		the AUC value
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
\code{\link{consmatrix-class}}
}
\examples{
#load up a test cluscomp result
data('testcmr');

#look at the result structure
summary(testcmr);

#calculate an individual AUC value for a consensus matrix
ac <- auc(testcmr$e1_kmeans_k2@cm);

#calculate all of the AUC values from the \code{cluscomp} result for algorithm 'kmeans'
kmeanscmr <- testcmr[grep('kmeans',names(testcmr))];
acs <- aucs(kmeanscmr);
}