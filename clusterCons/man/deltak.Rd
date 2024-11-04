\name{deltak}
\alias{deltak}
\title{
Function to calculate the change in the area under the curve (AUC) across a range of cluster number values
}
\description{
This function takes an \code{"auc"} class object and calculates the difference in AUC value by cluster number (called delta-K). Peaks in delta-K
coincide with the cluster numbers that are most robust and provide estimates for the optimal cluster number.
}
\usage{
deltak(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
		a valid \code{"auc"} class object, normally provided as a result from the \code{\link{aucs}} function.
}
}

\value{ 
	\code{deltak(x)} returns a data.frame with the following variables.
	\item{k}{
		cluster number as a factor
		}
	\item{a}{
		algorithm identifier as a factor
		}
	\item{dk}{
		the delta-K value
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
Also see the \code{\link{aucs}} function.
}
\examples{
#load a test cluscomp result set
data(testcmr)

#calculate all of the AUC values from the \code{cluscomp} result for algorithm 'kmeans'
kmeanscmr <- testcmr[grep('kmeans',names(testcmr))];
acs <- aucs(kmeanscmr);

#calculate the delta-K values
dks <- deltak(acs);
}