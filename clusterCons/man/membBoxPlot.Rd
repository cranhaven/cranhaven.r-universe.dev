\name{membBoxPlot}
\alias{membBoxPlot}
\title{
Generate a box and whisker plot of membership robustness for all clusters
}
\description{
This function uses the \code{\link{lattice}} function \code{\link{bwplot}} to generate a box and whisker plot of membership robustness from the result of a call to the \code{memrob} function.  
}
\usage{
membBoxPlot(x)
}

\arguments{
	\item{x}{
		the result of a call to the \code{memrob} function.
	}
}

\value{No return value, called for side effects}

\references{
Merged consensus clustering to assess and improve class discovery with microarray data. Simpson TI, Armstrong JD and Jarman AP. BMC Bioinformatics 2010, 11:590.
}

\author{
Dr. T. Ian Simpson
\email{ian.simpson@ed.ac.uk}
}

\seealso{
\code{\link{memroblist-class}},\code{\link{memrob}}
}
\examples{
#load up a test cluscomp result
data('testcmr');

#calculate the membershpi robustness for one of the clustering results
mr <- memrob(testcmr$e1_kmeans_k5);

#plot the bwplot
membBoxPlot(mr);
}