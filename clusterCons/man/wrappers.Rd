\name{wrappers}
\alias{wrappers}
\alias{agnes_clmem}
\alias{pam_clmem}
\alias{hclust_clmem}
\alias{diana_clmem}
\alias{kmeans_clmem}
\alias{apcluster_clmem}
\title{
Functions to wrap command calls to clustering functions
}
\description{
These are primarily internal functions called by \code{\link{cluscomp}} to execute 
clustering runs and are unlikely to be used directly. The wrappers are detailed in 
the \code{algorithm.R} file of the \code{clusterCons} package and the user can add
their own wrappers to this to extend the number of algorithms supported. These five
wrappers allow the user to specify the conditions under which the corresponding 
clustering algorithms are run and follow exactly the same specifications as the
corresponding \code{\link{cluster}} functions (see \code{\link{agnes}}, \code{\link{pam}},
 \code{\link{hclust}}, \code{\link{diana}} and \code{\link{kmeans}}).
}
\usage{
agnes_clmem(x, clnum, params = list())
pam_clmem(x, clnum, params = list())
hclust_clmem(x, clnum, params = list())
diana_clmem(x, clnum, params = list())
kmeans_clmem(x, clnum, params = list())
apcluster_clmem(x,clnum,params = list())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
		A data.frame of numerical values to be clustered which must pass the \code{\link{data_check}}
		function. This function simply checks that there are no missing values, that all of the data
		is numeric and that row.names and column.names are unique. This is essential to ensure that
		individual rows (e.g. genes) and columns (e.g. experimental conditions) can be identified
		consistently. 
}
  \item{clnum}{
		The number of specified clusters. When using the \code{\link{cluscomp}} function, this will
		be over-ridden by the cluster range specified using the parameters \code{clmin}
		and \code{clmax} (see \code{\link{cluscomp}} for details).
}
  \item{params}{
  		A list of key, value pairs specifying the parameters to pass to the clustering algorithm. These
  		follow the exact specification of the original functions in the \code{\link{cluster}} package 
  		(see \code{\link{agnes}}, \code{\link{pam}}, \code{\link{hclust}}, \code{\link{diana}} and \code{\link{kmeans}}).
  		}
}
\value{
Returns a data.frame with row.names matching that of the data.
	\item{cm}{
		cluster membership identifier specifying the cluster into which the row has been classified
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
\code{\link{cluster}}, \code{\link{agnes}}, \code{\link{pam}}, \code{\link{hclust}}, \code{\link{diana}}, \code{\link{kmeans}} and \code{\link{apclusterK}}
}
\examples{
#load some data
data(sim_profile);

#run a basic agnes clustering with 3 clusters
cm <- agnes_clmem(sim_profile,3);

#pass some more complex parameters
agnes_params = list(metric='manhattan',method='single');
cm <- agnes_clmem(sim_profile, 3,params=agnes_params);

}