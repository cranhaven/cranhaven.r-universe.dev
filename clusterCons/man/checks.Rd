\name{checks}
\alias{checks}
\alias{data_check}
\alias{validConsMatrixObject}
\alias{validMemRobListObject}
\alias{validMemRobMatrixObject}
\alias{validMergeMatrixObject}
\alias{validAUCObject}
\alias{validDkObject}

\title{
Functions to check the integrity of various objects
}
\description{
These methods are mainly internal although the user may like to check their original data using 
\code{data_check} before they perform consensus clustering experiments.
}
\usage{
data_check(x)
validConsMatrixObject(object)
validMemRobListObject(object)
validMemRobMatrixObject(object)
validMergeMatrixObject(object)
validAUCObject(object)
validDkObject(object)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
	The data.frame object to be checked prior to using with the \code{\link{cluscomp}} function.
	}
  \item{object}{
  	The object to be checked with the suitable function by type. These are used internally by several of the functions in the package.
  }
}

\value{
returns \code{TRUE} if check is passed or an error message if it is not
}

\references{
Merged consensus clustering to assess and improve class discovery with microarray data. Simpson TI, Armstrong JD and Jarman AP. BMC Bioinformatics 2010, 11:590.
}

\author{
Dr. T. Ian Simpson
\email{ian.simpson@ed.ac.uk}
}

\examples{
#load data
data(sim_profile);

#check if this can be used by cluscomp
data_check(sim_profile);

#perform a clusomp run
cmr <- cluscomp(sim_profile,clmin=2,clmax=2,rep=10);

#check one of the consensus matrices
validConsMatrixObject(cmr$e1_kmeans_k2)

}