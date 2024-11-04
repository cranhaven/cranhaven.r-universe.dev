\name{expSetProcess}
\alias{expSetProcess}

\title{
Internal function to extract the data from an expressionSet class object from the affy package for use with cluscomp
}
\description{
This is a convenience function that is used internally to allow the user to pass an expressionSet object from the microarray processing package 'affy'
directly to the \code{\link{cluscomp}} function.
}
\usage{
expSetProcess(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
	An object of class expressionSet from the Bioconductor package 'affy'.
	}
}

\value{
when called directly, returns a suitably labeled data.frame() object of the expressionSet expression values.
} 

\references{
Merged consensus clustering to assess and improve class discovery with microarray data. Simpson TI, Armstrong JD and Jarman AP. BMC Bioinformatics 2010, 11:590.
}

\author{
Dr. T. Ian Simpson
\email{ian.simpson@ed.ac.uk}
}