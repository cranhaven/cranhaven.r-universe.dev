\name{plotProfilesWithinBicluster}
\alias{plotProfilesWithinBicluster}
\title{
Plot Gene Expression Profiles within a (Core) Bicluster
}
\description{
Plot Gene Expression Profiles within a (Core) Bicluster
}
\usage{
plotProfilesWithinBicluster(x, main = "", sampleNames, geneNames = NULL)
}

\arguments{
  \item{x}{
	expression matrix (of class 'matrix') for the subset of genes and samples corresponding to the bicluster under study
}
 \item{main}{
	main title for the graph
}
  \item{sampleNames}{
		names of the samples to be used for annotating the x axis (character vector
		of length equal to the number of columns of the expression matrix 'x' (representing the bicluster)
}
  \item{geneNames}{
	names of the genes to be plotted in a legend (character vector of length equal
   to the number of rows of the expression matrix 'x'); only suitable for biclusters containing a
   small number of genes 

}
}
\value{
 no return value; a plot is drawn to the current device
}
\author{
Tatsiana Khamiakova
}
\seealso{
\code{\link{plotSuper}} 
}