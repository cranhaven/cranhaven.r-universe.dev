\name{waterfallPlot}
\alias{waterfallPlot}
\title{waterfallPlot: Create visualisations from BEEM output}
\description{
Takes the output from the function BISEP and a discreet mutation matrix as input.  The mutation matrix samples (columns) must mirror or overlap with the gene expression matrix.  The data in the mutation matrix must be a discreet 'WT' or 'MUT' call based on the status of each gene with each sample. Gene names must be available in the input matrices.
}
\usage{
waterfallPlot(
	bisepData=data, 
	mutData=mutData, 
	expressionGene, 
	mutationGene
	)
}
\arguments{
  \item{bisepData}{This should be the output from the BISEP function.
}
  \item{mutData}{This should be a matrix with genes rownames and samples as column names.  All cells should be made up of a discreet 'WT' or 'MUT' call.  There should be overlap (by sample) with the gene expression matrix.
}
  \item{expressionGene}{The gene whose expression you would like to plot.
}
  \item{mutationGene}{The gene whose mutation status you would like to overlap with the expression gene.
}
}
\details{
The function will return an error if any of the input information is incorrect or missing.  The resulting plot will be returned in real time.
}
\value{
A waterfall plot.  The plot is made up of two panels: the left panel is a density distribution of the expression gene provided and the right panel is a bar-chart of the gene expression level coloured by mutation status.
}

\author{
Mark Wappett
}

\examples{
data(BISEP_data)
data(MUT_data)
waterfallOut <- waterfallPlot(BISEP_data, MUT_data, expressionGene="micb", mutationGene="PBRM1")
}

\keyword{ Mutation }
\keyword{ SyntheticLethal }
