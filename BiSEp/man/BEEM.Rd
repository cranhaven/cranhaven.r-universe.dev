\name{BEEM}
\alias{BEEM}
\title{BEEM: Bimodal Expression Exclusive with Mutation}
\description{
Takes the output from the function BISEP and a discreet mutation matrix as input.  The mutation matrix samples (columns) must mirror or overlap with the gene expression matrix.  The data in the mutation matrix must be a discreet 'WT' or 'MUT' call based on the status of each gene with each sample. Detects mutations of genes enriched in either the high or low gene expression modes.
}
\usage{
BEEM(
	bisepData=data, 
	mutData=mutData, 
	sampleType=c("cell_line", "cell_line_low", "patient", "patient_low"), 
	minMut=10
	)
}
\arguments{
  \item{bisepData}{This should be the output from the BISEP function.
}
  \item{mutData}{This should be a matrix with genes rownames and samples as column names.  All cells should be made up of a discreet 'WT' or 'MUT' call.  There should be overlap (by sample) with the gene expression matrix.
}
  \item{sampleType}{The type of sample being analysed.  Select 'cell_line' or 'patient' for datasets with greater than ~200 samples.  For datasets with less than ~200 samples, use 'cell_line_low' or 'patient_low'.
}
  \item{minMut}{The minimum number of mutations you for a gene would consider for analysis.
}
}
\details{
Lower sample numbers have more stringent bimodality hurdles to clear in order to keep the false positive rate lower.  The tool returns a percentage complete text window so the user can observe the status of the job. 
}
\value{
A matrix containing 10 columns. Column 1 contains the bimodal genes from the expression data (gene 1) and column 2 contains the mutated candidate synthetic lethal gene pair (gene 2).  Columns 3 and 4 contain the number of mutations of gene 2 in the low and high expression modes of gene 1.  Column 5 contains the fishers p value that evaluates enrichment of mutation in either the high or low mode (indicated by column 10).  Columns 6 and 7 contain the percentage of samples in the low and high expression modes of gene 1 that are mutated for gene 2.  Columns 8 and 9 contain information on the overall size (in terms of sample) of the low and high expression modes of gene 1. 
}

\author{
Mark Wappett
}

\keyword{ Mutation }
\keyword{ SyntheticLethal }
