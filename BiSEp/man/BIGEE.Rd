\name{BIGEE}
\alias{BIGEE}
\title{BIGEE: Bimodal Gene Expression Exclusivity.
}
\description{
Part of the Synthetic Lethality detection in Genomics toolkit.  Detects bimodality and non-normality in all genes across the dataset.  Compares all pairwise combinations of bimodal genes and searches for mutually exclusive low expression as evidence of potential synthetic lethality.  Scores gene-pairs based on the presence of mutual exclusive bimodality and the distribution of signal intensity across the rest of the dataset.
}
\usage{
BIGEE(
	bisepData=data, 
	sampleType=c("cell_line", "cell_line_low", "patient", "patient_low")
	)
}
\arguments{
  \item{bisepData}{This should be the output from the BISEP function.
}
  \item{sampleType}{The type of sample being analysed.  Select 'cell_line' or 'patient' for datasets with greater than ~200 samples.  For datasets with less than ~200 samples, use 'cell_line_low' or 'patient_low'.
}
}
\details{
Lower sample numbers have more stringent bimodality hurdles to clear in order to keep the false positive rate lower.  The tool returns a percentage complete text window so the user can observe the status of the job. 
}
\value{
A matrix containing three columns. Columns 1 and 2 are the gene symbols that make up the candidate synthetic lethal gene pairs.  Column 3 is the score calculated the tool to rank the statistical significance of the gene pairs.
}
\author{
Mark Wappett
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ SyntheticLethal }
\keyword{ Bimodality }
