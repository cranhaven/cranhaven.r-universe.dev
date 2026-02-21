\name{BISEP}
\alias{BISEP}
\title{BISEP: Bimodality in Gene Expression data.
}
\description{
Detects bimodality and non-normality in all genes across the dataset.  
}
\usage{
BISEP(
	data = data
	)
}
\arguments{
  \item{data}{This should be a log2 gene expression matrix with genes as rownames and samples as column names.  Suitable for gene expression data from any platform - NGS datasets should be RPKM or RSEM values.
}
}
\details{
The lower confidence calls will dramatically affect the number of gene pairs that the tool produces and increase the false positive rate.  The tool will take approximately 10 minutes to run a 5,000 row and 200 column input matrix using a 'medium,' confidence interval.
}
\value{
A list containing three matrices.  Matrix 1 contains the output of the BISEP algorithm - including the midpoint of the bimodal distribution and the associated p value. Matrix 2 contains the output from the BI algorithm - including the delta, pi and BI values.  Matrix 3 contains the input matrix.
}
\author{
Mark Wappett
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
data(INPUT_data)
outputBISEP <- BISEP(data=INPUT_data)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ SyntheticLethal }
\keyword{ Bimodality }
