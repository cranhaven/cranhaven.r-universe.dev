\name{lung}
\alias{lung}
\docType{data}
\title{
Lung cancer gene expression data
}
\description{
This data set consists of gene expression values of a subset of 5000 genes with the highest variance of the 12 625 genes measured using the Affymetrix 95av2 GeneChip on a set of 56 samples - 20 pulmonary carcinoid samples (Carcinoid), 6 small cell lung carcinoma samples (SmallCell), 13 colon cancer metastasis samples (Colon), and 16 normal lung samples (Normal). This example is used in Lee, Shen, Huang, and Marron (2010).
}
\usage{data(lung)}
\format{
  The format is an n x p matrix:
  (1) the rows are the samples - rownames containing the class labels.
  (2) the columns are the genes - colnames containing the affymetrix gene ids. 
}

\source{
This data set was published in the following paper:

Bhattacharjee, A., Richards, W., Staunton, J., Li, C., Monti, S., Vasa, P., Ladd, C., Beheshti, J., Bueno, R., Gillette, M., and others. (2001)  Classification of human lung carcinomas by mRNA expression profiling reveals distinct adenocarcinoma subclasses.  Proceedings of the National Academy of Sciences of the United States of America, 98(24), 13790--13795.  

It is publicly available at http://www.pnas.org/content/98/24/13790/suppl/DC1

}
\references{
Bhattacharjee et al. (2001) Classification of human lung carcinomas by mRNA expression profiling reveals distinct adenocarcinoma subclasses.  Proceedings of the National Academy of Sciences of the United States of America, 98(24), 13790--13795  
	
Used as an example in Lee, Shen, Huang, and Marron (2010), 'Biclustering via sparse singular value decomposition', Biometrics 66(4), 1087--1095.

Used as an example in KM Tan and D Witten (2014) Sparse biclustering of transposable data.  \emph{Journal of Computational and Graphical Statistics} 23(4):985-1008.
}
\examples{
data(lung)
}

\keyword{datasets}
