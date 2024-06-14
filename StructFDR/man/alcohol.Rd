\name{alcohol}
\alias{alcohol}
\docType{data}
\title{
%%   ~~ data name/kind ... ~~
Alcohol data set
}
\description{
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
The data set was taken from a study of the dietary association with the gut microbiome (Wu, 2011). Here we include the alcohol intake as the primary
phenotype. The OTU abundance data consists of 98 samples with 949 OTUs with prevalence > 10\%. The raw counts are normalized to account for variable
library sizes. 
}
\usage{data(alcohol)}
\format{
  The format is:
 chr "alcohol"
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
A list containing the normalized OTU data (\code{X}, counts divided by GMPR size factors), the taxonomic lineages of the OTUs (\code{otu.name}),
the alcohol intake phenotype (High and Low) (\code{Y}), the tree (\code{tree}).
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
Wu, Gary D., et al. "Linking long-term dietary patterns with gut microbial enterotypes." Science 334.6052 (2011): 105-108.}
\examples{
data(alcohol)
}
\keyword{datasets}
