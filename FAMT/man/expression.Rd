\name{expression}
\alias{expression}
\docType{data}
\title{Gene expressions data frame
}
\description{This dataset concerns hepatic transciptome profiles of 43 half sib male chickens selected for their variability on abdominal fatness (AF). Genes are in rows (9893 genes) and arrays in columns (43 arrays).}

\usage{data(expression)}
\format{
  A data frame with 9893 genes on 43 arrays. 
  %\describe{}
}
%\details{


\source{UMR Genetique Animale - INRA/AGROCAMPUS OUEST - Rennes, France.}
\references{Blum Y., Le Mignon G., Lagarrigue S. and Causeur S. (2010) - A factor model to analyze heterogeneity in gene expression, BMC Bioinformatics, 11:368.

Le Mignon, G. and Desert, C. and Pitel, F. and Leroux, S. and Demeure, O. and Guernec, G. and Abasht, B. and Douaire, M. and Le Roy, P. and Lagarrigue S. (2009) - Using trancriptome profling to characterize QTL regions on chicken chromosome 5. BMC Genomics, 10:575.
}
\examples{
data(expression)
dim(expression)
summary(expression)
}
\keyword{datasets}
