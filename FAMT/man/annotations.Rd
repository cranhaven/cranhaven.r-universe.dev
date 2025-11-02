\name{annotations}
\alias{annotations}
\docType{data}
\title{Gene annotations data frame
}
\description{A data frame with 6 columns describing the 9893 genes, which expressions are stored in the 'expression' dataset, in terms of functional categories, oligonucleotide size and location on the microarray. See also \code{\link{expression}}, \code{\link{covariates}}.
}
\usage{data(annotations)}
\format{
  A data frame with 9893 observations on the following 6 variables.
  \describe{
		\item{\code{ID}}{Gene identification}
    \item{\code{Name}}{Gene annotation (functional categories) (character)}
    \item{\code{Block}}{Location on the microarray(factor)}
    \item{\code{Column}}{Location on the microarray (factor)}
    \item{\code{Row}}{Location on the microarray (factor)}
    \item{\code{Length}}{Oligonucleotide size (numeric vector)}
}
}
%\details{}

\source{UMR Genetique Animale - INRA/AGROCAMPUS OUEST - Rennes, France.}
\references{Blum Y., Le Mignon G., Lagarrigue S. and Causeur S. (2010) - A factor model to analyze heterogeneity in gene expression, BMC Bioinformatics, 11:368.


Le Mignon, G. and Desert, C. and Pitel, F. and Leroux, S. and Demeure, O. and Guernec, G. and Abasht, B. and Douaire, M. and Le Roy, P. and Lagarrigue S. (2009) - Using trancriptome profling to characterize QTL regions on chicken chromosome 5. BMC Genomics, 10:575.}
\examples{
data(annotations)
dim(annotations) 
summary(annotations)
}
\keyword{datasets}
