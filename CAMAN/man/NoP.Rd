\name{NoP}
\docType{data}
\alias{NoP}
\title{Ames test data: Mutagenicity of 4NoP}
\description{
Ames test data where a strain of SalmonellaTA98 was activated with a homogenate of rat liver cells and exposed to 4-nitro-ophenylenediamine (4NoP) with various doses. 4NoP is a nitrated aromatic amine.  This chemical is a component of both semipermanent and permanent hair dye formulations. The substance is frequently used as a comparator in mutagenicity tests. The are  taken from Margolin et al. (1989) and denote the bacteria count for various doses of4NoP. The primary question of interest is whether 4NoP acts mutagenically in the Ames test.
}
\usage{data("NoP")}
\format{A data frame with 100 rows and 3 columns (dose, count, logd)}
\examples{
data(NoP)
ames3 <- mixcov(dep="count",fixed=c("dose", "logd"),random="",data=NoP,
                k=3,family="poisson")
}
\references{
Margolin, B. H., B. S. Kim, and K. J. Risko. (1989). \dQuote{The Ames Salmonella Microsome Mutagenicity Assay: Issues of Inference and Validation.} \emph{Journal of the American Statistical Association} \bold{84}, 651--661.

Schlattmann, P.(2009). \emph{Medical Applications of Finite Mixture Models.} Berlin: Springer.
} 
\keyword{datasets}
