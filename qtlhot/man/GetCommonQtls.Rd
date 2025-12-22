\name{GetCommonQtls}
\alias{GetCommonQtls}
\title{Get common QTLs for phenotypes}
\description{Perform joint QTL mapping for phenotypes
with marginal LOD peak positions higher than LOD threshold and within
set distance of each other
}
\usage{
GetCommonQtls(cross, pheno1, pheno2, thr = 3, peak.dist = 5,
  addcov1 = NULL, addcov2 = NULL, intcov1 = NULL, intcov2 = NULL)
}
\arguments{
  \item{cross}{object of class \code{cross}}
  \item{pheno1}{first phenotype column number or character string name}
  \item{pheno2}{second phenotype column number or character string name;
    if more than one, then all phenotypes will be tested against \code{pheno1}}
  \item{thr}{LOD threshold}
  \item{peak.dist}{maximal peak distance to be considered the same peak
    (in cM)}
  \item{addcov1, addcov2}{additive covariates for first and second
    phenotype, respectively}
  \item{intcov1, intcov2}{interactive covariates for first and second
    phenotype, respectively}
}
\references{
Chaibub Neto E, Broman AT, Keller MP, Attie AD, Zhang B, Zhu J, Yandell BS,
Causal model selection hypothesis tests in systems genetics.
Genetics (in review).
}
\seealso{\code{\link{CMSTCross}}}
\examples{
data(CMSTCross)
commqtls <- GetCommonQtls(CMSTCross, 
                          pheno1 = "y1", 
                          pheno2 = "y3",
                          thr = 3,
                          peak.dist = 5,
                          addcov1 = NULL, 
                          addcov2 = NULL, 
                          intcov1 = NULL, 
                          intcov2 = NULL)
commqtls
}
\keyword{utilities}
