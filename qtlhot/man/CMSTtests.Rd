\name{CMSTtests}
\alias{CMSTtests}
\alias{CMSTtestsList}
\title{Perform CMST Tests on cross object}
\description{Performs 6 separate CMST tests (3 versions, 2 penalties).
}
\usage{
CMSTtests(cross, pheno1, pheno2, Q.chr, Q.pos,
  addcov1 = NULL, addcov2 = NULL, intcov1 = NULL, intcov2 = NULL, 
  method = c("par", "non.par", "joint", "all"),
  penalty = c("bic", "aic", "both"), verbose = FALSE)
CMSTtestsList(cross, pheno1, pheno2, Q.chr, Q.pos,
  addcov1 = NULL, addcov2 = NULL, intcov1 = NULL, intcov2 = NULL, 
  method = c("par", "non.par", "joint", "all"),
  penalty = c("bic", "aic", "both"), verbose = TRUE)
}
\arguments{
  \item{cross}{object of class \code{cross}}
  \item{pheno1}{first phenotype column number or character string name}
  \item{pheno2}{second phenotype column number or character string name;
    if more than one, then all phenotypes will be tested against \code{pheno1}}
  \item{Q.chr}{QTL chromosome (number or label)}
  \item{Q.pos}{QTL position in cM}
  \item{addcov1, addcov2}{additive covariates for first and second
    phenotype, respectively}
  \item{intcov1, intcov2}{interactive covariates for first and second
    phenotype, respectively}
  \item{method}{test method; see details}
  \item{penalty}{type of penalty; see details}
  \item{verbose}{verbose printout if \code{TRUE}}
}
\details{
Explain method and penalty here.
}
\references{
Chaibub Neto E, Broman AT, Keller MP, Attie AD, Zhang B, Zhu J, Yandell BS,
Causal model selection hypothesis tests in systems genetics.
Genetics (in review).
}
\seealso{\code{\link{CMSTCross}}, \code{\link{PrecTpFpMatrix}},
\code{\link{FitAllTests}}}
\examples{
data(CMSTCross)
nms <- names(CMSTCross$pheno)
out1 <- CMSTtests(CMSTCross, 
                  pheno1 = nms[1], 
                  pheno2 = nms[2],
                  Q.chr = 1,
                  Q.pos = 55,
                  addcov1 = NULL, 
                  addcov2 = NULL, 
                  intcov1 = NULL, 
                  intcov2 = NULL, 
                  method = "all",
                  penalty = "both")
out1[1:6]
out1[7]
out1[8:12]
out1[13:17]
## list of phenotypes
out2 <- CMSTtests(CMSTCross, 
                  pheno1 = nms[1], 
                  pheno2 = nms[-1],
                  Q.chr = 1,
                  Q.pos = 55,
                  addcov1 = NULL, 
                  addcov2 = NULL, 
                  intcov1 = NULL, 
                  intcov2 = NULL, 
                  method = "par",
                  penalty = "bic")
out2
}
\keyword{utilities}
