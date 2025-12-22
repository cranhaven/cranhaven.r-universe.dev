\name{SimCrossCausal}
\alias{SimCrossCausal}
\alias{SimCrossIndep}
\alias{CMSTCross}
\title{Simulate Cross for Causal Tests}
\description{Creates cross with certain pattern of dependence across phenotypes.
}
\usage{
SimCrossCausal(n.ind, len, n.mar, beta, add.eff, dom.eff, 
  sig2.1 = 1, sig2.2 = 1, eq.spacing = FALSE, 
  cross.type = c("bc", "f2"), normalize = FALSE)
SimCrossIndep(n.ind, len, n.mar, beta, add.eff.1, dom.eff.1,
  add.eff.h, dom.eff.h, sig2.1 = 1, sig2.2 = 1, sig2.h = 1, 
  eq.spacing = FALSE, cross.type = "f2", normalize = FALSE)
data(CMSTCross)
}
\arguments{
  \item{n.ind}{number of individuals to simulate}
  \item{len}{vector specifying the chromosome lengths (in cM)}
  \item{n.mar}{vector specifying the number of markers per chromosome}
  \item{beta}{causal effect (slope) of first phenotype on others}
  \item{add.eff, add.eff.1, add.eff.h}{additive genetic effect}
  \item{dom.eff, dom.eff.1, dom.eff.h}{dominance genetic effect}
  \item{sig2.1}{residual variance for first phenotype}
  \item{sig2.2, sig2.h}{residual variance for all other phenotypes}
  \item{eq.spacing}{if \code{TRUE}, markers will be equally spaced}
  \item{cross.type}{type of cross (\code{bc} and \code{f2} for now)}
  \item{normalize}{normalize values if \code{TRUE}}
}
\references{
Chaibub Neto E, Broman AT, Keller MP, Attie AD, Zhang B, Zhu J, Yandell BS,
Causal model selection hypothesis tests in systems genetics.
Genetics (in review).
}
\examples{
set.seed(987654321)
CMSTCross <- SimCrossCausal(n.ind = 100, 
  len = rep(100, 3), n.mar = 101,
  beta = rep(0.5, 2), add.eff = 1, dom.eff = 0, 
  sig2.1 = 0.4, sig2.2 = 0.1, eq.spacing = FALSE, 
  cross.type = "bc", normalize = TRUE)
CMSTCross <- calc.genoprob(CMSTCross, step = 1)
\dontrun{
save(CMSTCross, file = "CMSTCross.RData", compress = TRUE)
}
}
\keyword{utilities}
