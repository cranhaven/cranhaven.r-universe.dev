\name{data.RAS}
\alias{data.RAS}
\docType{data}
\title{
The data of RAS interaction on chronic kidney disease.
}
\description{
This used the data from a meta-analysis included about 100 studies investigating the associations between ACE insertion/deletion (I/D) and chronic kidney disease, and re-collected the studies included AGT M235T information. Finally, there are 34 studies included in this data.
}
\usage{data("data.RAS")}
\format{
  A data frame with 34 observations on the following 12 variables.
  \describe{
    \item{\code{Author}}{the first author in included studies.}
    \item{\code{Year}}{the published year in included studies.}
    \item{\code{Race}}{the race of each population.}
    \item{\code{Tyep}}{the subtype of chronic kidney disease in each study.}
    \item{\code{case.ACE.0}}{the number of I allele (rs4340) in cases.}
    \item{\code{case.ACE.1}}{the number of D allele (rs4340) in cases (risk allele).}
    \item{\code{ctrl.ACE.0}}{the number of I allele (rs4340) in controls.}
    \item{\code{ctrl.ACE.1}}{the number of D allele (rs4340) in controls (risk allele).}
    \item{\code{case.AGT.0}}{the number of M allele (rs699) in cases.}
    \item{\code{case.AGT.1}}{the number of T allele (rs699) in cases (risk allele).}
    \item{\code{ctrl.AGT.0}}{the number of M allele (rs699) in controls.}
    \item{\code{ctrl.AGT.1}}{the number of T allele (rs699) in controls (risk allele).}
  }
}

\references{
Lin C, Yang HY, Wu CC, Lee HS, Lin YF, Lu KC, et al. (2014) Angiotensin-Converting Enzyme Insertion/Deletion Polymorphism Contributes High Risk for Chronic Kidney Disease in Asian Male with Hypertension: A Meta-Regression Analysis of 98 Observational Studies. PLoS ONE 9(1): e87604. doi:10.1371/journal.pone.0087604
%%  ~~ possibly secondary sources and usages ~~
}
\seealso{
\code{\link{ETMA}}
}
\examples{

data(data.RAS)
head(data.RAS)

}
\keyword{datasets}
