%  $Author: sinnwell $ 
%  $Date: 2011/11/10 15:29:41 $ 
%  $Header: /projects/genetics/cvs/cvsroot/haplo.stats/man/hla.demo.Rd,v 1.6 2011/11/10 15:29:41 sinnwell Exp $
%  $Locker:  $ 
%  $Log: hla.demo.Rd,v $
%  Revision 1.6  2011/11/10 15:29:41  sinnwell
%  major update to hapglm, minor changes to Rd files, prepare for version 1.5.0 release
%
%  Revision 1.5  2009/04/08 19:11:56  sinnwell
%  changed for version 2.9.1
%

\name{hla.demo}
\alias{hla.demo}
\docType{data}
\title{ HLA Loci and Serologic Response to Measles Vaccination  }
\description{
 A data frame with genotypes at eleven HLA-region loci genotyped for
  220 subjects, phase not known. Contains measles vaccination response
  with covariate data.
}
\usage{data(hla.demo)}
\format{
 A data frame with 220 observations on the following 26 variables.
  \describe{
    \item{\code{resp}}{numeric, Quantitative response to Measles Vaccination}
    \item{\code{resp.cat}}{Category of vaccination response, a factor with levels \code{high} \code{low} \code{normal}}
    \item{\code{male}}{numeric, indicator of gener, 1=male, 0=female}
    \item{\code{age}}{numeric, subject's age}
    \item{\code{DPB.a1}}{first allele of genotype }
    \item{\code{DPB.a2}}{second allele of genotype }
    \item{\code{DPA.a1}}{first allele of genotype }
    \item{\code{DPA.a2}}{second allele of genotype}
    \item{\code{DMA.a1}}{first allele of genotype}
    \item{\code{DMA.a2}}{second allele of genotype}
    \item{\code{DMB.a1}}{first allele of genotype}
    \item{\code{DMB.a2}}{second allele of genotype}
    \item{\code{TAP1.a1}}{first allele of genotype }
    \item{\code{TAP1.a2}}{ second allele of genotype}
    \item{\code{TAP2.a1}}{first allele of genotype }
    \item{\code{TAP2.a2}}{second allele of genotype }
    \item{\code{DQB.a1}}{first allele of genotype}
    \item{\code{DQB.a2}}{second allele of genotype}
    \item{\code{DQA.a1}}{first allele of genotype}
    \item{\code{DQA.a2}}{second allele of genotype}
    \item{\code{DRB.a1}}{first allele of genotype}
    \item{\code{DRB.a2}}{second allele of genotype}
    \item{\code{B.a1}}{first allele of genotype}
    \item{\code{B.a2}}{second allele of genotype}
    \item{\code{A.a1}}{first allele of genotype}
    \item{\code{A.a2}}{second allele of genotype}
  }
}
\source{
  Data set kindly provided by Gregory A. Poland, M.D. and the Mayo
  Clinic Vaccine Research Group for illustration only, and my not be
  used for publication.
}
\references{
  Schaid DJ, Rowland CM, Tines DE, Jacobson RM,  Poland  GA.
  "Score tests for association of traits with haplotypes when
  linkage phase is ambiguous." Amer J Hum Genet. 70 (2002):  425-434.
}
\examples{
data(hla.demo)
}
\keyword{datasets}
