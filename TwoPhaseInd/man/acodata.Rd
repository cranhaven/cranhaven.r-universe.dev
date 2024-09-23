\name{acodata}
\alias{acodata}
\docType{data}
\title{A dataset from the STEP trial to study the interactions between gene 
and vaccine on HIV infection
%%   ~~ data name/kind ... ~~
}
\description{
A dataset from the STEP trial to study the interactions between gene and 
vaccine on HIV infection
}
\usage{data("acodata")}
\format{
  A data frame with 907 observations on the following 14 variables.
  \describe{
    \item{\code{vacc1_evinf}}{the time to HIV infection, a numeric vector}
    \item{\code{f_evinf}}{the indicator variable for HIV infection, a numeric vector}
    \item{\code{subcoh}}{the indicator of whether the participant was selected into 
          the sub-cohort for genotyping, a logical vector}
    \item{\code{ptid}}{patricipant identifier, a numeric vector}
    \item{\code{f_treat}}{vaccine assignment variable, a numeric vector}
    \item{\code{fcgr2a.3}}{the genotype of Fcr receptor FcrRIIIa, the biomarker of 
          interest here, a numeric vector}
    \item{\code{f_agele30}}{a numeric vector}
    \item{\code{f_hsv_2}}{a numeric vector}
    \item{\code{f_ad5gt18}}{a numeric vector}
    \item{\code{f_crcm}}{a numeric vector}
    \item{\code{any_drug}}{a numeric vector}
    \item{\code{num_male_part_cat}}{a numeric vector}
    \item{\code{uias}}{a numeric vector}
    \item{\code{uras}}{a numeric vector}
  }
}
\details{
A dataset from the STEP trial to study the interactions between gene and vaccine on 
HIV infection
}

\references{
S. P. Buchbinder, D. V. Mehrotra, and D. Ann et al. Efficacy assessment of a 
cell-mediated immunity HIV-1 vaccine (the Step Study): a double-blind, randomised, 
placebo-controlled, test-of-concept trial. Lancet. 372(9653):1881-1893, 2008.

J. P. Pandey, A. M. Namboodiri, and S. Bu et l. Immunoglobulin genes and the 
acquisition of HIV infection in a randomized trial of recombinant adenovirus HIV 
vaccine. Virology, 441:70-74, 2013.
}
\examples{
data(acodata)
## maybe str(acodata)
}
\keyword{datasets}
