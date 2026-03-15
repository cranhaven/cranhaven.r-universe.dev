\name{haplo.power.cc}
\alias{haplo.power.cc}
\alias{haplo.power.cc.ncp}
\alias{find.intercept.logistic}
\title{
  Compute either power or sample size for haplotype associations in a
case-control study.
}
\description{
For a given set of haplotypes, their population frequencies, and assumed
logistic regression coefficients (log-odds-ratios per haplotype,
assuming a log-additive model of haplotype effects), determine either
the sample size (total number of subjects) to achieve a stated
power or the power for a stated sample size.
}
\usage{
haplo.power.cc(haplo, haplo.freq, base.index, haplo.beta, case.frac,
prevalence, alpha, sample.size=NULL, power=NULL)
}
\arguments{
\item{haplo}{
  matrix of haplotypes, with rows the different haplotypes and
  columns the alleles of the haplotypes. For H haplotypes of L loci,
  haplo has dimension H x L.
}
\item{haplo.freq }{
  vector of length H for the population haplotype frequencies
  (corresponding to the rows of haplo)
}
\item{base.index }{
  integer index of the haplotype considered to be the base-line for
  logistic regression (index between 1 and H); often, the most common
  haplotype is chosen for the base-line.
}
\item{haplo.beta }{
  vector of length H for the haplotype effects: each beta is the 
  log-odds-ratio for the corresponding haplotype effect. 
  The base-line hapoltype should have a beta=0, as this base-line
  beta coefficient will be automatically calculated according to the 
  haplotype frequencies, the other haplo.beta's, and the disease prevalence.
}
\item{case.frac }{
  fraction of cases in the total sample size (e.g., case.frac = .5
  for typical case-control studies with equal numbers of cases and controls)
}
\item{prevalence}{
   popultaion disease prevalence (used to calculate the base-line
  intercept beta)
}
\item{alpha}{
  type-I error rate
}
\item{sample.size }{
 total sample size (if power is to be calcualted). Either sample.size or
 power must be specified, but not both.
}
\item{power}{
 desired power (if sample.size is to be calculated). Either sample.size
 or power must be specified, but not both.
}
}
\value{
  list with components:

\item{ss.phased.haplo}{
sample size for phased haplotypes
}
\item{ss.unphased.haplo}{
sample size for unphased haplotypes
}
\item{power.phased.haplo}{
power for phased haplotypes
}
\item{power.unphased.haplo}{
power for unphased haplotypes
}
}
\details{
Asympotic power calcuations are based on the non-centrality parameter
of a non-central chi-square distribution. This non-centrality
parameter is determined by the specified regression coefficients
( values in haplo.beta), as well as the distribution of haplotypes
(determined by haplo.freq). To account for haplotypes with unknown
phase, all possible haplotype pairs are enumerated, and the EM
algorithm is used to determine the posterior probabilities of pairs of
haplotypes, conditional on unphased genotype data. Because this
function uses the function haplo.em, the  number of possible
haplotypes can be large when there is a large number of loci (i.e.,
large number of columns in the haplo matrix). If too large, the
function haplo.em will run out of memory, making this function
(haplo.power.cc) fail. If this occurs, then consider reducing the
"size" of the haplotypes, by reducing the number of columns of haplo,
and adjusting the corresponding vectors (e.g., haplo.freq, haplo.beta).
}
\references{
Schaid, DJ. Power and sample size for testing associations of
haplotypes with complex traits. Ann Hum Genet (2005) 70:116-130.
}
\seealso{
\code{\link{haplo.em}}
\code{\link{haplo.power.qt}}
}
\examples{
haplo <- rbind(
               c(     1,    2,    2,    1,    2),
               c(     1,    2,    2,    1,    1),
               c(     1,    1,    2,    1,    1),
               c(     1,    2,    1,    1,    2),
               c(     1,    2,    2,    2,    1),
               c(     1,    2,    1,    1,    1),
               c(     1,    1,    2,    2,    1),
               c(     1,    1,    1,    1,    2),
               c(     1,    2,    1,    2,    1),
               c(     1,    1,    1,    2,    1),
               c(     2,    2,    1,    1,    2),
               c(     1,    1,    2,    1,    2),
               c(     1,    1,    2,    2,    2),
               c(     1,    2,    2,    2,    2),
               c(     2,    2,    2,    1,    2),
               c(     1,    1,    1,    1,    1),
               c(     2,    1,    1,    1,    1),
               c(     2,    1,    2,    1,    1),
               c(     2,    2,    1,    1,    1),
               c(     2,    2,    1,    2,    1),
               c(     2,    2,    2,    1,    1))
dimnames(haplo)[[2]] <- paste("loc", 1:ncol(haplo), sep=".")
haplo <- data.frame(haplo)

haplo.freq <- c(0.170020121, 0.162977867, 0.123742455, 0.117706237, 0.097585513, 0.084507042,
                0.045271630, 0.039235412, 0.032193159, 0.019114688, 0.019114688, 0.013078471,
                0.013078471, 0.013078471, 0.013078471, 0.006036217, 0.006036217, 0.006036217,
                0.006036217, 0.006036217, 0.006036217)

# define index for risk haplotypes (having alleles 1-1 at loci 2 and 3)
haplo.risk <- (1:nrow(haplo))[haplo$loc.2==1 & haplo$loc.3==1]

# define index for baseline haplotype
base.index <-  1

# specify OR for risk haplotypes
or <- 1.25

# determine beta regression coefficients for risk haplotypes

haplo.beta <- numeric(length(haplo.freq))
haplo.beta[haplo.risk] <-  log(or)

# Note that non-risk haplotypes have beta=0, as does the intercept
# (haplotype with base.index value). 

# Compute total sample size for given power

haplo.power.cc(haplo, haplo.freq, base.index, haplo.beta, case.frac=.5,
prevalence=.1, alpha=.05, power=.8)

# Compute power for given sample size

haplo.power.cc(haplo, haplo.freq, base.index, haplo.beta, case.frac=.5,
prevalence=.1, alpha=.05, sample.size=11978)
}
\keyword{design}
% docclass is function
% Converted by Sd2Rd version 43268.
