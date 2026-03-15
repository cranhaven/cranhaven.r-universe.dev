\name{haplo.power.qt}
\alias{haplo.power.qt}
\alias{haplo.power.qt.ncp}
\title{
  Compute either power or sample size for haplotype associations with a
quantitative trait.
}
\description{
For a given set of haplotypes, their population frequencies, and assumed
linear regression coefficients (additive model of haplotype effects on
a quantitative trait), determine either
the sample size  to achieve a stated
power or the power for a stated sample size.
}
\usage{
haplo.power.qt(haplo, haplo.freq, base.index, haplo.beta, y.mu, y.var,
alpha, sample.size = NULL, power = NULL)
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
  amount of expected change per haplotype from the base-line average,
  and the beta for the base-line (indexed by base.index) is the 
  beta for the intercept.
}
\item{y.mu }{
  population mean of quantitative trait, y.
}
\item{y.var }{
   popultaion variance of quantitative trait, y.
}
\item{alpha}{
  type-I error rate
}
\item{sample.size }{
 sample size (if power is to be calcualted). Either sample.size or power must be specified, but not both.
}
\item{power}{
 desired power (if sample.size is to be calculated). Either sample.size or power must be specified, but not both.
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
of a non-central F distribution. This non-centrality
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
(haplo.power.qt) fail. If this occurs, then consider reducing the
"size" of the haplotypes, by reducing the number of columns of haplo,
and adjusting the corresponding vectors (e.g., haplo.freq, haplo.beta).
}
\references{
Schaid, DJ. Power and sample size for testing associations of
haplotypes with complex traits. Ann Hum Genet (2005) 70:116-130.
}
\seealso{
  \code{\link{find.haplo.beta.qt}},
  \code{\link{haplo.em}},
  \code{\link{haplo.power.cc}}
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

haplo.freq <- c(0.170020121, 0.162977867, 0.123742455, 0.117706237,
0.097585513, 0.084507042, 0.045271630, 0.039235412, 0.032193159,
0.019114688, 0.019114688, 0.013078471, 0.013078471, 0.013078471,
0.013078471, 0.006036217, 0.006036217, 0.006036217,
0.006036217, 0.006036217, 0.006036217)

# define index for risk haplotypes (having alleles 1-1 at loci 2 and 3)
haplo.risk <- (1:nrow(haplo))[haplo$loc.2==1 & haplo$loc.3==1]

# define index for baseline haplotype
base.index <-  1

# Because it can be easier to speficy genetic effect size in terms of
# a regression model R-squared value (r2), we use an
# auxiliary function to set up haplo.beta based on a specifed r2 value:

tmp <- find.haplo.beta.qt(haplo,haplo.freq,base.index,haplo.risk,
r2=0.01, y.mu=0, y.var=1)

haplo.beta <- tmp$beta  

# Compute sample size for given power

haplo.power.qt(haplo, haplo.freq, base.index, haplo.beta, y.mu=0,
y.var=1, alpha=.05, power=.80) 

# Compute power for given sample size

haplo.power.qt(haplo, haplo.freq, base.index, haplo.beta, y.mu=0,
y.var=1, alpha=.05, sample.size = 2091)  
}
\keyword{design}
% docclass is function
% Converted by Sd2Rd version 43268.
