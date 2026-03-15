%$Author: sinnwell $ 
%$Date: 2009/09/30 20:05:38 $ 
%$Header: /projects/genetics/cvs/cvsroot/haplo.stats/man/get.hapPair.Rd,v 1.3 2009/09/30 20:05:38 sinnwell Exp $ 
%$Locker:  $ 
%$Log: get.hapPair.Rd,v $
%Revision 1.3  2009/09/30 20:05:38  sinnwell
%rm keywords, or leave one with one from /doc/KEYWORDS
%
%Revision 1.2  2009/09/30 19:21:50  sinnwell
%remove empty sections
%
%Revision 1.1  2008/03/10 18:59:33  sinnwell
%Initial revision
%
%

\name{get.hapPair}
\alias{get.hapPair}
\title{
 Get a list of objects for haplotype pairs
}
\description{
 Get a list of objects for modeling haplotype pairs from a set of unique
 haplotypes and their frequencies, given the baseline haplotype
}
\usage{
get.hapPair(haplo, haplo.freq, base.index)
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
}
\value{
  list with components:
\item{p.g}{
Genotype probability under Hardy-Weinberg Equilibrium, where the
genotype is the haplotype pair
}
\item{x.haplo}{
Design matrix for all pairs of haplotypes, excluding the baseline
haplotype.  Effects are coded to an additive effect for the haplotypes.
}
\item{haplo.indx}{
two-column matrix containing the indices for the haplotypes in x.haplo.
The indices are the row of the haplotype in haplo.
}
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

hPair <- get.hapPair(haplo, haplo.freq, base.index=1)
names(hPair)
dim(hPair$x.haplo)
}
% docclass is function
