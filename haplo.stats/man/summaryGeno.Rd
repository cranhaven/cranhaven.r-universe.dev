% $Author: sinnwell $ 
% $Date: 2011/11/10 15:29:41 $ 
% $Header: /projects/genetics/cvs/cvsroot/haplo.stats/man/summaryGeno.Rd,v 1.3 2011/11/10 15:29:41 sinnwell Exp $ 
% $Locker:  $ 
% 
% $Log: summaryGeno.Rd,v $
% Revision 1.3  2011/11/10 15:29:41  sinnwell
% major update to hapglm, minor changes to Rd files, prepare for version 1.5.0 release
%
% Revision 1.2  2008/01/08 20:29:33  sinnwell
% fix comments
%
% Revision 1.1  2008/01/08 18:25:51  sinnwell
% Initial revision
%
% Revision 1.2  2004/03/02 15:17:29  sinnwell
% take out example, simple usage
%
% Revision 1.1  2004/02/26 22:06:58  sinnwell
% Initial revision

\name{summaryGeno}
\alias{summaryGeno}
\title{
  Summarize Full Haplotype Enumeration on Genotype Matrix 
}
\description{
Provide a summary of missing allele information for each individual 
in the genotype matrix.  The number of loci missing zero, one, or two 
alleles is computed, as well as the total number of haplotype pairs 
that could result from the observed phenotype.
}
\usage{
summaryGeno(geno, miss.val=0)
}
\arguments{
\item{geno}{
Matrix of alleles, such that each locus has a pair of adjacent columns
of alleles, and the order of columns corresponds to the order of loci 
on a chromosome. If there are K loci, then geno has 2*K columns. Rows 
represent all observed alleles for each subject.
}
\item{miss.val }{
Vector of codes for allele missing values.
}
}
\value{
Data frame with columns representing the number of loci with zero,
one, and two missing alleles, then the total haplotype pairs resulting
from full enumeration of the phenotype.
}
\details{
After getting information on the individual loci, this function makes 
a call to geno.count.pairs().  The E-M steps to estimate haplotype 
frequencies considers haplotypes that could result from a phenotype
with a missing allele.  It will not remove a subject's phenotype, only
the unlikely haplotypes that result from it.  
}
\seealso{
\code{\link{geno.count.pairs}},
\code{\link{haplo.em}}
}
% docclass is function
% Converted by Sd2Rd version 37351.
