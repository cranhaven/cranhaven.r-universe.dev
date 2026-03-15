% $Author: sinnwell $ 
% $Date: 2011/11/23 21:00:26 $ 
% $Header: /projects/genetics/cvs/cvsroot/haplo.stats/man/geno.count.pairs.Rd,v 1.3 2011/11/23 21:00:26 sinnwell Exp $ 
% $Locker:  $
% $Log: geno.count.pairs.Rd,v $
% Revision 1.3  2011/11/23 21:00:26  sinnwell
% rm geno.recode from example
%
% Revision 1.2  2011/11/10 15:29:41  sinnwell
% major update to hapglm, minor changes to Rd files, prepare for version 1.5.0 release
%
% Revision 1.1  2008/01/09 20:56:47  sinnwell
% Initial revision
%
%Revision 1.8  2004/03/17 16:53:36  sinnwell
%raided to raised
%
%Revision 1.7  2004/03/15 15:53:57  sinnwell
%summary.geno to summaryGeno
%
%Revision 1.6  2004/03/12 17:49:44  sinnwell
%change 2^(h-1) to 2 raised (h-1) for Rd errors
%
%Revision 1.5  2004/03/01 21:09:47  sinnwell
%make example run-able
%
%Revision 1.4  2003/04/21 20:15:26  sinnwell
%correct usage section
%
%Revision 1.3  2003/04/15 12:59:30  schaid
%updated description of details
%
%Revision 1.2  2003/04/14 13:41:14  sinnwell
%description change
%
%Revision 1.1  2003/04/09 18:43:50  sinnwell
%Initial revision

\name{geno.count.pairs}
\alias{geno.count.pairs}
\title{
  Counts of Total Haplotype Pairs Produced by Genotypes 
}
\description{
Provide a count of all possible haplotype pairs for each subject,
according to the phenotypes in the rows of the geno matrix.  
The count for each row includes the count for complete phenotypes, as 
well as possible haplotype pairs for phenotypes where there are
missing alleles at any of the loci.
}
\usage{
geno.count.pairs(geno)
}
\arguments{
\item{geno}{
Matrix of alleles, such that each locus has a  pair  of adjacent 
columns of alleles,  and the order of columns corresponds to the 
order of loci  on a chromosome.   If there are K loci, then geno 
has  2*K  columns.  Rows represent all observed alleles for each 
subject, their phenotype.
}

}
\value{
Vector where each element gives a count of the number haplotype pairs 
that are consistent with a  subject's phenotype, where a phenotype may
include 0, 1, or 2 missing alleles at any locus.
}
\details{
When a subject has no missing alleles, and has h heterozygous sites, 
there are 2**(h-1) haplotype pairs that are possible ('**'=power).
For loci with missing alleles, we consider all possible pairs of alleles 
at those loci.  Suppose that there are M loci with missing alleles, and 
let the vector V have values 1 or 0 acccording to whether these loci 
are imputed to be heterozygous or homozygous, respectively. The length
of V is M.  The total number of  possible states of V is 
2**M. Suppose that  the vector W, also of length M, provides a count
of the number of possible heterozygous/homozygous states at the loci
with missing data. For example, if one allele  is missing, and there
are K possible alleles at that locus, then there can be one homozygous
and (K-1) heterozygous genotypes. If two alleles are missing, there
can be K homozygous and K(K-1)/2 heterozygous genotypes. Suppose the
function H(h+V) counts the total number of heterozygous sites among
the loci without missing data (of which h are heterozygous) and the
imputed loci (represented by the  vector V). Then, the total number of
possible pairs of haplotypes can be respresented as SUM(W*H(h+V)),
where the sum is over all possible values for the vector V.
}
\seealso{
\code{\link{haplo.em}},
\code{\link{summaryGeno}}
}
\examples{
data(hla.demo)
genohla <- hla.demo[,c(17,18,21:24)]
geno <- setupGeno(genohla)
count.geno <- geno.count.pairs(geno)
print(count.geno)
}

% docclass is function
% Converted by Sd2Rd version 37351.
