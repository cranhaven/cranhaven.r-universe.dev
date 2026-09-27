% $Author: sinnwell $ 
% $Date: 2011/11/10 15:29:41 $ 
% $Header: /projects/genetics/cvs/cvsroot/haplo.stats/man/haplo.group.Rd,v 1.2 2011/11/10 15:29:41 sinnwell Exp $ 
% $Locker:  $
% $Log: haplo.group.Rd,v $
% Revision 1.2  2011/11/10 15:29:41  sinnwell
% major update to hapglm, minor changes to Rd files, prepare for version 1.5.0 release
%
% Revision 1.1  2008/01/09 17:35:35  sinnwell
% Initial revision
%
%Revision 1.12  2004/03/22 19:52:53  sinnwell
%speed up example, use keep
%
%Revision 1.11  2004/03/01 21:31:51  sinnwell
%fix example
%
%Revision 1.10  2004/02/06 20:58:36  sinnwell
%fix syntax for n.loci
%
%Revision 1.9  2003/10/27 19:24:51  sinnwell
%fixed syntax error
%
%Revision 1.8  2003/10/15 16:16:12  schaid
%*** empty log message ***
%
%Revision 1.7  2003/08/27 21:08:53  sinnwell
%update for PIN, mostly for em.control parameter list
%
%Revision 1.6  2003/03/24 19:13:47  sinnwell
%fix group.count description
%
%Revision 1.5  2003/03/24 16:38:38  sinnwell
%add haplo.em to <s-see>
%
%Revision 1.4  2003/03/11 21:45:43  sinnwell
%update for haplo.group returned as list
%
%Revision 1.3  2003/01/17 16:56:39  sinnwell
%revision for haplo.score version 1.2
%
%Revision 1.2  2002/09/16 14:46:42  sinnwell
%Fix RCS keywords

\name{haplo.group}
\alias{haplo.group}
\title{
  Frequencies for Haplotypes by Grouping Variable
}
\description{
Calculate maximum likelihood estimates of haplotype probabilities for 
the entire dataset and separately for each subset defined by the
levels of a group variable.  Only autosomal loci are considered.
}
\usage{
haplo.group(group, geno, locus.label=NA, 
            miss.val=0, weight=NULL, 
            control=haplo.em.control())
}
\arguments{
\item{group}{
Group can be of logical, numeric, character, or factor class type.
}
\item{geno}{
Matrix of alleles, such that each locus has a pair of
adjacent columns of alleles, and the order of columns
corresponds to the order of loci on a chromosome.  If
there are K loci, then geno has 2*K columns. Rows represent all
observed alleles for each subject.  
}
\item{locus.label }{
Vector of labels for loci, of length K (see definition of geno matrix).
}
\item{miss.val }{
Vector of codes for allele missing values.
}
\item{weight}{
weights for observations (rows of geno matrix). One reason to use is
to adjust for disproportionate sample of sub-groups.  Weights only
used in the frequency calculation for the pooled subject.
}
\item{control}{
list of control parameters for haplo.em (see haplo.em.control).
}
}
\value{
\item{list}{
A list as an object of the haplo.group class.  The three
elements of the list are described below.
}
\item{group.df}{
A data frame with the columns described as follows.
-haplotype:
Names for the K columns for the K alleles in the haplotypes.
-total:
Estimated frequencies for haplotypes from the total sample.
-group.name.i:
Estimated haplotype frequencies for the haplotype
if it occurs in the group referenced by 'i'.  Frequency is NA 
if it doesn't occur for the group.  The column name is the actual 
variable name joined with the ith level of that variable.
}
\item{group.count}{
Vector containing the number of subjects for each level of the 
grouping variable.
}
\item{n.loci}{
Number of loci occuring in the geno matrix.
}
}
\details{
Haplo.em is used to compute the maximum likelihood estimates of 
the haplotype frequencies for the total sample, then for each of 
the groups separately.  
}
\section{References}{
Schaid DJ, Rowland CM, Tines DE, Jacobson RM,  Poland  GA.
"Score tests for association of traits with haplotypes when
linkage phase is ambiguous." Amer J Hum Genet. 70 (2002):  425-434.
}
\seealso{
print.haplo.group, haplo.em
}
\examples{
  data(hla.demo)
  geno <- as.matrix(hla.demo[,c(17,18,21:24)])

# remove any subjects with missing alleles for faster examples, 
# but you may keep them in practice
  keep <- !apply(is.na(geno) | geno==0, 1, any)
  hla.demo <- hla.demo[keep,]
  geno <- geno[keep,]
  attach(hla.demo)
  
  y.ord <- as.numeric(resp.cat)
  y.bin <-ifelse(y.ord==1,1,0)
  group.bin <- haplo.group(y.bin, geno, miss.val=0)
  print.haplo.group(group.bin)
}
% docclass is function
% Converted by Sd2Rd version 37351.
