% $Author: sinnwell $
% $Date: 2011/12/12 17:02:15 $ 
% $Header: /projects/genetics/cvs/cvsroot/haplo.stats/man/haplo.em.Rd,v 1.5 2011/12/12 17:02:15 sinnwell Exp $ 
% $Locker:  $ 
% 
%$Log: haplo.em.Rd,v $
%Revision 1.5  2011/12/12 17:02:15  sinnwell
%notes about LC-COLLATE, and update see-also
%
%Revision 1.4  2011/11/10 15:29:41  sinnwell
%major update to hapglm, minor changes to Rd files, prepare for version 1.5.0 release
%
%Revision 1.3  2009/09/30 19:58:16  sinnwell
%remove keywords section
%
%Revision 1.2  2009/09/30 19:17:51  sinnwell
%remove empty sections
%
%Revision 1.1  2008/01/08 20:21:53  sinnwell
%Initial revision
%
%Revision 1.15  2004/03/16 22:35:51  sinnwell
%added empty space in example
%
%Revision 1.14  2004/03/16 15:56:04  sinnwell
%alter returns for ps file
%
%Revision 1.13  2004/03/01 21:14:27  sinnwell
%add example
%
%Revision 1.12  2004/02/26 22:53:55  sinnwell
%remove alias-ed topics
%
%Revision 1.11  2004/02/23 20:26:07  sinnwell
%add summary and print as topics, works as /alias for Rd
%
%Revision 1.10  2004/02/06 16:46:48  sinnwell
%add parameter weights
%
%Revision 1.9  2003/10/15 14:28:48  schaid
%*** empty log message ***
%
%Revision 1.8  2003/10/15 14:24:17  schaid
%*** empty log message ***
%
%Revision 1.7  2003/09/19 21:40:48  schaid
%*** empty log message ***
%
%Revision 1.6  2003/08/26 20:58:00  schaid
%Major update of haplo.em, with new version using progressive insertion, and this helpfile reflecting the many changes in terms of parameters and returned values

\name{haplo.em}
\alias{haplo.em}
\title{
  EM Computation of Haplotype Probabilities, with Progressive Insertion
of Loci
}
\description{
  For genetic marker phenotypes measured on unrelated subjects, with 
  linkage phase unknown, compute maximum likelihood estimates of
  haplotype probabilities.  Because linkage phase is unknown, there 
  may be  more than one pair of haplotypes that are consistent with 
  the oberved marker phenotypes, so posterior probabilities of pairs 
  of haplotypes for each subject are also computed. Unlike the usual EM
  which attempts to enumerate all possible pairs of haplotypes before
  iterating over the EM steps,  this "progressive insertion" algorithm 
  progressively inserts batches of loci into haplotypes of growing
  lengths, runs the EM steps, trims off
  pairs of haplotypes per subject when the posterior probability of the
  pair is below a specified threshold, and then continues these
  insertion, EM, and trimming steps until all loci are inserted into the
  haplotype. The user can choose the batch size. If the batch size
  is chosen to be all loci, and the threshold for trimming is set to
  0, then this algorithm reduces to the usual EM algorithm.
}
\usage{
haplo.em(geno, locus.label=NA, miss.val=c(0, NA), weight, control= 
           haplo.em.control())
}
\arguments{
\item{geno}{
  matrix of alleles, such that each locus has a pair of adjacent 
  columns of alleles, and the order of columns corresponds to 
  the order of loci on a chromosome.  If there are K loci, then 
  ncol(geno) = 2*K. Rows represent the alleles for each subject.
}
\item{locus.label }{
  vector of labels for loci.
}
\item{miss.val }{
  vector of values that represent missing alleles in geno.
}
\item{weight}{
  weights for observations (rows of geno matrix).  
}
\item{control}{
  list of control parameters. The default is constructed by the function
  haplo.em.control. The default behavior of this function results in the
  following parameter settings:  loci.insert.order=1:n.loci, 
  insert.batch.size=min(4,n.loci), min.posterior= 0.0001, tol=0.00001,
  max.iter=500, random.start=0 (no random start), iseed=NULL (no saved
  seed to start random start), verbose=0 (no printout during EM
  iterations). See haplo.em.control for more details.
}
}
\value{
  list with components:

\item{converge}{
indicator of convergence of the EM algorithm
  (1 = converge, 0 = failed).
}
\item{lnlike}{
  value of lnlike at last EM iteration (maximum lnlike if converged).
}
\item{lnlike.noLD}{
  value of lnlike under the null of linkage equilibrium at all loci.  
}
\item{lr}{
likelihood ratio statistic to test the final lnlike against the
  lnlike that assumes complete linkage equilibrium among all loci
  (i.e., haplotype frequencies are products of allele frequencies).
}
\item{df.lr}{
degrees of freedom for likelihood ratio statistic. The df for the
  unconstrained final model is the  number of non-zero haplotype frequencies
  minus 1, and the df for the null model of complete linkage 
  equilibrium is the sum, over all loci, of (number of alleles - 1). The
  df for the lr statistic is df[unconstrained] - df[null]. This can
  result in negative df, if many haplotypes are estimated to have zero 
  frequency, or if a large amount of trimming occurs, when using large
  values of min.posterior in the list of control parameters.
}
\item{hap.prob}{
vector of mle's of haplotype probabilities.  The ith
  element of hap.prob corresponds to the ith row of 
  haplotype.
}
\item{locus.label}{
vector of labels for loci, of length K 
(see definition of input values).
}
\item{subj.id}{
vector of id's for subjects used in the analysis, based on row number
  of input geno matrix. If subjects are removed, then their id will be
  missing from subj.id.
}
\item{rows.rem}{
now defunct, but set equal to a vector of length 0, to be compatible 
  with other functions that check for rows.rem.
}
\item{indx.subj}{
vector for row index of subjects after expanding to all 
 possible pairs of haplotypes for each person.  If indx.subj=i, 
 then i is the ith row of  geno.  If the ith 
 subject has n possible pairs of haplotypes that correspond 
 to their marker genotype, then i is repeated n times.
}
\item{nreps}{
vector for the count of haplotype pairs that map to
  each subject's marker genotypes.
}
\item{max.pairs}{
vector of maximum number of pairs of haplotypes per subject that are
  consistent with their marker data in the matrix geno. The
  length of max.pairs  = nrow(geno). This vector is computed by  
  geno.count.pairs.
}
\item{hap1code}{
vector of codes for each subject's first haplotype.
  The values in hap1code are the row numbers of the unique
  haplotypes in the returned matrix haplotype.
}
\item{hap2code}{
similar to hap1code, but for each subject's second haplotype.
}
\item{post}{
vector of posterior probabilities of pairs of haplotypes for 
  a person, given their marker phenotypes.
}
\item{haplotype}{
matrix of unique haplotypes. Each row represents a unique 
  haplotype, and the number of columns is the number of loci.
}
\item{control}{
list of control parameters for algorithm. See haplo.em.control
}
}
\details{
  The basis of this progressive insertion algorithm is from the sofware
  snphap by David Clayton. Although some of the features and control
  parameters of this S-PLUS version are modeled after snphap, there are
  substantial differences, such as extension to allow for more than two
  alleles per locus, and some other nuances on how the alogrithm is implemented.
}
\note{
  Sorted order of haplotypes with character alleles is system-dependent,
  and can be controlled via the LC_COLLATE locale environment
  variable. Different locale settings can cause results to be
  non-reproducible even when controlling the random seed. 
}
\seealso{
  \code{\link{setupGeno}},
  \code{\link{haplo.em.control}}
}
\examples{
data(hla.demo)
attach(hla.demo)
geno <- hla.demo[,c(17,18,21:24)]
label <-c("DQB","DRB","B")
keep <- !apply(is.na(geno) | geno==0, 1, any)

save.em.keep <- haplo.em(geno=geno[keep,], locus.label=label)

# warning: output will not exactly match

print.haplo.em(save.em.keep)
}
% docclass is function
% Converted by Sd2Rd version 37351.
