% $Author: sinnwell $ 
% $Date: 2009/09/30 19:58:16 $ 
% $Header: /projects/genetics/cvs/cvsroot/haplo.stats/man/haplo.scan.Rd,v 1.4 2009/09/30 19:58:16 sinnwell Exp $ 
% $Locker:  $ 
% $Log: haplo.scan.Rd,v $
% Revision 1.4  2009/09/30 19:58:16  sinnwell
% remove keywords section
%
% Revision 1.3  2009/09/30 19:01:46  sinnwell
% remove section{} and put in note{}
%
% Revision 1.2  2008/01/09 21:18:10  sinnwell
% fixed alias for haplo.scan.obs
%
% Revision 1.1  2008/01/09 19:30:31  sinnwell
% Initial revision
%
%Revision 1.6  2005/03/31 16:37:16  sinnwell
%code-doc agree on param order
%
%Revision 1.5  2005/03/28 22:23:03  sinnwell
%*** empty log message ***
%
%Revision 1.4  2005/03/24 16:45:10  schaid
%*** empty log message ***
%
%Revision 1.3  2005/03/24 16:08:13  schaid
%*** empty log message ***
%
%Revision 1.2  2005/03/24 14:41:32  sinnwell
%add the max.loc vector
%
%Revision 1.1  2005/03/23 18:07:44  sinnwell
%Initial revision

\name{haplo.scan}
\alias{haplo.scan}
\alias{haplo.scan.obs}
\alias{haplo.scan.sim}
\title{
Search for a trait-locus by sliding a fixed-width window over each marker locus
and scanning all possible haplotype lengths within the window
}
\description{
Search for haplotypes that have the strongest association with a
binary trait (typically case/control status) by sliding a fixed-width
window over each marker locus and scanning all possible haplotype
lengths within the window. For each haplotype length, a score
statistic is computed to compare the set of haplotypes with a given
length between cases versus controls. The locus-specific score statistic 
is the maximum score statistic calculated on loci containing
that locus.  The maximum score statistic over all haplotype lengths
within all possible windows is used for a global test for association.
Permutations of the trait are used to compute p-values.
}
\usage{
haplo.scan(y, geno, width=4, miss.val=c(0, NA),
          em.control=haplo.em.control(),
          sim.control=score.sim.control())

haplo.scan.obs(y, em.obj, width)

haplo.scan.sim(y.reord, save.lst, nloci)

}
\arguments{
\item{y}{
Vector of binary trait values, must be 1 for cases and 0 for controls.
}
\item{y.reord}{
Same as y, except the order is permuted
}
\item{geno}{
Matrix of alleles, such that each locus has a pair of adjacent
columns of alleles, and the order of columns corresponds to the
order of loci on a chromosome. If there are K loci, then 
ncol(geno) = 2*K. Rows represent alleles for each subject.
}
\item{width}{
Width of sliding the window
}
\item{miss.val }{
Vector of codes for missing values of alleles
}
\item{em.control }{
A list of control parameters to determine how to perform the EM
algorithm for estimating haplotype frequencies when phase is  
unknown. The list is created by the function haplo.em.control -
see this function for more details.
}
\item{sim.control }{
A list of control parameters to determine how simulations are
performed for simulated p-values. The list is created by the
function score.sim.control and the default values of this
function can be changed as desired. See score.sim.control for
details.
}
\item{em.obj}{
Object returned from haplo.em, performed on geno
}

\item{save.lst}{
  Information on haplotypes needed for haplo.scan.sim, already
  calculated in haplo.scan
}
\item{nloci}{
number of markers
}

}
\value{
A list that has class haplo.scan, which contains the following items:

\item{call}{
The call to haplo.scan
}
\item{scan.df}{
A data frame containing the maximum test statistic for each window
around each locus, and its simulated p-value.
}
\item{max.loc}{
The loci (locus) which contain(s) the maximum observed test statistic
over all haplotype lengths and all windows.
}
\item{globalp}{
A p-value for the significance of the global maximum statistic.
}
\item{nsim}{
Number of simulations performed
}
}
\details{
Search for a region for which the haplotypes have the strongest
association with a binary trait by sliding a window of fixed width
over each marker locus, and considering all haplotype lengths within
each window. To acount for unknown linkage phase, the function haplo.em is
called prior to scanning, to create a list of haplotype pairs and 
posterior probabilities. To illustrate the scanning, consider a 10-locus
dataset. When placing a window of width 3 over locus 5, the possible 
haplotype lengths that contain locus 5 are three (loci 3-4-5,
4-5-6, and 5-6-7), two (loci 4-5, and 5-6) and one (locus 5).  
For each of these loci subsets a score statistic is computed, which is
based on the difference between the mean vector of haplotype counts
for cases and that for controls. The maximum of these score
statistics, over all possible haplotype lengths within a window, is 
the locus-specific test statistic. The global test statistic is the 
maximum over all computed score statistics. To compute p-values, 
the case/control status is randomly permuted.  Simulations are
performed until precision criteria are met for all p-values; the
criteria are controlled by score.sim.control. See the note for long run
times.
}
\references{
  \describe{
    \item{Cheng et al-1}{Cheng R, Ma JZ, Wright FA, Lin S, Gau X, Wang
      D, Elston RC, Li MD. "Nonparametric disequilibrium mapping of
      functional sites using haplotypes of multiple tightly linked
      single-nucleotide polymorphism markers". Genetics 164
      (2003):1175-1187.} 

    \item{Cheng et al-2}{Cheng R, Ma JZ, Elston RC, Li MD.  "Fine Mapping Functional Sites or
          Regions from Case-Control Data Using Haplotypes of Multiple
	  Linked SNPs."  Annals of Human Genetics 69 (2005): 102-112. 
	  }
  }
}
\note{
For datasets with many estimated haplotypes, the run-time can be very long.
}
\seealso{
\code{\link{haplo.em}},
\code{\link{haplo.em.control}},
\code{\link{score.sim.control}}
}
\examples{
  # create a random genotype matrix with 10 loci, 50 cases, 50 controls
  set.seed(1)
  tmp <- ifelse(runif(2000)>.3, 1, 2)
  geno <- matrix(tmp, ncol=20)
  y <- rep(c(0,1),c(50,50))

  # search 10-locus region, typically don't limit the number of
  # simulations, but run time can get long with many simulations

  scan.obj <- haplo.scan(y, geno, width=3,
                sim.control = score.sim.control(min.sim=10, max.sim=20))

  print(scan.obj)
}
% docclass is function
% Converted by Sd2Rd version 37351.
