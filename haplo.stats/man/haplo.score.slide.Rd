% $Author: sinnwell $ 
% $Date: 2011/11/10 15:29:41 $ 
% $Header: /projects/genetics/cvs/cvsroot/haplo.stats/man/haplo.score.slide.Rd,v 1.4 2011/11/10 15:29:41 sinnwell Exp $ 
% $Locker:  $ 
% $Log: haplo.score.slide.Rd,v $
% Revision 1.4  2011/11/10 15:29:41  sinnwell
% major update to hapglm, minor changes to Rd files, prepare for version 1.5.0 release
%
% Revision 1.3  2008/04/14 19:54:34  sinnwell
% change eps.svd note
%
% Revision 1.2  2008/04/10 14:36:49  sinnwell
% add eps.svd
%
% Revision 1.1  2008/01/09 19:38:58  sinnwell
% Initial revision
%
%Revision 1.11  2007/04/12 21:52:57  sinnwell
%add \s-arg
%Revision 1.10  2007/01/25 22:45:26  sinnwell
%add haplo.effect and min.count
%
%Revision 1.9  2005/03/03 20:50:55  sinnwell
%change default for skip.haplo
%
%Revision 1.8  2004/03/24 14:40:26  sinnwell
%comment out examples, except data setup, so users may uncomment and run
%
%Revision 1.7  2004/03/22 20:17:58  sinnwell
%shorten example
%
%Revision 1.6  2004/03/16 22:27:33  sinnwell
%put plot examples here to consolidate running the function
%
%Revision 1.5  2004/03/03 15:40:18  sinnwell
%add labels in example function calls
%
%Revision 1.4  2004/03/02 20:27:28  sinnwell
%binomial to ordinal in second example call
%Revision 1.3  2004/03/01 22:46:41  sinnwell
%fix examples
%
%Revision 1.2  2004/02/16 17:32:06  sinnwell
%change F to FALSE
%
%Revision 1.1  2003/08/22 20:04:58  sinnwell
%Initial revision

\name{haplo.score.slide}
\alias{haplo.score.slide}
\title{
  Score Statistics for Association of Traits with Haplotypes
}
\description{
Used to identify sub-haplotypes from a group of loci.  Run haplo.score
on all contiguous subsets of size n.slide from the loci in a genotype
matrix (geno).  From each call to haplo.score, report the global score
statistic p-value. Can also report global and maximum score statistics
simulated p-values.
}
\usage{
haplo.score.slide(y, geno, trait.type="gaussian", n.slide=2,
                  offset = NA, x.adj = NA, min.count=5,
                  skip.haplo=min.count/(2*nrow(geno)),
                  locus.label=NA, miss.val=c(0,NA),
                  haplo.effect="additive", eps.svd=1e-5,
                  simulate=FALSE, sim.control=score.sim.control(),
                  em.control=haplo.em.control())
}
\arguments{
\item{y}{
Vector of trait values. For trait.type = "binomial", y must 
have values of 1 for event, 0 for no event.
}
\item{geno}{
Matrix of alleles, such that each locus has a pair of
adjacent columns of alleles, and the order of columns
corresponds to the order of loci on a chromosome.  If
there are K loci, then ncol(geno) = 2*K. Rows represent
alleles for each subject.
}
\item{trait.type }{
Character string defining type of trait, with values of 
"gaussian", "binomial", "poisson", "ordinal".
}
\item{n.slide }{
Number of loci in each contiguous subset.  The first subset is the ordered
loci numbered 1 to n.slide, the second subset is 2 through n.slide+1
and so on.  If the total number of loci in geno is n.loci, then there
are n.loci - n.slide + 1 total subsets.
}
\item{offset}{
Vector of offset when trait.type = "poisson"
}
\item{x.adj }{
Matrix of non-genetic covariates used to adjust the score 
statistics.  Note that intercept should not be included, 
as it will be added in this function. 
}
\item{min.count }{
The minimum number of counts for a haplotype to be included in the
model.  First, the haplotypes selected to score are chosen by minimum
frequency greater than skip.haplo (based on min.count, by default).
It is also used when haplo.effect is either dominant or
recessive. This is explained best in the recessive instance, where
only subjects who are homozygous for a haplotype will contribute
information to the score for that haplotype.  If fewer than min.count
subjects are estimated to be affected by that haplotype, it is not
scored.  A warning is issued if no haplotypes can be scored.
}
\item{skip.haplo }{
For haplotypes with frequencies < skip.haplo, categorize them into a common
group of rare haplotypes.
}
\item{locus.label }{
Vector of labels for loci, of length K (see definition of geno matrix).
}
\item{miss.val }{
Vector of codes for missing values of alleles.
}
\item{haplo.effect }{
The "effect" pattern of haplotypes on the response. This parameter
determines the coding for scoring the haplotypes. 
Valid coding options for heterozygous and homozygous carriers of a
haplotype are "additive" (1, 2, respectively), "dominant" (1,1,
respectively), and "recessive" (0, 1, respectively).
}
\item{eps.svd}{
 epsilon value for singular value cutoff; to be used in the generalized
 inverse calculation on the variance matrix of the score vector. 
}
\item{simulate}{
Logical, if [F]alse (default) no empirical p-values are computed.
If [T]rue simulations are performed.  Specific simulation parameters
can be controlled in the sim.control parameter list.
}
\item{sim.control }{
A list of control parameters used to perform simulations for simulated
p-values in haplo.score.  The list is created by the function
score.sim.control and the default values of this function can be
changed as desired.
}
\item{em.control }{
A list of control parameters used to perform the em algorithm for
estimating haplotype frequencies when phase is unknown.  The list is
created by the function haplo.em.control and the default values of
this function can be changed as desired.
}
}
\value{
List with the following components:

\item{df}{
Data frame with start locus, global p-value, simulated global p-value,
and simulated maximum-score p-value.
}
\item{n.loci}{
Number of loci given in the genotype matrix.
}
\item{simulate}{
Same as parameter description above.
}
\item{haplo.effect}{
The haplotype effect model parameter that was selected for haplo.score.
}
\item{n.slide}{
Same as parameter description above.
}
\item{locus.label}{
Same as parameter description above.
}
\item{n.val.haplo}{
Vector containing the number of valid simulations used in the
maximum-score statistic p-value simulation.  The number of valid simulations
can be less than the number of simulations requested (by sim.control)
if simulated data sets produce unstable variables of the score
statistics. 
}
\item{n.val.global}{
Vector containing the number of valid simulations used in the
global score statistic p-value simulation.  
}
}
\details{
Haplo.score.slide is useful for a series of loci where little is
known of the association between a trait and haplotypes.  Using a
range of n.slide values, the region with the strongest association
will consistently have low p-values for locus subsets containing the
associated haplotypes.  The global p-value measures significance 
of the entire set of haplotypes for the locus subset.  Simulated
maximum score statistic p-values indicate when one or a few haplotypes
are associated with the trait.
}
\section{References}{
Schaid DJ, Rowland CM, Tines DE, Jacobson RM,  Poland  GA.
"Score tests for association of traits with haplotypes when
linkage phase is ambiguous." Amer J Hum Genet. 70 (2002):  425-434.
}
\seealso{
\code{\link{haplo.score}},
\code{\link{plot.haplo.score.slide}},
\code{\link{score.sim.control}}
}
\examples{
  data(hla.demo)

# Continuous trait slide by 2 loci on all 11 loci, uncomment to run it.
# Takes > 20 minutes to run
#  geno.11 <- hla.demo[,-c(1:4)]
#  label.11 <- c("DPB","DPA","DMA","DMB","TAP1","TAP2","DQB","DQA","DRB","B","A")
#  slide.gaus <- haplo.score.slide(hla.demo$resp, geno.11, trait.type = "gaussian",
#                                  locus.label=label.11, n.slide=2)

#  print(slide.gaus)
#  plot(slide.gaus)

# Run shortened example on 9 loci 
# For an ordinal trait, slide by 3 loci, and simulate p-values:
#  geno.9 <- hla.demo[,-c(1:6,15,16)]
#  label.9 <- c("DPA","DMA","DMB","TAP1","DQB","DQA","DRB","B","A")

#  y.ord <- as.numeric(hla.demo$resp.cat)

# data is set up, to run, run these lines of code on the data that was
# set up in this example. It takes > 15 minutes to run
#  slide.ord.sim <-  haplo.score.slide(y.ord, geno.9, trait.type = "ordinal",
#                      n.slide=3, locus.label=label.9, simulate=TRUE,
#                      sim.control=score.sim.control(min.sim=200, max.sim=500))

  # note, results will vary due to simulations
#  print(slide.ord.sim)
#  plot(slide.ord.sim)
#  plot(slide.ord.sim, pval="global.sim")
#  plot(slide.ord.sim, pval="max.sim")
}
\keyword{scores}
% docclass is function
% Converted by Sd2Rd version 37351.
