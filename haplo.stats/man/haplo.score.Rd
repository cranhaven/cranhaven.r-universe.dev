%$Author: sinnwell $ 
%$Date: 2011/11/10 15:29:41 $ 
%$Header: /projects/genetics/cvs/cvsroot/haplo.stats/man/haplo.score.Rd,v 1.6 2011/11/10 15:29:41 sinnwell Exp $ 
%$Locker:  $ 
%$Log: haplo.score.Rd,v $
%Revision 1.6  2011/11/10 15:29:41  sinnwell
%major update to hapglm, minor changes to Rd files, prepare for version 1.5.0 release
%
%Revision 1.5  2008/04/14 21:38:51  sinnwell
%fix syntax error
%
%Revision 1.4  2008/04/14 20:00:33  sinnwell
%change eps.svd comment
%
%Revision 1.3  2008/04/08 20:26:42  sinnwell
%add svd.eps, undo control parameter change
%
%Revision 1.2  2008/04/08 14:18:21  sinnwell
%move haplo.effect, sim.control, em.control to haplo.score.control with eps.svd
%
%Revision 1.1  2008/01/09 19:34:34  sinnwell
%Initial revision
%
%Revision 1.20  2007/01/23 22:09:39  sinnwell
%added parameters min.count and haplo.effect
%Revision 1.19  2005/01/28 21:40:27  sinnwell
%warn to not discard missings under standard usage
%Revision 1.18  2004/12/29 18:51:13  sinnwell
%update for skip.haplo default change
%Revision 1.17  2004/03/22 20:00:05  sinnwell
%limit simulations in example
%Revision 1.16  2004/03/03 15:52:59  sinnwell
%add labels in examples
%Revision 1.15  2004/03/02 17:15:59  sinnwell
%change sex to male of hla.demo
%Revision 1.14  2004/03/01 21:47:26  sinnwell
%fix examples
%Revision 1.13  2004/02/16 17:28:07  sinnwell
%change F to FALSE in call
%Revision 1.12  2003/08/27 21:04:55  schaid
%*** empty log message ***
%Revision 1.11  2003/08/22 20:10:36  sinnwell
%update for haplo.score version 1.2.0 with PIN and simulations updated
%Revision 1.10  2003/08/13 21:52:00  sinnwell
%update source cited
%Revision 1.9  2003/01/17 16:57:06  sinnwell
%revision for haplo.score version 1.2
%Revision 1.8  2002/09/13 21:04:14  sinnwell
%More commenting revisions
%Revision 1.7  2002/09/13 19:44:16  sinnwell
%Fix sgml commenting scheme
%Revision 1.6  2002/09/13 17:00:55  sinnwell
%Fixed RCS keywords to meet sgml rules
%Revision 1.5  2002/09/13 14:26:10  sinnwell
%Fix RCS keywords and comments.  
%Revision 1.4  2002/09/12 21:20:24  det01
%Added RCS keywords Author, Date, Header, Id, Locker, Log

\name{haplo.score}
\alias{haplo.score}
\title{
  Score Statistics for Association of Traits with Haplotypes
}
\description{
Compute score statistics to evaluate the association of a trait
with haplotypes, when linkage phase is unknown and diploid marker 
phenotypes are observed among unrelated subjects. For now, only 
autosomal loci are considered.  
}
\usage{
haplo.score(y, geno, trait.type="gaussian", offset = NA, x.adj = NA,
            min.count=5, skip.haplo=min.count/(2*nrow(geno)),
            locus.label=NA, miss.val=c(0,NA), haplo.effect="additive",
            eps.svd=1e-5, simulate=FALSE, sim.control=score.sim.control(),
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
Minimum haplotype frequency for which haplotypes are scored in the model.
By default, the frequency is based on "min.count" divided by the 2*N total haplotype 
occurrences in the sample.
}
\item{locus.label }{
Vector of labels for loci, of length K (see definition of geno matrix)
}
\item{miss.val }{
Vector of codes for missing values of alleles
}
\item{haplo.effect }{
the "effect" of a haplotypes, which determines the covariate (x)
coding of haplotypes. Valid options are "additive" (causing x = 0,
1, or 2, the count of a  particular haplotype), "dominant" (causing
x = 1 if heterozygous or homozygous carrier of a particular
haplotype; x = 0 otherwise), and "recessive" (causing x = 1 if
homozygous for a particular haplotype; x = 0 otherwise).
}
\item{eps.svd}{
 epsilon value for singular value cutoff; to be used in the generalized
 inverse calculation on the variance matrix of the score vector (see
 help(Ginv) for details).  
}
\item{simulate}{
Logical:  if FALSE, no empirical p-values are computed; if TRUE,
simulations are performed. Specific simulation parameters
can be controlled in the sim.control parameter list.  
}
\item{sim.control }{
A list of control parameters to determine how simulations are
performed for simulated p-values.  The list is created by the function
score.sim.control and the default values of this function can be
changed as desired.  See score.sim.control for details.
}
\item{em.control }{
A list of control parameters to determine how to perform the EM
algorithm for estimating haplotype frequencies when phase is unknown.
The list is created by the function haplo.em.control - see this function
for more details.
}
}
\value{
List with the following components:

\item{score.global}{
Global statistic to test association of trait with 
haplotypes that have frequencies >= skip.haplo.
}
\item{df}{
Degrees of freedom for score.global.
}
\item{score.global.p}{
P-value of score.global based on chi-square distribution, 
with degrees of freedom equal to df.
}
\item{score.global.p.sim}{
P-value of score.global based on simulations
(set equal to NA when simulate=F).
}
\item{score.haplo}{
Vector of score statistics for individual
haplotypes that have frequencies >= skip.haplo.
}
\item{score.haplo.p}{
Vector of p-values for score.haplo, based on a
chi-square distribution with 1 df.
}
\item{score.haplo.p.sim}{
Vector of p-values for score.haplo, based on
simulations (set equal to NA when simulate=F).
}
\item{score.max.p.sim}{
Simulated p-value indicating for simulations the number of times a 
maximum score.haplo value exceeds the maximum score.haplo from the
original data (equal to NA when simulate=F).
}
\item{haplotype}{
Matrix of hapoltypes  analyzed.  The ith row of
haplotype corresponds to the ith item of score.haplo,
score.haplo.p, and score.haplo.p.sim.
}
\item{hap.prob}{
Vector of haplotype probabilies, corresponding to the
haplotypes in the matrix haplotype.
}
\item{locus.label}{
Vector of labels for loci, of length  K  (same  as
input argument).
}
\item{call}{
The call to the haplo.score function; useful for recalling what
parameters were used.
}
\item{haplo.effect}{
The haplotype effect model parameter that was selected for haplo.score.
}
\item{simulate}{
Same as function input parameter.  If [T]rue, simulation results are
included in the haplo.score object.
}
\item{n.val.global}{
Vector containing the number of valid simulations used in the
global score statistic simulation. The number of valid simulations can
be less than the number of simulations requested (by sim.control)
if simulated data sets produce unstable variances of the score
statistics.  
}
\item{n.val.haplo}{
Vector containing the number of valid simulations used in the p-value
simulations for maximum-score statistic and scores for the individual
haplotypes.
}
}
\details{
Compute the maximum likelihood estimates of the haplotype frequencies
and the posterior probabilities of the pairs of haplotypes for each
subject using an EM algorithm. The algorithm begins with haplotypes
from a subset of the loci and progressively discards those with low
frequency before inserting more loci.  The process is repeated until
haplotypes for all loci are established.  The posterior probabilities
are used to compute the score statistics for the association of
(ambiguous) haplotypes with traits. The glm function is used to
compute residuals of the regression of the trait on the non-genetic
covariates.  
}
\section{References}{
Schaid DJ, Rowland CM, Tines DE, Jacobson RM,  Poland  GA.
"Score tests for association of traits with haplotypes when
linkage phase is ambiguous." Amer J Hum Genet. 70 (2002):  425-434.
}
\seealso{
\code{\link{haplo.em}},
\code{\link{plot.haplo.score}},
\code{\link{print.haplo.score}},
\code{\link{haplo.em.control}},
\code{\link{score.sim.control}}
}
\examples{
  # establish all hla.demo data, 
  # remove genotypes with missing alleles just so haplo.score runs faster 
  # with missing values included, this example takes 2-4 minutes
  # FOR REGULAR USAGE, DO NOT DISCARD GENOTYPES WITH MISSING VALUES

  data(hla.demo)
  geno <- as.matrix(hla.demo[,c(17,18,21:24)])
  keep <- !apply(is.na(geno) | geno==0, 1, any)
  hla.demo <- hla.demo[keep,]
  geno <- geno[keep,]
  attach(hla.demo)
  label <- c("DQB","DRB","B")
 
# For quantitative, normally distributed trait:

  score.gaus <- haplo.score(resp, geno, locus.label=label, 
                            trait.type = "gaussian")
  print(score.gaus)

# For ordinal trait:
  y.ord <- as.numeric(resp.cat)
  score.ord <- haplo.score(y.ord, geno, locus.label=label,
                           trait.type="ordinal")
  print(score.ord)

# For a  binary trait and simulations,
# limit simulations to 500 in score.sim.control, default is 20000
  y.bin <-ifelse(y.ord==1,1,0)
  score.bin.sim <- haplo.score(y.bin, geno, trait.type = "binomial",
                     locus.label=label, simulate=TRUE,
                     sim.control=score.sim.control(min.sim=200,max.sim=500))

  print(score.bin.sim)

# For a binary trait, adjusted for sex and age:

  x <- cbind(male, age)
  score.bin.adj <- haplo.score(y.bin, geno, trait.type = "binomial", 
                               locus.label=label, x.adj=x)
  print(score.bin.adj)
}
\keyword{scores}
% docclass is function
% Converted by Sd2Rd version 37351.
