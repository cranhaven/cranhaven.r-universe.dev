\name{NEWS}
\title{NEWS file for the haplo.stats package}
\section{Changes in version 1.9.7}{
  \itemize{
    \item clear unused C variables and routines
    \item make user manual as Rmd
    \item remove dependency on R.rsp
  }
}
\section{Changes in version 1.8.7}{
  \itemize{
    \item updated to testthat mechanisms to allow for tolerance
    differences
    \item take out linpack option in the svd call within Ginv().
    \item embed use manual into vignette.
    \item expanded haplo.cc to allow covariates as done in haplo.score
    with x.adj matrix
    \item removed PROBLEM S-macro in C code for R 4.1.x checks.
  }
}
\section{Changes in version 1.7.9}{
  \itemize{
    \item Updated parenthesis in if() statement of seqhap.c
    \item registered C code routines, per instructions from section 5.4.1
    of Writing R Extensions
  }
}
\section{Changes in version 1.7.7}{
  \itemize{
    \item Make test.haplo.em robust to system char/int recoding
    \item Add all.y=TRUE in merge in haplo.group so colSums of group
    haplotypes sum to 1. Creates NAs in pooled frequencies because some
    haplotypes only estimated within groups.
    \item The change above changes haplo.score.merge and haplo.cc
    results with more haplotypes
  }
}
\section{Changes in version 1.7.3}{
  \itemize{
    \item Remove special characters in comments in R code
    \item Replace S.h with R.h per note from Prof Ripley 1/27/2016
    \item Vignette manualHaploStats available as pre-made pdf, with R.rsp
    \item Ensure summary.haplo.em prints haplotypes in alpha-numeric
    order of the haplotypes and row names are 1:nrow
  }
}
\section{Changes in version 1.6.0}{
  \itemize{
    \item Removed .First.lib and .Last.lib b/c handled via namespace
    \item Updated test scripts to set LC_COLLATE and check values after set
  }
}
\section{Changes in version 1.5.6}{
  \itemize{
    \item Removed glm.fit.nowarn because glm.fit changed from R 2.15.0
    \item Added haplo.binomial for family=binomial in haplo.glm. Will
    work with either, and within haplo.glm it uses the initialize
    expression for haplo.binomial to ignore warnings of non-integer
    number of successes because we use posterior probability of a
    haplotype pair per subject as weights
  }
}
\section{Changes in version 1.5.4}{
  \itemize{
    \item Fixed haplo.glm bug in assigning names to var.mat when
    haplo.elim=NA
    \item replaced printf statements with REprintf (from R_ext/Print.h)
    in verbose C code sections
    \item Placed raw manual in rnw directory at top level, as suggested
    by Andreas Tille (2/11/2012)
  }
}
\section{Changes in version 1.5.2}{
  \itemize{
    \item Added methods for haplo.glm object: anova, residuals,
    vcov, fitted
    \item Updated haplo.glm to work more like glm object with
    methods
    \item Took out S-PLUS functionality for haplo.glm
    \item Remove notes for S-PLUS usage in documentation
    \item Added eps.svd argument to haplo.glm.control to 
    give users control over calculation information matrix rank
    \item Remove loci, geno.recode, allele.recode, which is now
    handled with setupGeno
    \item Add test suite with .R and .Rout.save files
    \item Major update to user manual in doc/ 
    \item Suggest Harrell's rms package instead of Design
    for haplo.score with ordinal traits
    \item Add NAMESPACE and NEWS files
  }
}
\section{Release Notes for version 1.4.4}{
\itemize{
  \item  No major updates to code. Updates are mostly for Rd files to keep up
  with changing requirements for packages.

  \item In functions that use 1-pchisq( ), replace with 
  pchisq(, , lower=FALSE) for better precision.  Precision is lost 
  subtracting a small p-value from 1.00, so computing it directly without 
  making the digits line up for the double-precision subraction 
}}
\section{Release Notes for version 1.4.1}{
\itemize{
  \item For the seqhap function, adapt the permutation rules used in 
haplo.score's sim.control parameter to ensure accuracy and precision 
thresholds for permutation p-values.  The permutations are carried out in 
seqhap.c, so the parameters p.threshold, min.sim, and max.sim are passed to 
the C code to permute the response until precision criteria met.  No longer 
use n.sim parameter; now sim.control=score.sim.control() handles the 
permutations.

  \item The user manual has been updated from version 1.3.1 to reflect all the
updates since then, and will be placed on Dan Schaid's software page, in
addition to its current location within the package

  \item help files for example datasets now pass R check.
}}
\section{ Release notes for version 1.3.8}{
\itemize{
  \item plot.seqhap, we handle very small p-values better by having a 
minimum allowable asymptotic p-value of .Machine.double$eps, and 
permutation p-value of 1/(n.sim+1).  It will also handle a ylim value 
if passed. Add more useful warning messages for when p-values are fixed 
for plotting.
  \item haplo.score, add eps.svd argument. In some assocation tests from 
  haplo.score, we have observed extremely significant values for the 
  global association test statistic. The degrees of freedom for the global 
  test is the rank of the score vector's covariance matrix.  We found the 
  source of the problem was having too low a cutoff (epsilon) for svd 
  values for determining rank of the covariance matrix.  We increased the 
  default for the epsilon from 1e-6 to 1e-5 and allow it to be changed 
  by the user as the eps.svd parameter in any function that uses haplo.score
  (haplo.score.slide, haplo.cc).

  \item Update haplo.cc parameters. We remove haplo.min.count as a 
  top-level parameter; it can only be used in the control() function, 
  just as in haplo.glm. Note that haplo.freq.min can also be used.  
  The eps.svd parameter is also added, as noted for haplo.score.
}}
\section{Releast notes for version 1.3.6}{
\itemize{
  \item Add haplo.power.qt and haplo.power.cc. Power and sample 
size calculations for haplotype association studies.
Calculations are performed given a set of haplotypes, their freqs, and
their beta coefficients, which can be converted by log(OR) for 
case-control (cc) or calculated for quantitative trait (qt) by R2 
variance explained by gene association.  For qt, use the
find.haplo.beta.qt to get these beta coefficients.  

  \item Add dataset hapPower.demo, an example dataset for
demonstrating the haplo.power.qt/cc functions in example() 
and in the manual.

  \item In past versions of haplo.em, a change was made to 
pre-calculate how much memory would be needed for all haplotype 
pairs, and issued a warning if that memory could not 
be allocated.  It stopped calculations that could have been 
completed by progressive insertion & trimming
steps because rare haplotypes are trimmed off and memory rarely meets
the max.  So the warning is taken off.

  \item Add min.posterior to haplo.em.control to give control to the user
versus the old default set at 1e-7.  In rare cases of some
datasets that had low LD and 10 or more markers, the trimming steps
actually trimmed away all haplotypes for a given person and the person
was removed.  We have changed min.posterior to 1e-9 and put in
warnings and check for this occuring.  Note, we have only observed this in
simulated data on very rare occasions.

  \item Remove allele.lev and miss.val parameters from call to haplo.glm.
We used to require the use of allele.lev as a parameter for haplo.glm,
and allow miss.val to specify codes for missing alleles in the
genotype matrix.  However, we require using setupGeno to prepare the
genotype matrix to be used in haplo.glm, after it is added to the
data.frame to be passed to haplo.glm.  miss.val is completely taken
care of there, and allele.lev is assigned as an attribute of geno.  
We have re-worked the formula and na.geno.keep to recognize these
values when it finds geno in the formula; therefore, these parameters
are not required in haplo.glm.

  \item More strict exclusion using na.geno.keep. We used to keep all 
subjects who were missing any number of alleles. However, if a subject 
is missing all alleles, they slow the calcualtions down, and don't add 
any information to the analysis. This function still removes subjects 
missing y or covariate values, and now removes subjects missing all 
their alleles.  After the removal, the attributes of the genotype 
matrix are re-calculated and retained for its use in haplo.model.frame.  

  \item In haplo.model.frame for haplo.glm, get allele.lev from geno in 
m[[]], not as passed paremeter from haplo.glm.

  \item In haplo.glm.control, enforce the default setting for 
haplo.min.count and haplo.freq.min in the function delcaration.  
In the declaration they were NA, but a default min.count of 5 was enforced.  
We have changed the default of haplo.freq.min of .01 to be enforced, 
and the delcaration now reflects the enforced default.

  \item Ginv.R for R and Ginv.q for Splus. Splus version 8.0.1 has a 
problem in its use of the svd fortran function, as called by svd.Matrix.
We contacted Insightful and they fixed it for version 8.0.4.  We include
the svd.Matrix function from version 7 and 8.0.4 in the Ginv.q file,
but only load it if the Splus version matches 8.0.1.

  \item louis.info.c -- Prior efforts to make all long integer values 
as int was not completed for this function.  The result was the 
package didn't work on linux 64bit machines.  Now it doesn't use long, 
and it should work on most platforms.

  \item louis.info.q -- When the variance of a quantitative trait is so 
high that the the information matrix becomes ill-conditioned, the Ginv 
determines the information matrix singular, and the standard errors are
incorrect. Change the epsilon parameter for the generalized inverse to
about 1e-8, versus the old default in Ginv of 1e-6.  
}}
\section{Release Notes for version 1.3.0 }{
\itemize{

  \item Add new function, seqhap, sequential haplotype selection in a 
set of loci, for choosing loci for haplotype associations, as 
described in Yu and Schaid, 2007.  The method performs three tests 
for association of a binary trait over a set of bi-allelic loci. 
When evaluating each locus, loci close to it are added in a sequential
manner based on the Mantel-Haenszel test. 

  \item geno1to2: convert geno from 1- to 2-column convert 1-column 
minor-allele-count matrix to two-column allele codes

  \item Make plot.haplo.score.slide better-handle near-zero pvalues.
For asymptotic pvalues near zero, set to epsilon. For simulated p-values, 
set to 0.5 divided by the number of simulations performed.

  \item New function, haplo.design, create design matrix for haplotypes.
In response to many requests made for getting columns for haplotype
effects to use in glm, survival, or other regression models, we
created a function to set up this kind of design matrix.  There are
issues surrounding the use of these effect columns, as outlined in the
user manual.  

  \item Ginv: svd problems continue. The Matrix library svd function 
has changed for Splus 8.0.1. Therefore, revert back to the default 
svd function in getting the generalized inverse.
}}
\section{Release notes for version 1.2.5}{
\itemize{

  \item haplo.glm: Iterative steps efficiency.
In consecutive steps of the IRWLS steps in haplo.glm, the starting
values for re-fitting the glm model were not updated to be the most
recently updated values.  This now saves about 20% of run time in
haplo.glm.

  \item haplo.score: haplo.effect allow additive, dominant, recessive 
A new option to make haplo.score more flexible.  Previously the scores
for haplotypes were computed assuming an additive effect for all
haplotypes.  A new parameter, haplo.effect, is in place to allow
either additive, dominant, or recessive effects.  

  \item haplo.score:  min.count parameter. The cut-off for selecting 
haplotypes to score is either by a minimum
frequency, skip.haplo, or a new option, min.count.  The min.count is
based on the same idea as that used in haplo.glm, where the minimum 
expected count of haplotypes in the population is enough such that 
accurate estimates of parameters and standard errors are computed.  The
min.count became needed when haplo.effect was added because under
the dominant or recessive models, the number of persons actually
having a haplotype effect could be fewer than the expected count
over the population (i.e., haplotype pair h1/h2 is coded as 0 for
both under recessive model, and h1/h1 is coded as 1 under dominant).  

  \item  haplo.em -- improved reliability of C routines.
Previously problems had been observed with running haplo.em and
haplo.glm on linux 64-bit machines, because of issues with the storage
of integers in R.  In R, all integers are stored as int, which are
stored differently on 64-bit and 32-bit machines.  We get around this
problem by using all int types for integers, which are only used for
indices of other data structures.  We find out the max value for integers
on the system, and if the indices are going to exceed the max, issue a
warning from C.  

  \item haplo.glm and Ginv: improvement of standard error calculations
Under some extreme circumstances, such as haplo.glm modeling
haplotypes with rare frequencies, or a high amount of variance in the
response, the standard error estimates were unreliable.
The issue came out in the Ginv function in haplo.stats, which needed a
smaller epsilon to decide on the rank of the information matrix. 
}}
\section{ Release notes for version 1.2.0 }{
\itemize{

  \item haplo.em: fixed memory leak.  Versions up to 1.1.1 had either 
one or two memory leaks in haplo.em. They are fixed.

  \item All .C functions: Long Integers warning for 64-bit machine
Due to problems with long integers between 32-bit and 64-bit machines
using R, all integers used in C functions will use unsigned integers.

  \item Allow haplo.effect="recessive" in haplo.glm. The estimation 
stops if no columns are left in the model.matrix for 
homozygotes with the haplotype, and for haplotypes that do not have 
any subjects with a posterior probability of being homozygous for the 
haplotype, those subjects are grouped into the baseline effect.
Guidelines for rare haplotypes are explained further in the manual.

  \item haplo.glm: na.action, when not specified got set to something 
besides the intended 'na.geno.keep'.  Now the default setting works.

  \item haplo.cc: New Function for Case-Control Analysis
New function added to combine methods of haplo.score, haplo.group and 
haplo.glm into one set of output for Case-Control data.  Choose 
haplotypes for analysis by haplo.min.count only, not a frequency cut-off.

  \item Set the new default for skip.haplo to be 5/(nrow(geno)*2)

  \item In haplo.glm: haplo.freq.min and haplo.min.count control parameters
Haplotypes used in the glm are still chosen by haplo.freq.min, but
the default is based on a minimum expected count of 5 in the
sample. The better choice for selecting haplotypes is
haplo.min.count. The issue is documented in the manual and help files.

  \item Describe the max-stat simulated p-value in more detail
in the user manual and help file

  \item haplo.em.control and haplo.em:  defaults for control parameters
changed The default for control parameter: max.iter=5000, changed from 500
insert.batch.size = 6, changed from 4
 
  \item locus function warning. The genetics package for R has a 
function named locus which does not agree with locus from haplo.stats.  
We do not plan to change it, so be aware of the possible clash if you 
use these two packages.

  \item New function haplo.scan, for analyzing a genome region with 
case-control data.  Search for a trait-locus by sliding a fixed-width 
window over each marker locus and scanning all possible haplotype 
lengths within the window
}}
\section{ Release Notes for version 1.1.1 }{
\itemize{

  \item haplo.glm: Warnings for non-integer weights
glm.fit for R does not allow non-integer weights for subjects, whereas
S-PLUS does.  Use a glm.fit.nowarn function for R to ignore warnings.

  \item haplo.glm: Character Alleles
Settings for strings as factors causes confusion for keeping
orinial character allele values.  To ensure consistency of allele
codes, use setupGeno() and then in the haplo.glm call, use allele.lev
as documented in the manual and help files.

  \item Add haplo.score.slide function, which rus haplo.score on all 
contiguous subsets of size n.slide from the loci in a genotype matrix(geno).

  \item haplo.score simulations controlled for precision.
Employ simulation precision criteria for p-values, adopted from
Besag and Clifford [1991].  Control simulations with 
score.sim.control.
}}
