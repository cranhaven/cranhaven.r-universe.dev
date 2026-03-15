% $Author: sinnwell $ 
% $Date: 2013/01/14 19:27:15 $ 
% $Header: /projects/genetics/cvs/cvsroot/haplo.stats/man/haplo.glm.Rd,v 1.6 2013/01/14 19:27:15 sinnwell Exp $
% $Locker:  $ 
% $Log: haplo.glm.Rd,v $
% Revision 1.6  2013/01/14 19:27:15  sinnwell
% add haplo.binomial
%
% Revision 1.5  2011/12/12 17:02:15  sinnwell
% notes about LC-COLLATE, and update see-also
%
% Revision 1.4  2011/11/10 15:29:41  sinnwell
% major update to hapglm, minor changes to Rd files, prepare for version 1.5.0 release
%
% Revision 1.3  2008/03/24 21:13:26  sinnwell
% rm allele.lev and miss.val, recommend setupGeno in details, add missing in return object
%
% Revision 1.2  2008/03/19 21:32:38  sinnwell
% change converge.em description
%
% Revision 1.1  2008/01/08 22:39:02  sinnwell
% Initial revision
%
%Revision 1.9  2004/03/16 22:34:30  sinnwell
%reduce data to make run faster
%
%Revision 1.8  2004/03/16 16:59:50  sinnwell
%add return chars
%
%Revision 1.7  2004/03/12 18:02:10  sinnwell
%add returns in usage
%
%Revision 1.6  2004/03/11 15:52:18  sinnwell
%fix more with setupGeno
%
%Revision 1.5  2004/03/10 15:54:16  sinnwell
%comments for allele.lev, final use of setupGeno and allele.lev in example
%
%Revision 1.4  2004/03/03 22:14:18  schaid
%added allele.lev
%
%Revision 1.3  2004/03/01 21:22:57  sinnwell
%fix example
%
%Revision 1.2  2004/02/16 17:26:08  sinnwell
%change T/F to TRUE/FALSE for R code-doc agreement
%
%Revision 1.1  2003/09/16 16:02:02  schaid
%Initial revision


\name{haplo.glm}
\alias{haplo.glm}
\alias{print.haplo.glm}
\title{
  GLM Regression of Trait on Ambiguous Haplotypes
}
\description{
Perform glm regression of a trait on haplotype effects, allowing for
ambiguous haplotypes. This method performs an iterative  two-step EM, 
with the posterior probabilities of pairs of haplotypes per subject used as
weights to update the regression coefficients, and the regression
coefficients used to update the posterior probabilities.
}
\usage{
haplo.glm(formula=formula(data), family=gaussian, data=parent.frame(),
           weights, na.action="na.geno.keep", start=NULL, 
           locus.label=NA, control=haplo.glm.control(), 
           method="glm.fit", model=TRUE, x=FALSE, y=TRUE, 
           contrasts=NULL, ...)
}
\arguments{
\item{formula}{
          a formula expression as for other regression models, of the
          form response ~ predictors. For details, see the documentation
          for lm and formula. 
}
\item{family}{
          a family object. This is a list of expressions for defining the
          link, variance function, initialization values, and iterative
          weights for the generalized linear model. Supported families
          are: gaussian, binomial, poisson. Currently, only the logit
          link is implemented for binimial.	  
}
\item{data}{
  a data frame in which to interpret the variables occurring in
  the formula. A CRITICAL element of the data frame is the
  matrix of genotypes, denoted here as "geno", although an 
  informative name should be used in practice. This geno matrix is actually a 
  matrix of alleles, such that each locus has a pair of adjacent 
  columns of alleles, and the order of columns corresponds to 
  the order of loci on a chromosome.  If there are K loci, then 
  ncol(geno) = 2*K. Rows represent the alleles for each subject. It is
  also CRITICAL that this matrix is defined as a model.matrix, so
  the columns of the matrix are packaged together into a single
  matrix object. If geno is a matrix of alleles, then before adding it to
  the data frame, use the setupGeno() function, which will assign this
  correct class.  The function will also recode alleles to numeric
  starting from 1, while saving the original alleles in the unique.alleles
  attribute.  This attribute is required in haplo.glm.
}
\item{weights}{
  the weights for observations (rows of the data frame). By default, all
  observations are weighted equally.
}
\item{na.action }{
  a function to filter missing data. This is applied to the
  model.frame. The default value of na.action=na.geno.keep will keep
  observations with some (but not all) missing alleles, but exclude
  observations missing any other data (e.g., response variable, other
  covariates, weight). The EM algorithm for ambiguous haplotypes accounts
  for missing alleles. Similar to the usual glm, na.fail creates an
  error if any missing values are found, and a third possible
  alternative is na.exclude, which deletes observations that contain
  one or more missing values for any data, including alleles.
}
\item{start}{
  a vector of initial values on the scale of the linear
  predictor. 
}
\item{locus.label }{
  vector of labels for loci.
}
\item{control}{
  list of control parameters. The default is constructed by the function
  haplo.glm.control. The items in this list control the regression
  modeling of the haplotypes (e.g., additive, dominant, recessive
  effects of haplotypes; which haplotype is chosen as the baseline for
  regression; how to handle rare haplotypes; control of the glm function
  - maximum number of iterations), and the EM algorithm for estimating initial
  haplotype frequencies. See haplo.glm.control for details.
}
\item{method}{
  currently, glm.fit is the only method allowed.
}
\item{model}{
  logical, if model=TRUE, the model.frame is returned.
}
\item{x}{
  logical, if x=TRUE, the model.matrix is returned.
}
\item{y}{
  logical, if y=TRUE, the response variable is returned.
}
\item{contrasts}{
  currently ignored
}
\item{\dots }{
  other arguments that may be passed - currently ignored.
}
}
\value{
  An object of class "haplo.glm" is returned.          
  The output object from haplo.glm has all the components of a
  glm object, with a few more. It is important to note that some of
  the returned components correpond to the "expanded" version
  of the data. This means that each observation is expanded into
  the number of terms in the
  observation's posterior distribution of haplotype pairs, given the
  marker data. For example, when fitting the response y on haplotype
  effects, the value of y[i], for the ith observation, is replicated
  m[i] times, where m[i] is the number of pairs of haplotypes consistent
  with the observed marker data. The returned components that are
  expanded are indicated below by [expanded] in the definition of the
  component.
  
  These expanded components may need to be collapsed, depending on the
  objective of the user. For example, when considering the influence of 
  an observation, it may make sense to examine the expanded residuals
  for a single observation, perhaps plotted against the haplotypes
  for that observation. In contrast, it would not be sensible to plot
  all residuals against non-genetic covariates, without first
  collapsing the expanded residuals for each observation. To collapse,
  one can use the average residual per observation, weighted according
  to the posterior probabilities. The appropriate weight can be
  computed as wt = weight.expanded * haplo.post.info[[post]].
  Then, the weighted average can be calculated as
  with(fit, tapply(residuals * wt, haplo.post.info[["indx"]], sum).

\item{coefficients}{
the coefficients of the linear.predictors, which multiply the
  columns of the model matrix. The names of the coefficients are
  the names of the column of the model matrix. For haplotype coefficients, the names
  are the concatentation of name of the geno matrix with a haplotype
  number. The haplotype number corresponds to the index of the
  haplotype. The default print will show the coefficients with haplotype
  number, along with the alleles that define the haplotype, and the
  estimated haplotype frequency. If the model is over-determined there
  will be missing values in the coefficients corresponding to
  inestimable coefficients.
}
\item{residuals}{
[expanded]  residuals from the final weighted least squares fit; also
  known as working residuals, these are typically not
  interpretable without rescaling by the weights
  (see glm.object and residuals.haplo.glm).
}
\item{fitted.values}{
[expanded] fitted mean values, obtained by transforming
  linear.predictors using the inverse link function (see glm.object).
}
\item{effects}{
[expaded] orthogonal, single-degree-of-freedom effects (see lm.object).          
}
\item{R}{
the triangular factor of the decomposition (see lm.object).                
}
\item{rank}{
the computed rank (number of linearly independent columns in
the model matrix), which is the model degrees of freedom
- see lm.object.
}
\item{assign}{
the list of assignments of coefficients (and effects) to the
  terms in the model (see lm.object).      
}
\item{df.residual}{
[expanded] number of degrees of freedom for residuals, corresponding
  to the expanded data.
}
\item{prior.weights}{
[expanded] input weights after expanding according to the number
  of pairs of haplotypes consistent with an observation's 
  marker genotype data. 
}
\item{family}{
a 3 element character vector giving the name of the family, the
 link and the variance function; mainly for printing purposes.           
}
\item{linear.predictors}{
[expanded] linear fit, given by the product of the model matrix and
  the coefficients. In a glm, eta.
}
\item{deviance}{
 up to a constant, minus twice the maximized log-likelihood.
 Similar to the residual sum of squares.         
}
\item{null.deviance}{
the deviance corresponding to the model with no predictors.    
}
\item{call}{
an image of the call that produced the object, but with the
   arguments all named and with the actual formula included as the
   formula argument.             
}
\item{iter}{
the number of IRLS iterations used to compute the estimates, for the
  last step of the EM fit of coefficients.
}
\item{y}{
expanded response.                
}
\item{contrasts}{
a list containing sufficient information to construct the
contrasts used to fit any factors occurring in the model (see lm.object).
}
\item{lnlike}{
log-likelihood of the fitted model.     
}
\item{lnlike.null}{
log-likelihood of the null model (intercept-only).      
}
\item{lrt}{
likelihood ratio test statistic to test whether all coefficients
  (excepet intercept) are zero: 2*(lnlike - lnlike.null)
}
\item{terms}{
an object of mode expression and class term summarizing the
  formula, but not complete for the final model. Because this does
  not represent expansion of the design matrix for the haplotypes, it
  is typically not of direct relevance to users.           
}
\item{control}{
list of all control parameters        
}
\item{haplo.unique}{
the data.frame of unique haplotypes   
}
\item{haplo.base}{
the index of the haplotype used as the base-line for the regression
  model. To see the actual haplotype definition, use the following: 
  with(fit, haplo.unique[haplo.base,]), where fit is the saved haplo.glm
  object (e.g., fit <- haplo.glm(y ~ geno, ...) ).
}
\item{haplo.freq}{
the final estimates of haplotype frequencies, after completing EM
  steps of updating haplotype frequencies and regression coefficients.
  The length of haplo.freq is the number of rows of haplo.unique, and
  the order of haplo.freq is the same as that for the rows of
  haplo.unique. So, the frequencies of the unique haplotypes can be 
  viewed as with(fit, cbind(haplo.unique, haplo.freq)).
}
\item{haplo.freq.init}{
the initial estimates of haplotype frequencies, based on the EM
  algorithm for estimating haplotype frequencies, ingnoring the
  trait. These can be compared with haplo.freq, to see the impact of 
  using the regression model to update the haplotype frequencies.
}
\item{converge.em}{
T/F whether the EM-glm steps converged
}
\item{haplo.common}{
the indices of the haplotypes determined to be "common" enough to
  estimate their corresponding regression coefficients.
}
\item{haplo.rare}{
the indices of all the haplotypes determined to be too rare to
  estimate their specific regression coefficients.     
}
\item{haplo.rare.term}{
T/F whether the "rare" term is included in the haplotype regression model.
}
\item{haplo.names}{
the names of the coefficients that represent haplotype effects.    
}
\item{haplo.post.info}{
a data.frame of information regarding the posterior
  probabilites. The columns of this data.frame are: indx (the index of
  the input obsevation; if the ith observation is repeated m times, then
  indx will show m replicates of i; hence, indx will correspond to the
  "expanded" observations); hap1 and hap2 (the indices of the
  haplotypes; if hap1=j and hap2=k, then the two haplotypes in terms
  of alleles are haplo.unique[j,] and haplo.unique[k,] from the fitted object);
  post.init (the initial posterior probability, based on
  haplo.freq.init); post (the final posterior probability, based on
  haplo.freq).
}
\item{x}{
the model matrix, with [expanded] rows, if x=T.              
}
\item{info}{
the observed information matrix, based on Louis' formula. The upper
  left submatrix is for the regression coefficient, the lower right
  submatrix for the haplotype frequencies, and the remaining is the
  information between regression coefficients and haplotype frequencies. 
}
\item{var.mat}{
the variance-covariance matrix of regression coefficients and
  haplotype frequencies, based on the inverse of info. Upper left
  submatrix is for regression coefficients, lower right submatrix for
  haplotype frequencies.
}
\item{haplo.elim}{
the indices of the haplotypes eliminated from the info and var.mat
  matrices because their frequencies are less than haplo.min.info (the
  minimum haplotype frequency required for computation of the
  information matrix - see haplo.glm.control)
}
\item{missing}{
a matrix of logical values, indicating whether rows of data were
removed for missing values in either genotype matrix (genomiss) or
any other variables (yxmiss), such as y, other covariates, or weights.  
}
\item{rank.info}{
rank of information (info) matrix.
}
}
\details{
  To properly prepare the data frame, the genotype matrix must be
  processed by setupGeno, and then included in the data frame with the
  response and other variables.

  For binomial family, the initialization of values gives warnings if
  non-integer number of successes, which is a concern in these models
  because of the weights of posterior probability of each haplotype pair
  per subject.  We supress the warnings by defining a haplo.binomial
  family, which we use if family=binomial is used.
}

\section{References}{
  Lake S, Lyon H, Silverman E, Weiss S, Laird N, Schaid D (2002)
  Estimation and tests of haplotype-environment interaction 
  when linkage phase is ambiguous. Human Heredity 55:56-65.
}
\seealso{
  \code{\link{haplo.glm.control}},
  \code{\link{haplo.em}},
  \code{\link{haplo.model.frame}}
}
\examples{
cat(" FOR REGULAR USAGE, DO NOT DISCARD GENOTYPES WITH MISSING VALUES\n")
cat(" WE ONLY SUBSET BY keep HERE SO THE EXAMPLES RUN FASTER\n")

 data(hla.demo)
 geno <- as.matrix(hla.demo[,c(17,18,21:24)])
 keep <- !apply(is.na(geno) | geno==0, 1, any) # SKIP THESE THREE LINES
 hla.demo <- hla.demo[keep,]                   # IN AN ANALYSIS
 geno <- geno[keep,]                           # 
 attach(hla.demo)
 label <-c("DQB","DRB","B")
 y <- hla.demo$resp
 y.bin <- 1*(hla.demo$resp.cat=="low")

# set up a genotype array as a model.matrix for inserting into data frame
# Note that hla.demo is a data.frame, and we need to subset to columns
# of interest. Also also need to convert to a matrix object, so that
# setupGeno can code alleles and convert geno to 'model.matrix' class.

 geno <- setupGeno(geno, miss.val=c(0,NA))

  # geno now has an attribute 'unique.alleles' which must be passed to
  # haplo.glm as allele.lev=attributes(geno)$unique.alleles, see below

 my.data <- data.frame(geno=geno, age=hla.demo$age, male=hla.demo$male,
                      y=y, y.bin=y.bin)

 fit.gaus <- haplo.glm(y ~ male + geno, family = gaussian,  na.action=
               "na.geno.keep",allele.lev=attributes(geno)$unique.alleles, 
               data=my.data, locus.label=label,
               control = haplo.glm.control(haplo.freq.min=0.02))
 fit.gaus
 
}
\keyword{glm}
% docclass is function
% Converted by Sd2Rd version 37351.
