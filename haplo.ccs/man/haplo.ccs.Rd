\name{haplo.ccs}
\alias{haplo.ccs}
\alias{haplo.ccs.fit}
\alias{summary.haplo.ccs}
\alias{print.haplo.ccs}
\alias{print.summary.haplo.ccs}
\alias{coef.haplo.ccs}
\alias{fitted.haplo.ccs}
\alias{residuals.haplo.ccs}
\alias{vcov.haplo.ccs}
\alias{haplo.freq}
\alias{anova.haplo.ccs}
\alias{logLik.haplo.ccs}
\alias{AIC.haplo.ccs}
\alias{one}
\alias{count.haps}
\alias{return.haps}
\title{Estimate Haplotype Relative Risks in Case-Control Data}
\description{
  
  'haplo.ccs' estimates haplotype and covariate relative risks in
  case-control data by weighted logistic regression. Diplotype
  probabilities, which are estimated by EM computation with progressive
  insertion of loci, are utilized as weights. The model is specified by
  a symbolic description of the linear predictor, which includes
  specification of an allele matrix, inheritance mode, and preferences
  for rare haplotypes using 'haplo'. Note that use of this function
  requires installation of the 'haplo.stats' and 'survival'
  packages. See 'haplo.em' for a description of EM computation of
  diplotype probabilities. Currently missing genotype information is not
  allowed.

}
\usage{

haplo.ccs(formula, data=NULL, ...)

haplo.ccs.fit(y, x, int, geno, inherit.mode, group.rare, rare.freq, 
              referent, names.x, names.int, ...)

}
\arguments{

  \item{formula}{a symbolic description of the model to be fit, which
    requires specification of an allele matrix and inheritance mode
    using 'haplo'. Note that 'additive' is the default inheritance mode
    for 'haplo'. Preferences for grouping rare haplotypes are also
    specified using 'haplo'. Note that by default 'haplo' groups
    haplotypes with an estimated population frequency less than
    0.02. More details on model formulae are given below.}

  \item{data}{an optional data frame, list, or environment (or object
    coercible by 'as.data.frame' to a data frame) containing the
    variables in the model. If not found in 'data', the variables are
    taken from 'environment(formula)', typically the environment from
    which 'haplo.ccs' is called.}

  \item{referent}{a character string representing the haplotype to be used
    as the referent. The haplotype with the highest estimated population
    frequency is the default referent.}
  
  \item{\dots}{optional model-fitting arguments to be passed to 'glm'.}

  \item{y}{a vector of observations.}

  \item{x}{the design matrix for environmental covariates.}

  \item{int}{the design matrix for haplotype-environment interaction.}

  \item{geno}{the allele matrix.}

  \item{inherit.mode}{the inheritance mode specified by 'haplo'.}

  \item{group.rare}{a logical value indicating whether rare haplotypes
    should be grouped, specified by 'haplo'.}
  
  \item{rare.freq}{the population haplotype frequency used to define the
    rare haplotypes, specified by 'haplo'.}

  \item{names.x}{the column names of the design matrix for covariates.}

  \item{names.int}{the column names of the design matrix for
    haplotype-environment interaction.}

}
\details{
  
  A formula has the form 'y ~ terms' where 'y' is a numeric vector
  indicating case-control status and 'terms' is a series of terms which
  specifies a linear predictor for 'y'. A terms specification of the
  form 'first + second' indicates all the terms in 'first' together with
  all the terms in 'second' with duplicates removed. The terms in the
  formula will be re-ordered so that main effects come first, followed
  by the interactions, all second-order, all third-order and so on. The
  specification 'first*second' indicates the cross of 'first' and
  'second'.
  
  Note that 'haplo.ccs.fit' is the workhorse function. The inputs 'y',
  'x', 'geno', and 'int' represent case-control status, the matrix of
  covariates, the matrix of alleles, and the matrix of terms that have
  interaction with the haplotypes to be estimated from the alleles. The
  argument 'inherit.mode' corresponds to the inheritance mode specified
  by 'haplo', and the arguments 'group.rare' and 'rare.freq' correspond
  to the preferences for grouping rare haplotypes specified by
  'haplo'. 'names.x' and 'names.int' correspond to the column names of
  'x' and 'int', respectively. The background functions 'one',
  'count.haps', and 'return.haps' are used in specifying the model terms
  and neatly packaging the results.

}
\value{

  'haplo.ccs' returns an object of class inheriting from
  '"haplo.ccs"'. More details appear later in this section. The function
  'summary' (i.e., 'summary.haplo.ccs') obtains or prints a summary of
  the results, which include haplotype and covariate relative risks,
  robust standard error estimates, and estimated haplotype
  frequencies. The generic accessory functions 'coefficients',
  'fitted.values', and 'residuals' extract corresponding features of the
  object returned by 'haplo.ccs'. The function 'vcov' (i.e.,
  'vcov.haplo.ccs') returns sandwich variance-covariance estimates. The
  function 'haplo.freq' extracts information returned by the EM
  computation of haplotype frequencies. Note that if rare haplotypes are
  grouped, then their individual estimated frequencies are summed. An
  object of class '"haplo.ccs"' is a list containing at least the
  following components:

  \item{formula}{the formula supplied.}

  \item{call}{the matched call.}
  
  \item{coefficients}{a named vector of coefficients.}

  \item{covariance}{a named matrix of sandwich variance-covariance
    estimates, computed using 'sandcov'.}

  \item{residuals}{the working residuals, i.e., the residuals from the
    final iteration of the IWLS fit.}

  \item{fitted.values}{the fitted mean values, obtained by transforming
    the linear predictors by the expit function.}

  \item{linear.predictors}{the linear fit on the logit scale.}
  
  \item{df}{the model degrees of freedom.}

  \item{rank}{the numeric rank of the fitted model.}

  \item{family}{the family object, in this case, quasibinomial.}

  \item{iter}{the number of iterations of IWLS used.}

  \item{weights}{the working weights, i.e., the weights from the final
    iteration of the IWLS fit.}

  \item{prior.weights}{the weights initially supplied, in this case, the
    diplotype probabilities estimated by the EM computation.}

  \item{y}{a vector indicating case-control status, expanded for each
    subject by the number of plausible diplotypes for that subject.}

  \item{id}{the numeric vector used to identify subjects, expanded for
    each subject by the number of plausible diplotypes for that
    subject.}

  \item{converged}{a logical indicating whether the IWLS fit converged.}

  \item{boundary}{a logical indicating whether the fitted values are on
    the boundary of the attainable values.}

  \item{model}{the model matrix used.}

  \item{terms}{the terms object used.}
  
  \item{offset}{the offset vector used.}
  
  \item{contrasts}{the contrasts used.}
  
  \item{xlevels}{a record of the levels of the factors used in fitting.}
  
  \item{inheritance.mode}{the method of inheritance.}

  \item{rare.freq}{the value used to define the rare haplotypes.}
  
  \item{em.lnlike}{the value of the log likelihood at the last EM
    iteration.}
  
  \item{em.lr}{the likelihood ratio statistic used to test the assumed
    model against the model that assumes complete linkage equilibrium
    among all loci.}
  
  \item{em.df.lr}{the degrees of freedom for the likelihood ratio
    statistic.}

  \item{em.nreps}{the count of haplotype pairs that map to
    each subject's marker genotypes.}
  
  \item{hap1}{character strings representing the possible first
    haplotype for each subject.}
  
  \item{hap2}{character strings representing the possible second
    haplotype for each subject.}
  
  \item{hap.names}{character strings representing the unique
    haplotypes.}
  
  \item{hap.probs}{the estimated frequency of each unique
    haplotype. Note that if rare haplotypes are grouped, then their
    individual estimated frequencies are summed.}

  \item{em.converged}{a logical indicating whether the EM computation
    converged.}

  \item{em.nreps}{the number of haplotype pairs that map to the marker
    genotypes for each subject.}
  
  \item{em.max.pairs}{the maximum number of pairs of haplotypes per
    subject that are consistent with their marker data.}
  
  \item{em.control}{a list of control parameters for the EM computation.}
  
}
\note{

  The functions 'anova', 'logLik', and 'AIC' are not appropriate for
  models of class '"haplo.ccs"', because 'haplo.ccs' does not fit by maximum
  likelihood. Accordingly, model and null deviance are not reported.

}
\author{

  Benjamin French and Thomas Lumley, University of Washington

}
\references{

  French B, Lumley T, Monks SA, Rice KM, Hindorff LA, Reiner AP, Psaty
  BM. Simple estimates of haplotype relative risks in case-control
  data. Genetic Epidemiology 2006; 30(6):485-494.
    
  The help files for 'glm', 'haplo.em', and 'haplo.glm' were
  instrumental in creating this help file.

}
\seealso{

  \code{\link{glm}},
  \code{\link{haplo}},
  \code{\link[haplo.stats]{haplo.em}},
  \code{\link[haplo.stats]{haplo.glm}},
  \code{\link{sandcov}}

}
\examples{

data(renin)

## Fit a model for haplotype effects.

haplo.ccs(case ~ haplo(geno))

## Fit a model for haplotype and covariate effects.

haplo.ccs(case ~ gender + age + factor(race) + haplo(geno))

## Fit a model for haplotype interaction with gender.

haplo.ccs(case ~ age + factor(race) + gender*haplo(geno))

}
\keyword{models}
\keyword{regression}
