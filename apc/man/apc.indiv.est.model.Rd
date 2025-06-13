\name{apc.indiv.est.model}
\alias{apc.indiv.est.model}
\alias{apc.indiv.design.collinear}
\alias{apc.indiv.design.model}
\alias{apc.indiv.fit.model}
\alias{apc.indiv.estimate.TS}
\alias{apc.indiv.logit.TS}
\title{Estimate a single APC model}
\description{The function \code{apc.indiv.est.model} is used to estimate any of:
  the APC model, any APC submodel, or the time-saturated model. To estimate
  the APC model or a submodel, it calls \code{apc.indiv.design.collinear},
  \code{apc.indiv.design.model}, and \code{apc.indiv.fit.model} in that order.
  To estimate the time-saturated (TS) model it calls either
  \code{apc.indiv.estimate.TS} or \code{apc.indiv.logit.TS}, depending on the
  selected \code{model.family}. These functions can also be called directly by
  the user.}
\usage{
  apc.indiv.est.model(data, unit = 1,
                      n.coh.excl.start=0, n.coh.excl.end=0,
                      n.per.excl.start=0, n.per.excl.end=0,
                      n.age.excl.start=0, n.age.excl.end=0,  
                      model.design = "APC", dep.var = NULL, 
                      covariates = NULL, model.family = "gaussian",
                      NR.controls = NULL, 
                      existing.collinear = NULL,
                      existing.design = NULL,
					  plmmodel = "notplm", id.var = NULL,
                      wt.var = NULL)					  
  apc.indiv.design.collinear(data, unit = 1,
                             n.coh.excl.start = 0, n.coh.excl.end = 0,
                             n.per.excl.start = 0, n.per.excl.end = 0,
                             n.age.excl.start = 0, n.age.excl.end = 0)
  apc.indiv.design.model(apc.indiv.design.collinear,
                         model.design = "APC", dep.var = NULL, 
                         covariates = NULL, plmmodel = "notplm",
                         wt.var = NULL, id.var = NULL)
  apc.indiv.fit.model(apc.indiv.design.model, model.family = "gaussian", DV = NULL)
  apc.indiv.estimate.TS(data, dep.var, covariates = NULL)
  apc.indiv.logit.TS(data, dep.var, covariates = NULL, NR.controls = NULL)}
\arguments{
  \item{data}{The data.frame in use}
  \item{unit}{The interval at which age, period, and cohort are recorded 
  (must be the same for each). Default 1.}
  \item{n.coh.excl.start}{If any cohorts have been censored (AP data only). 
  Default 0.}
  \item{n.coh.excl.end}{If any cohorts have been censored (AP data only). 
  Default 0.}
  \item{n.per.excl.start}{If any periods have been censored (AC data only). 
  Default 0.}
  \item{n.per.excl.end}{If any periods have been censored (AC data only). 
  Default 0.}
  \item{n.age.excl.start}{If any ages have been censored (PC data only). 
  Default 0.}
  \item{n.age.excl.end}{If any ages have been censored (PC data only). 
  Default 0.}
  \item{model.design}{The name of the model to be estimated. One of "TS",
  "APC", "AC", etc.}
  \item{dep.var}{The name of the dependent variable as it appears in the data}
  \item{DV}{apc.indiv.fit.model only. Optional. Vector containing dependent 
  variable. }
  \item{covariates}{A vector of the names of covariates as they appear in the 
  data. Default NULL.}
  \item{plmmodel}{Used to indicate whether a panel data model is to be estimated and if so what type. Default is "notplm", for not panel data. Other values are "pooling", "within", "random". Further details in \code{\link{plm}}.}
  \item{id.var}{Only if using panel data. The name of the individual ID variable.}
  \item{wt.var}{Only if using survey weights. The name of the weights variable.}
  \item{model.family}{Either "gaussian" or "binomial". Default "gaussian".}
  \item{NR.controls}{Optional list to modify aspects of the Newton-Rhapson 
  iteration for binomial TS model. Further information in "Details", below.}
  \item{existing.collinear}{Optional specify the output of 
  apc.indiv.design.collinear, if already run.}
  \item{existing.design}{Optional specify the output of apc.indiv.design.model,
  if already run.}
  \item{apc.indiv.design.collinear}{Output from the command 
  \code{apc.indiv.design.collinear}.}
  \item{apc.indiv.design.model}{Output from the command 
  \code{apc.indiv.design.model}.}
}
\details{The casual user should start with the general function 
  \code{apc.indiv.est.model} for analysis. The underlying functions should be
  employed if the user needs to run many models using the same relatively large
  dataset, in which case time can be saved by running 
  \code{apc.indiv.design.collinear} just once and using 
  \code{apc.indiv.design.model} and \code{apc.indiv.fit.model} to estimate 
  each of the models.
  
  The time-saturated (TS) binomial model is estimated by a customized
  Newton-Rhapson iteration. Aspects of this iteration can be controlled by
  specifying the \code{NR.controls} option of \code{apc.indiv.est.model} or of
  \code{apc.indiv.logit.TS}.
  \code{NR.controls} is a named list of length \code{8}.
  In order, the elements are:
  \code{maxit.loop}, \code{maxit.linesearch}, \code{tolerance}, \code{init},
  \code{inv.tol}, \code{d1.tol}, \code{custom.kappa}, \code{custom.zeta}.
  \code{maxit.loop} sets the maximum number of Newton-Rhapson iterations, and has a default of 10.
  \code{maxit.linesearch} sets the maximum number of linesearch iterations
  within each Newton-Rhapson iteration, and has a default of 20.
  \code{tolerance} sets the condition for convergence, i.e. the tolerated
  difference between likelihoods from one Newton-Rhapson iteration to the
  next; the default is \code{.002}.
  \code{init} sets the starting values for the iteration. The default is
  \code{"ols"}, meaning that estimates from the linear probability model are
  the starting values; one can also use \code{"zero"} to set the starting
  values to zero, or use \code{"custom"} and specify custom starting values
  using \code{custom.kappa} and \code{custom.zeta}.
  \code{inv.tol}sets the tolerance of small values when inverting a matrix
  (using \code{\link{solve}}), and the default is the machine precision.
  \code{d1.tol} sets the magnitude of norm of first derivative to be tolerated
  in Newton-Rhapson iteration, and has a default of \code{.002}.
  \code{custom.kappa} is used to specify custom starting values for the TS
  indicator parameters, while \code{custom.zeta} is used to specify custom
  starting values for parameters on any covariates.
}
\value{
  \item{fit}{The output of either \code{\link{glm}}, \code{\link{svyglm}}, or \code{\link{plm}} for repeated cross-section, repeated cross-section with survey weights, or panel models respectively. Can be used directly with follow-on functions like \code{\link{waldtest}}}.
  \item{coefficients.canonical}{Matrix of estimates, standard error, t-statistic, and p-value of canonical parameter.}
  \item{coefficients.covariates}{Matrix of estimates, standard error, t-statistic, and p-value of covariates.}
  \item{coefficients.TS}{TS model only: matrix of estimates, standard error, t-statistic, and p-value of TS indicators. }
  \item{aic}{TS model only: Akaike Information Criterion.}
  \item{likelihood}{model likelihood.}
  \item{model.design}{which APC submodel has been estimated.}
  \item{fixef}{When plmmodel = "within", estimated individual fixed effects. Otherwise NULL. }
  \item{full.design.collinear}{from apc.indiv.design.collinear only. The 
  collinear design matrix.}
  \item{full.design}{from apc.indiv.design.model only. The design matrix used to estimate the model.}
  \item{DV}{from apc.indiv.design.model only, if dep.var specified. A vector of the outcome variable. }
  \item{ID}{from apc.indiv.design.model only, if panel model. A vector of the individual ID variable.}
  \item{PER}{from apc.indiv.design.model only, if panel model. A vector of the period variable.}
  \item{WT}{from apc.indiv.design.model only, if wt.var specified. A vector of the survey weight variable. }
  \item{model.formula}{from apc.indiv.design.model only, the implied model formula. NULL if dep.var not specified.}
  \item{model.string}{from apc.indiv.design.model only, the implied model formula as a character string. RHS only if dep.var not specified. }  
}
\references{
  Fannon, Z. (2018) \code{apc.indiv}: R tools to estimate age-period-cohort models with
  repeated cross section data. Mimeo. University of Oxford.
  
  Fannon, Z., Monden, C. and Nielsen, B. (2018) Age-period-cohort modelling and
  covariates, with an application to obesity in England 2001-2014. Mimeo. 
  University of Oxford.
}
\author{Zoe Fannon <zoe.fannon@economics.ox.ac.uk> 26 Jun 2020}
\seealso{For model estimation: \code{\link{glm}}, \code{\link{svyglm}}, \code{\link{plm}}
For model testing: \code{\link{apc.indiv.model.table}}, code{\link{apc.indiv.compare.direct}}, \code{\link{waldtest}}, \code{\link{linearHypothesis}}
For plotting: \code{\link{apc.plot.fit}}.
The data in these examples are the
\code{\link[ISLR]{Wage}} data from the package ISLR
and the
\code{\link[AER]{PSID7682}} data from the package AER.

For examples, see the vignette
	\href{../doc/IntroductionIndividualData.pdf}{\code{IntroductionIndividualData.pdf}},
	\href{../doc/IntroductionIndividualData.R}{\code{IntroductionIndividualData.R}}
	on
	\href{../doc/index.html}{\code{Vignettes}}.
	Further examples in the vignette
	\href{../doc/IntroductionIndividualDataFurtherExamples.pdf}{\code{IntroductionIndividualDataFurtherExamples.pdf}},
	\href{../doc/IntroductionIndividualDataFurtherExamples.R}{\code{IntroductionIndividualDataFurtherExamples.R}}.
}
\examples{
#### see vignettes
}
\keyword{models}
