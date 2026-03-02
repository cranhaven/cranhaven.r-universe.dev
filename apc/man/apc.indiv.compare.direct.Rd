\name{apc.indiv.compare.direct}
\alias{apc.indiv.compare.direct}
\alias{apc.indiv.waldtest.fullapc}
\alias{apc.indiv.waldtest.TS}
\alias{apc.indiv.LRtest.fullapc}
\alias{apc.indiv.LRtest.TS}
\title{
Implements direct tests between APC models}
\description{
This function allows the user to directly compare any of the APC model, its
submodels, or the TS model to any smaller model. For example, the function
can be used to compare the TS to the Ad model or the Ad model to the A model. 
Comparisons are by likelihood ratio or Wald tests. 
}
\usage{	apc.indiv.compare.direct(data, big.model, small.model, unit=1,
        			dep.var, covariates=NULL, model.family,
                    n.coh.excl.start=0, n.coh.excl.end=0,
                    n.age.excl.start=0, n.age.excl.end=0,
                    n.per.excl.start=0, n.per.excl.end=0,
                    NR.controls=NULL, test, dist,
                    wt.var=NULL, plmmodel="notplm", id.var=NULL)
        apc.indiv.waldtest.fullapc(data, dist="F", big.model="APC", 
                    small.model, dep.var, covariates=NULL,
                    model.family="gaussian", unit=1, 
                    n.coh.excl.start=0, n.coh.excl.end=0,
                    n.age.excl.start=0, n.age.excl.end=0,
                    n.per.excl.start=0, n.per.excl.end=0,
                    existing.big.model.fit=NULL, 
                    existing.small.model.fit=NULL,
                    existing.collinear=NULL,
					plmmodel = "notplm", id.var=NULL, wt.var=NULL)
        apc.indiv.waldtest.TS(data, dist="F", small.model="APC",
                    dep.var, covariates=NULL,
                    model.family="gaussian", unit=1, 
                    n.coh.excl.start=0, n.coh.excl.end = 0,
                    n.age.excl.start=0, n.age.excl.end = 0,
                    n.per.excl.start=0, n.per.excl.end = 0,
                    existing.small.model.fit=NULL, 
                    existing.big.model.fit=NULL, 
                    existing.collinear=NULL)
        apc.indiv.LRtest.fullapc(data,  big.model="APC", 
                    small.model,
                    dep.var, covariates=NULL, 
                    model.family="binomial", unit=1,
                    n.coh.excl.start=0, n.coh.excl.end=0,
                    n.age.excl.start=0, n.age.excl.end=0,
                    n.per.excl.start=0, n.per.excl.end=0,
                    existing.big.model.fit=NULL,
                    existing.small.model.fit=NULL,
                    existing.collinear=NULL)
       apc.indiv.LRtest.TS(data, small.model="APC", dep.var, covariates=NULL,
                    model.family="binomial", unit=1, 
                    n.coh.excl.start=0, n.coh.excl.end=0,
                    n.age.excl.start=0, n.age.excl.end=0,
                    n.per.excl.start=0, n.per.excl.end=0,
                    existing.small.model.fit=NULL, 
                    existing.big.model.fit=NULL, 
                    existing.collinear=NULL,
                    NR.controls=NULL)                                      
}
\arguments{
  \item{data}{The data.frame in use.}
  \item{big.model}{The name of the larger of the two models to be tested.}
  \item{small.model}{The name of the smaller of the two models to be tested.}
  \item{unit}{The interval at which age, period, and cohort are recorded 
  (must be the same for each). Default 1.}
  \item{dep.var}{The name of the dependent variable as it appears in the data}
  \item{covariates}{A vector of the names of covariates as they appear in the
  data. Default NULL.}
  \item{model.family}{Either "gaussian" or "binomial"}
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
  \item{NR.controls}{Optional list to modify aspects of the Newton-Rhapson
  iteration for binomial TS model. See details in \code{\link{apc.indiv.est.model}}.}
  \item{test}{The type of test. One of "LR", "Wald".}
  \item{dist}{The distribution against which the test statistic is compared. One of "F", "Chisq".}
  \item{wt.var}{Only if using survey weights. The name of the weights variable.}
  \item{plmmodel}{Used to indicate whether a panel data model is to be estimated and if so what type. Default is "notplm", for not panel data. Other values are "pooling", "within", "random". Further details in \code{\link[plm]{plm}}.}
  \item{id.var}{Only if using panel data. The name of the individual ID variable.}
  \item{existing.big.model.fit}{Optional specify the output of
  apc.indiv.fit.model, if already run for the big model.}
  \item{existing.small.model.fit}{Optional specify the output of 
  apc.indiv.fit.model, if already run for the small model.}
  \item{existing.collinear}{Optional specify the output of 
  apc.indiv.design.collinear, if already run.}
}  
\details{These functions are designed to facilitate direct comparison between
sub-models. The functions are used to construct the rows of tables in 
\code{apc.indiv.model.table} but can also more helpfully be used to compare 
nested sub-models that gain similar levels of suport from such a table, e.g. 
PC to P. }
\value{
  \item{test.type}{The type of test, one of "LR", "Wald".}
  \item{dist.type}{The distribution against which the test statistic is compared. One of "F", "Chisq".}
  \item{test.stat}{The value of the test statistic.}
  \item{df}{Degrees of freedom.}
  \item{df.num}{Gaussian models only. Degrees of freedom used in the numerator 
  of the F-statistic.}
  \item{df.denom}{Gaussian models only. Degrees of freedom used in the 
  denominator of the F-statistic.}
  \item{p.value}{P-value from testing against a chi-square or F distribution.}
  \item{aic.big}{AIC of the big model.}
  \item{aic.small}{AIC of the small model.}
  \item{lik.big}{Log-likelihood of the big model.}
  \item{lik.small}{Log-likelihood of the small model.}
  \item{NR.report}{Binomial TS model only. Report on the Newton-Rhapson 
  algorithm. }
}
\references{
Fannon, Z. (2018) \code{apc.indiv}: R tools to estimate age-period-cohort models with 
repeated cross section data. Mimeo. University of Oxford.

Fannon, Z., Monden, C. and Nielsen, B. (2018) Age-period-cohort modelling and
covariates, with an application to obesity in England 2001-2014. Mimeo. 
University of Oxford.
}
\author{Zoe Fannon <zoe.fannon@economics.ox.ac.uk> 26 Jun 2020}
\seealso{For model estimation: \code{\link{apc.indiv.est.model}}.
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
