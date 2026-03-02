\name{apc.indiv.model.table}
\alias{apc.indiv.model.table}
\alias{apc.indiv.waldtable}
\alias{apc.indiv.waldtable.TS}
\alias{apc.indiv.LRtable}
\alias{apc.indiv.LRtable.TS}
\title{
Generate table to select APC submodel}
\description{
These functions test, for a given choice of dependent variable and covariates, which of the TS, APC, and APC submodels provides the best fit to the data. Comparison is by Wald or likelihood ratio test and  where appropriate by Akaike Information Criterion. A table is generated with these statistics for each model considered. 
}
\usage{apc.indiv.model.table(data, dep.var, covariates = NULL,
			unit = 1, n.coh.excl.start = 0, n.coh.excl.end = 0,
			n.age.excl.start = 0, n.age.excl.end = 0,
			n.per.excl.start = 0, n.per.excl.end = 0,
			model.family, NR.controls = NULL,
			test, dist,
			TS=FALSE, wt.var=NULL, plmmodel="notplm",
			id.var=NULL)
	   apc.indiv.waldtable(data, dep.var, covariates = NULL, 
	   dist="F", unit = 1, model.family, 
	    	n.coh.excl.start = 0, n.coh.excl.end = 0,
			n.age.excl.start = 0, n.age.excl.end = 0,
			n.per.excl.start = 0, n.per.excl.end = 0,
			 wt.var=NULL, plmmodel="notplm",
			id.var=NULL)
			apc.indiv.waldtable.TS(data, dep.var, covariates=NULL, dist = "F",
                                unit=1, model.family = "gaussian",
                                n.coh.excl.start=0, n.coh.excl.end=0, 
                                n.age.excl.start=0, n.age.excl.end=0, 
                                n.per.excl.start=0, n.per.excl.end=0)
     apc.indiv.LRtable(data,  dep.var, covariates=NULL, 
                              model.family, unit=1, 
                              n.coh.excl.start=0, n.coh.excl.end=0, 
                              n.age.excl.start=0, n.age.excl.end=0,
                              n.per.excl.start=0, n.per.excl.end=0)
	   apc.indiv.LRtable.TS(data,  dep.var, covariates=NULL, 
                                 model.family, unit=1, 
                                 n.coh.excl.start=0, n.coh.excl.end=0, 
                                 n.age.excl.start=0, n.age.excl.end=0,
                                 n.per.excl.start=0, n.per.excl.end=0,
                                 NR.controls=NR.controls)
}
\arguments{
  \item{data}{
The data.frame in use}
  \item{dep.var}{
The name of the dependent variable as it appears in the data}
  \item{covariates}{
A vector of the names of covariates as they appear in the data. Default NULL.}
  \item{unit}{
The interval at which age, period, and cohort are recorded (must be the same for each). Default 1.}
  \item{n.coh.excl.start}{
If any cohorts have been censored (AP data only). Default 0.}
  \item{n.coh.excl.end}{
If any cohorts have been censored (AP data only). Default 0.}
  \item{n.age.excl.start}{
If any ages have been censored (PC data only). Default 0.}
  \item{n.age.excl.end}{
If any ages have been censored (PC data only). Default 0.}
  \item{n.per.excl.start}{
If any periods have been censored (AC data only). Default 0.}
  \item{n.per.excl.end}{
If any periods have been censored (AC data only). Default 0.}
  \item{model.family}{
Either "gaussian" or "binomial"}
  \item{NR.controls}{
Optional list to modify aspects of the Newton-Rhapson iteration for binomial TS model.See details in \code{\link{apc.indiv.est.model}}.}
  \item{test}{The type of test. One of "LR", "Wald".}
  \item{TS}{...}
  \item{dist}{The distribution against which the test statistic is compared. One of "F", "Chisq".}
  \item{wt.var}{Only if using survey weights. The name of the weights variable.}
  \item{plmmodel}{Used to indicate whether a panel data model is to be estimated and if so what type. Default is "notplm", for not panel data. Other values are "pooling", "within", "random". Further details in \code{\link[plm]{plm}}.}
  \item{id.var}{Only if using panel data. The name of the individual ID variable.}
}
\details{Each row of the table corresponds to a single sub-model of the APC model. The first three columns test the sub-model in question against the time-saturated model. The next three columns test the sub-model against the full APC model. The final two columns report the likelihood and AIC of the estimated sub-model. The model with the lowest AIC value which is also not rejected in tests against the APC and TS models should be selected.}
\value{
  \item{table}{contains the table of comparison statistics.}
  \item{NR.report}{for logit models only, a report on the Newton-Rhapson algorithm used to estimate the time-saturated model.}
}
\references{
Fannon, Z. (2018) \code{apc.indiv}: R tools to estimate age-period-cohort models with repeated cross section data. Mimeo. University of Oxford.

Fannon, Z., Monden, C. and Nielsen, B. (2018) Age-period-cohort modelling and covariates, with an application to obesity in England 2001-2014. Mimeo. University of Oxford.
}
\author{Zoe Fannon <zoe.fannon@economics.ox.ac.uk> 26 Jun 2020}
\seealso{For model estimation: \code{\link{apc.indiv.est.model}}
For pairwise model comparison: \code{\link{apc.indiv.model.table}}, \code{\link[lmtest]{waldtest}}, \code{\link[car]{linearHypothesis}}.
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
