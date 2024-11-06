\name{freq_binom_two_singlestage}
\alias{freq_binom_two_singlestage}
\docType{methods}
\title{
Frequentist single-arm two-endpoint trial sample size
}
\description{
Generate  minimum sample size for the frequentist two-endpoint single-arm trial.
}
\usage{
freq_binom_two_singlestage(r0, r1, t0, t1, power, alpha.r, nmax = 100,
	alpha.t = alpha.r, nmin = 1, adjust = TRUE)
}
\arguments{
\item{r0}{Null hypothesis for the response endpoint}
\item{r1}{Alternative hypothesis for the response endpoint}
\item{t0}{Null hypothesis for the toxicity endpoint}
\item{t1}{Alternative hypothesis for the toxicity endpoint}
\item{power}{Required power for the trial design}
\item{alpha.r}{The maximum size for the type one error for the response endpoint}
\item{nmax}{Maximum sample size to look for solutions. Default 100}
\item{alpha.t}{The maximum size for the type one error for the toxicity endpoint. Optional}
\item{nmin}{Minimum sample size to look for solution, Default 1}
\item{adjust}{Boolean about whether to adjust for multiple endpoints or not}
}
\value{
Returns an object of class binom_two_singlestage. This can be transformed into an object of class \code{\linkS4class{trialDesign_binom_two}} using properties (see \code{\link{properties}}) and supplying the necessary values.
}
\seealso{
\code{\link{properties}}
}
\examples{
r1=0.35
r0=0.2
t0=0.3
t1=0.1

power=0.8
alpha=0.1

nmax=50
out_single=freq_binom_two_singlestage(r0,r1,t0,t1,power,alpha,nmax,adjust=TRUE)

########################################
# Information for properties
## Frequentist simulations
# modelled toxicity probability
t=c(0.1,0.3,0.1,0.3)
# modelled response probability
r=c(0.35,0.2,0.2,0.35)

## Bayesian uniform prior
pra=1;prb=1;pta=1;ptb=1
## bayesian cutoffs
futility_critical_value=0.35
efficacy_critical_value=0.2
toxicity_critical_value=0.1
no_toxicity_critical_value=0.3

single_stage=properties(out_single,t,r,pra,prb,pta,ptb,futility_critical_value,
	efficacy_critical_value,toxicity_critical_value,no_toxicity_critical_value)

single_stage
}
