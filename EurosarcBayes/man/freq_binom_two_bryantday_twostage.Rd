\name{freq_binom_two_bryantday_twostage}
\alias{freq_binom_two_bryantday_twostage}
\docType{methods}
\title{Single arm, two independent endpoint extension to Simons two-stage design}
\description{
This function searches for solutions to a single arm two-stage two-endpoint trial first proposed by Bryant and Day (1995). The two endpoints are assumed independent.
A wrapper function to compute the Bayesian properties is also provided.
}
\usage{
freq_binom_two_bryantday_twostage(r0=0.2, r1=0.35, t0=0.3, t1=0.1, 
  alpha.r, power, nrange, alpha.t=alpha.r)
}
\arguments{
\item{r0,r1}{Probability of success under H0 and H1}
\item{t0,t1}{Probability of toxicity under H0 and H1}
\item{alpha.r}{Probability of a false positive trial if the response H0 is true and toxicity is either H0 or H1}
\item{alpha.t}{Probability of a false positive trial if the toxicity H0 is true and response is either H0 or H1}
\item{power}{Probability of true positive trial result assuming H1 is true}
\item{nrange}{A vector of the total number of patients to recruit up to each stage of the trial}
}
\value{
Returns an object of class \code{\linkS4class{binom_two_bryantday}}. This can be transformed into an object of class \code{\linkS4class{trialDesign_binom_two}} using properties (see \code{\link{properties}}) and supplying the necessary values.
}
\references{
Simon R. Optimal two-stage designs for phase II clinical trials. Control Clin Trials 1989; 10: 1-10.

Bryant J, Day R. Incorporating toxicity considerations into the design of two-stage phase II clinical trials. Biometrics 1995; 51: 1372-1383.
}

\examples{

r1=0.3
r0=0.1
t0=0.3
t1=0.1

power=0.8
alpha=0.1

###############################################################
# Bryant and Day, two stage
nrange=20:27
out=freq_binom_two_bryantday_twostage(r0,r1,t0,t1,alpha,power,nrange)

###############################################################
## Frequentist simulations
# modelled toxicity probability
t=c(0.1,0.3,0.1,0.3)
# modelled response probability
r=c(0.3,0.1,0.1,0.3)

## Bayesian uniform prior
pra=1;prb=1;pta=1;ptb=1
## bayesian cutoffs
futility_critical_value=0.3
efficacy_critical_value=0.1
toxicity_critical_value=0.1
no_toxicity_critical_value=0.3

byrant_day_optimal=properties(out,t,r,pra,prb,pta,ptb,
                              futility_critical_value,efficacy_critical_value,
                              toxicity_critical_value,no_toxicity_critical_value,
                              target="optimal")

byrant_day_minmax=properties(out,t,r,pra,prb,pta,ptb,
                             futility_critical_value,efficacy_critical_value,
                             toxicity_critical_value,no_toxicity_critical_value,
                             target="minmax")

bayes_table=list(byrant_day_optimal=byrant_day_optimal,
                 byrant_day_minmax=byrant_day_minmax)
class(bayes_table)=c("list_trialDesign_binom_two",class(bayes_table))

bayes_table
}
