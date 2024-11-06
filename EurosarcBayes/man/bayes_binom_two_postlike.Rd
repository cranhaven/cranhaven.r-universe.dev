\name{bayes_binom_two_postlike}
\alias{bayes_binom_two_postlike}
\docType{methods}
\title{Bayesian, single arm, two endpoint trial designs.}
\description{
Computes the decision rules for a single arm, two endpoint bayesian trial using the likelihood of success to make decisions.
This program assumes that the two endpoints are independent.
}
\usage{
bayes_binom_two_postlike(t, r, reviews, pra, prb, pta, ptb,
 efficacy_critical_value, efficacy_prob_stop, toxicity_critical_value,
 toxicity_prob_stop, int_combined_prob, int_futility_prob,
 int_toxicity_prob, int_efficacy_prob, futility_critical_value,
 no_toxicity_critical_value)
}
\arguments{
\item{t,r}{A vector of the probability of response and toxicity for the simulation scenarios used to compute frequentist properties. The print function requires the first to be the alternative hypothesis and subsequent entries to be the three null hypotheses. This can be run with any scenario when not using the print method}
\item{reviews}{A vector of the number of patients each interim and final analysis will occur at}
\item{pra,prb,pta,ptb}{Numeric values for the beta prior distribution to be used}
\item{futility_critical_value,
efficacy_critical_value,
toxicity_critical_value,
no_toxicity_critical_value}{Four values, for the critical values to be used as thresholds for the posterior distribution}
\item{int_combined_prob, int_futility_prob, int_toxicity_prob, int_efficacy_prob}{Probabilities to stop at interim analyses}
\item{efficacy_prob_stop,
toxicity_prob_stop}{Values or vectors of the probability required to stop at this interim analysis. If you do not wish to stop due to a rule set this to 1 at that analysis. If you wish to ignor a rule when stopping set this to 0 at that analysis}
}
\details{
Returns an object of S4 class \code{\link{trialDesign_binom_two-class}}. This has plot and print methods. For comparison between designs saved as trialDesign_binom_two objects there is a print function for the S3 class \code{\link{list_trialDesign_binom_two}}.
}
\value{
Returns an object of class \code{\linkS4class{trialDesign_binom_two}}
}
\seealso{
\code{\link{bayes_binom_two_postprob}}, \code{\link{bayes_binom_two_postlike}},\code{\link{bayes_binom_two_loss}}
}
\examples{
# modelled toxicity probability
t=c(0.1,0.1,0.3,0.3)
# modelled response probability
r=c(0.35,0.2,0.2,0.35)

reviews=c(10,15,20,25,30,35,40)

# uniform prior
pra=1;prb=1;pta=1;ptb=1

# End of trial stopping rules for success
efficacy_critical_value=0.2
efficacy_prob_stop=0.9
toxicity_critical_value=0.2
toxicity_prob_stop=0.8

# interim required probability to stop
int_combined_prob=0.99
int_futility_prob=1
int_toxicity_prob=1
int_efficacy_prob=0.99

# unused in the design for comparison to previous design
futility_critical_value=0.35
no_toxicity_critical_value=0.3

s=bayes_binom_two_postlike(t,r,reviews,pra,prb,pta,ptb,
efficacy_critical_value,efficacy_prob_stop,toxicity_critical_value,
toxicity_prob_stop,int_combined_prob,int_futility_prob,
int_toxicity_prob,int_efficacy_prob,futility_critical_value,
no_toxicity_critical_value)

s

plot(s)
}