\name{bayes_binom_two_loss}
\alias{bayes_binom_two_loss}
\docType{methods}
\title{Bayesian, single arm, two endpoint trial designs, using loss functions to make decisions}
\description{
Computes the decision rules for a single arm, two endpoint bayesian trial using a region of acceptable designs and loss functions to make decisions.
This program assumes that the two endpoints are independent.
A number of region spaces are provided.
This function has the option of providing pre-existing decision matrices to skip this section if you wish to run additional simulations on an already computed design.
}
\usage{
bayes_binom_two_loss(t, r, reviews, pra, prb, pta, ptb,
 l_alpha_beta, l_alpha_c, stage_after_trial, fun.integrate,
 efficacy_critical_value, toxicity_critical_value,
 futility_critical_value, no_toxicity_critical_value, decision=NULL,   
 W=NULL, fun.graph=NULL, ...)
}
\arguments{
\item{t,r}{A vector of the probability of response and toxicity for the simulation scenarios used to compute frequentist properties. The print function requires the first to be the alternative hypothesis and subsequent entries to be the three null hypotheses. This can be run with any scenario when not using the print method}
\item{reviews}{A vector of the number of patients each interim and final analysis will occur at}
\item{pra,prb,pta,ptb}{Numeric values for the beta prior distribution to be used}
\item{l_alpha_beta,l_alpha_c}{The two loss function variables weighting between stopping early for futility or efficacy and continuing the trial}
\item{fun.integrate}{function used to integrate the probability of being in the region of interest given the posterior distributions of the data and prior information}
\item{stage_after_trial}{Optional argument for censored stages after the trial has completed. This is likely to create a region of inclusiveness upon concluding the trial}
\item{futility_critical_value,
efficacy_critical_value,
toxicity_critical_value,
no_toxicity_critical_value}{Four values, for the critical values to be used as thresholds for the posterior distribution}
\item{decision}{Optional input the decision matrices from a previous run to perform additional frequentist simulations on the design.}
\item{W}{Optional input the posterior probabilities from a previous run to perform additional frequentist simulations on the design.}
\item{fun.graph}{Optional function printing a graph of the region of interest. No region is plotted if this is left blank}
\item{...}{Options passed to the integration function}
}
\details{
Returns an object of S4 class \code{\link{trialDesign_binom_two-class}}. This has plot and print methods. For comparison between designs saved as trialDesign_binom_two objects there is a print function for the S3 class \code{\link{list_trialDesign_binom_two}}.

The following region spaces are included in the package:
tradeoff_square_integrate tradeoff_square_graph
tradeoff_ratio_intercepts tradeoff_linear_graph
tradeoff_ratio_integrate tradeoff_ratio_graph
tradeoff_ellipse_integrate tradeoff_ellipse_graph
}
\value{
Returns an object of class \code{\linkS4class{trialDesign_binom_two}}
}
\seealso{
\code{\link{bayes_binom_two_postprob}}, \code{\link{bayes_binom_two_postlike}}

Integration functions and corresponding graphs:
\code{\link{tradeoff_square_integrate}},\code{\link{tradeoff_ellipse_integrate}},\code{\link{tradeoff_linear_integrate}},\code{\link{tradeoff_ratio_integrate}}
}
\references{
Chen Y, Smith BJ. Adaptive group sequential design for phase II clinical trials: a Bayesian decision theoretic approach. Stat Med 2009; 28: 3347-3362.
}
\examples{
# modelled toxicity probability
t=c(0.1,0.1,0.3,0.3)
# modelled response probability
r=c(0.35,0.2,0.2,0.35)

reviews=c(10,15,20,25,30,35,40)
stage_after_trial=40

# uniform prior
pra=1;prb=1;pta=1;ptb=1

efficacy_critical_value=0.2
futility_critical_value=0.35
toxicity_critical_value=0.1
no_toxicity_critical_value=0.3

# alpha/beta ratio
l_alpha_beta=3
# cost of continuing compared to cost of alpha
l_alpha_c=750

efficacy_region_min=0.2
toxicity_region_max=0.3

########################################
# square region
s=bayes_binom_two_loss(t,r,reviews,pra,prb,pta,ptb,l_alpha_beta,
l_alpha_c,stage_after_trial,fun.integrate=tradeoff_square_integrate,
fun.graph=tradeoff_square_graph,efficacy_critical_value,
toxicity_critical_value,futility_critical_value,
no_toxicity_critical_value,efficacy_region_min=efficacy_region_min,
toxicity_region_max=toxicity_region_max)

plot(s)


########################################
# ellipse region
efficacy_region_min=0.2
efficacy_region_max=0.35
toxicity_region_min=0.1
toxicity_region_max=0.3


s=bayes_binom_two_loss(t,r,reviews,pra,prb,pta,ptb,l_alpha_beta,
l_alpha_c,stage_after_trial,fun.integrate=tradeoff_ellipse_integrate,
fun.graph=tradeoff_ellipse_graph,efficacy_critical_value,
toxicity_critical_value,futility_critical_value,
no_toxicity_critical_value,efficacy_region_min=efficacy_region_min,
toxicity_region_max=toxicity_region_max,
efficacy_region_max=efficacy_region_max,
toxicity_region_min=toxicity_region_min)


plot(s)

}
