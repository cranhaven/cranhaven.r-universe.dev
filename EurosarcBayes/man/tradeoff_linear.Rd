\name{tradeoff linear}
\alias{tradeoff_linear_integrate}
\alias{tradeoff_linear_graph}
\title{
Functions for integration for Bayesian loss methodology
}
\description{
An integral and graph for an acceptable region for the bayesian loss function approach (see \code{\link{bayes_binom_two_loss}})
}
\usage{
tradeoff_linear_integrate(ar, br, at, bt, efficacy_region_min, 
	toxicity_region_max, efficacy_region_max, toxicity_region_min)

tradeoff_linear_graph(input)
}
\arguments{
  \item{ar, br}{Parameters for the posterior distribution for response}
  \item{at, bt}{Parameters for the posterior distribution for toxicity}
  \item{efficacy_region_min}{Smallest acceptable efficacy}
  \item{toxicity_region_max}{Largest acceptable toxicity}
  \item{efficacy_region_max}{Point where no more tradeoff occurs for efficacy}
  \item{toxicity_region_min}{Point where no more tradeoff occurs for toxicity}
  \item{input}{A list values needed for the graph. It is expecting max.patients, efficacy_region_min, toxicity_region_max and will error without}
}
\value{
Returns value of the integration.
}
\references{
Chen Y, Smith BJ. Adaptive group sequential design for phase II clinical trials: a Bayesian decision theoretic approach. Stat Med 2009; 28: 3347-3362.
}
\seealso{
\code{\link{bayes_binom_two_loss}}

Integration functions and corresponding graphs:
\code{\link{tradeoff_square_integrate}},\code{\link{tradeoff_ellipse_integrate}},\code{\link{tradeoff_linear_integrate}},\code{\link{tradeoff_ratio_integrate}}
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
# linear region
efficacy_region_min=0.2
efficacy_region_max=0.35
toxicity_region_min=0.1
toxicity_region_max=0.3

s=bayes_binom_two_loss(t,r,reviews,pra,prb,pta,ptb,l_alpha_beta,
	l_alpha_c,stage_after_trial,fun.integrate=tradeoff_linear_integrate,
	fun.graph=tradeoff_linear_graph,efficacy_critical_value,
	toxicity_critical_value,futility_critical_value,
	no_toxicity_critical_value,efficacy_region_min=efficacy_region_min,
	toxicity_region_max=toxicity_region_max,
	efficacy_region_max=efficacy_region_max,
	toxicity_region_min=toxicity_region_min)

plot(s)


}
