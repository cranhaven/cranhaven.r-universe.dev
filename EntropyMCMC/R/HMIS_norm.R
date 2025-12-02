##########################################################
## Functions for Hasting-Metropolis Independence Sampler
## multidim algorithm with gaussian proposal
## user-defined "target" density MUST be defined as:
## user_target(x,param)
## where x = d-dim point at which the pdf is evaluated
##       param = a list of ALL the needed parameters 
## the actual name of the target is passed in the 
## target parameter of HM_algo() 
## accept_ratio and other generic functions are defined in 
# HMRW -> move in a generic file later?
##########################################################

## proposal density q(y|x) for the IS HM
# here q(y|x) = q(y) = mvgaussian N(param$mu,param$v) pdf at y 
# x unused, but needs to be specified as a generic 
# proposal_pdf(y,x,q_param)
# NB: imports def from mixtools
q_pdf_ISnorm <- function(y,x,param){
		mixtools::dmvnorm(y, mu=param$mean, sigma=param$v)}
		
## single random draw from proposal: y ~ q(.|x)
## for IS q(.|x) = q(.) = N(param$mean,param$v)
# look for n draws later (vectorize?)
q_proposal_ISnorm <- function(x,param){
  mixtools::rmvnorm(1, mu=param$mean, sigma=param$v)
	}

############## HM "generic" iteration ###############
# accept_ratio uses symmetric=FALSE (the default)
# theta = current value; theta_new = next value
# nba (# accept) is incremented by one if acceptation holds
HM_step <- function(theta, target, q_pdf, q_proposal,
					f_param, q_param, nba){
theta_new <- q_proposal(theta, q_param)
a = accept_ratio(theta, theta_new, target, q_pdf,
				f_param, q_param)
u = runif(1)
if (u < a) {
		nba <- nba + 1
		}
	else theta_new <- theta
list(theta_new=theta_new, nba=nba)
}

## def of mcmc_algo for HM Independence Sampler algorithm ##
# Simulation of the RW IS Markov Chain
# the dimension d of theta is deduced from theta0
# theta = the simulated MC 
# paccept = the empiricalacceptance proba
HMIS_norm_chain = function(theta0,it=100, target, 
				f_param, q_param,
				q_pdf=q_pdf_ISnorm, q_proposal=q_proposal_ISnorm){
	d <- length(theta0)
	theta <- matrix(0,nrow=it,ncol=d)
	theta[1,] <- theta0; 
	r <- list(theta_new=theta0,nba=0)
	for (i in 1:(it-1)){		
		r <- HM_step(theta[i,], target, q_pdf, q_proposal,
					f_param, q_param, r$nba)
		theta[i+1,] <- r$theta_new
	}
	paccept <- r$nba/it
	result <- list(theta=theta, paccept=paccept, algo="HMISnorm")
	result
	}
