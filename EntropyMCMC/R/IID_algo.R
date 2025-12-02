#######################################################
## Functions for a "fake" MCMC, for testing purposes
## multidim algorithm that simply simulates iid draws
## from a target which is N(param$mu,param$v) here
## user-defined "target" density MUST be defined as:
## target(x,param)
## q_proposal_ISnorm() is defined in HMIS algorithm
#######################################################

############### IID Gaussian algorithm ################
# Simulation of the iid sequence
# the dimension d of theta is deduced from theta0
# several parameters are unused here (NULL), 
# maintained for compatibility with parallelMCMC() calls
IID_chain <- function(theta0=NULL,it=100,  target, 
					f_param, q_param=NULL,
					q_pdf=NULL, q_proposal=NULL){
	d <- length(theta0)
	theta <- matrix(0,nrow=it,ncol=d)
#	theta[1,] <- theta0; # unused in this case
	for (i in 1:it)
		theta[i,] <- q_proposal_ISnorm(param=f_param)  # simple iid ~ f
	result <- list(theta=theta, paccept=1)
	result
	}


## simple mcmc_step compliant definition for futur EntropyParallel() usage
IID_step <- function(theta, target, 
				q_pdf=NULL, q_proposal=q_proposal_ISnorm,
				f_param, q_param=NULL, nba=NULL){
	theta_new <- q_proposal(param=f_param)  # simple iid ~ f
	list(theta_new=theta_new, nba=nba)
	}