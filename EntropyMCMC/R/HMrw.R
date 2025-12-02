#######################################################
## Functions for Random Walk Hasting-Metropolis  (RWHM)
## multidim algorithm with gaussian proposal
## user-defined "target" density MUST be defined as:
## user_target(x,param)
## where x = d-dim point at which the pdf is evaluated
##       param is a list of ALL the needed parameters 
## the actual name of the target is passed in the 
## target parameter of RWHM_algo() 
##
## caution: for RWHM the accept ratio uses the option
## symmetric=TRUE, 
## whereas for other HM (such as, eg, the independence sampler),
## symmetric=FALSE *must* be used
##
#######################################################

## gaussian proposal density q(y|x) for the random walk HM
# here q(y|x) = mvgaussian N(x,v) pdf at point y 
# NB import from mixtools
gaussian_pdf <- function(y,x,param){
		mixtools::dmvnorm(y, mu=x, sigma=param$v)}
		
## single random draw from gaussian proposal: y ~ q(.|x)
# look for n draws later (vectorize?)
gaussian_proposal <- function(x,param){
  mixtools::rmvnorm(1, mu=x, sigma=param$v)
	}

# acceptance ratio alpha(x,y) x=current, y=next (proposal)
# proposal_pdf name passed (new version! to check) 
# MUST BE proposal_pdf(y,x,q_param) even if x is unused (IS)
# this version check and avoid div/0
# may still be a problem if both numerator and divisor are small
accept_ratio <- function(x, y, target, q_pdf,
						f_param, q_param, symmetric=FALSE){
	a1 <- target(y,f_param) # numerator
	a2 <- target(x,f_param) # denominator
	# if (a2 == 0 && a1 == 0) cat("0/0 occurs\n")
	if (symmetric) {
		if (a1 > a2) alpha <- 1
			else if (a1 == 0) alpha <- 0 # even in 0/0 case
				else alpha <- a1/a2      # 0 < a1 <= a2 case
		}
	else {  # general (non symmetric) case
		b1 <- a1*q_pdf(x,y,q_param)
		b2 <- a2*q_pdf(y,x,q_param)
		if (b1 > b2) alpha <- 1
			else if (b1 == 0) alpha <- 0 # even in 0/0 case
				else alpha <- b1/b2
		}
alpha
}


############## Random Walk HM iteration ###############
# accept_ratio uses symmetric=TRUE for (gaussian) Random Walk HM
# theta = current value; theta_new = next value
# nba (# accept) is incremented by one if acceptation holds
RWHM_step <- function(theta, target, 
					q_pdf=gaussian_pdf, q_proposal=gaussian_proposal,
					f_param, q_param, nba){
	theta_new <- q_proposal(theta, q_param)
	a = accept_ratio(theta, theta_new, target, q_pdf,
					f_param, q_param, symmetric=TRUE)
	u = runif(1)
	if (u < a) {
			nba <- nba + 1
			}
		else theta_new <- theta
	list(theta_new=theta_new, nba=nba)
}


########## def of mcmc_algo for RW HM algorithm ##########
# Simulation of a SINGLE RW HM Markov Chain
# the dimension d of theta is deduced from theta0
# target(x,param) = user-defined function
# the proposal pdf q(|.) used by default is gaussian
# return :
# theta = the simulated MC 
# paccept = the empiricalacceptance proba
RWHM_chain <- function(theta0,it=100, target, 
				f_param, q_param,
				q_pdf=gaussian_pdf, q_proposal=gaussian_proposal){
		d <- length(theta0)
	theta <- matrix(0,nrow=it,ncol=d)
	theta[1,] <- theta0; 
	r <- list(theta_new=theta0,nba=0)
	for (i in 1:(it-1)){
		r <- RWHM_step(theta[i,], target, q_pdf, q_proposal,
					f_param, q_param, r$nba)
		theta[i+1,] <- r$theta_new
	}
	paccept <- r$nba/it
	result <- list(theta=theta, paccept=paccept, algo="RWHM")
	result
	}
