#######################################################
## Specific functions for Adaptive-Metropolis (AM) 
## as in Haario (2001)
## multidim algorithm with gaussian proposal
## user-defined "target" density MUST be defined as:
## user_target(x,param)
## the adaptive cov matrix is passed in param$v
## the other functions :
##  q_pdf(y,x,param)
##  q_proposal(x,param)
##  acceptance ratio alpha(x,y)
##  HM_step(theta, target, proposal_pdf, f_param, q_param, nba)
## are generic and defined in RWHM.R

# TODO: HM_step has been redefined like
# HM_step <- function(theta, target, q_pdf, q_proposal,
#					f_param, q_param, nba)
# probably have to write a specific HM_step handling adaptive cov
# at each step, for the EntropyParallel usage  
#######################################################


########## Adaptive HM RW single chain algorithm ##########
# the dimension d of theta is deduced from theta0
# target(x,param) = user-defined function
# q_param must contains in this case:
# 	$v = initial cov matrix
#	$t0 = end of initial stage with cov v
#	$epsi = epsilon parameter (for the nondegenerate matrix part)
# return the 3 mandatory objects :
# 	theta = the simulated MC 
# 	paccept = the empiricalacceptance proba
# 	algo = string name of the algorithm
# plus 
# 	finalcov = the final cov matrix
AMHaario_chain <- function(theta0, it=100, target, f_param, q_param,
					q_pdf=gaussian_pdf, q_proposal=gaussian_proposal){
	d <- length(theta0)
	t0 <- q_param$t0 # burnin duration with C0 cov matrix
	if (t0 > it) t0 <- round(it/2)
	s_d <- 2.4^2/d    # scaling factor
	epsi <- s_d*q_param$epsi
	theta <- matrix(0,nrow=it,ncol=d)
	theta[1,] <- theta0; 
	r <- list(theta_new=theta0,nba=0)
	
	# first t0 iterations with initial cov matrix
	q0_param <- list(v=q_param$v) # comply to the q_param list def
	for (i in 1:(t0-1)){
		r <- RWHM_step(theta[i,], target, q_pdf, q_proposal,
						f_param, q0_param, r$nba)
		theta[i+1,] <- r$theta_new
		}
	
	# adaptive stage 
	C <- t(theta[1:t0,]) %*% theta[1:t0,]  
	# (d,d) matrix sum of Cross-products
	St <- colSums(theta[1:t0,]) # sum over t0 first iterations 
	for (i in t0:(it-1)){
		# build (sequential) cov matrix from the past
		Vseq <- C/i - (St/i) %*% t(St/i)		
		# for check & debugging
		# vcov <- cov(theta[1:i,])*((i-1)/i) # R covariance div by n
		# cat("i=",i,"\n"); 
		# print(Vseq-vcov)		
		q1_param <- list(v=(s_d*Vseq + epsi*diag(d)))
		r <- RWHM_step(theta[i,], target, q_pdf, q_proposal,
					f_param, q1_param, r$nba)
		theta[i+1,] <- r$theta_new
		# update cross-products and sums
		C <- C + theta[i+1,] %*% t(theta[i+1,])
		St <- St + theta[i+1,]
	}
	paccept <- r$nba/it
	result <- list(theta=theta, paccept=paccept, 
					finalcov = q1_param$v,
					algo="Haario AM")
	result
	}
