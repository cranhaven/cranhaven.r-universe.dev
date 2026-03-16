initialize_SSM_params_EKF_interp_joint <- function(env_obj) {

		
	env_obj$f <- function(mk, new_logv, theta, dtprev) {
						# mk = X,Y, logv, bearing
						mk <- as.vector(as.numeric(mk))
					
						es <- exp_safe(mk[3]) * dtprev
						mk_new <- cbind(es, es, new_logv, normalize_angle(mk[4]+theta))
						mk_new[,1] <- mk[1] + mk_new[,1] * cos(mk[4]) 
						mk_new[,2] <- mk[2] + mk_new[,2] * sin(mk[4])
						
						#mk_new[1:2,] <- mk[1:2] + mk_new[1:2,]*c(cos(mk[4]), sin(mk[4]))  					 
						 
						mk_new
						
					}

	env_obj$h <- function(mk, dtprev) {
						mk <- as.vector(as.numeric(mk))
						mk[4] <- normalize_angle(mk[4])
						es <- exp_safe(mk[3]) * dtprev
						
						mk_new <- cbind(mk[1] + es * cos(mk[4]), 
										mk[2] + es * sin(mk[4]))
						
						mk_new
						}

	#here Hx is just matrix multiplication
	env_obj$Fx <- function(mk, dtprev) {
						P <- diag(c(1,1,0,1))
						mk <- as.vector(as.numeric(mk))
						  
						 
						P[1:2,3:4] <- exp_safe(mk[3])*dtprev
						P[cbind(1:2,3:4)] <- P[cbind(1:2,3:4)]*cos(mk[4])
						P[cbind(1:2,4:3)] <- c(-1,1)*P[cbind(1:2,4:3)]*sin(mk[4])
						P
					}

	env_obj$Hx <- function(mk, dtprev) {
						  #P <- matrix(0, ncol=4, nrow=2)
						  P <- cbind(diag(2),diag(2))
						  mk <- as.vector(as.numeric(mk))
						  #P[,3] <- undo_trans_df(x=mk[3])*dtprev
						  #P[,4] <- undo_trans(x=mk[3])*dtprev
						  P[,3:4] <- exp_safe(mk[3])*dtprev
						  
						  P[cbind(1:2, 3:4)] <- P[cbind(1:2, 3:4)]*cos(mk[4])
						  P[cbind(1:2, 4:3)] <- c(-1,1)*P[cbind(1:2, 4:3)]*sin(mk[4])
						  #P[1,3] <- P[2,4] <- exp(mk[3])*dtprev*cos(mk[4])
						  #P[1,4] <- P[2,3] <- exp(mk[3])*dtprev*sin(mk[4])
						  #P[1,4] <- -1*P[1,4]
						  P
						}
						  
						 
	#function is the derivative of log2rad to approximate variance of turn angle


	  
	if (env_obj$nstates==1) {
		env_obj$sigma_pars <- env_obj$sigma_pars[1:2]
		env_obj$tau_pars <- env_obj$tau_pars[1:2]
		
	}  



	#variance for error in predicting y locations (next), depends on state
	env_obj$XY_errvar <- list()
	#alpha, beta, k parameters, each index is for one state,

	env_obj$mu <- array(NA, dim=c(env_obj$nstates, 2, 2, env_obj$npart, env_obj$nsharks), dimnames=list(env_obj$state_names, c("alpha","beta"),c("mu","V"), env_obj$pnames, env_obj$shark_names))
	env_obj$mu[,"alpha","mu",,] <- env_obj$mu0_pars$alpha
	env_obj$mu[,"beta","mu",,] <- env_obj$mu0_pars$beta
	
	env_obj$mu[,"alpha","V",,] <- env_obj$V0_pars$alpha
	env_obj$mu[,"beta","V",,] <- env_obj$V0_pars$beta	
			

	for (k in 1:env_obj$nstates) {
	 
		env_obj$XY_errvar[[ k ]] <- list(sig=as.matrix(Matrix::nearPD(env_obj$Errvar0[[ k ]], ensureSymmetry=TRUE)$mat), dof=env_obj$Errvar_df[ k ])
	 
	}


	#variance for error between steps
	env_obj$XY_errvar <- rep(list(rep(list(env_obj$XY_errvar), env_obj$npart)), env_obj$nsharks)
	#variance for error between particles and yt
	env_obj$Particle_errvar <- rep(list(rep(list(list(sig=as.matrix(Matrix::nearPD(env_obj$Particle_errvar0, ensureSymmetry=TRUE)$mat), dof=env_obj$Particle_err_df)), env_obj$npart)), env_obj$nsharks)
	names(env_obj$XY_errvar) <- names(env_obj$Particle_errvar) <- env_obj$shark_names


	#matrices to hold sums of squares for P and V for each; 

	#SSquare_XY is the error from x to y depending on state
	env_obj$SSquare_XY <- env_obj$XY_errvar_draw <- array(0, dim=c(2, 2, env_obj$nstates, env_obj$npart, env_obj$nsharks), dimnames=list(c("X","Y"), c("X","Y"), env_obj$state_names, env_obj$pnames, env_obj$shark_names))

	#particle error does not
	env_obj$SSquare_particle <- array(0, dim=c(2, 2, env_obj$npart, env_obj$nsharks), dimnames=list(c("X","Y"), c("X","Y"), env_obj$pnames, env_obj$shark_names))

	env_obj$errs <- array(0, dim=c(3, env_obj$npart, env_obj$nsharks), dimnames=list(c("var1","cov","var2"), env_obj$pnames, env_obj$shark_names))




	#the variance of logVt and bearing depend on current state
	#variance of positions depends on previous state

	P00 <- diag(4)
	env_obj$Pk_actual <- array(0, dim=c(4,4,env_obj$npart, env_obj$nsharks), dimnames=list(c("X","Y","logv","bearing_rad"), c("X","Y","logv","bearing_rad"), env_obj$pnames, env_obj$shark_names))
	env_obj$Pk_prev <- Pk <- array(0, dim=c(4,4, env_obj$nstates, env_obj$npart, env_obj$nsharks), dimnames=list(c("X","Y","logv","bearing_rad"), c("X","Y","logv","bearing_rad"), env_obj$state_names, env_obj$pnames, env_obj$shark_names))
	
	env_obj$mk_actual <- array(0, dim=c(4,env_obj$npart, env_obj$nsharks), dimnames=list(c("X","Y","logv","bearing_rad"),env_obj$pnames, env_obj$shark_names))
	env_obj$mk_prev <- array(NA, dim=c(4, env_obj$nstates, env_obj$npart, env_obj$nsharks), dimnames=list(c("X","Y","logv","bearing_rad"), env_obj$state_names, env_obj$pnames, env_obj$shark_names))
						
	#history of mean vectors to use in calculating particle error (especially with delayed resampling)					
	env_obj$mk_actual_history <- array(NA, dim=c(env_obj$N, 4, env_obj$npart, env_obj$nsharks),
									   dimnames= list(env_obj$Nnames, c("X","Y","logv","bearing_rad"), env_obj$pnames, env_obj$shark_names))

						
	env_obj$Qt <- array(diag(4), dim=c(4,4,env_obj$nstates,env_obj$npart,env_obj$nsharks), dimnames=list(c("X","Y","logv","bearing_rad"),c("X","Y","logv","bearing_rad"), env_obj$state_names, env_obj$pnames, env_obj$shark_names) )


	#this is the actual variance for the first one; only elements 3 and 4 depends on its state

	for (s in env_obj$shark_names) {
		#populate both states and also actual
	#	z0 <- Xpart_history[first_intervals[ s ], "lambda",1,s]
			
		
		for (k in 1:env_obj$nstates) {	
			for (p in 1:env_obj$npart) {	
			
				env_obj$Pk_prev[1:2,1:2,k,p,s] <- MCMCpack::riwish(v=env_obj$Particle_errvar[[ s ]][[ p ]]$dof, S=env_obj$Particle_errvar[[ s ]][[ p ]]$sig)
				env_obj$Pk_prev[,,k,p,s][3:4, 3:4] <- diag(c(MCMCpack::rinvgamma(n=1, env_obj$sigma_pars[ 2*k -1 ], env_obj$sigma_pars[ 2*k ]), MCMCpack::rinvgamma(n=1, env_obj$tau_pars[ 2*k -1 ], env_obj$tau_pars[ 2*k ])))
				env_obj$mk_prev[3,k,p,s] <- rnorm(n=1, mean=env_obj$mu[k,"alpha", "mu",p,s], sd=sqrt(env_obj$mu[k,"alpha", "V",p,s] * env_obj$Pk_prev[3,3,k,p,s]))
				
			
			}
			
		}
		
	}
	
	env_obj$Pk_prev <- keep_finite(env_obj$Pk_prev)
	env_obj$mk_prev <- keep_finite(env_obj$mk_prev)

	#initialize values so have correct dimensions
	env_obj$mk <- env_obj$mk_prev
	env_obj$Pk <- env_obj$Pk_prev

	#variances of logV

	env_obj$igamma_par_names <- paste(c("a","b"), rep(1:env_obj$nstates, each=2), sep="")

	env_obj$sigma_pars <- array(matrix(env_obj$sigma_pars, ncol=2*env_obj$nstates, nrow=env_obj$npart, byrow=TRUE), dim=c(env_obj$npart, 2*env_obj$nstates, env_obj$nsharks))
	env_obj$tau_pars <- array(matrix(env_obj$tau_pars, ncol=2*env_obj$nstates, nrow=env_obj$npart, byrow=TRUE), dim=c(env_obj$npart, 2*env_obj$nstates, env_obj$nsharks))
	dimnames(env_obj$sigma_pars) <- dimnames(env_obj$tau_pars) <- list(env_obj$pnames, env_obj$igamma_par_names, env_obj$shark_names)

	env_obj$sigma_draw <- env_obj$tau_draw <- array(NA, dim=c(env_obj$npart, env_obj$nstates, env_obj$nsharks), dimnames=list(env_obj$pnames, env_obj$state_names, env_obj$shark_names))
	env_obj$wn_seq <- ((-5):5)*2*pi

	 
	env_obj$sigma_hist <- matrix(NA, nrow=env_obj$N-1, ncol=3*env_obj$nstates)



	env_obj$tau_hist <- matrix(NA, nrow=env_obj$N-1, ncol=3*env_obj$nstates)


	#alpha and beta for each state
	#mu_hist <- rep(list(matrix(NA, ncol=6, nrow=N-1)), env_obj$nstates)
	#mu_hist[[ 1 ]][1,] <- rep( mu[[ 1 ]],each=3)
	#if (env_obj$nstates>1) { mu_hist[[ 2 ]][1,] <- rep( mu[[ 2 ]],each=3) }

	#mu <- rep(list(rep(list(mu), env_obj$npart)), env_obj$nsharks)
	#names(mu) <- env_obj$shark_names



	# mu_hist_allpart <- mu
	# #make multidimensional list
	# for (p in 1:env_obj$npart) {
	  # mu_hist_allpart[[ p ]] <- list(  mu_hist_allpart[[ p ]]  )
	# }

	env_obj$logv_angle_mu_draw <- array(NA, dim=c(env_obj$npart, 2, env_obj$nstates, env_obj$nsharks), dimnames=list(env_obj$pnames, c("logv","turn"), env_obj$state_names, env_obj$shark_names))


	#list to hold base matrices of Wishart distribution of the inverse covariance matrices
	#degrees of freedom for Wishart distribution, basically number of obs. we are basing the 

	#median_position <- matrix(NA, ncol=2, nrow=N-1)
	#colnames(median_position) <- c("X","Y")
	#median_position[1,] <- apply(sapply(Xpart_history, function(x) x[1, c("X","Y")]),1, median)


	#history of covariance matrices
	#mh <- matrix(NA, ncol=3, nrow=N)
	#colnames(mh) <- c("var1","cov","var2")
	#median_matrices <- list(position=mh, velocity=mh)

	env_obj$pred_cov <- matrix(NA, ncol=9, nrow=env_obj$N)

	#logv_hist <- matrix(NA, ncol=5, nrow=N-1)
	#turn_hist <- matrix(NA, ncol=3, nrow=N-1)
	#bear_hist <- matrix(NA, ncol=3, nrow=N-1)
	#xdiff_hist <- matrix(NA, ncol=3, nrow=N-1)
	#ydiff_hist <- matrix(NA, ncol=3, nrow=N-1)



	#lists for calculating the Bayes Factor
	#basically keep history of states and their matrices

	#this is a list of the means and covariances of x_{t+1}|x_t and parameters theta
	env_obj$sigma_hist_allpart <- rep(list(list(rep(0, env_obj$nstates))), env_obj$npart)
	env_obj$tau_hist_allpart <-   rep(list(list(rep(0, env_obj$nstates))), env_obj$npart)

	#how many times need to reject sample

	env_obj$reject_samp_hist <- array(NA, dim=c(env_obj$N, 2, env_obj$nsharks), dimnames=list(env_obj$Nnames, c("mean","median"), env_obj$shark_names))
	env_obj$num_reject <- c()


	par(mfrow=c(2,3))
	env_obj$step_skipped <- FALSE

	env_obj$j_list <- list()
	for (s in env_obj$shark_names) {
		env_obj$j_list[[ s ]] <- list()
	}
	names(env_obj$j_list) <- env_obj$shark_names

	env_obj$MuY <- env_obj$SigY <- env_obj$Kgain <- env_obj$densities_components <- rep(list(NA), env_obj$nsharks)
	names(env_obj$MuY) <- names(env_obj$SigY) <- names(env_obj$Kgain) <- names(env_obj$densities_components) <- env_obj$shark_names

	invisible(NULL)
}