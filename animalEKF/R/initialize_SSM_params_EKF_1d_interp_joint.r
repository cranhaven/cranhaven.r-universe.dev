initialize_SSM_params_EKF_1d_interp_joint <- function(env_obj) {

	#EKF functions
	env_obj$f <- function(mk, new_logv, dtprev) {
				  mk <- as.vector(as.numeric(mk))
				 
				  #mk_new <- matrix(c(mk[1]+ undo_trans(x=mk[2])*dtprev, new_logv), ncol=1)
				  mk_new <- rbind(mk[1] + mk[2]*dtprev, new_logv)
				  
				  #matrix(c(mk[1]+ mk[2]*dtprev, new_logv), ncol=1)
				 
				  mk_new
				}

	env_obj$h <- function(mk, dtprev) {
				  mk <- as.vector(as.numeric(mk))
				  #mk[1]+undo_trans(mk[2])*dtprev
				  as.matrix(mk[1]+mk[2]*dtprev, ncol=1)
				}

	#here Hx is just matrix multiplication
	env_obj$Fx <- function(mk, dtprev) {
				  mk <- as.vector(as.numeric(mk))	
				  P <- diag(c(1,1))
				  P[1,2] <- dtprev #undo_trans_df(x=mk[2])*dtprev
				  
				  P
				}

	env_obj$Hx <- function(mk, dtprev) {
					matrix(c(1,dtprev), ncol=2, nrow=1)
					
				}

	 
	#hyperparameter values  
	if (env_obj$nstates==1) {
	  env_obj$sigma_pars <- env_obj$sigma_pars[1:2]	  
	}  


	#variance for error in predicting y locations (next), depends on state
	env_obj$XY_errvar <- list()
	#alpha, beta, k parameters, each index is for one state,

	if (env_obj$show_prints) print(env_obj$alpha0_pars)
	
	# env_obj$mu0_range <- range(env_obj$alpha0$mu0)
	# if (diff(env_obj$mu0_range)==0) { env_obj$mu0_range <- env_obj$mu0_range + c(-1/3, 1/3)*env_obj$mu0_range[ 1 ] } 
	# env_obj$mu0_range <- env_obj$mu0_range + c(-1/3,1/3)*diff(env_obj$mu0_range)
				

	for (k in 1:env_obj$nstates) {
	   env_obj$XY_errvar[[ k ]] <- list(sig=as.matrix(abs(env_obj$Errvar0[[ k ]])), dof=env_obj$Errvar_df[ k ]) 
	}

	#variance for error between steps
	env_obj$XY_errvar <- rep(list(rep(list(env_obj$XY_errvar), env_obj$npart)), env_obj$nsharks)
	#variance for error between particles and yt
	env_obj$Particle_errvar <- rep(list(rep(list(list(sig=as.matrix(abs(env_obj$Particle_errvar0)), dof=env_obj$Particle_err_df)), env_obj$npart)), env_obj$nsharks)
	names(env_obj$XY_errvar) <- names(env_obj$Particle_errvar) <- env_obj$shark_names

	#matrices to hold sums of squares for P and V for each; 

	env_obj$SSquare_XY <- env_obj$XY_errvar_draw <- array(0, dim=c(1, 1, env_obj$nstates, env_obj$npart, env_obj$nsharks), dimnames=list(1, 1, env_obj$state_names, env_obj$pnames, env_obj$shark_names))
	env_obj$SSquare_particle <- array(0, dim=c(1, 1, env_obj$npart, env_obj$nsharks), dimnames=list(1, 1, env_obj$pnames, env_obj$shark_names))
	
	env_obj$errs <- array(0, dim=c(env_obj$npart, env_obj$nsharks), dimnames=list(env_obj$pnames, env_obj$shark_names))


	P00 <- diag(2)
	env_obj$Pk_actual <- array(0, dim=c(2, 2, env_obj$npart, env_obj$nsharks), dimnames=list(c("X","velocity"), c("X","velocity"), env_obj$pnames, env_obj$shark_names))
	env_obj$Pk_prev <- env_obj$Pk <- array(0, dim=c(2, 2, env_obj$nstates, env_obj$npart, env_obj$nsharks), dimnames=list(c("X","velocity"), c("X","velocity"), env_obj$state_names, env_obj$pnames, env_obj$shark_names))
	
	env_obj$mk_actual <- array(0, dim=c(2, env_obj$npart, env_obj$nsharks), dimnames=list(c("X","velocity"), env_obj$pnames, env_obj$shark_names))						  
	env_obj$mk_prev <- array(NA, dim=c(2, env_obj$nstates, env_obj$npart, env_obj$nsharks), dimnames=list(c("X","velocity"), env_obj$state_names, env_obj$pnames, env_obj$shark_names))
					
	#history of mean vectors to use in calculating particle error (especially with delayed resampling)					
	env_obj$mk_actual_history <- array(NA, dim=c(env_obj$N, 2, env_obj$npart, env_obj$nsharks),
									   dimnames= list(env_obj$Nnames, c("X","velocity"), env_obj$pnames, env_obj$shark_names))

	env_obj$param_sampling_weights <- rep(1, env_obj$npart) 

	#this is the actual variance for the first one; only elements 3 and 4 depends on its state

	for (s in env_obj$shark_names) {
		#populate both states and also actual
	#	z0 <- Xpart_history[first_intervals[ s ], "lambda",1,s]
			
		for (p in 1:env_obj$npart) {
			for (k in 1:env_obj$nstates) {	
				env_obj$Pk_prev[1,1,k,p,s] <- MCMCpack::riwish(v=env_obj$Particle_errvar[[ s ]][[ p ]]$dof, S=env_obj$Particle_errvar[[ s ]][[ p ]]$sig)
				env_obj$Pk_prev[2,2,k,p,s] <- MCMCpack::rinvgamma(n=1, env_obj$sigma_pars[ 2*k -1 ], env_obj$sigma_pars[ 2*k ])
				env_obj$mk_prev[2,k,p,s] <- rnorm(n=1, mean=env_obj$alpha0_pars$mu0[ k ], sd=sqrt(env_obj$alpha0_pars$V0[ k ]*env_obj$Pk_prev[2,2,k,p,s]))
			
			}
	#	mk_actual[,p,s] <- mk_prev[,z0,p,s] 
	#	Pk_actual[,,p,s] <- Pk_prev[,, z0,p,s]
		}	
	}
	
	env_obj$Pk_prev <- keep_finite(env_obj$Pk_prev)
	env_obj$mk_prev <- keep_finite(env_obj$mk_prev)


	#Pk_prev_interp <- rep(list(NA), nsharks)
	#names(Pk_prev_interp) <- shark_names
	#initialize values so have correct dimensions
	env_obj$mk <- env_obj$mk_prev
	env_obj$Pk <- env_obj$Pk_prev

	env_obj$Qt <- array(diag(2), dim=c(2, 2, env_obj$nstates, env_obj$npart, env_obj$nsharks), dimnames=list(c("X","velocity"), c("X","velocity"), env_obj$state_names, env_obj$pnames, env_obj$shark_names) )

	#variances of logV

	env_obj$igamma_par_names <- paste(c("a","b"), rep(1:env_obj$nstates, each=2), sep="")

	env_obj$sigma_pars <- array(matrix(env_obj$sigma_pars, ncol=2*env_obj$nstates, nrow=env_obj$npart, byrow=TRUE), 
								dim=c(env_obj$npart, 2*env_obj$nstates, env_obj$nsharks),
								dimnames=list(env_obj$pnames, env_obj$igamma_par_names, env_obj$shark_names))

	env_obj$sigma_draw <- array(NA, dim=c(env_obj$npart, env_obj$nstates, env_obj$nsharks), dimnames=list(env_obj$pnames, env_obj$state_names, env_obj$shark_names))
	 
	#env_obj$sigma_hist <- matrix(NA, nrow=env_obj$N-1, ncol=3*env_obj$nstates)


	#alpha and beta for each state
	#mu_hist <- rep(list(matrix(NA, ncol=3, nrow=N-1)), nstates)
	#mu_hist[[ 1 ]][1,] <- rep( mu[[ 1 ]],each=3)
	#if (nstates>1) { mu_hist[[ 2 ]][1,] <- rep( mu[[ 2 ]],each=3) }

	env_obj$mu <- array(NA, dim=c(env_obj$nstates, 2, env_obj$npart, env_obj$nsharks), dimnames=list(env_obj$state_names, c("mu","V"), env_obj$pnames, env_obj$shark_names))
	env_obj$mu[,"mu",,] <- env_obj$alpha0_pars$mu0[1:env_obj$nstates]
	env_obj$mu[,"V",,] <- env_obj$alpha0_pars$V0[1:env_obj$nstates]
	




	env_obj$logv_angle_mu_draw <- array(NA, dim=c(env_obj$npart, 1, env_obj$nstates, env_obj$nsharks), dimnames=list(env_obj$pnames, "velocity", env_obj$state_names, env_obj$shark_names))
	#logv_angle_draw <- logv_angle_mu_draw


	env_obj$pred_cov <- matrix(NA, ncol=9, nrow=env_obj$N)

	#this is a list of the means and covariances of x_{t+1}|x_t and parameters theta
	#env_obj$sigma_hist_allpart <- rep(list(list(rep(0, env_obj$nstates))), env_obj$npart)

	#reject_samp_hist <- matrix(NA, ncol=2, nrow=N-1)
	#colnames(reject_samp_hist) <- c("mean","median")


	par(mfrow=c(2,2))
	#step_skipped <- FALSE

	env_obj$j_list <- list()
	for (s in env_obj$shark_names) {
		env_obj$j_list[[ s ]] <- list()
	}
	names(env_obj$j_list) <- env_obj$shark_names

	env_obj$MuY <- env_obj$SigY <- env_obj$Kgain <- env_obj$densities_components <- rep(list(NA), env_obj$nsharks)
	names(env_obj$MuY) <- names(env_obj$SigY) <- names(env_obj$Kgain) <- names(env_obj$densities_components) <- env_obj$shark_names

	invisible(NULL)

}