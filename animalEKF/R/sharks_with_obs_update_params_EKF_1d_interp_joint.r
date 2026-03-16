sharks_with_obs_update_params_EKF_1d_interp_joint <- function(env_obj) {


	#sum covariances of errors across y
	error_xt_to_yt <- matrix(0, ncol=length(env_obj$sharks_with_obs), nrow=env_obj$npart, dimnames=list(1:env_obj$npart, env_obj$sharks_with_obs))
	
	for (s in env_obj$sharks_with_obs) {
	
		#sum the errors from observed to the simulated interpolations there, only for the state simulated to have occurred there.
		#and only for the step where observations actually occurred, since need to compute distance from observations
		
						
	
		#if include first step (never used to update), then include that.  otherwise include
		#and include current step
		steps_tmp <- c(env_obj$steps_to_resamp[[ s ]], env_obj$i)
		steps_tmp_diff <- env_obj$steps_to_resamp[[ s ]]
		if (! (steps_tmp[ 1 ] %in% env_obj$first_intervals[[ s ]])) { steps_tmp <- steps_tmp[-1 ] } 	
		
		if (env_obj$update_eachstep | env_obj$update_params_for_obs_only) { 
			#take this for updating particle errs
			#this is the previous step
			steps_tmp_diff <- env_obj$i - 1
			steps_tmp <- env_obj$i 
		}	
		
		#number of observations
		dof_increase_xy <- sum(env_obj$j_list[[ s ]][[ env_obj$i ]])
		#number of steps
		dof_increase_part <- length(steps_tmp_diff)
	
	
		for (p in 1:env_obj$npart) {
		
			error_xt_to_yt <- keep_finite(env_obj$ynext[ rownames(env_obj$ynext)==s,"X"] - env_obj$MuY[[ s ]][, env_obj$lambda_matrix[p,env_obj$i,s], p])
			env_obj$error_beforesamp_allpart[env_obj$i,p,s] <- keep_finite(sum(abs(error_xt_to_yt)))#sqrt(sum(error_xt_to_yt^2))
			
	
			#newV <- matrix(apply(Xpart_history[steps_to_resamp[[ s ]], c("logv","turn_log","lambda"),p,s, drop=FALSE], MARGIN=2, FUN=rbind), ncol=3)

			newV <- matrix(env_obj$Xpart_history[steps_tmp, c("velocity","lambda"), p, s], ncol=2)
								
			colnames(newV) <- c("velocity", "lambda")
			
			#newV <- na.omit(newV)
			newV_num <- table(factor(newV[,"lambda", drop=FALSE], levels=1:env_obj$nstates))
			#print(cbind(newV, turn_rad=log2rad(newV[,"turn_log"])))
			
			for (z in which(newV_num >0)) {
			
				nn <- newV_num[ z ]
				zmembs <- newV[,"lambda"]==z
				newV_bar <- mean(newV[ zmembs, "velocity"])
				#if no turn (if first observation, then dont update parameters just for that)
											
				mu0 <- env_obj$mu[z, "mu", p , s]
				V0 <- env_obj$mu[z, "V", p , s]		
				
				env_obj$mu[z, "V", p , s] <- 1/(nn + 1/V0)
				
				
				# #if V0 is smaller then mu0 is weighted more.
				env_obj$mu[z, "mu", p , s] <- ((1/V0)*mu0 + newV_bar*nn) * env_obj$mu[z, "V", p , s]
				
				
				# #update only the parameters for that state, multiply z by 2
				env_obj$sigma_pars[ p, 2*z, s]  <- pmin(env_obj$sigma_pars[ p, 2*z, s] + 0.5*((mu0^2)/V0 + sum(newV[zmembs, "velocity" ]^2) - (env_obj$mu[z, "mu", p , s]^2) / env_obj$mu[z, "V", p , s]), 3000)
				
				# mu_hist_allpart[[ p ]][[ step_seq[ jj ] ]] <- mu[[ p ]]
			
					
			}
	
			#only take the error that is for that state
			z <- env_obj$lambda_matrix[p,env_obj$i,s]
			
			env_obj$SSquare_XY[,,z,p,s] <- keep_finite(env_obj$SSquare_XY[,,z,p,s] + keep_finite(sum(keep_finite(env_obj$error_xt_to_yt^2))))
			
			#update the covariance matrix for errors predicting y from x 
			#these updates NOT defined recursively  
		
			env_obj$XY_errvar[[ s ]][[ p ]][[ z ]]$sig <- as.matrix(keep_finite(env_obj$Errvar0[[ z ]] + env_obj$SSquare_XY[,,z,p,s]))# as.matrix(Matrix::nearPD(as.matrix(Errvar0[[ z ]] + SSquare_Err[,,z,p,s]))$mat)
			env_obj$XY_errvar[[ s ]][[ p ]][[ z ]]$dof <- env_obj$XY_errvar[[ s ]][[ p ]][[ z ]]$dof + 1 #dt[ i ]/cov_adjustment[ i ]#1
			
			#regardless of state
			
			#error_xtprev_to_xt <- keep_finite(apply(env_obj$Xpart_history[ steps_tmp_diff, c("X","velocity"),p,s, drop=FALSE], 1, function(x) env_obj$h(mk=x, dtprev=env_obj$reg_dt)) - env_obj$Xpart_history[ steps_tmp_diff + 1, "X",p,s])
			error_xtprev_to_xt <- keep_finite(env_obj$mk_actual_history[ steps_tmp, "X",p,s] - env_obj$Xpart_history[ steps_tmp, "X",p,s]) 

			
			env_obj$SSquare_particle[,,p,s] <- keep_finite(env_obj$SSquare_particle[,,p,s] + keep_finite(sum(keep_finite(error_xtprev_to_xt^2))))    #Dp2[p,]%*%t(Dp2[p,])
			#smat <- as.matrix(Particle_errvar0 + SSquare_XY[,,p,s])
			
			#if (any(abs(smat) ==Inf) | any(is.na(s))) { print(smat) ; print(error_xtprev_to_xt[p, ,s]) }
			env_obj$Particle_errvar[[ s ]][[ p ]]$sig <- as.matrix(keep_finite(env_obj$Particle_errvar0 + env_obj$SSquare_particle[,,p,s]))
			env_obj$Particle_errvar[[ s ]][[ p ]]$dof <- env_obj$Particle_errvar[[ s ]][[ p ]]$dof + dof_increase_part #dt[ i ]/cov_adjustment[ i ]#1
		
			#draw next location
			
			env_obj$Xpart_history[env_obj$i + 1,"X",p,s] <- rnorm(n=1, mean=env_obj$h(mk=env_obj$Xpart_history[env_obj$i, c("X","velocity"), p, s], dtprev=env_obj$reg_dt), 
																  sd=sqrt(MCMCpack::riwish(v=env_obj$Particle_errvar[[ s ]][[ p ]]$dof, S=env_obj$Particle_errvar[[ s ]][[ p ]]$sig)))
			
		
		}#loop over particles
		
		env_obj$Xpart_history[env_obj$i + 1,"X",,s] <- keep_finite(env_obj$Xpart_history[env_obj$i + 1,"X",,s])
		
		#env_obj$mk_actual[,,s] <- env_obj$Xpart_history[env_obj$i, c("X","velocity"),,s]
		
		env_obj$error_beforesamp_quantiles[env_obj$i,,s] <- quantile(env_obj$error_beforesamp_allpart[env_obj$i,,s], p=c(.1, .5, .9))

		#update inverse gamma parameters
		
		state_by_seq <- t(apply(env_obj$lambda_matrix[,steps_tmp,s, drop=FALSE],1, function(x) table(factor(x, levels=1:env_obj$nstates))))
		
		env_obj$sigma_pars[, 2*(1:env_obj$nstates) -1,s] <- env_obj$sigma_pars[,2*(1:env_obj$nstates) -1, s] + 0.5*state_by_seq
		#do this even though for first observation don't have turn angle 
		#tau_pars[,seq(1, 2*nstates -1, by=2),s] <- tau_pars[,seq(1, 2*nstates -1, by=2),s] + 0.5*state_by_seq
		
		
		
		if (env_obj$nstates > 1) { env_obj$trans_mean[,env_obj$i,s] <- apply(sapply( lapply(env_obj$transition_mat[[ s ]], "[[", i=1),"[", i=3:2), 1, mean) }
						

	}#end updates by shark
	
	invisible(NULL)

}