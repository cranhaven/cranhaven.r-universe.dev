sharks_with_obs_update_params_EKF_interp_joint <- function(env_obj) {

	#new position differences 
	
	
	
	for (s in env_obj$sharks_with_obs) {
							
	
		env_obj$pred_xt_loc[,,s] <- keep_finite(apply(t(env_obj$Xpart_history[env_obj$i, c("X","Y","log_speed","bearing_rad"),,s]), 1, function(x) env_obj$h(mk=x, dtprev=env_obj$reg_dt)))
			
					
				
		#if include first step (never used to update), then include that.  otherwise include
		#and include current step
		steps_tmp <- c(env_obj$steps_to_resamp[[ s ]], env_obj$i)
		steps_tmp_diff <- env_obj$steps_to_resamp[[ s ]]
		
		
		#if only update last step (others were already updated before)
		if (env_obj$update_eachstep | env_obj$update_params_for_obs_only) { 
			#take this for updating particle errs
			steps_tmp_diff <- steps_tmp_diff[ length(steps_tmp_diff) ]
			
			#if update eachstep, then next time the particle error will be updated
			
			steps_tmp <- env_obj$i 
		}	
		
		
		#dof_increase_xy <- length(j_list[[ s ]][[ i ]])
		
		#dof should depend on the gaps
		dof_increase_xy <- sum(env_obj$j_list[[ s ]][[ env_obj$i ]]) #max(env_obj$j_list[[ s ]][[ env_obj$i ]]) #
		dof_increase_part <- length(steps_tmp_diff)

						
		for (p in 1:env_obj$npart) {

			#matrix of errors not summed yet
			#error_xt_to_yt <-  t(apply(ynext[ rownames(ynext)==s, c("X","Y"),drop=FALSE], 1, function(x) x-env_obj$Xpart_history[i,c("X","Y"),p,s]))
			
			#newV <- matrix(apply(env_obj$Xpart_history[env_obj$steps_to_resamp[[ s ]], c("logv","turn_log","lambda"),p,s, drop=FALSE], MARGIN=2, FUN=rbind), ncol=3)

			newV <- matrix(env_obj$Xpart_history[steps_tmp, c("log_speed","turn_rad","lambda"),p,s], ncol=3)
			
			
			colnames(newV) <- c("log_speed","turn_rad","lambda")
			#newV <- na.omit(newV)
			newV_num <- table(factor(newV[,"lambda", drop=FALSE], levels=1:env_obj$nstates))
			#print(cbind(newV, turn_rad=log2rad(newV[,"turn_log"])))
			
			for (z in which(newV_num > 0)) {
			
				nn <- newV_num[ z ]
				zmembs <- newV[,"lambda", drop=FALSE] == z
				newV_bar <- apply(newV[ zmembs, c("log_speed","turn_rad"), drop=FALSE], 2, function(x) mean(x, na.rm=TRUE)) 
				#if no turn (if first observation, then dont update parameters just for that)
			
				not_na <- which(! is.na(newV_bar))
									
				mu0 <- env_obj$mu[z,,"mu",p,s]
				V0 <- env_obj$mu[z,,"V",p,s]
				 
				  
				 
				#mu[[ s ]][[ p ]]$V[[ z ]][ not_na ] <- 1/(nn + 1/V0[ not_na ])
				
				env_obj$mu[z,not_na,"V",p,s] <- 1/(nn + 1/V0[ not_na ])
				env_obj$mu[z,"alpha","mu",p,s] <- ((1/V0[ 1 ])*mu0[ 1 ] + newV_bar[ 1 ]*nn) * env_obj$mu[z,"alpha","V",p,s]
				
				#update turn parameters
				if (2 %in% not_na) {
				
					#print("obs_turn")
					obs_turn <- newV[zmembs,"turn_rad",drop=FALSE]
					#print(obs_turn)
					obs_turn <- obs_turn[ ! is.na(obs_turn) ]
					#print(obs_turn)
					theta_vals <- sapply(obs_turn, function(ff) ff + env_obj$wn_seq) #matrix with npart columns
					#in case there are gaps?
					#print("theta")
					#print(theta_vals)
					
					#theta_vals <- t(apply(newV[zmembs,"turn_rad",drop=FALSE], 1, function(x) x + env_obj$wn_seq))
					#theta_vals <- theta_vals[ apply(theta_vals, 1, function(x) all(! is.na(x))),,drop=FALSE]
					#print("wts")
					beta_weights <- keep_finite(dnorm(theta_vals, mean=env_obj$logv_angle_mu_draw[p,"turn",z,s], sd=sqrt(env_obj$tau_draw[p,z,s])))
					#print(beta_weights)
					beta_weights <- apply(beta_weights, 2, function(x) x/sum(x))
					#print(beta_weights)
								
													
					wtd_x <- colSums(keep_finite(theta_vals * beta_weights))
					wtd_x2 <- keep_finite(colSums(keep_finite(keep_finite(theta_vals^2) * beta_weights)))
					wtd_mu <- mean(wtd_x)
					
					
					env_obj$mu[z,"beta","mu",p,s] <- normalize_angle(((1/V0[ 2 ])*mu0[ 2 ] + wtd_mu*nn) * env_obj$mu[z,"beta","V",p,s])
					
					env_obj$tau_pars[ p, 2*z, s]  <- pmin(env_obj$tau_pars[ p, 2*z, s] + 0.5*((mu0[ 2 ]^2)/V0[ 2 ] + sum(wtd_x2, na.rm=TRUE) - (env_obj$mu[z,"beta","mu",p,s]^2)/env_obj$mu[z,"beta","V",p,s]), 50 * (env_obj$tau_pars[ p, 2*z - 1, s] - 1)/ env_obj$mu[z,"beta","V",p,s])
										
				}	
				
				
				env_obj$sigma_pars[ p, 2*z, s]  <- pmin(env_obj$sigma_pars[ p, 2*z, s] + 0.5*((mu0[ 1 ]^2)/V0[ 1 ] + sum(newV[zmembs, "log_speed", drop=FALSE ]^2, na.rm=TRUE) - (env_obj$mu[z,"alpha","mu",p,s]^2) / env_obj$mu[z,"alpha","V",p,s]), 500 * (env_obj$sigma_pars[ p, 2*z - 1, s] - 1)/ env_obj$mu[z,"alpha","V",p,s])

				
					
			}
									
			
			
			
			#only take the error that is for that state
			z <- env_obj$lambda_matrix[p, env_obj$i, s]
			
			#error is error from X prediction to Y
			
			error_xt_to_yt <- keep_finite(env_obj$ynext[ rownames(env_obj$ynext)==s,c("X","Y"), drop=FALSE] - t(env_obj$MuY[[ s ]][,,z,p]))
			env_obj$error_beforesamp_allpart[ env_obj$i,p,s] <- keep_finite(sum(keep_finite(apply(keep_finite(error_xt_to_yt^2), 1, function(x) sqrt(sum(x))))))
							
								
			
			
			#depends on the state
			env_obj$SSquare_XY[,,z,p,s] <- keep_finite(env_obj$SSquare_XY[,,z,p,s] + keep_finite(t(error_xt_to_yt)%*%error_xt_to_yt))
			
									
			env_obj$XY_errvar[[ s ]][[ p ]][[ z ]]$sig <- keep_finite(as.matrix(Matrix::nearPD(keep_finite(env_obj$Errvar0[[ z ]] + env_obj$SSquare_XY[,,z,p,s]), ensureSymmetry=TRUE)$mat))
			env_obj$XY_errvar[[ s ]][[ p ]][[ z ]]$dof <- env_obj$XY_errvar[[ s ]][[ p ]][[ z ]]$dof + dof_increase_xy #dt[ i ]/cov_adjustment[ i ]#1
			
			#regardless of state
			#predict each future location for particles
			
			
			
			#pred_xt_loc_steps <- keep_finite(apply(env_obj$Xpart_history[ steps_tmp_diff, c("X","Y","log_speed","bearing_rad"),p,s, drop=FALSE], 1, function(x) env_obj$h(mk=x, dtprev=env_obj$reg_dt)))
				
			#error_xtprev_to_xt <- keep_finite(matrix(env_obj$Xpart_history[ steps_tmp_diff + 1, c("X","Y"),p,s] - pred_xt_loc_steps, ncol=2)) 
			error_xtprev_to_xt <- keep_finite(matrix(env_obj$mk_actual_history[ steps_tmp, c("X","Y"),p,s] - env_obj$Xpart_history[ steps_tmp, c("X","Y"),p,s], ncol=2)) 
			
			
			env_obj$SSquare_particle[,,p,s] <- keep_finite(env_obj$SSquare_particle[,,p,s] + keep_finite(t(error_xtprev_to_xt) %*% error_xtprev_to_xt))    #Dp2[p,]%*%t(Dp2[p,])
			#smat <- Particle_errvar0 + SSquare_particle[,,p,s]
			
			##if (any(abs(smat) ==Inf) | any(is.na(s))) { print(smat) ; print(error_xtprev_to_xt[p, ,s]) }
									
			env_obj$Particle_errvar[[ s ]][[ p ]]$sig <- keep_finite(as.matrix(Matrix::nearPD(keep_finite(env_obj$Particle_errvar0 + env_obj$SSquare_particle[,,p,s]), ensureSymmetry=TRUE)$mat))
			env_obj$Particle_errvar[[ s ]][[ p ]]$dof <- env_obj$Particle_errvar[[ s ]][[ p ]]$dof + dof_increase_part #dt[ i ]/cov_adjustment[ i ]#1
		
			# env_obj$Xpart_history[i+1,c("X","Y"),p,s] <- reject_sampling(mu=h(mk=env_obj$Xpart_history[i,c("X","Y","logv","bearing_rad"),p,s], dtprev=reg_dt), 
																# cmat=MCMCpack::riwish(v=Particle_errvar[[ s ]][[ p ]]$dof, S=Particle_errvar[[ s ]][[ p ]]$sig),
																# prev_val=env_obj$Xpart_history[i,c("X","Y"),p,s], bounds=area_map)$val
			# z <- env_obj$Xpart_history[i,"lambda",p,s]
			# env_obj$Xpart_history[i+1,"region",p,s] <- which_region(env_obj$Xpart_history[i+1,c("X","Y"),p,s], centroid=centroids)
			# env_obj$Xpart_history[i+1,"lambda",p,s] <-  low_var_sample(wts=transition_mat[[ s ]][[ p ]]$mat[[ env_obj$Xpart_history[i,"region",p,s] ]][z,], M=1) 
			
		
	  
		}#loop over particles
	
	
		#env_obj$sigma_pars[,,s] <- pmax(env_obj$sigma_pars[,,s], 1e-2)
		#tau_pars[,,s] <- pmax(tau_pars[,,s], 1e-2)
	
						
		env_obj$Xpart_history[steps_tmp, c("bearing_rad","turn_rad"),,s] <- normalize_angle(env_obj$Xpart_history[steps_tmp, c("bearing_rad","turn_rad"),,s])

		env_obj$error_beforesamp_quantiles[ env_obj$i,,s] <-  quantile(env_obj$error_beforesamp_allpart[ env_obj$i,,s], p=c(.1, .5, .9))
		
			
		#update inverse gamma parameters
		
		state_by_seq <- t(apply(env_obj$lambda_matrix[,steps_tmp,s, drop=FALSE],1, function(x) table(factor(x, levels=1:env_obj$nstates))))
	
		#print(lambda_matrix[,step_seq,s])
		#print(state_by_seq)
		
		env_obj$sigma_pars[,2*(1:env_obj$nstates) -1,s] <- env_obj$sigma_pars[,2*(1:env_obj$nstates) -1,s] + 0.5*state_by_seq
		#do this even though for first observation don't have turn angle 
		env_obj$tau_pars[,2*(1:env_obj$nstates) -1,s] <- env_obj$tau_pars[,2*(1:env_obj$nstates) -1,s] + 0.5*state_by_seq
		
		
		
	
	}#end updates by shark
  
	
	
	#update a parameters of inverse gamma
	#if multiple states then update transition probabilities too
	if (env_obj$nstates > 1) {
		
		for (s in env_obj$sharks_with_obs) {	
			for (r in 1:env_obj$nregions) {
		
				tmp <- sapply(lapply(lapply(env_obj$transition_mat[[ s ]], "[[", i=1), "[[", i=r), "[", i=3:2)
				#print(tmp)
				#print( apply(tmp,1, mean))
				env_obj$trans_mean_byregion[,c(env_obj$steps_to_resamp[[ s ]], env_obj$i), r, s] <- t(apply(tmp,1, mean))
				
				#trans_prob_byregion_particle_hist[r,,,i] <- trans_probs_region
				#trans_mean_byregion[i, c(2*r -1, 2*r)] <- apply(trans_probs_region , 1, mean)

				   
			}
	
			region_fracs_bypart <- apply(env_obj$region_counts[,,s, drop=FALSE], 1, function(x) x/sum(x))
			p1to2_region_part <- sapply(lapply(env_obj$transition_mat[[ s ]], "[[", i=1)[ 1:env_obj$nstates ], "[[", i=1)
			#print(p1to2_region_part)
		
			env_obj$trans_mean["p1to2", env_obj$i, s] <- sum(env_obj$trans_mean_byregion["p1to2", env_obj$i, ,s]*(apply(env_obj$region_counts[,,s,drop=FALSE], 2, mean)/(env_obj$i)))
			env_obj$trans_mean["p2to1", env_obj$i, s] <- sum(env_obj$trans_mean_byregion["p2to1", env_obj$i, ,s]*(apply(env_obj$region_counts[,,s,drop=FALSE], 2, mean)/(env_obj$i)))

			
		}#end updating transition probabilities
	   
	}
	
	invisible(NULL)
	
}

