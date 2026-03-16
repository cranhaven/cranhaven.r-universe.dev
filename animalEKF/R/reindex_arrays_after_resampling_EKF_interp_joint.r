reindex_arrays_after_resampling_EKF_interp_joint <- function(env_obj) {

	for (s in env_obj$sharks_to_resample) {

		
		#Xpart[,,,"curr",s] <- Xpart[env_obj$indices[,s] ,,,"curr",s, drop=FALSE]
		env_obj$mk[,,,s] <- env_obj$mk[,, env_obj$indices[,s], s , drop=FALSE]
		env_obj$mk_prev[,,,s] <- env_obj$mk_prev[ ,,env_obj$indices[,s], s , drop=FALSE]
		env_obj$mk_actual[,,s] <- env_obj$mk_actual[ ,env_obj$indices[,s], s, drop=FALSE]
		
		env_obj$densities <- env_obj$densities[ env_obj$indices[,s], , drop=FALSE]
				
		#Pk_prev_interp[[ s ]] <- Pk_prev_interp[[ s ]][,,,, env_obj$indices[,s] , drop=FALSE]
		env_obj$MuY[[ s ]] <- env_obj$MuY[[ s ]][,,, env_obj$indices[,s] , drop=FALSE]
		env_obj$SigY[[ s ]] <- env_obj$SigY[[ s ]][,,,, env_obj$indices[,s], drop=FALSE ]
		
		env_obj$Particle_errvar[[ s ]] <- env_obj$Particle_errvar[[ s ]][ env_obj$indices[,s] ]
		env_obj$XY_errvar[[ s ]] <- env_obj$XY_errvar[[ s ]][ env_obj$indices[,s] ]
		#env_obj$mu[[ s ]] <- env_obj$mu[[ s ]][ env_obj$indices[,s] ]
		env_obj$mu[,,,,s] <- env_obj$mu[,,,env_obj$indices[,s],s]
	
		env_obj$Qt[,,,, s] <- env_obj$Qt[,,,env_obj$indices[,s], s]
		
	

		env_obj$densities_bystate[[ s ]] <- env_obj$densities_bystate[[ s ]][ env_obj$indices[,s],,drop=FALSE ]
							
		if (env_obj$nstates > 1) { 	
		
			env_obj$transition_mat[[ s ]] <- env_obj$transition_mat[[ s ]][ env_obj$indices[,s] ] 
			env_obj$state_counts[,,s] <- env_obj$state_counts[ env_obj$indices[,s],,s, drop=FALSE]
					
			print(env_obj$densities_bystate[[ s ]])
			# need to remove state favoring, that was just for the resampling, so divide by factor
			env_obj$densities_bystate[[ s ]] <- t(apply(env_obj$densities_bystate[[ s ]], 1, function(x) x/env_obj$state_favor))
		}
		
		env_obj$Pk[,,,, s] <- env_obj$Pk[,,, env_obj$indices[,s], s, drop=FALSE]
		env_obj$Pk_prev[,,,, s] <- env_obj$Pk_prev[,,, env_obj$indices[,s], s, drop=FALSE]
		env_obj$Pk_actual[,,,s] <- env_obj$Pk_actual[, ,env_obj$indices[,s], s , drop=FALSE]
		env_obj$XY_errvar_draw[,,,,s] <- env_obj$XY_errvar_draw[,,, env_obj$indices[,s],s , drop=FALSE]
		
		
		env_obj$region_counts[,,s] <- env_obj$region_counts[ env_obj$indices[,s],,s, drop=FALSE]
		
		#resample since last resampled
		if (env_obj$resamp_full_hist) { i_tmp <- 1:(env_obj$i - 1) }
		else { i_tmp <- env_obj$steps_to_resamp[[ s ]] }
		
		env_obj$mk_actual_history[i_tmp,,,s] <- env_obj$mk_actual_history[i_tmp, ,env_obj$indices[,s], s, drop=FALSE]
		
		env_obj$cov_err_hist[,,,i_tmp,s,"resamp"] <- env_obj$cov_err_hist[,,env_obj$indices[,s],i_tmp,s,"orig"]
		
		env_obj$lambda_matrix_beforesamp[,env_obj$steps_to_resamp[[ s ]],s] <- env_obj$lambda_matrix[,env_obj$steps_to_resamp[[ s ]],s]
	
		env_obj$lambda_matrix[,i_tmp,s] <- env_obj$lambda_matrix[ env_obj$indices[,s],i_tmp,s]
		
		env_obj$Xpart_history[i_tmp, ,,s] <- env_obj$Xpart_history[i_tmp, ,env_obj$indices[,s],s]
		
		if (env_obj$interact) {
			env_obj$interact_intensity_draw[,,i_tmp,s] <- env_obj$interact_intensity_draw[env_obj$indices[,s],,i_tmp,s]
			env_obj$neib_fracs[,,i_tmp,s] <- env_obj$neib_fracs[env_obj$indices[,s],,i_tmp,s]
			env_obj$spatial_interact_pars[,,s] <- env_obj$spatial_interact_pars[env_obj$indices[,s],,s]
		}
		
		#so dont resample twice
		#print(env_obj$Xpart_history[seq_hist,"lambda",,s])
		#print(t(env_obj$lambda_matrix[,seq_hist,s]))
		#env_obj$Xpart_history[i_tmp,"lambda",,s] <- t(env_obj$lambda_matrix[,i_tmp,s])
					
		
		#env_obj$Xpart_history[seq_hist,"lambda",,s] <- t(env_obj$lambda_matrix[,seq_hist,s])
		
		if (env_obj$time_dep_trans) {
			env_obj$prob_draws_hist[i_tmp,,,s] <- env_obj$prob_draws_hist[i_tmp,, env_obj$indices[,s],s]
		}	

		env_obj$SSquare_XY[,,,,s] <- env_obj$SSquare_XY[,,, env_obj$indices[,s], s, drop=FALSE]
		env_obj$SSquare_particle[,,,s] <- env_obj$SSquare_particle[,, env_obj$indices[,s], s, drop=FALSE]
		
		env_obj$sigma_pars[,,s] <- env_obj$sigma_pars[ env_obj$indices[,s],, s, drop=FALSE]
		env_obj$tau_pars[,,s] <- env_obj$tau_pars[ env_obj$indices[,s],, s, drop=FALSE]
		
		env_obj$sigma_draw[,,s] <- env_obj$sigma_draw[ env_obj$indices[,s],, s, drop=FALSE]
		env_obj$tau_draw[,,s] <- env_obj$tau_draw[ env_obj$indices[,s],, s, drop=FALSE]
		env_obj$logv_angle_mu_draw[,,,s] <- env_obj$logv_angle_mu_draw[env_obj$indices[,s],,,s, drop=FALSE]	
		
	}#loop over sharks to resample individually
	
	invisible(NULL)
	
}
							
					