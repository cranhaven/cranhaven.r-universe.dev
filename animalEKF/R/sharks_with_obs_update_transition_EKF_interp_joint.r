sharks_with_obs_update_transition_EKF_interp_joint <- function(env_obj) {

	for (s in env_obj$sharks_with_obs) {

		#update transition counts for each particle based on region and for previously simulated results
			
		step_seq_tmp <- c(env_obj$steps_to_resamp[[ s ]][ env_obj$steps_to_resamp[[ s ]] %in% env_obj$shark_valid_steps[[ s ]]], env_obj$i)
		
		if (env_obj$update_eachstep | env_obj$update_params_for_obs_only) {
			ss1 <- env_obj$i-1
			ss2 <- env_obj$i
			# only look at the last step
			step_seq_tmp <- step_seq_tmp[length(step_seq_tmp)]
		
		}
		else {
				
			#need to fix this
			ss1 <- step_seq_tmp[ -length(step_seq_tmp) ]
			ss2 <- step_seq_tmp[ -1 ]
		}
		
		if (env_obj$nstates >1 ) {
			for (p in 1:env_obj$npart) {
				
			
				#if no interaction, don't use to update parameters
				
				ss1_some_interact <- rep(FALSE, length(ss1))
				
				
				
				z_seq <- env_obj$lambda_matrix[p,ss2,s]
				z_seq_prev <- env_obj$lambda_matrix[p,ss1,s]	
												
				trans_tmp <- as.character(paste(z_seq_prev,z_seq, sep=""))[ ss1_some_interact==FALSE ]
					 
				n_tmp <- paste("n",z_seq_prev,z_seq, sep="")
				a_tmp <- paste("a",z_seq_prev,z_seq, sep="")
					
				
				lookup_tmp <- cbind(env_obj$Xpart_history[ss1[ ss1_some_interact==FALSE ], "region",p,s], match(trans_tmp, env_obj$trans_names))
	
				
				if (nrow(lookup_tmp)>0) {
					for (aa in 1:nrow(lookup_tmp)) {
						env_obj$transition_mat[[ s ]][[ p ]]$counts[ lookup_tmp[aa,] ] <- env_obj$transition_mat[[ s ]][[ p ]]$counts[ lookup_tmp[aa,] ] + 1
					}
				}
			}	
		
			#update alpha (dirichlet) parameters for each particle and re-draw transition probabilities				
		
			if (env_obj$time_dep_trans) {
			
				nsteps <- length(step_seq_tmp)
				
				for (p in 1:env_obj$npart) {
				
					regions <- env_obj$Xpart_history[step_seq_tmp, "region",p,s]
					prev_z <- env_obj$lambda_matrix[ p, step_seq_tmp,s]
					z_oth <- (env_obj$nstates:1)[ prev_z ]		
					tis <- env_obj$Xpart_history[step_seq_tmp, "time_in_state",p,s]
														
					#update for past but not for current state since not sure what will happen next time		
					for (jj in which(ss1_some_interact==FALSE)) {
						#only update if stayed
						if (prev_z[ jj ] != prev_z[ jj+1 ]) {
							env_obj$transition_mat[[ s ]][[ p ]]$dirichlet_pars[ regions[ jj ], prev_z[ jj ]] <- (env_obj$transition_mat[[ s ]][[ p ]]$dirichlet_pars[ regions[ jj ], prev_z[ jj ]]*tis[ jj ] +1)/tis[ jj ]
						}
					}	
				}
	
			}
			else {
				#update dirichlet parameters
				
				for (p in 1:env_obj$npart) {	
					z_seq <- env_obj$lambda_matrix[p,ss2,s]
					z_seq_prev <- env_obj$lambda_matrix[p,ss1,s]	
												
					trans_tmp <- as.character(paste(z_seq_prev,z_seq, sep=""))[ ss1_some_interact==FALSE ]
					
					
					lookup_tmp <- cbind(env_obj$Xpart_history[ss1[ ss1_some_interact==FALSE ], "region",p,s], match(trans_tmp, env_obj$trans_names))
			
					#print(transition_mat[[ s ]][[ p ]]$dirichlet_pars[ lookup_tmp[,1], ])
					
					env_obj$transition_mat[[ s ]][[ p ]]$dirichlet_pars[ lookup_tmp ] <- env_obj$region_alphas[ lookup_tmp ] + env_obj$transition_mat[[ s ]][[ p ]]$counts[ lookup_tmp ]
					
					#print(trans_tmp)
					#print(lookup_tmp)
					#print("after")
					#print(transition_mat[[ s ]][[ p ]]$dirichlet_pars[ lookup_tmp[,1], ])
					
					regions <- env_obj$Xpart_history[step_seq_tmp, "region",p,s]
					
					
					for (rr in unique(regions)) {
						
						for (k in 1:env_obj$nstates) {
							env_obj$transition_mat[[ s ]][[ p ]]$mat[[ rr ]][ k, ] <- MCMCpack::rdirichlet(n=1, alpha=env_obj$transition_mat[[ s ]][[ p ]]$dirichlet_pars[ rr, c(2*k -1, 2*k) ]) 
						}
					}
				}					
			}
		} #if env_obj$nstates
	}#loop over sharks
	
	
	invisible(NULL)
}	