sharks_with_obs_update_transition_EKF_1d_interp_joint <- function(env_obj) {


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
		
		for (p in 1:env_obj$npart) {
			
			z_seq <- env_obj$lambda_matrix[p,ss2,s]
			z_seq_prev <- env_obj$lambda_matrix[p,ss1,s]	
											
			trans_tmp <- as.character(paste(z_seq_prev, z_seq, sep=""))
				
			n_tmp <- paste("n", z_seq_prev, z_seq, sep="")
			a_tmp <- paste("a", z_seq_prev, z_seq, sep="")
				
		
			lookup_tmp <- match(trans_tmp, env_obj$trans_names)
			env_obj$transition_mat[[ s ]][[ p ]]$counts[1, lookup_tmp ] <- env_obj$transition_mat[[ s ]][[ p ]]$counts[1, lookup_tmp ] + 1
		}	
				
		#update alpha (dirichlet) parameters for each particle and re-draw transition probabilities				
	
		if (env_obj$time_dep_trans) {
			
			nsteps <- length(step_seq_tmp)						
			
			for (p in 1:env_obj$npart) {
				#regions <- Xpart_history[step_seq_tmp, "region",p,s]
				prev_z <- env_obj$lambda_matrix[ p, step_seq_tmp,s]
				z_oth <- (env_obj$nstates:1)[ prev_z ]		
				tis <- env_obj$Xpart_history[step_seq_tmp, "time_in_state",p,s]
				
				#update for past but not for current state since not sure what will happen next time		
				for (jj in 1:length(ss1)) {
					a_tmp <- env_obj$transition_mat[[ s ]][[ p ]]$dirichlet_pars[ 1, c(2*prev_z[ jj ]-1, 2*prev_z[ jj ]) ]
					#parameter not of that state
					a_tmp[ z_oth[ jj ]] <- a_tmp[ z_oth[ jj ] ] + 1
					a_tmp[ prev_z[ jj ] ] <- a_tmp[ prev_z[ jj ] ] - tis[ jj ] * log(env_obj$prob_draws_hist[ ss1[ jj ], prev_z[ jj ],p,s])
				}	
				
				#draw for next time based on current state
				a_tmp <- env_obj$transition_mat[[ s ]][[ p ]]$dirichlet_pars[ 1, c(2*prev_z[ nsteps ]-1, 2*prev_z[ nsteps ]) ]
				gamma_ratio <-  rgamma(n=1, shape=a_tmp[ z_oth[ nsteps ] ], rate=a_tmp[ prev_z[ nsteps ] ])
				
				
				env_obj$transition_mat[[ s ]][[ p ]]$mat[ prev_z[ nsteps ], prev_z[ nsteps ]] <- rbeta(n=1, shape1=gamma_ratio * tis[ nsteps ], shape2=1)
				env_obj$transition_mat[[ s ]][[ p ]]$mat[ prev_z[ nsteps] , z_oth[ nsteps ]] <- 1 - env_obj$transition_mat[[ s ]][[ p ]]$mat[ prev_z[ nsteps ], prev_z[ nsteps ]]
			}
		}
		else {
			#update dirichlet parameters
			
			for (p in 1:env_obj$npart) {	
				z_seq <- env_obj$lambda_matrix[p,ss2,s]
				z_seq_prev <- env_obj$lambda_matrix[p,ss1,s]	
											
				trans_tmp <- as.character(paste(z_seq_prev, z_seq, sep=""))
			
				lookup_tmp <-  match(trans_tmp, env_obj$trans_names)

		
				env_obj$transition_mat[[ s ]][[ p ]]$dirichlet_pars[ lookup_tmp ] <- env_obj$region_alphas[ lookup_tmp ] + env_obj$transition_mat[[ s ]][[ p ]]$counts[ lookup_tmp ]
								
				for (k in 1:env_obj$nstates) {
					env_obj$transition_mat[[ s ]][[ p ]]$mat[ k, ] <- MCMCpack::rdirichlet(n=1, alpha=env_obj$transition_mat[[ s ]][[ p ]]$dirichlet_pars[ 1, c(2*k -1, 2*k) ]) 
		
				}
			}					
		}

	}#loop over sharks
			
	invisible(NULL)

}	