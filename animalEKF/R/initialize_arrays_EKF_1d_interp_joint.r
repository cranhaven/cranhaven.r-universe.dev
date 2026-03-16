initialize_arrays_EKF_1d_interp_joint <- function(env_obj) {

	#names for array index labels
	env_obj$pnames <- paste("p", 1:env_obj$npart, sep="")
	env_obj$state_names <- paste("state", 1:env_obj$nstates, sep="")
	env_obj$rnames <- paste("r", 1:env_obj$nregions, sep="")
	env_obj$Nnames <- paste("N",1:env_obj$N, sep="")

	#lookup vector for 11 12 21 22

	env_obj$trans_names <- as.character(do.call(paste, c(expand.grid(1:env_obj$nstates, 1:env_obj$nstates)[,2:1], sep="")))

	env_obj$lambda_matrix_beforesamp <- env_obj$lambda_matrix <- array(NA, dim=c(env_obj$npart, env_obj$N, env_obj$nsharks),
																	   dimnames=list(env_obj$pnames, env_obj$Nnames, env_obj$shark_names))

	env_obj$state_counts <- array(0, dim=c(env_obj$npart, env_obj$nstates, env_obj$nsharks), dimnames=list(env_obj$pnames, env_obj$state_names, env_obj$shark_names))

	#drop tags and convert to matrix for easier computation
	#history of particle positions: each index is one particle, are resampled after every step


	env_obj$Xpart_history <- array(NA, dim=c(env_obj$N, 6, env_obj$npart, env_obj$nsharks), 
								   dimnames=list(env_obj$Nnames, c("X","velocity","lambda","time_in_state","state_change","time_to_next"),
											     env_obj$pnames, env_obj$shark_names))
	env_obj$Xpart_history[,"time_to_next",,] <- env_obj$reg_dt


	env_obj$eff_size_hist <- matrix(NA, nrow=env_obj$N, ncol=env_obj$nsharks)
	colnames(env_obj$eff_size_hist) <- env_obj$shark_names	
										  
											
	env_obj$error_beforesamp_quantiles <- env_obj$error_final_quantiles <- array(NA, dim=c(env_obj$N, 3, env_obj$nsharks), dimnames=list(env_obj$Nnames, c("Q10","Q50","Q90"), env_obj$shark_names))
	env_obj$error_beforesamp_allpart <- array(NA, dim=c(env_obj$N, env_obj$npart, env_obj$nsharks), dimnames=list(env_obj$Nnames, env_obj$pnames, env_obj$shark_names))

	#prior matrix for state switching:
	#initialize transition probabilities one for each space and for each particle.  use means initially so all regions are equal for each particle

	if (env_obj$nstates > 1) {
	  
		env_obj$trans_mean <- array(NA, dim=c(2, env_obj$N, env_obj$nsharks), dimnames=list(c("p1to2","p2to1"), env_obj$Nnames, env_obj$shark_names))	
		
			#interaction
		#if no interaction this is automatically 1
		
		env_obj$interact_intensity_draw <- array(1, dim=c(env_obj$npart, env_obj$nstates, env_obj$N, env_obj$nsharks), dimnames=list(env_obj$pnames, env_obj$state_names, env_obj$Nnames, env_obj$shark_names))
		env_obj$trans_mean <- array(NA, dim=c(2, env_obj$N, env_obj$nsharks), dimnames=list(c("p1to2","p2to1"), env_obj$Nnames, env_obj$shark_names))	
						
						

		if (env_obj$interact) {
			env_obj$min_num_neibs <- max(1, env_obj$min_num_neibs)
			
			env_obj$neib_fracs <- array(0, dim=c(env_obj$npart, env_obj$nstates, env_obj$N, env_obj$nsharks), dimnames=list(env_obj$pnames, env_obj$state_names, env_obj$Nnames, env_obj$shark_names))
			
			env_obj$tau_vals <- matrix(env_obj$interact_pars$known_precision, ncol=env_obj$nstates-1, nrow=env_obj$npart, byrow=TRUE)
			#spatial interaction
			env_obj$interact_pars$known_sd <- 1/sqrt(env_obj$interact_pars$known_precision)
			env_obj$spatial_interact_pars <- matrix(c(env_obj$interact_pars$mu0, env_obj$interact_pars$precision0), ncol=2*(env_obj$nstates-1), nrow=env_obj$npart, byrow=TRUE)
				
			env_obj$mu_names <- paste("mu", 1:(env_obj$nstates-1), sep="")
			env_obj$prec_names <- paste("precision", 1:(env_obj$nstates-1), sep="")
				
			env_obj$spatial_interact_pars <- array(env_obj$spatial_interact_pars,  dim=c(env_obj$npart, 2*(env_obj$nstates-1), env_obj$nsharks), dimnames=list(env_obj$pnames, c(env_obj$mu_names, env_obj$prec_names), env_obj$shark_names))
			#print(spatial_interact_pars)
			env_obj$num_neibs <- array(0, dim=c(env_obj$npart, env_obj$nstates, env_obj$nsharks), dimnames=list(env_obj$pnames, env_obj$state_names, env_obj$shark_names))
			
			env_obj$spatial_interact_intensity_history <- array(NA, dim=c(env_obj$N, env_obj$nstates-1, env_obj$nsharks), dimnames=list(env_obj$Nnames, env_obj$state_names[ -env_obj$nstates ], env_obj$shark_names))
			env_obj$spatial_interact_mu_history <- env_obj$spatial_interact_intensity_history

		
		}	
		
		
		#state transition matrices
		
		if (env_obj$time_dep_trans) {
			#time_dep_trans_init <- rep(list(time_dep_trans_init[1:min(length(time_dep_trans),4*nregions)]), nregions*length(time_dep_trans_init))[ 1:nregions ] 
			#print(time_dep_trans_init)
			env_obj$region_alphas <- matrix(env_obj$time_dep_trans_init, ncol=4, nrow=env_obj$nregions, byrow=TRUE)
			print(env_obj$region_alphas)
			colnames(env_obj$region_alphas) <- c("a11","a12","a21","a22")
			  
			#need this for updating time dependency, need to keep record of draws
			env_obj$prob_draws_hist <- array(NA, dim=c(env_obj$N, env_obj$nstates, env_obj$npart, env_obj$nsharks), dimnames=list(env_obj$Nnames, env_obj$state_names, env_obj$pnames, env_obj$shark_names))
			  
		}
		else { 
		 # dirichlet_init <- rep(dirichlet_init, nregions)[ 1:nregions ]
		 # region_alphas <- matrix(unlist(dirichlet_init), ncol=4, nrow=nregions, byrow=TRUE)
			if (is.list(env_obj$dirichlet_init)) { env_obj$dirichlet_init <- env_obj$dirichlet_init[[ 1 ]] }			 	
			env_obj$region_alphas <- matrix(env_obj$dirichlet_init, ncol=4, nrow=1, byrow=TRUE)
			colnames(env_obj$region_alphas) <- c("a11","a12","a21","a22")
		}
	   
		env_obj$trans_counts <- matrix(0, ncol=4, nrow=1)
		colnames(env_obj$trans_counts) <- c("n11", "n12", "n21", "n22")
		env_obj$transition_mat <- rep(list(rep(list(list(mat=diag(2), counts=env_obj$trans_counts, dirichlet_pars=env_obj$region_alphas)), env_obj$npart)), env_obj$nsharks)
		names(env_obj$transition_mat) <- env_obj$shark_names
		
		#initialize parameter lists
		# if (time_dep_trans) {
			
			# for (s in shark_names) {
				# gamma_ratios <- cbind(rgamma(n=npart, shape=region_alphas[ 2 ], rate=region_alphas[ 1 ]),
									  # rgamma(n=npart, shape=region_alphas[ 3 ], rate=region_alphas[ 4 ]))
					
				# for (p in 1:npart) {
					# #trans_mean_byregion[ ,1,rr ,s] <- diag(transition_mat[[ s ]][[ p ]]$mat[[ rr ]]) <- c(rbeta(n=1, shape1=gamma_ratios[p,1]*1, shape2=1),  rbeta(n=1, shape1=gamma_ratios[p,2]*1, shape2=1))
					
					# transition_mat[[ s ]][[ p ]]$mat[cbind(1:nstates,nstates:1)] <- 1-diag(transition_mat[[ s ]][[ p ]]$mat
						
				# }
			# }	
		# }
		#else {
		  
		for (s in env_obj$shark_names) {
			for (p in 1:env_obj$npart) {
				env_obj$transition_mat[[ s ]][[ p ]]$mat <- rbind(MCMCpack::rdirichlet(n=1, alpha=env_obj$region_alphas[ 1:2 ]), 
																  MCMCpack::rdirichlet(n=1, alpha=env_obj$region_alphas[ 3:4 ]))
																 
				}
			}
			
		#}
		
	}
	else{
		env_obj$Xpart_history[,"lambda",,] <-  1
		env_obj$lambda_matrix[,,] <- 1
		env_obj$lambda_matrix_beforesamp[,,] <- 1

	}	
		#end setting up transition matrices and histories

	env_obj$resample_history <- array(NA, dim=c(env_obj$N, env_obj$nsharks), dimnames=list(env_obj$Nnames, env_obj$shark_names))

	#matrix to hold densities for weights

	env_obj$densities <- array(1, dim=c(env_obj$npart, env_obj$nstates, env_obj$nsharks), dimnames=list(env_obj$pnames, env_obj$state_names, env_obj$shark_names))
	
	
	invisible(NULL)
}
						
