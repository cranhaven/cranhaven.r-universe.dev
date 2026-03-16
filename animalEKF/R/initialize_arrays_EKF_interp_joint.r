initialize_arrays_EKF_interp_joint <- function(env_obj) {



	env_obj$lambda_matrix_beforesamp <- env_obj$lambda_matrix <- array(NA, dim=c(env_obj$npart, env_obj$N, env_obj$nsharks), dimnames=list(env_obj$pnames, env_obj$Nnames, env_obj$shark_names))

	env_obj$state_counts <- array(0, dim=c(env_obj$npart, env_obj$nstates, env_obj$nsharks), dimnames=list(env_obj$pnames, env_obj$state_names, env_obj$shark_names))
	env_obj$region_counts <- array(0, dim=c(env_obj$npart, env_obj$nregions, env_obj$nsharks), dimnames=list(env_obj$pnames, env_obj$rnames, env_obj$shark_names))

	env_obj$pred_xt_loc <- array(0, dim=c(env_obj$npart, 2, length(env_obj$shark_names)), dimnames=list(env_obj$pnames, c("X","Y"), env_obj$shark_names))
	env_obj$pred_xt_loc_bystate <- array(0, dim=c(env_obj$npart, 2, env_obj$nstates, length(env_obj$shark_names)), dimnames=list(env_obj$pnames, c("X","Y"), env_obj$state_names, env_obj$shark_names))
	env_obj$pred_xt_loc_bystate_var <- array(0, dim=c(env_obj$npart, 2, 2, env_obj$nstates, length(env_obj$shark_names)), dimnames=list(env_obj$pnames, c("X","Y"),c("X","Y"), env_obj$state_names, env_obj$shark_names))				



	env_obj$Xpart_history <- array(NA, dim=c(env_obj$N, 11, env_obj$npart, env_obj$nsharks), 
								   dimnames=list(env_obj$Nnames, c("X","Y","log_speed","bearing_rad","turn_rad","lambda","region","time_in_state","state_change","time_to_next", "speed"),
											  env_obj$pnames, env_obj$shark_names))
	env_obj$Xpart_history[,"time_to_next",,] <- env_obj$reg_dt
										  

	#Xpart <- array(NA,  dim=c(env_obj$npart, 4, env_obj$nstates, 3, env_obj$nsharks), 
	#					dimnames=list(env_obj$pnames, c("X","Y","logv","bearing_rad"), env_obj$state_name,	c("curr", "d","next_t"), env_obj$shark_names))
												
												
	env_obj$error_beforesamp_quantiles <- env_obj$error_final_quantiles <- array(NA, dim=c(env_obj$N, 3, env_obj$nsharks), dimnames=list(env_obj$Nnames, c("Q10","Q50","Q90"), env_obj$shark_names))
	env_obj$error_beforesamp_allpart <- array(NA, dim=c(env_obj$N, env_obj$npart, env_obj$nsharks), dimnames=list(env_obj$Nnames, env_obj$pnames, env_obj$shark_names))

	
	
	if (env_obj$smoothing) {  
		env_obj$error_smoothed_quantiles  <- env_obj$error_final_quantiles
		env_obj$error_smoothed_allpart <- array(NA, dim=c(env_obj$N, env_obj$npart, env_obj$nsharks), dimnames=list(env_obj$Nnames, env_obj$pnames, env_obj$shark_names))
			
	}
	else { 
		env_obj$smooth_parameters <- FALSE 
	}
	#prior matrix for state switching:
	#initialize transition probabilities one for each space and for each particle.  use means initially so all regions are equal for each particle

	env_obj$cov_err_hist <- array(NA, dim=c(3, 1 + env_obj$nstates, env_obj$npart, env_obj$N, env_obj$nsharks, 2),
										dimnames=list(c("X","Y","cov"), c("particle",paste("state",1:env_obj$nstates)), env_obj$pnames, env_obj$Nnames, env_obj$shark_names, c("orig","resamp")))
						
						
	env_obj$eff_size_hist <- matrix(NA, nrow=env_obj$N, ncol=env_obj$nsharks)
	colnames(env_obj$eff_size_hist) <- env_obj$shark_names					

						
						
	if (env_obj$nstates >1) {
	  
		#interaction
		#if no interaction this is automatically 1
		
		env_obj$interact_intensity_draw <- array(1, dim=c(env_obj$npart, env_obj$nstates, env_obj$N, env_obj$nsharks), dimnames=list(env_obj$pnames, env_obj$state_names, env_obj$Nnames, env_obj$shark_names))
		env_obj$trans_mean_byregion <- array(NA, dim=c(2, env_obj$N, env_obj$nregions, env_obj$nsharks), dimnames=list(c("p1to2","p2to1"), env_obj$Nnames, env_obj$rnames, env_obj$shark_names))	
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
			#time_dep_trans_init <- rep(list(time_dep_trans_init[1:min(length(time_dep_trans_init),4*env_obj$nregions)]), env_obj$nregions*length(time_dep_trans_init))[ 1:env_obj$nregions ] 
			#print(time_dep_trans_init)
			 
			#use kumaraswamy distribution, where alpha=stay into behavior	
			env_obj$region_alphas <- matrix(env_obj$dirichlet_init[ c(1,2) ], ncol=2, nrow=env_obj$nregions, byrow=TRUE)
			  
			#need this for updating time dependency, need to keep record of draws
			env_obj$prob_draws_hist <- array(NA, dim=c(env_obj$N, env_obj$nstates, env_obj$npart, env_obj$nsharks), dimnames=list(env_obj$Nnames, env_obj$state_names, env_obj$pnames, env_obj$shark_names))
			  
			
		}
		else { 
		 
		  env_obj$region_alphas <- matrix(rep_len(env_obj$dirichlet_init, env_obj$nregions*env_obj$nstates^2), ncol=env_obj$nstates^2, nrow=env_obj$nregions, byrow=TRUE)
		  colnames(env_obj$region_alphas) <- c("a11","a12","a21","a22")
		}
		
		env_obj$trans_counts <- matrix(0, ncol=4, nrow=env_obj$nregions)
		colnames(env_obj$trans_counts) <- c("n11", "n12", "n21", "n22")
		env_obj$transition_mat <- rep(list(rep(list(list(mat=rep(list(diag(env_obj$nstates)), env_obj$nregions), counts=env_obj$trans_counts, dirichlet_pars=env_obj$region_alphas)), env_obj$npart)), env_obj$nsharks)
		names(env_obj$transition_mat) <- env_obj$shark_names
		#print(transition_mat[[ "GSH1" ]][[ 1 ]]$mat)
		#print(length(transition_mat[[ "GSH1" ]][[ 1 ]]))
		
	   
	    if (env_obj$nregions >1) {
			env_obj$components_variance <- matrix(NA, nrow=env_obj$N, ncol=env_obj$nstates*2)
		
		}
		
		
		
		#initialize parameter lists
		if (env_obj$time_dep_trans) {
			
			for (s in env_obj$shark_names) {
				for (p in 1:env_obj$npart) {
					for (rr in 1:env_obj$nregions) {
						
						pleave <- c()
						for (k in 1:env_obj$nstates) {
							pleave <- c(pleave, rbeta(n=1, shape1=env_obj$transition_mat[[ s ]][[ p ]]$dirichlet_pars[rr, k ]*1, shape2=1))
						}
						
						env_obj$transition_mat[[ s ]][[ p ]]$mat[[ rr ]][ cbind(1:2, 2:1 )] <- pleave
						diag(env_obj$transition_mat[[ s ]][[ p ]]$mat[[ rr ]]) <- 1 - pleave
						
					}
				}
			}		
		}
		else {
		  #rr <- X0_tmp[ 1, "region"]
		 # trans_mean[1,] <- env_obj$region_alphas[ rr, 2:3]/c(sum(env_obj$region_alphas[ rr, 1:2]), sum(env_obj$region_alphas[ rr, 3:4]))
			
			for (s in env_obj$shark_names) {
			
				env_obj$trans_mean_byregion[ "p1to2",1, ,s] <- env_obj$region_alphas[,2]/rowSums(env_obj$region_alphas[,1:2, drop=FALSE])
				env_obj$trans_mean_byregion[ "p2to1",1, ,s] <- env_obj$region_alphas[,3]/rowSums(env_obj$region_alphas[,3:4, drop=FALSE])
		  
			
				for (p in 1:env_obj$npart) {
					for (rr in 1:env_obj$nregions) {
						env_obj$transition_mat[[ s ]][[ p ]]$mat[[ rr ]] <- rbind(MCMCpack::rdirichlet(n=1, alpha=env_obj$region_alphas[ rr, 1:2 ]), 
																				  MCMCpack::rdirichlet(n=1, alpha=env_obj$region_alphas[ rr, 3:4 ]))
					}										     
				}
			}
			
		}
		
	
	}#end setting up transition matrices and histories
	else {
		#if only one state
		env_obj$Xpart_history[,"lambda",,] <-  1
		env_obj$lambda_matrix[,,] <- 1
		env_obj$lambda_matrix_beforesamp[,,] <- 1
	
	}

	env_obj$resample_history <- array(NA, dim=c(env_obj$N, env_obj$nsharks), dimnames=list(env_obj$i, env_obj$shark_names))

	#matrix to hold densities for weights

	env_obj$densities <- array(1, dim=c(env_obj$npart, env_obj$nstates, env_obj$nsharks), dimnames=list(env_obj$pnames, env_obj$state_names, env_obj$shark_names))

	invisible(NULL)
}