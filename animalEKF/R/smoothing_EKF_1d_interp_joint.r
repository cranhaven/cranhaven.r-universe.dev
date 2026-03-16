smoothing_EKF_1d_interp_joint <- function(env_obj){
	 


	 
	if (env_obj$nstates ==1 ) { env_obj$fix_smoothed_behaviors <- TRUE }
	n_ind <- ifelse(env_obj$fix_smoothed_behaviors, 1, env_obj$nstates)
	
	env_obj$smooth_iter <- env_obj$npart
	
	env_obj$mk_prev_smoothed <- env_obj$mk_curr_smoothed  <- array(NA, dim=c(2, n_ind, env_obj$npart, env_obj$nsharks), dimnames=list(c("X","velocity"), 1:n_ind, env_obj$pnames, env_obj$shark_names))
	env_obj$Pk_prev_smoothed <- env_obj$Pk_curr_smoothed <- env_obj$Pk_prev #array(NA, dim=c(2,2, npart, nsharks), dimnames=list(c("X","logv"), c("X","logv"), pnames, shark_names))
	
	if (env_obj$interact) { 
		env_obj$spatial_interact_pars_smoothed <- env_obj$spatial_interact_pars 
		env_obj$interact_intensity_draw_smoothed <- matrix(1, ncol=env_obj$nstates, nrow=env_obj$npart)
	}
	
	env_obj$logv_angle_mu_draw <- matrix(NA, nrow=env_obj$nstates, ncol=env_obj$npart)
	env_obj$sigma_pars_smoothed <- env_obj$sigma_pars
	env_obj$Particle_errvar_smoothed <- env_obj$Particle_errvar
	env_obj$mu_smoothed <- env_obj$mu
	env_obj$transition_mat_smoothed <- env_obj$transition_mat
	
	transition_draws <- matrix(NA, ncol=env_obj$nstates, nrow=env_obj$npart)

	part_with_neibs <- matrix(FALSE, ncol=env_obj$nsharks, nrow=env_obj$npart)
	colnames(part_with_neibs) <- env_obj$shark_names
	


	
	#densities_smoothed <- rep(list(matrix(NA, ncol=1+(nstates>1), nrow=npart)), nstates)
	
	
	
	densities_smoothed <- array(NA, dim=c(env_obj$npart, n_ind, env_obj$nsharks), dimnames=list(env_obj$pnames, 1:n_ind, env_obj$shark_names))
	

	if (env_obj$lowvarsample) {
		indices <- t(sapply(1:env_obj$nsharks, function(zz) low_var_sample(wts=rep(1, env_obj$npart), M=env_obj$smooth_iter)))
	}
	else {
		indices <- t(sapply(1:env_obj$nsharks, function(zz) sample.int(n=env_obj$npart, size=env_obj$smooth_iter, replace=TRUE)))
	}
	rownames(indices) <- env_obj$shark_names
	
	
	#store the states
	env_obj$Xpart_history_smoothed <- array(NA, dim=c(env_obj$N, 3, env_obj$smooth_iter, env_obj$nsharks), 
											dimnames=list(env_obj$Nnames, c("X","velocity","lambda"), 1:env_obj$smooth_iter, env_obj$shark_names))
	
	#can't use first step because need to condition on previous bearing each time
	smooth_steps <- env_obj$shark_valid_steps
	smooth_steps <- lapply(smooth_steps, function(x) x[ (x-1) %in% x ])

			
	#draws for variables
	if (env_obj$show_prints) {
		print("shark valid steps")
		print(env_obj$shark_valid_steps)
	}
	
	for (i in (env_obj$N - 1):1) {	
	
		part_with_neibs[,] <- FALSE

	
		if ((i %% 3) == 0 & env_obj$show_prints) { print(paste("smoothing step", i)) }
				
		valid_sharks <- sapply(smooth_steps, function(x) i %in% x)
		valid_sharks <- names(valid_sharks[ valid_sharks==TRUE ])
		
		if (env_obj$interact) {
			temp_neib_range <- which((env_obj$t_reg < env_obj$t_reg[ i+1 ]) & (env_obj$t_reg >= (env_obj$t_reg[ i+1 ] - env_obj$time_radius)))
		}
		
		for (s in valid_sharks) {
								
			if (! ((i + 1) %in% env_obj$shark_valid_steps[[ s ]])) {

				if (env_obj$lowvarsample) {	
					indices[s,] <- low_var_sample(wts=rep(1, env_obj$npart), M=env_obj$smooth_iter)
				}
				else {
					indices[s,] <- sample.int(n=env_obj$smooth_iter, replace=TRUE, size=env_obj$smooth_iter)
				}
				
				env_obj$Xpart_history_smoothed[ i + 1,c("X","velocity","lambda") ,, s] <- env_obj$Xpart_history[ i + 1,c("X","velocity","lambda") ,indices[s, ], s]  
				
			}
			
			
			
			if (env_obj$fix_smoothed_behaviors) {
								
				z_prev <- env_obj$Xpart_history[ i, "lambda",, s]
				
				#set up covariance matrices
				if (env_obj$nstates > 1) {
					for (p in 1:env_obj$npart) {
						transition_draws[ p, ] <- MCMCpack::rdirichlet(n=1, alpha=env_obj$transition_mat_smoothed[[ s ]][[ p ]]$dirichlet_pars[ 1, c(2*z_prev[ p ]-1, 2*z_prev[ p ]) ]) * env_obj$state_favor 
					}
				}
				
			}			
			else {
				z_curr <- env_obj$Xpart_history_smoothed[ i + 1, "lambda",, s]
		
				#CHOOSE previous behavior scaled by the probability they 
				#set up covariance matrices
				
				
				if (env_obj$nstates > 1) {
					for (p in 1:env_obj$npart) {
						
						transition_draws[ p, ] <- MCMCpack::rdirichlet(n=1, alpha=env_obj$transition_mat_smoothed[[ s ]][[ p ]]$dirichlet_pars[ 1, z_curr[ p ] + c(0,2) ]) * env_obj$state_favor 
					}
				}
										
			}
			
			

			for (k in 1:env_obj$nstates) {		
				
				env_obj$Qt["velocity","velocity",k,,s] <- MCMCpack::rinvgamma(n=env_obj$npart, env_obj$sigma_pars_smoothed[,2*k-1,s], env_obj$sigma_pars_smoothed[,2*k,s])
				
				for (p in 1:env_obj$npart) {
				
					env_obj$logv_angle_mu_draw[k,p] <- keep_finite(mvtnorm::rmvnorm(n=1, mean=env_obj$mu_smoothed[k, "mu", p, s], sigma=as.matrix(env_obj$Qt[2, 2,k,p,s] * env_obj$mu_smoothed[k, "V", p, s])))
				}

				
			}
			
			
			#this part doesnt depend on the fixing of states	

			
			for (p in 1:env_obj$npart) {
									
				env_obj$Qt["X","X",,p,s] <- MCMCpack::riwish(v=env_obj$Particle_errvar_smoothed[[ s ]][[ p ]]$dof, S=env_obj$Particle_errvar_smoothed[[ s ]][[ p ]]$sig)
				
				#set X location (and later Y and previous bearing)
				env_obj$mk_prev_smoothed["X",,p,s] <- env_obj$Xpart_history[ i,"X",p,s]
			
			}
			
			env_obj$Qt["X","X",,,s] <- keep_finite(env_obj$Qt["X","X",,,s])
			
			#if interact, need to calculate neighbors for each s_ind, given previous states
			if (env_obj$interact) {
				
				oth_sharks <- env_obj$shark_names[ env_obj$shark_names != s ]
				
				
				#just the indices
				possible_prev_temp_neibs <- apply(env_obj$Xpart_history_smoothed[ temp_neib_range, "lambda", 1, oth_sharks, drop=FALSE], 4, function(x) any(! is.na(x)))
				possible_prev_temp_neibs <- names(possible_prev_temp_neibs[ possible_prev_temp_neibs ])
				#print(s)
				
				
				#do this now so don't have to repeat each time for s_ind
				if (length(possible_prev_temp_neibs)) {
					interact_mu_draw_smoothed <- matrix(rnorm(n=env_obj$npart*(env_obj$nstates-1), mean=env_obj$spatial_interact_pars_smoothed[,env_obj$mu_names,s], 
															  sd=1/sqrt(env_obj$spatial_interact_pars_smoothed[,env_obj$prec_names,s])), ncol=env_obj$nstates-1, nrow=env_obj$npart, byrow=TRUE)
				
					neibs <- list()
					for (p in 1:env_obj$npart) {
						neibs[[ p ]] <- na.omit(matrix(apply(env_obj$Xpart_history_smoothed[ temp_neib_range, c("X","lambda"), p,possible_prev_temp_neibs, drop=FALSE], MARGIN=2, FUN=rbind), ncol=2))
						colnames(neibs[[ p ]]) <- c("X","lambda")
					}
					
				}
											
			}
			else {
				#do this so later condition works if interact=FALSE
				possible_prev_temp_neibs <- c()
			}
			
			
							
			if (env_obj$fix_smoothed_behaviors) {
			
				env_obj$mk_prev_smoothed["velocity",,,s] <- env_obj$logv_angle_mu_draw[ cbind(z_prev, 1:env_obj$npart) ]
								
				
				for (p in 1:env_obj$npart) {	
					
					Fx_tmp <- keep_finite(env_obj$Fx(mk=env_obj$mk_prev_smoothed[,1,p,s], dtprev=env_obj$reg_dt))	
					
					env_obj$Pk_curr_smoothed[,,1,p,s] <- keep_finite(as.matrix(Matrix::nearPD(keep_finite(keep_finite(keep_finite(Fx_tmp %*% env_obj$Pk_prev[,,z_prev[ p ],p,s]) %*% t(Fx_tmp)) + env_obj$Qt[,,z_prev[ p ],p,s]), ensureSymmetry=TRUE)$mat)) #R_{t+1}
					env_obj$mk_curr_smoothed["X",1,p,s] <- keep_finite(env_obj$h(mk=env_obj$mk_prev_smoothed[,1,p,s], dtprev=env_obj$reg_dt))				
				}

				for (s_ind in 1:env_obj$smooth_iter) {
				
					#replace each value of "logv" for all particles with the historical value to evaluate density
					
					env_obj$mk_curr_smoothed["velocity",1,,s] <- env_obj$Xpart_history_smoothed[i+1,"velocity",s_ind,s]
					
					for (p in 1:env_obj$npart) {
					
						densities_smoothed[ p, 1, s] <- keep_finite(mvtnorm::dmvnorm(x=env_obj$Xpart_history_smoothed[i + 1,c("X","velocity"), s_ind, s], mean=env_obj$mk_curr_smoothed[,1,p,s], sigma=env_obj$Pk_curr_smoothed[,,1,p,s]))
						
					}
					
					if (env_obj$nstates > 1) { 
						#transitions into the actual behavior observed
						densities_smoothed[,1,s] <- densities_smoothed[,1,s] * transition_draws[,env_obj$Xpart_history_smoothed[ i+1, "lambda",s_ind, s] ]
					}
					
					if (env_obj$interact & length(possible_prev_temp_neibs)) {
					
						#make default =1
						env_obj$interact_intensity_draw_smoothed[,] <- 1
						
						#this has to be done separately for each particle and s_ind
						#determine fraction
						num_neibs <- matrix(0, ncol=env_obj$nstates, nrow=env_obj$npart)
												
						for (p in 1:env_obj$npart) {
							
							#now calculate the spatial distances
							if (nrow(neibs[[ p ]]) > 0) {
																					
								neib_dist <- abs(env_obj$Xpart_history_smoothed[i+1, "X", s_ind,s] - neibs[[ p ]][,"X",drop=FALSE])
								neibs_tmp <- neibs[[ p ]][ neib_dist <= env_obj$spat_radius,,drop=FALSE]
					
								#fraction of neighbors that are of each state
								if (nrow(neibs_tmp) > 0) { num_neibs[p,] <- table(factor(neibs_tmp[,"lambda"], levels=1:env_obj$nstates)) }
							}
				
						}#loop over particles	
				
						tmp <- rowSums(num_neibs)
													
						part_with_neibs[,s] <- (tmp >= env_obj$min_num_neibs)
						num_part_with_neibs <- sum(part_with_neibs[,s])
						neib_fracs_smoothed <- apply(num_neibs, 1, function(x) x/sum(x))
						neib_fracs_smoothed[ -part_with_neibs,] <- NA
			
						#if some neighbors
						if (num_part_with_neibs > 0) {
							env_obj$interact_intensity_draw_smoothed[,] <- 1		
							
							for (k in 1:(env_obj$nstates-1)) {
											
								env_obj$interact_intensity_draw_smoothed[part_with_neibs[,s], k] <- rlnorm(n=num_part_with_neibs, meanlog=env_obj$interact_mu_draw_smoothed[ part_with_neibs[,s] ,k] * neib_fracs_smoothed[ part_with_neibs[,s],k], sdlog=env_obj$interact_pars$known_sd[k])
							}
							env_obj$interact_intensity_draw_smoothed[part_with_neibs[,s],] <- keep_finite(env_obj$interact_intensity_draw_smoothed[part_with_neibs[,s],])
							#multiply by interaction draw, but condition on the behavior that ocurred before
							densities_smoothed[,,s] <- keep_finite(densities_smoothed[,,s] * keep_finite(env_obj$interact_intensity_draw_smoothed[ cbind(1:env_obj$npart, z_prev) ]))
							
						}
												
					
					}
					
					
					#sample one index and make it time i-1 of that indicator
					swts <- apply(densities_smoothed[,,s,drop=FALSE], 1, sum)
					if (env_obj$lowvarsample) {
						indices[ s, s_ind ] <- low_var_sample(wts=swts, M=1)
					}
					else {
						indices[ s, s_ind ] <- sample.int(n=env_obj$smooth_iter, prob=swts, size=1)
					}
					
				
				}
				env_obj$Xpart_history_smoothed[ i, ,, s] <- env_obj$Xpart_history[ i, c("X","velocity","lambda"), indices[s,], s]


				
			}
			else {
							
			
				env_obj$mk_prev_smoothed["velocity",,,s] <- env_obj$logv_angle_mu_draw
				
				
					
				for (p in 1:env_obj$npart) {
					for (k in 1:env_obj$nstates) {
						
						Fx_tmp <- keep_finite(env_obj$Fx(mk=env_obj$mk_prev_smoothed[,k,p,s], dtprev=env_obj$reg_dt))
						env_obj$Pk_curr_smoothed[,,k, p,s] <- keep_finite(as.matrix(Matrix::nearPD(keep_finite(keep_finite(keep_finite(Fx_tmp %*% env_obj$Pk_prev[,,k,p,s]) %*% t(Fx_tmp)) + env_obj$Qt[,,k,p,s]), ensureSymmetry=TRUE)$mat)) #R_{t+1}
								#Pk_curr_smoothed[1:2,1:2,k, p,s] <- as.matrix(Matrix::nearPD(Hx_tmp%*%Pk_prev_smoothed[,,k,p,s]%*%t(Hx_tmp) + Qt[1:2,1:2,k,p,s], ensureSymmetry=TRUE)$mat) #R_{t+1}
								
						env_obj$mk_curr_smoothed["X",k ,p,s] <- keep_finite(env_obj$h(mk=env_obj$mk_prev_smoothed[,k,p,s], dtprev=env_obj$reg_dt))
					}
				}
			
				for (s_ind in 1:env_obj$smooth_iter) {
					env_obj$mk_curr_smoothed["velocity",,,s] <- env_obj$Xpart_history_smoothed[i + 1,"velocity",s_ind,s]
					
					
											
					
					#density conditional on each potential behavior
					for (p in 1:env_obj$npart) {
						for (k in 1:env_obj$nstates) {
					
							densities_smoothed[ p, k, s] <- mvtnorm::dmvnorm(x=env_obj$Xpart_history_smoothed[i + 1,c("X","velocity"), s_ind, s], mean=env_obj$mk_curr_smoothed[,k,p,s], sigma=env_obj$Pk_curr_smoothed[,,k,p,s])
						}
					}
					
					
					densities_smoothed[,,s] <- keep_finite(densities_smoothed[,,s]) * transition_draws
					
					swts <- rowSums(densities_smoothed[,,s])
					
					if (env_obj$lowvarsample) {
						indices[s, s_ind ] <- low_var_sample(wts=swts, M=1)
					}
					else {
						indices[s, s_ind ] <- sample.int(n=env_obj$smooth_iter, size=1, prob=swts)
					}
					
					#choose behavior lambda based on the relative behavior weights of the chosen particle for s_ind
					env_obj$Xpart_history_smoothed[i, "lambda", s_ind, s] <- low_var_sample(wts=densities_smoothed[ indices[s, s_ind ],,s], M=1)
								
				}
				
				#now use overall weights to select other variables
				env_obj$Xpart_history_smoothed[ i,c("X","velocity") ,, s] <- env_obj$Xpart_history[ i, c("X","velocity"), indices[s,], s]

			}

			if (env_obj$smooth_parameters) {
				#resample parameters by smoothing weights (regardless of fix_smoothed_behaviors)
				env_obj$mu_smoothed[,,,s] <- env_obj$mu_smoothed[,,indices[s,],s]  
				env_obj$transition_mat_smoothed[[ s ]] <- env_obj$transition_mat_smoothed[[ s ]][ indices[s,] ] 
				env_obj$sigma_pars_smoothed[,,s] <- env_obj$sigma_pars_smoothed[indices[s,],,s]
				env_obj$Particle_errvar_smoothed[[ s ]] <- env_obj$Particle_errvar_smoothed[[ s ]][ indices[s,] ]

				if (env_obj$interact) {
					env_obj$spatial_interact_pars_smoothed[,,s] <- env_obj$spatial_interact_pars_smoothed[indices[s,],,s]
				}
			
			
			}	
				
				
			if (! ((i - 1) %in% smooth_steps[[ s ]])) { 
				env_obj$Xpart_history_smoothed[ i - 1, c("X","lambda","velocity"),, s] <- env_obj$Xpart_history[ i-1,c("X","lambda","velocity") ,indices[s,], s]  
					
			}	
		
			
		}#iterate over sharks	
			
	}#over time steps
	
	#do final fixing of the bearings and logvelocities to get them to actually line up

	for (s in env_obj$shark_names) {
		
		steps <- which(! is.na(env_obj$Xpart_history_smoothed[,"X",1,s]))
		steps <- steps[ (steps+1) %in% steps]
		dist_x <- abs(env_obj$Xpart_history_smoothed[steps+1,"X",,s] - env_obj$Xpart_history_smoothed[steps,"X",,s])
		env_obj$Xpart_history_smoothed[steps, "velocity",,s ] <- dist_x/env_obj$reg_dt
		
		
	}

	#these are meaningless in this case
	if (env_obj$smooth_parameters == FALSE) {
		rm(list=c("mu_smoothed", "transition_mat_smoothed", "sigma_pars_smoothed", "Particle_errvar_smoothed", "spatial_interact_pars_smoothed"), envir=env_obj)
		
	}	

	invisible(NULL)
		
}		