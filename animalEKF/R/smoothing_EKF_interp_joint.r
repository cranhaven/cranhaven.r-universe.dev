smoothing_EKF_interp_joint <- function(env_obj) {

	if (env_obj$nstates == 1 ) { env_obj$fix_smoothed_behaviors <- TRUE }
	
	env_obj$smooth_iter <- env_obj$npart
	
	n_ind <- ifelse(env_obj$fix_smoothed_behaviors, 1, env_obj$nstates)
	n_ind_names <- paste("nind", 1:n_ind, sep="")
	
	env_obj$mk_prev_smoothed <- env_obj$mk_curr_smoothed  <- array(NA, dim=c(4, n_ind, env_obj$npart, env_obj$nsharks), dimnames=list(c("X","Y","logv","bearing_rad"), n_ind_names, env_obj$pnames, env_obj$shark_names))
	
	if (env_obj$interact) { 
		env_obj$spatial_interact_pars_smoothed <- env_obj$spatial_interact_pars 
		env_obj$interact_intensity_draw_smoothed <- matrix(1, ncol=env_obj$nstates, nrow=env_obj$npart)
	}
	
	
	env_obj$transition_mat_smoothed <- env_obj$transition_mat
	env_obj$sigma_pars_smoothed <- env_obj$sigma_pars
	env_obj$tau_pars_smoothed <- env_obj$tau_pars
	env_obj$Particle_errvar_smoothed <- env_obj$Particle_errvar
	env_obj$XY_errvar_smoothed <- env_obj$XY_errvar
	env_obj$mu_smoothed <- env_obj$mu
	
	part_with_neibs <- matrix(FALSE, ncol=env_obj$nsharks, nrow=env_obj$npart)
	colnames(part_with_neibs) <- env_obj$shark_names
		
	
	
	#the smoothed one doesnt need to change
	env_obj$Pk_prev_smoothed <- env_obj$Pk_curr_smoothed <- env_obj$Pk_prev
	env_obj$logv_angle_mu_draw <- array(NA, dim=c(2, n_ind, env_obj$npart), dimnames=list(c("logv","turn"), n_ind_names, env_obj$pnames))
					
				
	#densities_smoothed <- rep(list(matrix(NA, ncol=1+(env_obj$nstates>1), nrow=env_obj$npart)), env_obj$nstates)
	
	transition_draws <- matrix(NA, ncol=env_obj$nstates, nrow=env_obj$npart)

	
	densities_smoothed <- array(NA, dim=c(env_obj$npart, n_ind, env_obj$nsharks), dimnames=list(env_obj$pnames, 1:n_ind, env_obj$shark_names))


	if (env_obj$lowvarsample) {
		indices <- t(sapply(1:env_obj$nsharks, function(zz) low_var_sample(wts=rep(1, env_obj$npart), M=env_obj$smooth_iter)))
	}
	else {
		indices <- t(sapply(1:env_obj$nsharks, function(zz) sample.int(n=env_obj$npart, size=env_obj$smooth_iter, replace=TRUE)))
	}
	rownames(indices) <- env_obj$shark_names
	

	
	#store the states
	var_subset <- c("X","Y","log_speed","bearing_rad","turn_rad","lambda","region", "speed")
	env_obj$Xpart_history_smoothed <- array(NA, dim=c(env_obj$N, 8, env_obj$smooth_iter, env_obj$nsharks), 
											dimnames=list(env_obj$Nnames, var_subset, 1:env_obj$smooth_iter, env_obj$shark_names))
	
	#can't use first step because need to condition on previous bearing each time
	smooth_steps <- env_obj$shark_valid_steps
	smooth_steps <- lapply(smooth_steps, function(x) x[ (x-1) %in% x ])
			
	#draws for variables
	for (i in (env_obj$N-1):1) {
	
		part_with_neibs[,] <- FALSE
		
		if ((i %%3)==0 & env_obj$show_prints) { print(paste("smoothing step",i)) }
		
		valid_sharks <- sapply(smooth_steps, function(x) i %in% x)
		valid_sharks <- names(valid_sharks[ valid_sharks == TRUE ])
		
		if (env_obj$interact) {
			temp_neib_range <- which((env_obj$t_reg < env_obj$t_reg[ i+1 ]) & (env_obj$t_reg >= (env_obj$t_reg[ i+1 ] - env_obj$time_radius)))
		}
		
		#this step will not be in 
		for (s in valid_sharks) {
			
			if (! ((i+1) %in% smooth_steps[[ s ]])) { 

				if (env_obj$lowvarsample) {	
					indices[s,] <- low_var_sample(wts=rep(1, env_obj$npart), M=env_obj$smooth_iter)
				}
				else {
					indices[s,] <- sample.int(n=env_obj$smooth_iter, replace=TRUE, size=env_obj$smooth_iter)
				}
				
				env_obj$Xpart_history_smoothed[ i+1, ,, s] <- env_obj$Xpart_history[ i+1, var_subset ,indices[s,], s]  
				
			}
			
								
								
			#if we want to keep the behaviors fixed as they were, take the previous behavior as fixed
			if (env_obj$fix_smoothed_behaviors) {
				z_prev <- env_obj$Xpart_history[ i, "lambda",, s]
				regions <- env_obj$Xpart_history[ i, "region",, s]
				
				#set up covariance matrices
				if (env_obj$nstates>1) {
					for (p in 1:env_obj$npart) {
						transition_draws[ p, ] <- MCMCpack::rdirichlet(n=1, alpha=env_obj$transition_mat[[ s ]][[ p ]]$dirichlet_pars[ regions[ p ], c(2*z_prev[ p ]-1, 2*z_prev[ p ]) ]) * env_obj$state_favor 
					}
				}
				
				cbs0 <- cbind(env_obj$state_names[z_prev], env_obj$pnames, s)							
	
				
				#only use position 1	
			
				#print(cbs0)

				env_obj$Qt["logv","logv",,,s][ cbind(z_prev, 1:env_obj$npart) ] <- MCMCpack::rinvgamma(n=env_obj$npart, env_obj$sigma_pars[,,s][ cbind(1:env_obj$npart, 2*z_prev-1) ], env_obj$sigma_pars[,,s][ cbind(1:env_obj$npart, 2*z_prev) ])
				#try(print(env_obj$Qt["logv","logv",,,,drop=FALSE][ cbs0 ]))
				#try(print(env_obj$Qt["logv","logv",,,s][ cbind(z_prev, 1:env_obj$npart) ]))
				env_obj$Qt["bearing_rad","bearing_rad",,,s][ cbind(z_prev, 1:env_obj$npart) ] <- MCMCpack::rinvgamma(n=env_obj$npart, env_obj$tau_pars[,,s][ cbind(1:env_obj$npart, 2*z_prev-1) ], env_obj$tau_pars[,,s][ cbind(1:env_obj$npart, 2*z_prev) ])
				
				
			}
			else {
				z_curr <- env_obj$Xpart_history_smoothed[ i+1, "lambda",, s]
				regions <- env_obj$Xpart_history_smoothed[ i+1, "region",, s]
				
				#if (any(is.na(z_curr))) { print("z"); print(z_curr) }

				#if (any(is.na(regions))) { print("region") ;print(regions) }
				
				#CHOOSE previous behavior scaled by the probability they 
				#set up covariance matrices
				if (env_obj$nstates>1) {
					for (p in 1:env_obj$npart) {
						
						transition_draws[ p, ] <- MCMCpack::rdirichlet(n=1, alpha=env_obj$transition_mat_smoothed[[ s ]][[ p ]]$dirichlet_pars[ regions[ p ], z_curr[ p ] + c(0,2) ]) * env_obj$state_favor 
					}
				}
				
					
				
				
				#set X location (and later Y and previous bearing)
				#do it for both states
				for (k in 1:env_obj$nstates) {
				
					env_obj$Qt["logv","logv",k,,s] <- MCMCpack::rinvgamma(n=env_obj$npart, env_obj$sigma_pars_smoothed[, 2*k-1,s], env_obj$sigma_pars_smoothed[, 2*k,s])
					env_obj$Qt["bearing_rad","bearing_rad",k,,s] <- MCMCpack::rinvgamma(n=env_obj$npart, env_obj$tau_pars_smoothed[, 2*k-1,s], env_obj$tau_pars_smoothed[,2*k,s])
				}
			
									
			}
			
			for (p in 1:env_obj$npart) {
																
				env_obj$Qt[1:2,1:2 ,,p,s] <- MCMCpack::riwish(v=env_obj$Particle_errvar_smoothed[[ s ]][[ p ]]$dof, S=env_obj$Particle_errvar_smoothed[[ s ]][[ p ]]$sig)
				#Qt[1:2,1:2 ,k,p,s] <- MCMCpack::riwish(v=env_obj$XY_errvar[[ s ]][[ p ]][[ k ]]$dof, S=env_obj$XY_errvar[[ s ]][[ p ]][[ k ]]$sig)
	
			}
			env_obj$Qt[1:2,1:2 ,,,s] <- keep_finite(env_obj$Qt[1:2,1:2 ,,p,s])
			
			for (k in 1:n_ind) {
				#set X location (and later Y and previous bearing)
				env_obj$mk_prev_smoothed[c("X","Y"),k,,s] <- env_obj$Xpart_history[ i,c("X","Y"),,s]
				env_obj$mk_prev_smoothed["bearing_rad",k,,s] <- env_obj$Xpart_history[ i-1,"bearing_rad",,s]
			
			}
			
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
						neibs[[ p ]] <- na.omit(matrix(apply(env_obj$Xpart_history_smoothed[ temp_neib_range, c("X","Y","lambda"), p,possible_prev_temp_neibs, drop=FALSE], MARGIN=2, FUN=rbind), ncol=3))
						colnames(neibs[[ p ]]) <- c("X","Y","lambda")
					}
					
				}
				
											
			}
			else {
				#do this so later condition works if interact=FALSE
				possible_prev_temp_neibs <- c()
			}
									
			if (env_obj$fix_smoothed_behaviors) {
			
				#only draw logv/turn from one behavior, that of z_prev, not both possible
				for (p in 1:env_obj$npart) {
					#logv_angle_mu_draw[,z_prev[ p ],p] <- mvtnorm::rmvnorm(n=1, mean=env_obj$mu[[ s ]][[ p ]][[ z_prev[ p ] ]], sigma=as.matrix(Qt[3:4, 3:4,z_prev[ p ],p,s]*env_obj$mu[[ s ]][[ p ]]$V[[ z_prev[ p ] ]]))
					env_obj$logv_angle_mu_draw[,1,p] <- mvtnorm::rmvnorm(n=1, mean=env_obj$mu[ z_prev[ p ],,"mu",p,s], sigma=as.matrix(env_obj$Qt[3:4, 3:4,z_prev[ p ],p,s] * env_obj$mu[ z_prev[ p ],,"V",p,s]))
				}
				
				env_obj$logv_angle_mu_draw["turn",,] <- normalize_angle(env_obj$logv_angle_mu_draw["turn",,])
				
				cbs <- cbind(n_ind_names[z_prev], env_obj$pnames, s)							
				
				env_obj$mk_prev_smoothed[ cbind("logv", cbs) ] <- env_obj$logv_angle_mu_draw["logv",1, ]
				env_obj$mk_prev_smoothed[ cbind("bearing_rad", cbs) ]  <- normalize_angle(env_obj$mk_prev_smoothed[ cbind("bearing_rad", cbs) ] + env_obj$logv_angle_mu_draw["turn",1, ])
				
				for (p in 1:env_obj$npart) {
					Fx_tmp <- keep_finite(env_obj$Fx(mk=env_obj$mk_prev_smoothed[, z_prev[ p ],p,s], dtprev=env_obj$reg_dt))														
					
					env_obj$Pk_curr_smoothed[,,1, p,s] <- keep_finite(as.matrix(Matrix::nearPD(keep_finite(keep_finite(keep_finite(Fx_tmp %*% env_obj$Pk_prev_smoothed[,,z_prev[ p ],p,s]) %*% t(Fx_tmp)) + env_obj$Qt[,,z_prev[ p ],p,s]), ensureSymmetry=TRUE)$mat)) #R_{t+1}
					
					
					#env_obj$Pk_curr_smoothed[,,1, p,s] <- as.matrix(Matrix::nearPD(Fx_tmp%*%env_obj$Pk_prev_smoothed[,,z_prev[ p ],p,s]%*%t(Fx_tmp) + Qt[,,z_prev[ p ],p,s], ensureSymmetry=TRUE)$mat) #R_{t+1}
					
					#do this to cut down on computation for env_obj$mk_curr_smoothed
					#this part stays the same
					env_obj$mk_curr_smoothed[c("X","Y"), 1,p,s] <- keep_finite(env_obj$h(mk=env_obj$mk_prev_smoothed[,z_prev[ p ], p,s], dtprev=env_obj$reg_dt))
										
				}
				
				
			
				for (s_ind in 1:env_obj$smooth_iter) {
					#this new part will depend on the turn angle and speed that actually happened
					env_obj$mk_curr_smoothed["logv",1,,s] <- env_obj$Xpart_history_smoothed[i+1,"log_speed",s_ind,s]
					env_obj$mk_curr_smoothed["bearing_rad",1,,s] <- normalize_angle(env_obj$mk_prev_smoothed[ cbind("bearing_rad", cbs) ] + env_obj$Xpart_history_smoothed[i+1,"turn_rad",s_ind,s])
					
					#the densities smoothed matrix gets overridden every time
					for (p in 1:env_obj$npart) {
					
						#env_obj$mk_curr_smoothed[,1,p,s] <- f(env_obj$mk=env_obj$mk_prev_smoothed[,z_prev[ pp,s], new_logv=env_obj$Xpart_history_smoothed[i+1,"logv",s_ind,s], 
						#							theta=env_obj$Xpart_history[i,"turn_rad",p,s], dtprev=reg_dt) 					
						#densities_smoothed[ p, ] <- mvtnorm::dmvnorm(x=env_obj$Xpart_history_smoothed[i+1,c("X","Y","logv","bearing_rad"), s_ind, s], mean=env_obj$mk_curr_smoothed[,1,p,s], sigma=env_obj$Pk_curr_smoothed[,,1,p,s])
						#densities_smoothed[ p,,s ] <- mvtnorm::dmvnorm(x=env_obj$Xpart_history_smoothed[i+1,c("X","Y","logv","bearing_rad"), s_ind, s], mean=env_obj$mk_curr_smoothed[,1,p,s], sigma=env_obj$Pk_curr_smoothed[,,1,p,s])
						
						#use wrapped normal here for the last component
						
						densities_smoothed[ p,,s ] <- keep_finite(mvtnorm::dmvnorm(x=env_obj$Xpart_history_smoothed[i+1,c("X","Y","log_speed"), s_ind, s], mean=env_obj$mk_curr_smoothed[1:3,1,p,s], sigma=env_obj$Pk_curr_smoothed[1:3,1:3,1,p,s]))
						densities_smoothed[ p,,s ] <- keep_finite(densities_smoothed[ p,,s ] * keep_finite(dwrpnorm_tmp(x=env_obj$Xpart_history_smoothed[i+1,"bearing_rad", s_ind, s], mean=env_obj$mk_curr_smoothed["bearing_rad",1,p,s], sd=sqrt(env_obj$Pk_curr_smoothed[4,4,1,p,s]))))
					}
				
					if (env_obj$nstates > 1) { 
						#transitions into the actual behavior observed
						densities_smoothed[,,s] <- densities_smoothed[,,s] * transition_draws[,env_obj$Xpart_history_smoothed[ i+1, "lambda",s_ind, s] ]
														
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
																					
								neib_dist <- dist_func(center=as.vector(env_obj$Xpart_history_smoothed[i+1,c("X","Y"), s_ind,s]), otherXY=neibs[[ p ]][,c("X","Y"),drop=FALSE])
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
											
								env_obj$interact_intensity_draw_smoothed[part_with_neibs[,s], k] <- keep_finite(rlnorm(n=num_part_with_neibs, meanlog=env_obj$interact_mu_draw_smoothed[ part_with_neibs[,s] ,k] * neib_fracs_smoothed[ part_with_neibs[,s],k], sdlog=env_obj$interact_pars$known_sd[k]))
							}
						
							#multiply by interaction draw, but condition on the behavior that ocurred before
							densities_smoothed[,,s] <- keep_finite(densities_smoothed[,,s] * env_obj$interact_intensity_draw_smoothed[ cbind(1:env_obj$npart, z_prev) ])
							
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

				#get everything except for lambda
				env_obj$Xpart_history_smoothed[ i, ,, s] <- env_obj$Xpart_history[ i, var_subset, indices[s,], s]
			
			}			
			else {

			
				#now allow behaviors to change
				for (k in 1:env_obj$nstates) {
					for (p in 1:env_obj$npart) {
						#logv_angle_mu_draw[,k,p] <- mvtnorm::rmvnorm(n=1, mean=env_obj$mu[[ s ]][[ p ]][[ k ]], sigma=as.matrix(Qt[3:4, 3:4,k,p,s]*env_obj$mu[[ s ]][[ p ]]$V[[ k ]]))
						env_obj$logv_angle_mu_draw[,k,p] <- mvtnorm::rmvnorm(n=1, mean=env_obj$mu_smoothed[k,,"mu",p,s], sigma=as.matrix(env_obj$Qt[3:4, 3:4,k,p,s] * env_obj$mu_smoothed[k,,"V",p,s]))
					}
				}
				env_obj$mk_prev_smoothed[ "logv",,,s] <- env_obj$logv_angle_mu_draw["logv",,]
				env_obj$mk_prev_smoothed[ "bearing_rad",,,s]  <- normalize_angle(env_obj$mk_prev_smoothed[ "bearing_rad",,,s] + env_obj$logv_angle_mu_draw["turn",,])
				
				for (p in 1:env_obj$npart) {
					for (k in 1:env_obj$nstates) {
						#Hx_tmp <- Hx(env_obj$mk=env_obj$mk_prev_smoothed[,k,p,s], dtprev=reg_dt)
					
						Fx_tmp <- keep_finite(env_obj$Fx(mk=env_obj$mk_prev_smoothed[,k,p,s], dtprev=env_obj$reg_dt))
						
																							
						env_obj$Pk_curr_smoothed[,,k, p,s] <- keep_finite(as.matrix(Matrix::nearPD(keep_finite(keep_finite(keep_finite(Fx_tmp %*% env_obj$Pk_prev_smoothed[,,k,p,s]) %*% t(Fx_tmp)) + env_obj$Qt[,,k,p,s]), ensureSymmetry=TRUE)$mat)) #R_{t+1}
						#env_obj$Pk_curr_smoothed[1:2,1:2,k, p,s] <- as.matrix(Matrix::nearPD(Hx_tmp%*%env_obj$Pk_prev_smoothed[,,k,p,s]%*%t(Hx_tmp) + Qt[1:2,1:2,k,p,s], ensureSymmetry=TRUE)$mat) #R_{t+1}
						
						env_obj$mk_curr_smoothed[c("X","Y"),k ,p,s] <- keep_finite(env_obj$h(mk=env_obj$mk_prev_smoothed[,k,p,s], dtprev=env_obj$reg_dt))
					}
				}
			
				for (s_ind in 1:env_obj$smooth_iter) {
					env_obj$mk_curr_smoothed["logv",,,s] <- env_obj$Xpart_history_smoothed[i+1,"log_speed",s_ind,s]
					env_obj$mk_curr_smoothed["bearing_rad",,,s] <- normalize_angle(env_obj$mk_prev_smoothed["bearing_rad",,,s] + env_obj$Xpart_history_smoothed[i+1,"turn_rad",s_ind,s])
					
					for (k in 1:env_obj$nstates) {
						for (p in 1:env_obj$npart) {
						
							#env_obj$mk_curr_smoothed[,1,p,s] <- f(env_obj$mk=env_obj$mk_prev_smoothed[,z_prev[ pp,s], new_logv=env_obj$Xpart_history_smoothed[i+1,"logv",s_ind,s], 
							#							theta=env_obj$Xpart_history[i,"turn_rad",p,s], dtprev=reg_dt) 					
							
							#use wrapped normal
							densities_smoothed[ p,k, s] <- keep_finite(mvtnorm::dmvnorm(x=env_obj$Xpart_history_smoothed[i+1, c("X","Y","log_speed"), s_ind, s], mean=env_obj$mk_curr_smoothed[1:3,k,p,s], sigma=env_obj$Pk_curr_smoothed[1:3,1:3,k,p,s]))
							densities_smoothed[ p,k, s] <- keep_finite(densities_smoothed[ p,k, s] * keep_finite(dwrpnorm_tmp(x=env_obj$Xpart_history_smoothed[i+1,"bearing_rad", s_ind, s], mean=env_obj$mk_curr_smoothed["bearing_rad",k,p,s], sd=sqrt(env_obj$Pk_curr_smoothed[4,4,k,p,s]))))
							
							#densities_smoothed[ p,k ] <- mvtnorm::dmvnorm(x=env_obj$Xpart_history_smoothed[i+1,c("X","Y"), s_ind, s], mean=env_obj$mk_curr_smoothed[1:2,k,p,s], sigma=env_obj$Pk_curr_smoothed[1:2,1:2,k,p,s])
			
						}
					}
				
					#densities_smoothed <- t(apply(densities_smoothed*transition_draws, 1, function(x) x*state_favor))
					densities_smoothed[,,s] <- densities_smoothed[,,s] * transition_draws
					#undo the state_favor when choosing the behaviors
					
					if (env_obj$interact & length(possible_prev_temp_neibs)) {
					
						#make default =1
						env_obj$interact_intensity_draw_smoothed[,] <- 1
						
						#this has to be done separately for each particle and s_ind
						#determine fraction
						num_neibs <- matrix(0, ncol=env_obj$nstates, nrow=env_obj$npart)
												
						for (p in 1:env_obj$npart) {
							
							#now calculate the spatial distances
							if (nrow(neibs[[ p ]]) > 0) {
																					
								neib_dist <- dist_func(center=as.vector(env_obj$Xpart_history_smoothed[i+1,c("X","Y"), s_ind,s]), otherXY=neibs[[ p ]][,c("X","Y"),drop=FALSE])
								neibs_tmp <- neibs[[ p ]][ neib_dist <= env_obj$spat_radius,,drop=FALSE]
					
								#fraction of neighbors that are of each state
								if (nrow(neibs_tmp) > 0) { num_neibs[p,] <- table(factor(neibs_tmp[,"lambda"], levels=1:env_obj$nstates)) }
							}
				
						}#loop over particles	
				
						tmp <- rowSums(num_neibs)
													
						part_with_neibs[,s] <- (tmp >= env_obj$min_num_neibs)
						num_part_with_neibs <- sum(part_with_neibs[,s])
						neib_fracs_smoothed <- apply(num_neibs, 1, function(x) x/sum(x))
						neib_fracs_smoothed[ -part_with_neibs[,s],] <- NA
			
						#if some neighbors
						if (num_part_with_neibs > 0) {
							env_obj$interact_intensity_draw_smoothed[,] <- 1		
							
							for (k in 1:(env_obj$nstates-1)) {
											
								env_obj$interact_intensity_draw_smoothed[part_with_neibs[,s], k] <- keep_finite(rlnorm(n=num_part_with_neibs, meanlog=env_obj$interact_mu_draw_smoothed[ part_with_neibs[,s] ,k] * neib_fracs_smoothed[ part_with_neibs[,s], k], 
																										   sdlog=env_obj$interact_pars$known_sd[k]))
							}
						
							#multiply by interaction draw, don;t condition on the behavior that ocurred before
							densities_smoothed[,,s] <- keep_finite(densities_smoothed[,,s] * env_obj$interact_intensity_draw_smoothed)
							
						}
												
					
					}#end interact
					
					
					
					#densities_smoothed <- t(apply(densities_smoothed, 1, function(x) x/state_favor))
					
					#sample one index and make it time i-1 of that indicator
					swts <- rowSums(densities_smoothed[,,s])
					
					if (env_obj$lowvarsample) {
						indices[s, s_ind ] <- low_var_sample(wts=swts, M=1)
					}
					else {
						indices[s, s_ind ] <- sample.int(n=env_obj$smooth_iter, size=1, prob=swts)
					}
					
					env_obj$Xpart_history_smoothed[i, "lambda", s_ind, s] <- low_var_sample(wts=densities_smoothed[ indices[s, s_ind ],, s], M=1)
				
				}
				
				#resample so use 
				#env_obj$transition_mat_smoothed[[ s ]] <- env_obj$transition_mat_smoothed[[ s ]][ indices[s,] ]
				#mu_smoothed[[ s ]] <- mu_smoothed[[ s ]][ indices[s,] ]
				#env_obj$tau_pars_smoothed[,,s] <- env_obj$tau_pars_smoothed[ indices[s,],,s]
				#env_obj$sigma_pars_smoothed[,,s] <- env_obj$sigma_pars_smoothed[ indices[s,],,s]
				
				
				#if (interact) { env_obj$spatial_interact_pars_smoothed[,,s] <- env_obj$spatial_interact_pars_smoothed[indices[s,],,s] }
				
				env_obj$Xpart_history_smoothed[ i, ,, s] <- env_obj$Xpart_history[ i, var_subset, indices[s,], s]
			
			}
			
			#resample parameters by smoothing weights (regardless of fix_smoothed_behaviors)
			if (env_obj$smooth_parameters) {
				
				env_obj$mu_smoothed[,,,,s] <- env_obj$mu_smoothed[,,,indices[s,],s]  
				env_obj$transition_mat_smoothed[[ s ]] <- env_obj$transition_mat_smoothed[[ s ]][ indices[s,] ] 
				env_obj$sigma_pars_smoothed[,,s] <- env_obj$sigma_pars_smoothed[indices[s,],,s]
				env_obj$tau_pars_smoothed[,,s] <- env_obj$tau_pars_smoothed[indices[s,],,s]
				env_obj$Particle_errvar_smoothed[[ s ]] <- env_obj$Particle_errvar_smoothed[[ s ]][ indices[s,] ]


				if (env_obj$interact) {
					env_obj$spatial_interact_pars_smoothed[,,s] <- env_obj$spatial_interact_pars_smoothed[indices[s,],,s]
				}
		
			}
			
			

			if (! ((i-1) %in% smooth_steps)) { 
				#print("adding missing")
				#print(env_obj$Xpart_history[ i-1,c("X","Y","logv","bearing_rad","turn_rad","lambda","region") ,1, s])  
				env_obj$Xpart_history_smoothed[ i-1, ,, s] <- env_obj$Xpart_history[ i-1, var_subset,indices[s,], s]  
			
			}						

			
		}#end over sharks
		
	
	}#iterate over time	

	
					
	#do final fixing of the bearings and logvelocities to get them to actually line up

	for (s in env_obj$shark_names) {
		
		steps <- which(! is.na(env_obj$Xpart_history_smoothed[,"X",1,s]))
		steps <- steps[ (steps+1) %in% steps]
		steps_turn <- steps[ (steps-1) %in% steps]
		dxy <- env_obj$Xpart_history_smoothed[steps+1,c("X","Y"),,s] - env_obj$Xpart_history_smoothed[steps,c("X","Y"),,s]
		dist_xy <- apply(dxy, c(1,3), function(x) sqrt(sum(x^2)))
		env_obj$Xpart_history_smoothed[steps, "log_speed",,s ] <- log_safe(dist_xy/env_obj$reg_dt)
		env_obj$Xpart_history_smoothed[ steps, "bearing_rad",,s ] <- normalize_angle(atan2(y=dxy[,"Y",], x=dxy[,"X",]))
		env_obj$Xpart_history_smoothed[ steps+1, "turn_rad",,s] <- normalize_angle(env_obj$Xpart_history_smoothed[ steps+1, "bearing_rad",,s] - env_obj$Xpart_history_smoothed[ steps, "bearing_rad",,s])

		
	}
	
	env_obj$Xpart_history_smoothed[,"speed",,] <- exp_safe(env_obj$Xpart_history_smoothed[,"log_speed",,])
	
	if (env_obj$smooth_parameters == FALSE) {
		rm(list=c("mu_smoothed", "transition_mat_smoothed", "sigma_pars_smoothed", "tau_pars_smoothed", "Particle_errvar_smoothed", "spatial_interact_pars_smoothed"), envir=env_obj)
		
	}

	invisible(NULL)

}
		