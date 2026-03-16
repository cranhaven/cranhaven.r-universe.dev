calculate_resampling_indices_EKF_interp_joint <- function(env_obj) {

	if (env_obj$do_trunc_adjust) {
			
		trunc_adjustment <- matrix(NA, ncol=env_obj$nstates, nrow=env_obj$npart) 

	}

	for (s in env_obj$sharks_with_obs) {
			
		tmp_mat <- matrix(1, ncol=env_obj$yobs_sharks[ s ] + (env_obj$nstates > 1) + (env_obj$interact==TRUE), nrow=env_obj$npart)
		colnames(tmp_mat) <- paste("t",1:ncol(tmp_mat))
		colnames(tmp_mat)[ 1:env_obj$yobs_sharks[ s ] ] <- paste("y", 1:env_obj$yobs_sharks[ s ], sep="_")
		
		
		if (env_obj$nstates > 1) {
			colnames(tmp_mat)[ env_obj$yobs_sharks[ s ] + 1] <- "trans"
			if (env_obj$interact==TRUE) {
				colnames(tmp_mat)[ env_obj$yobs_sharks[ s ] + 2] <- "interact"
			}
		}

		env_obj$densities_components[[ s ]] <- rep(list(tmp_mat), env_obj$nstates)
		
		prev_region <- env_obj$Xpart_history[env_obj$i-1,"region",,s]
		prev_z <- env_obj$lambda_matrix[,env_obj$i-1,s]
		
									
		if (env_obj$nstates > 1) {
		
							
			for (k in 1:env_obj$nstates) {
				
				for (p in 1:env_obj$npart) {
					env_obj$densities_components[[ s ]][[ k ]][p,"trans"] <- env_obj$transition_mat[[ s ]][[ p ]]$mat[[ prev_region[ p ] ]][ prev_z[ p ],k]
				}
				env_obj$densities_components[[ s ]][[ k ]][,"trans"] <- env_obj$densities_components[[ s ]][[ k ]][,"trans"] * env_obj$state_favor[ k ]
				
				#only use the first draw, the second is 1
				if (env_obj$interact & k < env_obj$nstates) {
					env_obj$densities_components[[ s ]][[ k ]][,"interact"] <- env_obj$interact_intensity_draw[,k,env_obj$i,s]
				}
						
			}
						
		}#transition probabilities	


		#always calculate the density because need to sample states 

		for (y in 1:env_obj$yobs_sharks[ s ]) {
			for (k in 1:env_obj$nstates) {
				for (p in 1:env_obj$npart) {

					env_obj$densities_components[[ s ]][[ k ]][p,y] <- pmax(1e-50, mvtnorm::dmvnorm(x=env_obj$ynext[ which(rownames(env_obj$ynext)==s)[ y ],c("X","Y")], mean=env_obj$MuY[[ s ]][,y,k,p], sigma=env_obj$SigY[[ s ]][,,y,k,p]))
				}
				
				env_obj$densities_components[[ s ]][[ k ]] <- keep_finite(env_obj$densities_components[[ s ]][[ k ]])
			
			}

			# if truncate, adjust the density at each of the intermediate points by the fraction inside
			if (env_obj$do_trunc_adjust) {
				for (k in 1:env_obj$nstates) {
					for (p in 1:env_obj$npart) {
				
						trunc_adjustment[p,k] <- fraction_inside(mu=env_obj$MuY[[ s ]][,y,k,p], cmat=env_obj$SigY[[ s ]][,,y,k,p], nsim=500, obj=env_obj)
					}
				
					trunc_adjustment[,k] <- pmax(1e-3, trunc_adjustment[,k])
					# division by fraction inside adjusts the density so it's truncated
					env_obj$densities_components[[ s ]][[ k ]][,y] <- env_obj$densities_components[[ s ]][[ k ]][,y] / trunc_adjustment[,k]
					
				}
				
			}
		
		}
		

	}#end calculating components by shark and 		

	# now multiply all for each particle, across observations (rows)
	env_obj$densities_bystate <- lapply(env_obj$densities_components[ env_obj$sharks_with_obs ], function(x) sapply(x, function(y) pmax(1e-50, apply(y, 1, prod))))
	
	# now convert to matrix	
	env_obj$densities_bystate <- lapply(env_obj$densities_bystate, function(x) matrix(x, nrow=env_obj$npart))
	

	if (env_obj$nstates > 1) { env_obj$densities_bystate <- lapply(env_obj$densities_bystate, function(x) t(apply(x, 1, function(x) pmax(1e-50, x*env_obj$state_favor)))) }
	
	
	#print(env_obj$densities_bystate[ env_obj$sharks_with_obs ])
	
	names(env_obj$densities_bystate) <- env_obj$sharks_with_obs
	#print(env_obj$densities_bystate)

	env_obj$densities <- sapply(env_obj$densities_bystate, function(x) rowSums(x[,drop=FALSE]))	
	colnames(env_obj$densities) <- env_obj$sharks_with_obs
	na_dens <- apply(env_obj$densities, 1, function(x) any(is.na(x)))
	
	if (any(na_dens) & env_obj$show_prints) {
		print(env_obj$densities[ na_dens,])
		#print(env_obj$densities_bystate[ na_dens,])
		for (s in env_obj$sharks_with_obs) {
			for (k in 1:env_obj$nstates) {
				print(env_obj$densities_components[[ s ]][[ k ]][ na_dens,])
			}
		}
	}
		
	
	rs <- rowSums(env_obj$densities, na.rm=TRUE)
	if (any(is.na(env_obj$densities))) { print(env_obj$densities); print(env_obj$densities_components[[ s ]]) }
	if (all(rs==0)) { rs <- rep(1, env_obj$npart) }
	#print(env_obj$densities_components[ env_obj$sharks_with_obs ])
	
	eff_size <- eff_ss(p=rowSums(pmax(env_obj$densities, 1e-15, na.rm=TRUE)))
	env_obj$eff_size_hist[env_obj$i, s] <- eff_size
	
	if (env_obj$show_prints) print(paste("Effective size is", round(eff_size, 1)))

	if (eff_size >= env_obj$npart * env_obj$neff_sample) {
		#if effective size is too big, check if any are on the final obs, in which case resample anyhow
		#if none, then end up not resampling any since eliminate all columns
		swo <- env_obj$sharks_with_obs[ env_obj$shark_final_obs[ env_obj$sharks_with_obs ] == env_obj$i]
		env_obj$densities <- env_obj$densities[ swo ]
	}
	
	env_obj$sharks_to_resample <- colnames(env_obj$densities)
	nsharks_resample <- length(env_obj$sharks_to_resample)
	
	env_obj$indices <- matrix(1, ncol=nsharks_resample, nrow=env_obj$npart)
	colnames(env_obj$indices) <- env_obj$sharks_to_resample	
	
	#if end up resampling
	if (nsharks_resample > 0) {
		
		if (env_obj$show_prints) {
			print("resampling...")
			if (env_obj$nsharks > 1) {
				print(env_obj$sharks_to_resample)
			}
		}
		#indices <- rep(0,env_obj$npart)
		#sample separately by shark
								
		iter <- 0
		#resample particles with weights (sum of densities across rows) for next particles
		#enforce that cant have all indices the same
		
		if (env_obj$interact) { dens_prob <- apply(env_obj$densities, 1, prod) }
		
		if (env_obj$lowvarsample==TRUE) { 
				
		
			while(any(apply(env_obj$indices, 2, function(y) length(unique(y))) ==1) & iter < 100) {
				
				
				if (env_obj$interact) { 
					#if interact, use the same indices for all sharks
					env_obj$indices <- matrix(low_var_sample(wts=dens_prob, M=env_obj$npart), ncol=length(env_obj$sharks_with_obs), nrow=env_obj$npart)
				}	
				else { 
					env_obj$indices <- apply(env_obj$densities, 2, function(y) low_var_sample(wts=y, M=env_obj$npart)) 
				}

				
				iter <- iter + 1
			}
		}	  
		else { 
			while(any(apply(env_obj$indices, 2, function(y) length(unique(y))) ==1) & iter < 100) {
				if (env_obj$interact) { 
					#if interact, use the same indices for all sharks
					env_obj$indices <- matrix(sample(x=1:env_obj$npart, size=env_obj$npart, prob=dens_prob, replace=TRUE), ncol=length(env_obj$sharks_with_obs), nrow=env_obj$npart)
				}	
				else { 
					env_obj$indices <- apply(env_obj$densities, 2, function(y) sample(x=1:env_obj$npart, size=env_obj$npart, prob=y, replace=TRUE)) 
				}
			  
				iter <- iter + 1
			}
		}
		
		colnames(env_obj$indices) <- env_obj$sharks_to_resample #env_obj$sharks_with_obs
		env_obj$resample_history[env_obj$i, env_obj$sharks_to_resample] <- apply(env_obj$indices, 2, function(x) length(unique(x))/env_obj$npart )
				
		
	}
	else{
		if (env_obj$show_prints) print("not resampling since effective size is above threshold")
	}

	invisible(NULL)
	
}