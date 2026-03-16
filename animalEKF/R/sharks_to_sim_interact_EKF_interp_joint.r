sharks_to_sim_interact_EKF_interp_joint <- function(env_obj) {

	#set to 1 by default	
	env_obj$interact_intensity_draw[,, env_obj$i, env_obj$s] <- 1			
	#look for whether previous observations of other sharks in temporal window
	
	possible_prev_temp_neibs <- apply(env_obj$Xpart_history[ env_obj$temp_neib_range, "lambda", 1, env_obj$shark_names[ env_obj$shark_names != env_obj$s ], drop=FALSE], 4, function(x) any(! is.na(x)))
	#names of possible neighboring sharks previously
	possible_prev_temp_neibs <- names(possible_prev_temp_neibs[ possible_prev_temp_neibs ])
	
	
	if (length(possible_prev_temp_neibs)) {
		#collect these observations BY PARTICLE and see which ones are spatial neighbors 
		
		num_neibs <- matrix(0, ncol=env_obj$nstates, nrow=env_obj$npart)
		
		for (p in 1:env_obj$npart) {
										
			neibs <- matrix(apply(env_obj$Xpart_history[ env_obj$temp_neib_range, c("X","Y","lambda"), p, possible_prev_temp_neibs, drop=FALSE], MARGIN=2, FUN=rbind), ncol=3)
			colnames(neibs) <- c("X","Y","lambda")
			
			#ignore all other sharks simulated becuase dont do them in a particular order							
			
			neibs <- na.omit(neibs)
			
			#now calculate the spatial distances
			if (nrow(neibs) > 0) {
				#print(Xpart[p,c("X","Y"),k,"curr", env_obj$s])
				#print(neibs)
												
				neib_dist <- dist_func(center=as.vector(env_obj$Xpart_history[ env_obj$i-1, c("X","Y"),p, env_obj$s]), otherXY=neibs[,c("X","Y"),drop=FALSE])
				
				neibs <- neibs[ neib_dist <= env_obj$spat_radius,,drop=FALSE]
				
				#fraction of neighbors that are of each state
				if (nrow(neibs) > 0 ) { num_neibs[p,] <- table(factor(neibs[,"lambda"], levels=1:env_obj$nstates)) }
			}
			
		}#loop over particles		
		
		tmp <- rowSums(num_neibs)
		
		
		env_obj$part_with_neibs[, env_obj$s] <- (tmp >= env_obj$min_num_neibs)
		num_part_with_neibs <- sum(env_obj$part_with_neibs[, env_obj$s])
		
		if (env_obj$show_prints) {
			print(paste("shark", env_obj$s,"maximum neighborhood size is", max(tmp)))
			print(paste("number with neighbors:", num_part_with_neibs))
		}
	
		if (num_part_with_neibs > 0) {
		
			#print("some neighbors")
			#this is eta_k
			# interact_mu_draw <- cbind(rnorm(n=env_obj$npart, mean=spatial_interact_pars[,"mu1", env_obj$s], sd=1/sqrt(spatial_interact_pars[,"precision1", env_obj$s])),
									  # rnorm(n=env_obj$npart, mean=spatial_interact_pars[,"mu2", env_obj$s], sd=1/sqrt(spatial_interact_pars[,"precision2", env_obj$s])))

			
			env_obj$interact_mu_draw <- keep_finite(matrix(rnorm(n=env_obj$npart*(env_obj$nstates - 1), mean=env_obj$spatial_interact_pars[, env_obj$mu_names, env_obj$s], 
													sd=1/sqrt(env_obj$spatial_interact_pars[,env_obj$prec_names, env_obj$s])), ncol=env_obj$nstates-1, byrow=TRUE))
														
															  
		#	print("original mu summary")
		#	print(apply(interact_mu_draw, 2, summary))
		#	print(num_neibs[ part_with_neibs,,drop=FALSE])
		#	print(apply(num_neibs[ part_with_neibs,,drop=FALSE], 1, function(x) x/sum(x)))
			
			
			env_obj$neib_fracs[env_obj$part_with_neibs[, env_obj$s],, env_obj$i, env_obj$s] <- t(apply(num_neibs[ env_obj$part_with_neibs[, env_obj$s],,drop=FALSE], 1, function(x) x/sum(x)))
			#print(neib_fracs[part_with_neibs,, env_obj$i, env_obj$s])
			
			
			#print(head(neib_fracs[ part_with_neibs,, env_obj$i, env_obj$s]))					
		
			for (k in 1:(env_obj$nstates-1)) {
				
												
				env_obj$interact_intensity_draw[env_obj$part_with_neibs[, env_obj$s], k, env_obj$i, env_obj$s ] <- keep_finite(rlnorm(n=num_part_with_neibs, 
																																	  meanlog=env_obj$interact_mu_draw[ env_obj$part_with_neibs[, env_obj$s] ,k]*env_obj$neib_fracs[ env_obj$part_with_neibs[, env_obj$s],k, env_obj$i, env_obj$s],
																																	  sdlog=env_obj$interact_pars$known_sd[k]))
												
			}
			
			
			
			env_obj$spatial_interact_mu_history[env_obj$i,, env_obj$s] <- apply(env_obj$interact_mu_draw[env_obj$part_with_neibs[, env_obj$s],,drop=FALSE], 2, median)
			
			env_obj$spatial_interact_intensity_history[env_obj$i,, env_obj$s] <- apply(env_obj$interact_intensity_draw[env_obj$part_with_neibs[, env_obj$s], -env_obj$nstates, env_obj$i, env_obj$s, drop=FALSE ], 2, median)
							
		}
	}
	
	invisible(NULL)

}


