calculate_resampling_indices_EKF_1d_interp_joint <- function(env_obj) {						
	
	for (s in env_obj$sharks_with_obs) {
			
		#tmp_mat <- matrix(1, ncol=yobs_sharks[ s ] +(nstates>1) + (interact==TRUE), nrow=npart)
		tmp_mat <- matrix(1, ncol=env_obj$yobs_sharks[ s ] + (env_obj$nstates > 1) + (env_obj$interact==TRUE), nrow=env_obj$npart)
		colnames(tmp_mat) <- paste("t",1:ncol(tmp_mat))
		colnames(tmp_mat)[ 1:env_obj$yobs_sharks[ s ] ] <- paste("y", 1:env_obj$yobs_sharks[ s ], sep="_")
		
		if (env_obj$nstates > 1) {
			colnames(tmp_mat)[ env_obj$yobs_sharks[ s ] + 1 ] <- "trans"
			if (env_obj$interact) {
				colnames(tmp_mat)[ env_obj$yobs_sharks[ s ] + 2 ] <- "interact"
			}
		}
		env_obj$densities_components[[ s ]] <- rep(list(tmp_mat), env_obj$nstates)
		
		#prev_region <- Xpart_history[i-1,"region",,s]
		prev_z <- env_obj$lambda_matrix[,env_obj$i-1,s]
		
									
		if (env_obj$nstates >1 ) {
			for (k in 1:env_obj$nstates) {
				
				for (p in 1:env_obj$npart) {
					env_obj$densities_components[[ s ]][[ k ]][p,"trans"] <- env_obj$transition_mat[[ s ]][[ p ]]$mat[ prev_z[ p ],k]
				}
				env_obj$densities_components[[ s ]][[ k ]][,"trans"] <- env_obj$densities_components[[ s ]][[ k ]][,"trans"] * env_obj$state_favor[ k ]
				
			}
			
		}#transition probabilities	
		
		#print(MuY[[ s ]])
		#print(SigY[[ s ]])
		#always calculate the density because need to sample states 
		for (k in 1:env_obj$nstates) {
			for (p in 1:env_obj$npart) {
			#multiply by transition probabilities INTO each state lambda_{t+1} from lambda_t
			#remember, here lambda_matrix[p, i]== lambda_t, not lambda_{t+1}

				for (y in 1:env_obj$yobs_sharks[ s ]) {
					#print(mvtnorm::dmvnorm(x=ynext[ which(rownames(ynext)==s)[ y ],c("X","Y")], mean=MuY[[ s ]][,y,k,p], sigma=SigY[[ s ]][,,y,k,p]))
					#print(ynext[ which(rownames(ynext)==s)[ y ],c("X","Y")])
					#print(MuY[[ s ]][,y,k,p])
					#print(SigY[[ s ]][,,y,k,p])
					#densities_components[[ s ]][[ k ]][p,y] <- mvtnorm::dmvnorm(x=ynext[ which(rownames(ynext)==s)[ y ],c("X","Y")], mean=MuY[[ s ]][,y,k,p], sigma=SigY[[ s ]][,,y,k,p])
					
					env_obj$densities_components[[ s ]][[ k ]][p,y] <- pmax(1e-50, dnorm(x=env_obj$ynext[ which(rownames(env_obj$ynext)==s)[ y ],"X"], mean=env_obj$MuY[[ s ]][y,k,p], sd=sqrt(env_obj$SigY[[ s ]][y,k,p])))

				}
				
				if (env_obj$interact & k < env_obj$nstates) {
					env_obj$densities_components[[ s ]][[ k ]][,"interact"] <- env_obj$interact_intensity_draw[,k,env_obj$i,s]
				}
				
			}

					
			env_obj$densities_components[[ s ]][[ k ]] <- keep_finite(env_obj$densities_components[[ s ]][[ k ]])
		}	

	}#end calculating components by shark and 		
	#print(densities_components[[ s ]])
	#print(densities_components[ sharks_with_obs ])

		#not sure if should multiply or add yobs then multiply
		#need to multiply by final transition probabilities
		#if (nregions >1) { densities <- sapply(densities_components, function(x) apply(x, 1, function(y) sum(y[1:yobs])*y[yobs+1])) }
		#else { densities <- sapply(densities_components, function(x) apply(x, 1, sum)) }
	env_obj$densities_bystate <- lapply(env_obj$densities_components[ env_obj$sharks_with_obs ], function(x) sapply(x, function(y) pmax(1e-50, apply(y, 1, prod))))
	env_obj$densities_bystate <- lapply(env_obj$densities_bystate[ env_obj$sharks_with_obs ], function(x) matrix(x, nrow=env_obj$npart))	

	names(env_obj$densities_bystate) <- env_obj$sharks_with_obs
	
	env_obj$densities <- sapply(env_obj$densities_bystate, function(x) rowSums(x[,drop=FALSE]))	
	colnames(env_obj$densities) <- env_obj$sharks_with_obs

	na_dens <- apply(env_obj$densities,1, function(x) any(is.na(x)))

	if (any(na_dens) & env_obj$show_prints) {
		print(env_obj$densities[ na_dens,])
		#print(densities_bystate[ na_dens,])
		for (s in env_obj$sharks_with_obs) {
			for (k in 1:env_obj$nstates) {
				print(env_obj$densities_components[[ s ]][[ k ]][ na_dens,])
			}
		}
	}
		

	rs <- rowSums(env_obj$densities)
	if (all(rs==0)) { rs <- rep(1, env_obj$npart) }
	#print(densities_components[ sharks_with_obs ])

	 
	eff_size <- eff_ss(p=rowSums(pmax(env_obj$densities, 1e-15, na.rm=TRUE)))	 
	env_obj$eff_size_hist[env_obj$i, s] <- eff_size

	#if don't fall below effective size threshold, only resample shakrs with final obs

	if (eff_size >= env_obj$npart * env_obj$neff_sample) {
		swo <- env_obj$sharks_with_obs[ env_obj$shark_final_obs[ env_obj$sharks_with_obs ] == env_obj$i]
		env_obj$densities <- env_obj$densities[ swo ]
	}

	
	env_obj$sharks_to_resample <- colnames(env_obj$densities)
	nsharks_resample <- length(env_obj$sharks_to_resample)
	
		
	env_obj$indices <- matrix(1, ncol=nsharks_resample, nrow=env_obj$npart, byrow=FALSE)
	colnames(env_obj$indices) <- env_obj$sharks_to_resample	

	if (nsharks_resample > 0) {

		if (env_obj$show_prints) {
			print("resampling...")
			if (env_obj$nsharks > 1) {
				print(env_obj$sharks_to_resample)
			}
		}
		iter <- 0
		#resample particles with weights (sum of densities across rows) for next particles
	   
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
				env_obj$indices <- matrix(env_obj$indices, ncol=nsharks_resample, nrow=env_obj$npart)
			}
		}	  
		else { 
			while(any(apply(env_obj$indices, 2, function(y) length(unique(y))) ==1) & iter < 100) {
			
				if (env_obj$interact) { 
					#if interact, use the same indices for all sharks
					env_obj$indices <- matrix(sample(x=1:env_obj$npart, size=env_obj$npart, prob=dens_prob, replace=TRUE), ncol=length(env_obj$sharks_with_obs), nrow=env_obj$npart)
				}	
				else{
					env_obj$indices <- apply(env_obj$densities, 2, function(y) sample(x=1:env_obj$npart, size=env_obj$npart, prob=y, replace=TRUE))
				}
				
				iter <- iter + 1
				env_obj$indices <- matrix(env_obj$indices, ncol=nsharks_resample, nrow=env_obj$npart)
			}
		}	  
		

		colnames(env_obj$indices) <- env_obj$sharks_to_resample
		env_obj$resample_history[env_obj$i, env_obj$sharks_to_resample] <- apply(env_obj$indices, 2, function(x) length(unique(x))/env_obj$npart )
		
	}
	else{
		if (env_obj$show_prints) print("not resampling since effective size is above threshold")
	}
	
	invisible(NULL)
}
		