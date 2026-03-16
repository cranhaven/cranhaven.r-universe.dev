sharks_with_obs_interact_EKF_interp_joint <- function(env_obj) {

	env_obj$interact_intensity_draw[,, env_obj$i, env_obj$sharks_with_obs] <- 1

	for (s in env_obj$sharks_with_obs) {
		
		#look for whether previous observations of other sharks in temporal window
		#interact_intensity_draw[,,i,s] <- 1
		possible_prev_temp_neibs <- apply(env_obj$Xpart_history[ env_obj$temp_neib_range, "lambda", 1, env_obj$shark_names[ env_obj$shark_names != s ], drop=FALSE], 4, function(x) any(! is.na(x)))
		#names of possible neighboring sharks previously
		possible_prev_temp_neibs <- names(possible_prev_temp_neibs[ possible_prev_temp_neibs ])
		
		#sapply(env_obj$Xpart_history[ env_obj$sharks_with_obs[ env_obj$sharks_with_obs != s ] ], function(x) all(is.na(x[[ 1 ]][env_obj$temp_neib_range,"lambda"])))
		
		if (length(possible_prev_temp_neibs)) {
			#collect these observations BY PARTICLE and see which ones are spatial neighbors 
			
			env_obj$num_neibs[,,s] <- 0 

			for (p in 1:env_obj$npart) {

				#neibs <- env_obj$Xpart_history[ env_obj$temp_neib_range, c("X","Y","lambda"), p,possible_prev_temp_neibs, drop=]
				#print(neibs)
				#print(env_obj$Xpart_history[ env_obj$temp_neib_range, c("X","Y","lambda"), p,possible_prev_temp_neibs, drop=FALSE])
				#print(plyr::adply(env_obj$Xpart_history[ env_obj$temp_neib_range, c("X","Y","lambda"), p,possible_prev_temp_neibs, drop=FALSE], c(1,3)))
				neibs <- matrix(apply(env_obj$Xpart_history[ env_obj$temp_neib_range, c("X","Y","lambda"), p, possible_prev_temp_neibs, drop=FALSE], MARGIN=2, FUN=rbind), ncol=3)
				colnames(neibs) <- c("X","Y","lambda")
				#now add 'curr' observations for all other sharks just simulated
				#remember the 'curr' X-Y is the same for both states
				
				if (env_obj$i > 1) {
				
					for (k in 1:env_obj$nstates) {	
						if (length(env_obj$sharks_with_obs)>1) {
						
							tmp1 <- matrix( t(env_obj$mk_prev[ c("X","Y"), k, p, env_obj$sharks_with_obs[ env_obj$sharks_with_obs != s ] ]), ncol=2)
							colnames(tmp1) <- c("X","Y")
							tmp1 <- cbind(tmp1, lambda=k)
																										
							#other sharks with obs
							neibs <- rbind(neibs, tmp1)
						
						}

					}
					
					#other sharks simulated
					tmp2 <- matrix(t(env_obj$Xpart_history[ env_obj$i, c("X","Y","lambda"), p, env_obj$sharks_to_sim ]), ncol=3)
					colnames(tmp2) <-  c("X","Y","lambda")
					neibs <- rbind(neibs, tmp2)					
					
				}
				
				neibs <- na.omit(neibs)
				
				#now calculate the spatial distances
				if (nrow(neibs) > 0) {
					#print(Xpart[p,c("X","Y"),k,"curr",s])
					#print(neibs)
					neib_dist <- dist_func(center=as.vector(env_obj$mk_prev[c("X","Y"), k, p, s]), otherXY=neibs[,c("X","Y"),drop=FALSE])
					
					neibs <- neibs[ neib_dist <= env_obj$spat_radius,,drop=FALSE]
					
					#fraction of neighbors that are of each state
					if (nrow(neibs) > 0) { 

						env_obj$num_neibs[p,,s] <- as.vector(table(factor(neibs[,"lambda", drop=FALSE], levels=1:env_obj$nstates))) 
					}
				
				}
			}#loop over particles		
			
			tmp <- rowSums(env_obj$num_neibs[,,s,drop=FALSE])
			
			env_obj$part_with_neibs[,s] <- (tmp >= env_obj$min_num_neibs)
			
			#print(part_with_neibs)
			num_part_with_neibs <- sum(env_obj$part_with_neibs[,s])
			
			if (env_obj$show_prints) {
				print(paste("shark",s,"maximum neighborhood size is", max(tmp)))
				print(paste("number with neighbors:", num_part_with_neibs))
			}
			#make it into fractions
			#this is the p_k
			
			#simulate from
			#these are the rho_k
			if (num_part_with_neibs > 0) {
				
				#this is eta_k
				#interact_mu_draw <- cbind(rnorm(n=env_obj$npart, mean=spatial_interact_pars[,"mu1",s], sd=1/sqrt(spatial_interact_pars[,"precision1",s])),
				#						  rnorm(n=env_obj$npart, mean=spatial_interact_pars[,"mu2",s], sd=1/sqrt(spatial_interact_pars[,"precision2",s])))

				interact_mu_draw <- keep_finite(matrix(rnorm(n=env_obj$npart*(env_obj$nstates - 1), mean=env_obj$spatial_interact_pars[,env_obj$mu_names,s],
															sd=1/sqrt(env_obj$spatial_interact_pars[,env_obj$prec_names,s])), ncol=env_obj$nstates - 1, byrow=TRUE))
																	
				#print(env_obj$num_neibs[ env_obj$part_with_neibs[,s],,s, drop=FALSE])													
				#print(t(apply(env_obj$num_neibs[ env_obj$part_with_neibs[,s],,s, drop=FALSE], 1, function(x) x/sum(x))))
				
				env_obj$neib_fracs[env_obj$part_with_neibs[,s],,env_obj$i,s] <- t(apply(env_obj$num_neibs[ env_obj$part_with_neibs[,s],,s, drop=FALSE], 1, function(x) x/sum(x)))
									
												
			
				for (k in 1:(env_obj$nstates-1)) {
					
					env_obj$interact_intensity_draw[env_obj$part_with_neibs[,s], k, env_obj$i, s ] <- keep_finite(rlnorm(n=num_part_with_neibs, meanlog=interact_mu_draw[ env_obj$part_with_neibs[,s] ,k]*env_obj$neib_fracs[ env_obj$part_with_neibs[,s], k, env_obj$i,s],
																											 sdlog=env_obj$interact_pars$known_sd[k]))
											
				}
						
				
				
				#print("mu summary for particles with neighbors")
				#print(apply(interact_mu_draw[ part_with_neibs,,drop=FALSE], 2, summary)) 
				#print("rho summary for particles with neighbors")
				#print(apply(interact_intensity_draw[ part_with_neibs,,i,s,drop=FALSE], 2, summary)) 	
				
				env_obj$spatial_interact_mu_history[env_obj$i,,s] <- apply(interact_mu_draw[env_obj$part_with_neibs[,s],,drop=FALSE], 2, median)
				
				env_obj$spatial_interact_intensity_history[env_obj$i,,s] <- apply(env_obj$interact_intensity_draw[env_obj$part_with_neibs[,s], -env_obj$nstates, env_obj$i, s, drop=FALSE ], 2, median)
				
				
				# rho_k <- interact_intensity_draw[,,i,s]
				
				# print("intensity parameter summary")
				# print(apply(rho_k, 2, summary))
												
				# #update priors of normal distribution
				# mu0 <- spatial_interact_pars[,c("mu1","mu2") ,s]
				# tau0 <- spatial_interact_pars[,c("precision1","precision2") ,s]
				
				
				# spatial_interact_pars[,c("precision1","precision2") ,s]  <- tau_vals*(neib_fracs^2) + tau0
				# spatial_interact_pars[,c("mu1","mu2") ,s] <- (log(rho_k)*neib_fracs[,,i,s] + tau0)/spatial_interact_pars[,c("precision1","precision2") ,s]
			}	
		}#end if possible
	}#loop over sharks
	
	invisible(NULL)

}	
	