sharks_to_sim_update_EKF_interp_joint <- function(env_obj) {

	for (s in env_obj$sharks_to_sim) {

		z <- env_obj$lambda_matrix[,env_obj$i, s]
		prev_region <- env_obj$Xpart_history[env_obj$i-1, "region",,s]
		prev_z <- env_obj$Xpart_history[env_obj$i-1, "lambda",,s]
		
		#t here is the previous t
		tis <- env_obj$Xpart_history[env_obj$i-1 , "time_in_state",, s]
		
			
		newV <- env_obj$Xpart_history[env_obj$i, c("log_speed","turn_rad"),,s]
		
		access_mu_alpha <- access_mu_beta <- cbind(env_obj$state_names[z], "alpha", "mu", env_obj$pnames, s) 
		access_mu_beta[,2] <- "beta"
		access_V_alpha <- access_mu_alpha
		access_V_beta <- access_mu_beta
		access_V_alpha[,3] <- "V"
		access_V_beta[,3] <- "V"
		
		#initial values
		mua0 <- env_obj$mu[access_mu_alpha]
		mub0 <- env_obj$mu[access_mu_beta]
		
		Va0 <- env_obj$mu[access_V_alpha]
		Vb0 <- env_obj$mu[access_V_beta]
			
		#V	
		env_obj$mu[access_V_alpha] <- 1/(1 + 1/Va0)
		env_obj$mu[access_V_beta] <-  1/(1 + 1/Vb0)
			
		#mu update	
		env_obj$mu[access_mu_alpha] <- ((1/Va0)*mua0 + newV["log_speed",]*1) * env_obj$mu[access_V_alpha]
				
			
		theta_vals <- sapply(newV[ "turn_rad", ], function(ff) ff + env_obj$wn_seq) #matrix with npart columns
		beta_weights <- sapply(1:env_obj$npart, function(pp) keep_finite(dnorm(x=theta_vals[,pp], mean=env_obj$logv_angle_mu_draw[pp,"turn",z[ pp ],s], sd=sqrt(env_obj$tau_draw[pp,z[ pp ],s]))))
		beta_weights <- apply(beta_weights, 2, function(pp) pp/sum(pp))
		wtd_x <- colSums(keep_finite(theta_vals * beta_weights)) 
		wtd_x2 <- colSums(keep_finite(keep_finite(theta_vals^2) * beta_weights)) 
			
		
		env_obj$mu[access_mu_beta] <- normalize_angle(((1/Vb0)*mub0 + wtd_x*1) * env_obj$mu[access_V_beta])
		

			
		#wtd_mu is just wtd_x just not squared, since just one number
		access_igamma <- cbind(1:env_obj$npart, 2*z)
		env_obj$tau_pars[,,s][ access_igamma ]  <- env_obj$tau_pars[,,s][ access_igamma ] + 0.5*((mub0^2)/Vb0 + wtd_x2 - (env_obj$mu[access_mu_beta]^2)/env_obj$mu[access_V_beta])
		env_obj$sigma_pars[,,s][ access_igamma ]  <- env_obj$sigma_pars[,,s][ access_igamma ] + 0.5*((mua0^2)/Va0 + newV["log_speed",]^2 - (env_obj$mu[access_mu_alpha]^2)/env_obj$mu[access_V_alpha])
			
		#matrix of 1s and 0s for how many of which state was observed
		access_igamma2 <- cbind(1:env_obj$npart, 2*z - 1)


		env_obj$sigma_pars[,,s][ access_igamma2 ] <- env_obj$sigma_pars[,,s][  access_igamma2 ] + 0.5
		env_obj$tau_pars[,,s][  access_igamma2 ] <- env_obj$tau_pars[,,s][ access_igamma2 ] + 0.5
		
		
		#make sure stay within reasonable bounds
		env_obj$tau_pars[,,s][ access_igamma ] <- pmin(env_obj$tau_pars[,,s][ access_igamma ], 50 * (env_obj$tau_pars[,,s][  access_igamma2 ] - 1) / env_obj$mu[access_V_beta])
		env_obj$sigma_pars[,,s][ access_igamma ] <- pmin(env_obj$sigma_pars[,,s][ access_igamma ], 500 * (env_obj$sigma_pars[,,s][  access_igamma2 ] - 1)  / env_obj$mu[access_V_alpha])

			
		#only update x to x error, not x to y since don't have it
		#prediction from i-1
		#err_tmp <- h(mk=env_obj$Xpart_history[i-1, c("X","Y","logv","bearing_rad"),p,s], dtprev=reg_dt) - env_obj$Xpart_history[i,c("X","Y"),p,s]

		for (p in 1:env_obj$npart) {
			
			
			#err_tmp <- keep_finite(t(env_obj$mk_prev[c("X","Y"),z[ p ],p,s]) - env_obj$Xpart_history[env_obj$i,c("X","Y"),p,s])
			err_tmp <- keep_finite(t(env_obj$mk_actual_history[env_obj$i, c("X","Y"),p,s]) - env_obj$Xpart_history[env_obj$i,c("X","Y"),p,s])
			
			#does not depend on the state
			env_obj$SSquare_particle[,,p,s] <- keep_finite(env_obj$SSquare_particle[,,p,s] + keep_finite(t(err_tmp)%*%err_tmp))
			
			
			env_obj$Particle_errvar[[ s ]][[ p ]]$sig <- keep_finite(as.matrix(Matrix::nearPD(keep_finite(env_obj$Particle_errvar0 + env_obj$SSquare_particle[,,p,s]), ensureSymmetry=TRUE)$mat))
			env_obj$Particle_errvar[[ s ]][[ p ]]$dof <- env_obj$Particle_errvar[[ s ]][[ p ]]$dof + 1 
			
			if (env_obj$nstates > 1) {
			
				#transition parameters
				trans_tmp <- paste(env_obj$lambda_matrix[p, (env_obj$i-1):env_obj$i, s], collapse="")
				lookup_tmp <- cbind(prev_region[ p ], match(trans_tmp, env_obj$trans_names))
				#print(trans_tmp)
				#print(lookup_tmp)
				#print("before")
				#print(transition_mat[[ s ]][[ p ]]$dirichlet_pars)
				
				#ONLY UPDATE IF THERE WAS NO NEIGHBORHOOD INTERACTION
				#OTHERWISE ITS A NEIB INTERACTION, NOT REGIONAL
				#AUTOMATICALLY TRUE IF NO INTERACTION
				
				#if (part_with_neibs[ p,s ]==FALSE) {
				
				env_obj$transition_mat[[ s ]][[ p ]]$counts[ lookup_tmp ] <- env_obj$transition_mat[[ s ]][[ p ]]$counts[ lookup_tmp ] + 1
				
				#}
				#if leave, then update, otherwise not
				if (env_obj$time_dep_trans & (trans_tmp %in% c("12","21"))) {
					
					env_obj$transition_mat[[ s ]][[ p ]]$dirichlet_pars[ prev_region, prev_z[ p ]] <- (env_obj$transition_mat[[ s ]][[ p ]]$dirichlet_pars[ prev_region, prev_z[ p ]]*tis[ p ] + 1)/tis[ p ]
					
				}
				else {
					
					env_obj$transition_mat[[ s ]][[ p ]]$dirichlet_pars[ lookup_tmp ] <- env_obj$region_alphas[ lookup_tmp ] + env_obj$transition_mat[[ s ]][[ p ]]$counts[ lookup_tmp ]
					
					
					
					for (k in 1:env_obj$nstates) {
						env_obj$transition_mat[[ s ]][[ p ]]$mat[[ prev_region[ p ] ]][ k, ] <- MCMCpack::rdirichlet(n=1, alpha=env_obj$transition_mat[[ s ]][[ p ]]$dirichlet_pars[ prev_region[ p ], c(2*k -1, 2*k) ]) 
					}
				
				}
			}#if multiple states				
			
					
		}
		
							
		
	}

	invisible(NULL)
	
}
	
	