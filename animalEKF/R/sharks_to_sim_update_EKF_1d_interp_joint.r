sharks_to_sim_update_EKF_1d_interp_joint <- function(env_obj) {

	for (s in env_obj$sharks_to_sim) {

		z <- env_obj$lambda_matrix[, env_obj$i, s] 
		newV <- env_obj$Xpart_history[env_obj$i, "velocity",, s] 
		
		access_mu <- access_V <- cbind(env_obj$state_names[z], "mu", env_obj$pnames, s)
		access_V[,2] <- "V"	
		
		#update sigma (tau will be NA)
		mu0 <- env_obj$mu[access_mu]
		V0 <-  env_obj$mu[access_V]
				  
		env_obj$mu[access_V] <- 1/(1 + 1/V0)
			
		env_obj$mu[access_mu] <- ((1/V0) * mu0 + newV) * env_obj$mu[access_V]
		
		#sigma
		access_igamma <- cbind(1:env_obj$npart, 2*z)
		
		env_obj$sigma_pars[,,s][ access_igamma ] <- pmin(env_obj$sigma_pars[,,s][ access_igamma ]  + 0.5 * ((mu0^2)/V0 + newV^2 - (env_obj$mu[access_mu]^2) / env_obj$mu[access_V]), 3000)
		access_igamma2 <- cbind(1:env_obj$npart, 2* z  - 1)

		
		env_obj$sigma_pars[,,s][ access_igamma2 ] <- env_obj$sigma_pars[,,s][ access_igamma2 ] + 0.5
	
		
		#only update x to x error, not x to y since don't have it
		#err_tmp <- apply(Xpart_history[i-1, c("X","logv"),,s], 2, function(x) h(mk=x, dtprev=reg_dt)) - Xpart_history[i,"X",,s]
		
		
		
		#err_tmp <- keep_finite(env_obj$Xpart_history[ env_obj$i, "X",, s]  - env_obj$mk_actual["X",, s])  # matrix(mk_prev["X",,,s], nrow=nstates, ncol=npart)[ cbind(z, 1:npart) ]

		err_tmp <- keep_finite(env_obj$mk_actual_history[env_obj$i, "X",,s] - env_obj$Xpart_history[env_obj$i,"X",,s])

		
		env_obj$SSquare_particle[1, 1, , s]  <- keep_finite(env_obj$SSquare_particle[1, 1, , s]  + keep_finite(err_tmp^2))

									
		for (p in 1:env_obj$npart) {
			
			env_obj$Particle_errvar[[ s ]][[ p ]]$sig <- keep_finite(as.matrix(Matrix::nearPD(keep_finite(env_obj$Particle_errvar0 + env_obj$SSquare_particle[,,p,s]), ensureSymmetry=TRUE)$mat))
			env_obj$Particle_errvar[[ s ]][[ p ]]$dof <- env_obj$Particle_errvar[[ s ]][[ p ]]$dof + 1 
			
			if (env_obj$nstates > 1) {
				#transition parameters
				trans_tmp <- paste(env_obj$lambda_matrix[p, (env_obj$i-1):env_obj$i, s] , collapse="")
				lookup_tmp <- match(trans_tmp, env_obj$trans_names)

				
				env_obj$transition_mat[[ s ]][[ p ]]$counts[ lookup_tmp ] <- env_obj$transition_mat[[ s ]][[ p ]]$counts[ lookup_tmp ] + 1
				
				env_obj$transition_mat[[ s ]][[ p ]]$dirichlet_pars[ lookup_tmp ] <- env_obj$region_alphas[ lookup_tmp ] + env_obj$transition_mat[[ s ]][[ p ]]$counts[ lookup_tmp ]
				#print("after")
				#print(transition_mat[[ s ]][[ p ]]$dirichlet_pars)
									
				#rr <- Xpart_history[i-1,"region",p,s]
				for (k in 1:env_obj$nstates) {
				
					env_obj$transition_mat[[ s ]][[ p ]]$mat[ k, ] <- MCMCpack::rdirichlet(n=1, alpha=env_obj$transition_mat[[ s ]][[ p ]]$dirichlet_pars[ 1, c(2*k -1, 2*k) ]) 
				}
			}
					
		}
		
		
		
	}
	
	invisible(NULL)
	
}