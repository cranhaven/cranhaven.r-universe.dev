sharks_to_sim_EKF_1d_interp_joint <- function(env_obj) {

	prev_z <- env_obj$Xpart_history[env_obj$i-1,"lambda",,env_obj$s] 
					
	#if this is true then draw probabilities for each region
	if (env_obj$time_dep_trans) {	
		tis <- env_obj$Xpart_history[env_obj$i-1,"time_in_state",,env_obj$s] 
											
		for (p in 1:env_obj$npart) {
			#rr <- prev_region[ p ]
			z <- prev_z[ p ]
			z_oth <- which(1:env_obj$nstates != z)

			a_tmp <- env_obj$transition_mat[[ env_obj$s ]][[ p ]]$dirichlet_pars[ 1, c(2*z-1, 2*z) ]
			
			gamma_ratio <- rgamma(n=1, shape=a_tmp[ z_oth ], rate=a_tmp[ z ])
			
			#only draw for the state that happened previously, since depends on t_k
			env_obj$transition_mat[[ env_obj$s ]][[ p ]]$mat[z,z] <-  rbeta(n=1, shape1=gamma_ratio*tis[ p ], shape2=1)
			env_obj$transition_mat[[ env_obj$s ]][[ p ]]$mat[z,z_oth] <- 1 - env_obj$transition_mat[[ env_obj$s ]][[ p ]]$mat[z,z]
			env_obj$prob_draws_hist[ env_obj$i, , p, env_obj$s]  <- env_obj$transition_mat[[ env_obj$s ]][[ p ]]$mat[z,]
		}
		
	}
	if (env_obj$nstates > 1) {			
		for (p in 1:env_obj$npart) {
			
			#generate state for interval i based on transition probabilities
			env_obj$Xpart_history[env_obj$i,"lambda",p, env_obj$s]  <- env_obj$lambda_matrix[p,env_obj$i, env_obj$s]  <- low_var_sample(wts=env_obj$transition_mat[[ env_obj$s ]][[ p ]]$mat[prev_z[ p ],] * env_obj$interact_intensity_draw[p,, env_obj$i, env_obj$s], M=1) 
						
		}
	}
	
	z <- env_obj$Xpart_history[env_obj$i,"lambda",,env_obj$s] 
	
	env_obj$Xpart_history[env_obj$i , "time_in_state", z == prev_z, env_obj$s] <- env_obj$Xpart_history[env_obj$i-1 , "time_in_state", z==prev_z, env_obj$s] + 1 
	env_obj$Xpart_history[env_obj$i , "time_in_state", z != prev_z, env_obj$s] <- 1	
	
	#print(cbind(prev_z, z))
	#print(apply(cbind(prev_z, z), 2, function(x) paste(x, collapse="")))
	env_obj$Xpart_history[env_obj$i, "state_change", , env_obj$s] <- as.numeric(apply(rbind(prev_z, z), 2, function(x) paste(x, collapse=""))) 
	
	#increase state counts by 1
	z <- env_obj$lambda_matrix[,env_obj$i, env_obj$s] 
	access_sigma <- cbind(env_obj$pnames, env_obj$state_names[z], env_obj$s)

	
	env_obj$state_counts[ access_sigma ] <- env_obj$state_counts[ access_sigma ] + 1
	#print(sigma_pars[,,env_obj$s])

	env_obj$sigma_draw[access_sigma] <- keep_finite(pmax(1e-15, MCMCpack::rinvgamma(n=env_obj$npart, shape=env_obj$sigma_pars[,,env_obj$s][ cbind(1:env_obj$npart, 2*z -1)], scale=env_obj$sigma_pars[,,env_obj$s][ cbind(1:env_obj$npart, 2*z)])))


	for (p in 1:env_obj$npart) { 

		z <- env_obj$lambda_matrix[p, env_obj$i, env_obj$s] 			
		#particle covariance, one for each state. only second part depends on state though
		  
		
		env_obj$Qt[1:2,1:2,z,p,env_obj$s] <- diag(keep_finite(c(MCMCpack::riwish(v=env_obj$Particle_errvar[[ env_obj$s ]][[ p ]]$dof, S=env_obj$Particle_errvar[[ env_obj$s ]][[ p ]]$sig),
																env_obj$sigma_draw[p, z, env_obj$s])))


		#draw values for mu_alpha, mu_beta
		#here beta is the mean of the log-transformed angle, which we have to change to 
	
		#draw block covariance matrices, same as D before, depends on the state of xt ->yt
		#this is R_k

		env_obj$logv_angle_mu_draw[p,,z,env_obj$s] <- as.numeric(mvtnorm::rmvnorm(n=1, mean=env_obj$mu[z, "mu", p, env_obj$s], sigma=as.matrix(env_obj$Qt[2, 2,z,p,env_obj$s] * env_obj$mu[z, "V", p, env_obj$s])))
		#logv_angle_draw[p,,z] <-  as.numeric(mvtnorm::rmvnorm(n=1, mean=logv_angle_mu_draw[p,,z], sigma=as.matrix(Qt[2,2])))		   

		env_obj$mk_prev[,z,p,env_obj$s] <- keep_finite(env_obj$f(mk=env_obj$mk_actual[,p,env_obj$s] , new_logv=env_obj$logv_angle_mu_draw[p,"velocity",z, env_obj$s], dtprev=env_obj$reg_dt)) #a_{t+1}
		
		Fx_tmp <- keep_finite(env_obj$Fx(mk=env_obj$mk_actual[,p,env_obj$s] , dtprev=env_obj$reg_dt))

		env_obj$Pk_prev[,,z,p,env_obj$s] <- keep_finite(as.matrix(Matrix::nearPD(keep_finite(keep_finite(keep_finite(Fx_tmp %*% env_obj$Pk_actual[,,p,env_obj$s]) %*% t(Fx_tmp)) + env_obj$Qt[,,z,p,env_obj$s]), ensureSymmetry=TRUE)$mat)) #R_{t+1}

		env_obj$Xpart_history[env_obj$i , c("X","velocity"), p, env_obj$s]  <- keep_finite(mvtnorm::rmvnorm(n=1, mean=env_obj$mk_prev[,z,p,env_obj$s] , sigma=env_obj$Pk_prev[,,z,p,env_obj$s]))
		#Xpart_history[i , c("X","logv"), p, env_obj$s] <- mvtnorm::rmvnorm(n=1, mean=f(mk=Xpart_history[ i-1, c("X","logv"),p,env_obj$s], new_logv=logv_angle_draw[p,"logv",z], dtprev=reg_dt), sigma=Qt)

		#store what is actually done

		env_obj$mk_actual[,p,env_obj$s] <- env_obj$mk_prev[,z,p,env_obj$s] 
		env_obj$Pk_actual[,,p,env_obj$s] <- env_obj$Pk_prev[,,z,p,env_obj$s] 
		
	}#loop over part

	# store for the future
	env_obj$mk_actual_history[env_obj$i,,,env_obj$s] <- env_obj$mk_actual[,,env_obj$s]



invisible(NULL)
}
		
