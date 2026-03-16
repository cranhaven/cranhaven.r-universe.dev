sharks_to_sim_EKF_interp_joint <- function(env_obj) {
										   
										   

	prev_region <- env_obj$Xpart_history[env_obj$i-1,"region",, env_obj$s]
	prev_z <- env_obj$Xpart_history[env_obj$i-1,"lambda",, env_obj$s]
					
	#if this is true then draw probabilities for each region
	if (env_obj$time_dep_trans) {	
		tis <- env_obj$Xpart_history[env_obj$i-1,"time_in_state",, env_obj$s]
											
		for (p in 1:env_obj$npart) {
			rr <- prev_region[ p ]
			z <- prev_z[ p ]
			z_oth <- which(1:env_obj$nstates != z)
			pleave <- rbeta(n=1, shape1=tis[ p ]*env_obj$transition_mat[[ env_obj$s ]][[ p ]]$dirichlet_pars[ rr, z ], shape2=1)
			
			env_obj$transition_mat[[ env_obj$s ]][[ p ]]$mat[[ rr ]][z, c(z, z_oth)] <- c(1-pleave, pleave)
			
			env_obj$prob_draws_hist[env_obj$i, , p, env_obj$s] <- env_obj$transition_mat[[ env_obj$s ]][[ p ]]$mat[[ rr ]][z,]
		}
		#print(s)
		#print(env_obj$transition_mat[[ env_obj$s ]][[ 1 ]]$mat[[ prev_region[ 1 ] ]])
		
	}
	
	if (env_obj$nstates > 1) {
		for (p in 1:env_obj$npart) {
			
			#generate state for interval i based on transition probabilities
			env_obj$Xpart_history[env_obj$i, "lambda",p, env_obj$s] <- env_obj$lambda_matrix[p, env_obj$i, env_obj$s] <- low_var_sample(wts=env_obj$transition_mat[[ env_obj$s ]][[ p ]]$mat[[ prev_region[ p ] ]][prev_z[ p ],] * env_obj$interact_intensity_draw[p,, env_obj$i, env_obj$s], M=1) 
		
		}
	}
	
	z <- env_obj$Xpart_history[env_obj$i, "lambda",, env_obj$s]
	
	env_obj$Xpart_history[env_obj$i,  "time_in_state", z==prev_z, env_obj$s] <- env_obj$Xpart_history[env_obj$i-1 , "time_in_state", z==prev_z, env_obj$s] + 1 
	env_obj$Xpart_history[env_obj$i,  "time_in_state", z != prev_z, env_obj$s] <- 1	
	env_obj$Xpart_history[env_obj$i,  "state_change", , env_obj$s] <- as.numeric(apply(rbind(prev_z, z), 2, function(x) paste(x, collapse=""))) 
	
	#print("new tis")
	#print(env_obj$Xpart_history[i , "time_in_state", , env_obj$s])	
	
	
	#increase state counts by 1
	z <- env_obj$lambda_matrix[, env_obj$i, env_obj$s]
	
	#only update state counts, used to update transition parameters
	
	env_obj$state_counts[,, env_obj$s][ cbind(1:env_obj$npart, z) ] <- env_obj$state_counts[,, env_obj$s][ cbind(1:env_obj$npart, z) ] + 1

		
	#print(sigma_draw[,, env_obj$s])#[ cbind(1:env_obj$npart, 2*z) ])	
	env_obj$sigma_draw[,, env_obj$s][ cbind(1:env_obj$npart, z) ] <- keep_finite(pmax(1e-15, MCMCpack::rinvgamma(n=env_obj$npart, shape=env_obj$sigma_pars[,, env_obj$s][ cbind(1:env_obj$npart, 2*z -1)], scale=env_obj$sigma_pars[,, env_obj$s][ cbind(1:env_obj$npart, 2*z)])))
	env_obj$tau_draw[,, env_obj$s][ cbind(1:env_obj$npart, z) ]   <- keep_finite(pmax(1e-15, MCMCpack::rinvgamma(n=env_obj$npart, shape=env_obj$tau_pars[,, env_obj$s][ cbind(1:env_obj$npart, 2*z -1)], scale=env_obj$tau_pars[,, env_obj$s][ cbind(1:env_obj$npart, 2*z)])))

	
	#bad <- which(is.na(sigma_draw) | is.na(tau_draw))
	#if (length(bad)) { print("NA variances") ; print(sigma_pars[bad,, env_obj$s]) ; print(tau_pars[bad,, env_obj$s]) }
	
	
	for (p in 1:env_obj$npart) { 
		z <- env_obj$lambda_matrix[p, env_obj$i, env_obj$s]

		#particle covariance, one for each state. only second part depends on state though
		
		env_obj$Qt[1:2, 1:2,z,p, env_obj$s] <- keep_finite(MCMCpack::riwish(v=env_obj$Particle_errvar[[ env_obj$s ]][[ p ]]$dof, S=env_obj$Particle_errvar[[ env_obj$s ]][[ p ]]$sig))
		
		
		env_obj$cov_err_hist[,"particle",p, env_obj$i, env_obj$s,"orig"] <- env_obj$cov_err_hist[,"particle",p, env_obj$i, env_obj$s,"resamp"] <- env_obj$Qt[1:2, 1:2,z,p, env_obj$s][ c(1,4,2) ]
						
		#draw values for mu_alpha, mu_beta
		#here beta is the mean of the log-transformed angle, which we have to change to 

		#this is the error for xt| x_{t-1}: first part is the same for each state
		env_obj$Qt[3:4, 3:4,z,p, env_obj$s] <- diag(c(env_obj$sigma_draw[p,z, env_obj$s], env_obj$tau_draw[p,z, env_obj$s]))
	 
		#draw block covariance matrices, same as D before, depends on the state of xt ->yt
		#this is R_k
		#XY_errvar_draw[[ env_obj$s ]][[ p ]][[ z ]]  <- MCMCpack::riwish(v=XY_errvar[[ env_obj$s ]][[ p ]][[ z ]]$dof, S=XY_errvar[[ env_obj$s ]][[ p ]][[ z ]]$sig)
		#logv_angle_mu_draw[p,,z] <- as.numeric(mvtnorm::rmvnorm(n=1, mean=mu[[ env_obj$s ]][[ p ]][[ z ]], sigma=Qt[3:4, 3:4,z,p, env_obj$s]*mu[[ env_obj$s ]][[ p ]]$V[[ z ]]))
		
		env_obj$logv_angle_mu_draw[p,,z, env_obj$s] <- rnorm(n=2, mean=env_obj$mu[z,,"mu",p, env_obj$s], sd=sqrt(diag(env_obj$Qt[3:4, 3:4,z,p, env_obj$s]) * env_obj$mu[z,,"V",p, env_obj$s]))

		
		#multiply by gradient since later will be variance of theta
		#Qt[4,4,z,p, env_obj$s] <- Qt[4,4,z,p, env_obj$s]*(grad_ginv(psi=normalize_angle(logv_angle_mu_draw[p,"turn",z])))^2
			
		#logv_angle_mu_draw[p,"turn", z] <- normalize_angle(log2rad(logv_angle_mu_draw[p,"turn",z]))
		#logv_angle_draw[p,"turn", z] <- normalize_angle(log2rad(logv_angle_draw[p,"turn",z]))
		
		env_obj$mk_prev[,z,p, env_obj$s] <- keep_finite(env_obj$f(mk=env_obj$mk_actual[,p, env_obj$s], new_logv=env_obj$logv_angle_mu_draw[p,"logv",z, env_obj$s], 
											 theta=normalize_angle(env_obj$logv_angle_mu_draw[p,"turn",z, env_obj$s]), dtprev=env_obj$reg_dt)) #a_{t+1}
		
		Fx_tmp <- keep_finite(env_obj$Fx(mk=env_obj$mk_actual[,p, env_obj$s], dtprev=env_obj$reg_dt))

		

		env_obj$Pk_prev[,,z,p, env_obj$s] <- keep_finite(as.matrix(Matrix::nearPD(keep_finite(keep_finite(keep_finite(Fx_tmp %*% env_obj$Pk_actual[,,p, env_obj$s]) %*% t(Fx_tmp)) + env_obj$Qt[,,z,p, env_obj$s]), ensureSymmetry=TRUE)$mat)) #R_{t+1}

		
		
		if (env_obj$truncate_to_map) {
					
			tmp <- reject_sampling(mu=env_obj$mk_prev[,z,p, env_obj$s], cmat=env_obj$Pk_prev[,,z,p, env_obj$s], prev_val=env_obj$Xpart_history[env_obj$i-1 , c("X","Y","log_speed","bearing_rad"), p, env_obj$s], obj=env_obj)
			env_obj$Xpart_history[env_obj$i , c("X","Y","log_speed","bearing_rad"), p, env_obj$s] <- keep_finite(tmp$val)
			env_obj$num_reject[ p ] <- tmp$iter
		}
		else {
			env_obj$Xpart_history[env_obj$i, c("X","Y","log_speed","bearing_rad"), p, env_obj$s] <- keep_finite(mvtnorm::rmvnorm(n=1, mean=env_obj$mk_prev[,z,p, env_obj$s], sigma=env_obj$Pk_prev[,,z,p, env_obj$s]))
		
		}	
		
   
				
   
	}#loop over part
	
	
	# restrict logvelocity to range so variance doesn't blow up
	env_obj$Xpart_history[env_obj$i, "log_speed",, env_obj$s] <- pmin(pmax(env_obj$logvelocity_truncate[1], env_obj$Xpart_history[env_obj$i, "log_speed",, env_obj$s]), env_obj$logvelocity_truncate[2])
	
	
	if (env_obj$truncate_to_map) {
		env_obj$reject_samp_hist[env_obj$i, , env_obj$s] <- c(mean(env_obj$num_reject), median(env_obj$num_reject))
	}
	
	#z <- env_obj$lambda_matrix[, env_obj$i, env_obj$s]			
	#print(round(apply(logv_angle_draw[,"turn",], 2, summary), digits=3))

	env_obj$Xpart_history[env_obj$i,  "region",,env_obj$s ] <- apply(env_obj$Xpart_history[env_obj$i, c("X","Y"),, env_obj$s], 2, function(x) which_region(x, env_obj$centroids))
	env_obj$Xpart_history[env_obj$i,  "bearing_rad",,env_obj$s ] <- normalize_angle(env_obj$Xpart_history[env_obj$i,  "bearing_rad",,env_obj$s ])
	env_obj$Xpart_history[env_obj$i,  "turn_rad",,env_obj$s ] <- normalize_angle(env_obj$Xpart_history[env_obj$i,  "bearing_rad",,env_obj$s ] - env_obj$Xpart_history[ env_obj$i-1, "bearing_rad",,env_obj$s ])
	
	#mk_actual[4,, env_obj$s] <- normalize_angle(mk_actual[4,, env_obj$s])
	env_obj$mk_prev[4,,, env_obj$s] <- normalize_angle(env_obj$mk_prev[4,,, env_obj$s])
			
	
	for (p in 1:env_obj$npart) {
		z <- env_obj$lambda_matrix[p, env_obj$i, env_obj$s]
		env_obj$mk_actual[,p, env_obj$s] <- env_obj$mk_prev[,z,p, env_obj$s] #env_obj$Xpart_history[env_obj$i,  c("X","Y","logv","bearing_rad"),, env_obj$s]
		env_obj$Pk_actual[,,p, env_obj$s] <- env_obj$Pk_prev[,,z,p, env_obj$s]
	}
	
	# store for the future
	env_obj$mk_actual_history[env_obj$i,,,env_obj$s] <- env_obj$mk_actual[,,env_obj$s]


			
	invisible(NULL)
}