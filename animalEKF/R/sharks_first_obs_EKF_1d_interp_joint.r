sharks_first_obs_EKF_1d_interp_joint <- function(env_obj) {
	
	
	for (s in env_obj$sharks_first_obs) {
	
		ids <- (env_obj$d[,"t_intervals"] == env_obj$i) & (env_obj$tags == s) 
					
		env_obj$y_first <- env_obj$d[ ids, c("X","velocity","date_as_sec","t_intervals","state.guess2","shark_obs_index"), drop=FALSE]
		env_obj$j_list[[ s ]][[ env_obj$i ]] <- pmin(pmax((env_obj$y_first[,"date_as_sec"] - env_obj$t_reg[env_obj$i])/env_obj$reg_dt, 1e-5), 1-(1e-5))
		jtmp <- env_obj$j_list[[ s ]][[ env_obj$i ]]	
		
		
		
		if (env_obj$show_prints) {
			print(paste("j:", paste(round(env_obj$j_list[[ s ]][[ env_obj$i ]], digits=4), collapse=", ")))

		#choose what the behavior at beginning of time i is, based on what was observed before.
		#ignore this, really, dont store anything for it
			print(env_obj$y_first)
		}
	
		
		z <- low_var_sample(wts=table(factor(env_obj$y_first[,"state.guess2"], levels=1:env_obj$nstates)), M=env_obj$npart)
						
		env_obj$Xpart_history[env_obj$i,"lambda",,s]  <- env_obj$lambda_matrix[,env_obj$i,s]  <- z
		#Xpart_history[i,"region",,s] <- y_first["region"]
		
		#initial bearing approximation		
		nobs <- nrow(env_obj$y_first)
		first_pos <- env_obj$y_first[1, "X"]
		initial_time_back <- jtmp[1] * env_obj$reg_dt

		initial_velocity_forward <- env_obj$y_first[1, "velocity"]
	
		# in case start time and first observed time don't match up
		time0_estimate <- env_obj$h(mk=c(first_pos, -1 * initial_velocity_forward), dtprev=initial_time_back)
		anchor_pos <- env_obj$y_first[nobs, "X"]

		#use interpolation estimate location at end of first interval. 
		#go forward a given number of steps
		
		end_of_interval_estimate <- env_obj$h(mk=env_obj$y_first[nobs, c("X", "velocity")], dtprev=(1-jtmp[nobs]) * env_obj$reg_dt)

		velocity_est_to_first <- (end_of_interval_estimate - time0_estimate)/env_obj$reg_dt
		
		#speed and turn (unused) angles given start XY and given state
		sigma_draw <- MCMCpack::rinvgamma(n=env_obj$npart, env_obj$sigma_pars[,,s][ cbind(1:env_obj$npart, 2*z -1)], env_obj$sigma_pars[,,s][ cbind(1:env_obj$npart, 2*z)])
		
		env_obj$Qt[2,2,,,s][ cbind(z, 1:env_obj$npart) ] <- sigma_draw
		
		for (p in 1:env_obj$npart) { 
			
			#particle covariance, one for each state. only second part depends on state though
			
			env_obj$Pk_actual[,,p,s]  <- keep_finite(env_obj$Pk_prev[,,z[ p ],p,s] )	  
			#print(Qt[1,1,p])
			env_obj$Qt[1,1,z[ p ],p,s]  <- keep_finite(MCMCpack::riwish(v=env_obj$Particle_errvar[[ s ]][[ p ]]$dof, S=env_obj$Particle_errvar[[ s ]][[ p ]]$sig))
			#print(Qt[1,1,p])	
			#print(Particle_errvar[[ s ]][[ p ]])
			#draw values for mu_alpha, mu_beta
			#here beta is the mean of the log-transformed angle, which we have to change to 
								
			#draw block covariance matrices, same as D before, depends on the state of xt ->yt
				
			env_obj$logv_angle_mu_draw[p,"velocity",z[ p ], s] <- as.numeric(mvtnorm::rmvnorm(n=1, mean=env_obj$mu[z[p], "mu", p, s], 
																							sigma=as.matrix(env_obj$Qt[2, 2, z[ p ],p,s] * env_obj$mu[z[p], "V", p, s])))
			
		
			#take the logvelocity mu back fraction of sectonds to beginning of interval, then use that log-velocity to go forwards next.
			
			mk_tmp <- c(first_pos, -1 * env_obj$logv_angle_mu_draw[p,"velocity",z[ p ],s])
			
			env_obj$mk_actual[,p,s] <- keep_finite(env_obj$f(mk=mk_tmp, new_logv= env_obj$logv_angle_mu_draw[p,"velocity",z[ p ], s], dtprev=initial_time_back)) #a_{t+1}
			
			#print(mk_actual[,p,s])
			Fx_tmp <- keep_finite(env_obj$Fx(mk=mk_tmp, dtprev=initial_time_back))
			#print(Fx_tmp)
			#print(Fx_tmp%*%Pk_actual[,,p,s]%*%t(Fx_tmp) + Qt[,,p])
			
			env_obj$Pk_actual[,,p,s]  <- keep_finite(as.matrix(Matrix::nearPD(keep_finite(keep_finite(keep_finite(Fx_tmp %*% env_obj$Pk_actual[,,p,s])  %*% t(Fx_tmp)) + (jtmp[1] * env_obj$Qt[,,z[ p ],p,s])), ensureSymmetry=TRUE)$mat)) #R_{t+1}
			

			#Xpart_history[ i, "logv",p,s ] <- logv_angle_draw[p,"logv",z[ p ]]
			#simulate location if traveled backwards (negative velocity)
			#Xpart_history[ i, "X", p, s ] <- rnorm(n=1, mean=h(mk=c(y_first[ "X" ], -1*Xpart_history[i, "logv", p, s]), dtprev=dt_tmp), sd=sqrt(Qt[1,1,p]))
			
			#go back distance from y_first
			#mk_actual[2,p,s] <- -1*mk_actual[2,p,s]

			env_obj$Xpart_history[ env_obj$i, c("X","velocity"), p, s]  <- keep_finite(mvtnorm::rmvnorm(n=1, mean=env_obj$mk_actual[,p,s], sigma=env_obj$Pk_actual[,,p,s] ))
			
			
		}#loop over part
		
		# store for the future
		env_obj$mk_actual_history[env_obj$i,,,s] <- env_obj$mk_actual[,,s]
		
		env_obj$Xpart_history[ env_obj$i, "time_in_state",,s ] <- 1
		
		# correct this in the history, since are actually going the reverse direction
		# simulated X is actually the simulated starting point going back a fraction from the first observed location
		err_tmp <- c()
		
		for (y in 1:nrow(env_obj$y_first)) {
			err_tmp <- cbind(err_tmp, apply(env_obj$Xpart_history[env_obj$i, c("X","velocity"),,s], 2, function(x) abs(env_obj$h(mk=x, dtprev=jtmp[y] * env_obj$reg_dt) - env_obj$y_first[y,"X"])))
	 
		}
		
		env_obj$error_beforesamp_allpart[ env_obj$i,,s] <- rowSums(err_tmp)
		env_obj$error_beforesamp_quantiles[ env_obj$i,,s] <-  quantile(env_obj$error_beforesamp_allpart[ env_obj$i,,s], p=c(.1, .5, .9))
		
	}
	invisible(NULL)

}		