sharks_with_obs_sim_EKF_1d_interp_joint <- function(env_obj) {

	for (s in env_obj$sharks_with_obs) {

		#print("tis values")
		#print(s)
		#print(Xpart_history[1:i,c("time_in_state","lambda"),1,s])
		#print(shark_intervals[[ s ]])
		
		#sequence of regular steps for which we will update values
		#should be different for each shark
		#sequence of step since last observation
		
		env_obj$steps_to_resamp[[ s ]] <- min((max(env_obj$steps_to_resamp[[ s ]]) + 1), env_obj$i-1):(env_obj$i-1)


		
				
		for (k in 1:env_obj$nstates) {

		  env_obj$sigma_draw[,k,s] <- keep_finite(pmax(1e-15, MCMCpack::rinvgamma(n=env_obj$npart, shape=env_obj$sigma_pars[, 2*k-1, s], scale=env_obj$sigma_pars[, 2*k, s])))
			
		}

		env_obj$Qt[2,2,,,s] <- t(env_obj$sigma_draw[,,s])
		
		
		for (p in 1:env_obj$npart) { 
	  
			#particle covariance, one for each state. only second part depends on state though
			
			env_obj$Qt[1, 1,, p,s] <- keep_finite(MCMCpack::riwish(v=env_obj$Particle_errvar[[ s ]][[ p ]]$dof, S=env_obj$Particle_errvar[[ s ]][[ p ]]$sig))
			  
			#draw values for mu_alpha, mu_beta
			#here beta is the mean of the log-transformed angle, which we have to change to 
		  
			for (k in 1:env_obj$nstates) {
			
				#this is the error for xt| x_{t-1}: first part is the same for each state
				#Qt[2,2,k,p] <- sigma_draw[p,k]
		   
				#draw block covariance matrices, same as D before, depends on the state of xt ->yt
				#this is R_k
				env_obj$XY_errvar_draw[1,  1, k, p, s] <- MCMCpack::riwish(v=env_obj$XY_errvar[[ s ]][[ p ]][[ k ]]$dof, S=env_obj$XY_errvar[[ s ]][[ p ]][[ k ]]$sig)

				#mu has diagonal matrix
				#on first step dont have any turn, assume you know it

				
				#draw logV and normalized turn for each state from current (or prior estimates)
			
				env_obj$logv_angle_mu_draw[p,,k,s] <- mvtnorm::rmvnorm(n=1, mean=env_obj$mu[k, "mu", p, s], sigma=as.matrix(env_obj$Qt[2, 2,k,p,s] * env_obj$mu[k, "V", p, s]))
			
			}
		}#loop over part and k	
		
		env_obj$XY_errvar_draw[,,,, s] <- keep_finite(env_obj$XY_errvar_draw[,,,, s])
			
		#multiply by gradient since later will be variance of theta
		
		#print(	logv_angle_mu_draw[,"turn",])			
		
		#logv_angle_draw[,"logv",] <- normalize_logv(logv_angle_draw[,"logv",])

		for (k in 1:env_obj$nstates) {
		
			for (p in 1:env_obj$npart) {
			
				env_obj$mk_prev[,k,p,s] <- keep_finite(env_obj$f(mk=env_obj$mk_actual[,p,s], new_logv=env_obj$logv_angle_mu_draw[p,"velocity",k,s], dtprev=env_obj$reg_dt)) #a_{t+1}
				Fx_tmp <- keep_finite(env_obj$Fx(mk=env_obj$mk_actual[,p,s], dtprev=env_obj$reg_dt))
				env_obj$Pk_prev[,,k,p,s] <- keep_finite(as.matrix(Matrix::nearPD(keep_finite(keep_finite(keep_finite(Fx_tmp %*% env_obj$Pk_actual[,,p,s]) %*% t(Fx_tmp)) + env_obj$Qt[,,k,p,s]), ensureSymmetry=TRUE)$mat)) #R_{t+1}

			}
				
		 		
		}
		
		
		#interpolation fraction	for each shark
		
		shark_rows <- rownames(env_obj$ynext) == s
		env_obj$j_list[[ s ]][[ env_obj$i ]] <- pmax((env_obj$ynext[shark_rows, "date_as_sec"] - env_obj$t_reg[env_obj$i]) / env_obj$reg_dt, 1e-10)
		
					
		print(paste("j:", paste(round(env_obj$j_list[[ s ]][[ env_obj$i ]], digits=4), collapse=", ")))
		
		env_obj$MuY[[ s ]] <- array(NA, dim=c(env_obj$yobs_sharks[ s ], env_obj$nstates, env_obj$npart), dimnames=list(1:env_obj$yobs_sharks[ s ], env_obj$state_names, env_obj$pnames)) 
		#rep(list(rep(list(rep(list(matrix(NA, ncol=1, nrow=2)), yobs_sharks[ s ])), nstates)), npart)
		env_obj$SigY[[ s ]] <- array(NA, dim=c(env_obj$yobs_sharks[ s ], env_obj$nstates, env_obj$npart), dimnames=list(1:env_obj$yobs_sharks[ s ], env_obj$state_names, env_obj$pnames)) 
		#rep(list(rep(list(rep(list(diag(2)), yobs_sharks[ s ])), nstates)), npart)
		#Pk_prev_interp[[ s ]] <- array(NA, dim=c(2,2,yobs_sharks[ s ], nstates, npart), dimnames=list(1:2,1:2,1:yobs_sharks[ s ], state_names, pnames)) 
		#rep(list(rep(list(rep(list(diag(4)), yobs_sharks[ s ])), nstates)), npart)	
		env_obj$Kgain[[ s ]] <- array(NA, dim=c(2, env_obj$yobs_sharks[ s ], env_obj$nstates, env_obj$npart), dimnames=list(1:2, 1:env_obj$yobs_sharks[ s ], env_obj$state_names, env_obj$pnames)) 
		#rep(list(rep(list(rep(list(matrix(0, ncol=2, nrow=4)), yobs_sharks[ s ])), nstates)), npart)	
		
		#print(yobs_sharks)
		#prediction of y is the direct interpolation
		
		#print("Pk")
		#print(Pk_actual[,,,s])
		
		j_tmp <- diff(c(0, env_obj$j_list[[ s ]][[ env_obj$i ]]))
		
		
		for (p in 1:env_obj$npart) {			
			for (k in 1:env_obj$nstates) {
			
				mk_tmp <- c(env_obj$mk_prev[,k,p,s])
				Pk_tmp <- env_obj$Pk_prev[,,k,p,s]

				for (y in 1:env_obj$yobs_sharks[ s ]) {
					
					if (y > 1) {
						#take previous x values and starting logv
						#mk_tmp[1] <- env_obj$MuY[[ s ]][y-1,k,p]
						
						#take previous x-y values and starting logv and bearing
						mk_tmp[1] <- env_obj$MuY[[ s ]][y-1,k,p]
						Pk_tmp[1,1] <- env_obj$SigY[[ s ]][y-1,k,p]
					}
					
						
					env_obj$MuY[[ s ]][y,k,p] <- keep_finite(env_obj$h(mk=mk_tmp, dtprev=j_tmp[ y ] * env_obj$reg_dt))
					Hx_tmp <- keep_finite(env_obj$Hx(mk=env_obj$mk_prev[,k,p,s], dtprev=j_tmp[ y ] * env_obj$reg_dt))
					
					# env_obj$SigY[[ s ]][y,k,p] <- keep_finite(as.matrix(Matrix::nearPD(keep_finite(Hx_tmp %*% env_obj$Pk_prev[,,k,p,s] %*% t(Hx_tmp)  + (j_tmp[ y ] * env_obj$XY_errvar_draw[,,k,p,s])), ensureSymmetry=TRUE)$mat))
					env_obj$SigY[[ s ]][y,k,p] <- keep_finite(as.matrix(Matrix::nearPD(keep_finite(keep_finite(keep_finite(Hx_tmp %*% Pk_tmp) %*% t(Hx_tmp))  + (j_tmp[ y ] * env_obj$XY_errvar_draw[,,k,p,s])), ensureSymmetry=TRUE)$mat))
				
					
					
					# Fx_tmp <- Fx(mk=mk_prev[,k,p,s], dtprev=j_list[[ s ]][[ i ]][ y ]*reg_dt)
					# Pk_prev_interp[[ s ]][,,y,k,p] <- as.matrix(Matrix::nearPD(Fx_tmp%*%Pk_actual[,,p,s]%*%t(Fx_tmp) + Qt[,,k,p], ensureSymmetry=TRUE)$mat) #R_{t+1}
					
					# MuY[[ s ]][y,k,p] <- keep_finite(h(mk=Xpart[p,,k,"curr",s], dtprev=j_list[[ s ]][[ i ]][ y ]*reg_dt))
					# Hx_tmp <- Hx(mk=Xpart[p,,k,"curr",s], dtprev=j_list[[ s ]][[ i ]][ y ]*reg_dt)
					
					# SigY[[ s ]][y,k,p] <- keep_finite(as.matrix(Matrix::nearPD(Hx_tmp%*%Pk_prev_interp[[ s ]][,,y,k,p]%*%t(Hx_tmp)  + (j_list[[ s ]][[ i ]][ y ]^2)*XY_errvar_draw[,,k,p,s], ensureSymmetry=TRUE)$mat))
				
				}
			}
		}

	}
	
	invisible(NULL)
}