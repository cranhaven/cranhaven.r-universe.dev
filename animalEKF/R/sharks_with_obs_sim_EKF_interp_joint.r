sharks_with_obs_sim_EKF_interp_joint <- function(env_obj) {

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
			
			
		  env_obj$sigma_draw[,k,s] <- keep_finite(pmax(1e-15, MCMCpack::rinvgamma(n=env_obj$npart, shape=env_obj$sigma_pars[,2*k-1,s], scale=env_obj$sigma_pars[,2*k,s])))
			env_obj$tau_draw[,k,s] <- keep_finite(pmax(1e-15, MCMCpack::rinvgamma(n=env_obj$npart, shape=env_obj$tau_pars[,2*k-1,s],  scale=env_obj$tau_pars[,2*k,s])))
			
		}
		
		
		for (p in 1:env_obj$npart) { 
	  
			#particle covariance, one for each state. only second part depends on state though
			
			pe_draw <- keep_finite(MCMCpack::riwish(v=env_obj$Particle_errvar[[ s ]][[ p ]]$dof, S=env_obj$Particle_errvar[[ s ]][[ p ]]$sig))

			#draw values for mu_alpha, mu_beta
			#here beta is the mean of the log-transformed angle, which we have to change to 
			
			for (k in 1:env_obj$nstates) {
				
				env_obj$Qt[1:2, 1:2,k, p,s] <- pe_draw 
			
				#this is the error for xt| x_{t-1}: first part is the same for each state
				diag(env_obj$Qt[3:4,3:4,k,p, s]) <- c(env_obj$sigma_draw[p,k,s], env_obj$tau_draw[p,k,s])
		   
				#draw block covariance matrices, same as D before, depends on the state of xt ->yt
				#this is R_k
				env_obj$XY_errvar_draw[,,k,p,s] <- MCMCpack::riwish(v=env_obj$XY_errvar[[ s ]][[ p ]][[ k ]]$dof, S=env_obj$XY_errvar[[ s ]][[ p ]][[ k ]]$sig)
				
				#print(XY_errvar_draw[,,k,p,s])
				#mu has diagonal matrix
				#on first step dont have any turn, assume you know it

				
				#draw logV and normalized turn for each state from current (or prior estimates)
			
				#logv_angle_mu_draw[p,,k] <- rnorm(n=2, mean=mu[[ s ]][[ p ]][[ k ]], sd=sqrt(diag(Qt[3:4, 3:4,k,p, s])*mu[[ s ]][[ p ]]$V[[ k ]]))
				env_obj$logv_angle_mu_draw[p,,k, s] <- rnorm(n=2, mean=env_obj$mu[k,,"mu",p,s], sd=sqrt(diag(env_obj$Qt[3:4, 3:4,k,p, s]) * env_obj$mu[k,,"V",p,s]))
				#logv_angle_draw[p,,k] <-    rnorm(n=2, mean=logv_angle_mu_draw[p,,k], sd=sqrt(diag(Qt[3:4, 3:4,k,p, s])))
			
			}
		}#loop over part and k	
		#print(XY_errvar_draw[,,,1:5,s])
		
		env_obj$XY_errvar_draw[,,,,s] <- keep_finite(env_obj$XY_errvar_draw[,,,,s])
		
		env_obj$logv_angle_mu_draw[,"turn",,s] <- normalize_angle(env_obj$logv_angle_mu_draw[,"turn",,s])		

		env_obj$cov_err_hist["X","particle",, env_obj$i,s,"orig"] <- env_obj$cov_err_hist["X","particle",, env_obj$i,s,"resamp"] <-  env_obj$Qt["X","X",1,,s]
		env_obj$cov_err_hist["Y","particle",, env_obj$i,s,"orig"] <- env_obj$cov_err_hist["Y","particle",, env_obj$i,s,"resamp"] <- env_obj$Qt["Y","Y",1,,s]
		env_obj$cov_err_hist["cov","particle",, env_obj$i,s,"orig"] <- env_obj$cov_err_hist["cov","particle",, env_obj$i,s,"resamp"] <- env_obj$Qt["X","Y",1,,s]

		env_obj$cov_err_hist["X",-1,, env_obj$i,s,"orig"] <- env_obj$cov_err_hist["X",-1,, env_obj$i,s,"resamp"] <- env_obj$XY_errvar_draw["X","X",,,s]
		#print(apply(cov_err_hist["X",-1,, env_obj$i,s,"orig"], 1, summary))
		env_obj$cov_err_hist["Y",-1,, env_obj$i,s,"orig"] <- env_obj$cov_err_hist["Y",-1,, env_obj$i,s,"resamp"] <- env_obj$XY_errvar_draw["Y","Y",,,s]
		#print(apply(cov_err_hist["Y",-1,, env_obj$i,s,"orig"], 1, summary))
		env_obj$cov_err_hist["cov",-1,, env_obj$i,s,"orig"] <- env_obj$cov_err_hist["cov",-1,, env_obj$i,s,"resamp"] <- env_obj$XY_errvar_draw["X","Y",,,s]	
		#print(apply(cov_err_hist["cov",-1,, env_obj$i,s,"orig"], 1, summary))
		
		#multiply by gradient since later will be variance of theta
		
			
	
		#mk_prev is starting X,Y coordinate and logv, bearing from the starting point of the interval
		#from that, take fractions of dt for each observation

		for (p in 1:env_obj$npart) {

			for (k in 1:env_obj$nstates) {
			
				#mk actual is x_{t-1}, where do we go from here with each lambda_t, to each observation?
				env_obj$mk_prev[,k,p,s] <- keep_finite(env_obj$f(mk=env_obj$mk_actual[,p,s], new_logv=env_obj$logv_angle_mu_draw[p,"logv",k, s], 
													   theta=env_obj$logv_angle_mu_draw[p,"turn",k, s], dtprev=env_obj$reg_dt)) #a_{t+1}
				
			
				#mk_prev[4,k,p,s] <- normalize_angle(mk_prev[4,k,p,s])
				Fx_tmp <- keep_finite(env_obj$Fx(mk=env_obj$mk_actual[,p,s], dtprev=env_obj$reg_dt))
				
				env_obj$Pk_prev[,,k,p,s] <- keep_finite(as.matrix(Matrix::nearPD(keep_finite(keep_finite(keep_finite(Fx_tmp %*% env_obj$Pk_actual[,,p,s]) %*% t(Fx_tmp)) + env_obj$Qt[,,k,p, s]), ensureSymmetry=TRUE)$mat)) #R_{t+1}

			}

			
			
		}
		
		
		
		
		#add a little fudge so always have interpolation, never occur at the boundary
		shark_rows <- rownames(env_obj$ynext) == s
		env_obj$j_list[[ s ]][[ env_obj$i ]] <- pmin(pmax((env_obj$ynext[shark_rows,"date_as_sec"] - env_obj$t_reg[env_obj$i])/env_obj$reg_dt, 1e-5), 1-(1e-5))
				
		if (env_obj$show_prints) print(paste("j:", paste(round(env_obj$j_list[[ s ]][[ env_obj$i ]], digits=4), collapse=", ")))
				
		
		env_obj$MuY[[ s ]] <- array(NA, dim=c(2, env_obj$yobs_sharks[ s ], env_obj$nstates, env_obj$npart), dimnames=list(c("X","Y"), 1:env_obj$yobs_sharks[ s ], env_obj$state_names, env_obj$pnames)) 
		#rep(list(rep(list(rep(list(matrix(NA, ncol=1, nrow=2)), env_obj$yobs_sharks[ s ])), env_obj$nstates)), env_obj$npart)
		env_obj$SigY[[ s ]] <- array(NA, dim=c(2,2, env_obj$yobs_sharks[ s ], env_obj$nstates, env_obj$npart), dimnames=list(c("X","Y"), c("X","Y"), 1:env_obj$yobs_sharks[ s ], env_obj$state_names, env_obj$pnames)) 
		#rep(list(rep(list(rep(list(diag(2)), env_obj$yobs_sharks[ s ])), env_obj$nstates)), env_obj$npart)
		#Pk_prev_interp[[ s ]] <- array(NA, dim=c(4,4, env_obj$yobs_sharks[ s ], env_obj$nstates, env_obj$npart), dimnames=list(1:4,1:4, 1:env_obj$yobs_sharks[ s ], state_names, pnames)) 
		#rep(list(rep(list(rep(list(diag(4)), env_obj$yobs_sharks[ s ])), env_obj$nstates)), env_obj$npart)	
		env_obj$Kgain[[ s ]] <- array(NA, dim=c(4,2, env_obj$yobs_sharks[ s ], env_obj$nstates, env_obj$npart), dimnames=list(1:4,c("X","Y"), 1:env_obj$yobs_sharks[ s ], env_obj$state_names, env_obj$pnames)) 
		#rep(list(rep(list(rep(list(matrix(0, ncol=2, nrow=4)), env_obj$yobs_sharks[ s ])), env_obj$nstates)), env_obj$npart)	

		
		
		
		#print(env_obj$yobs_sharks)
		#prediction of y is the direct interpolation
		#j_tmp i the differences, not the original fractions
		j_tmp <- diff(c(0, env_obj$j_list[[ s ]][[ env_obj$i ]]))
		
		
		for (p in 1:env_obj$npart) {
			for (k in 1:env_obj$nstates) {
				
				mk_tmp <- c(env_obj$mk_prev[,k,p,s])
				Pk_tmp <- env_obj$Pk_prev[,,k,p,s]
			
				for (y in 1:env_obj$yobs_sharks[ s ]) {					
					
					if (y > 1) {
						#take previous x-y values and starting logv and bearing
						mk_tmp[1:2] <- env_obj$MuY[[ s ]][,y-1,k,p]
						Pk_tmp[1:2, 1:2] <- env_obj$SigY[[ s ]][,,y-1,k,p]
					}
										
					# env_obj$MuY[[ s ]][,y,k,p] <- keep_finite(env_obj$h(mk=env_obj$mk_prev[,k,p,s], dtprev=j[ y ] * env_obj$reg_dt))
					env_obj$MuY[[ s ]][,y,k,p] <- keep_finite(env_obj$h(mk=mk_tmp, dtprev=j_tmp[ y ] * env_obj$reg_dt))
					
					Hx_tmp <- keep_finite(env_obj$Hx(mk=mk_tmp, dtprev=j_tmp[ y ] * env_obj$reg_dt))
					# print("Hx")
					# print(Hx_tmp)
					# print("Pk")
					# print(Pk_tmp)
					# print("all")
					# print(Hx_tmp %*% Pk_tmp)
					# print(Hx_tmp %*% Pk_tmp %*% t(Hx_tmp))
					# print("errvar")
					# print(env_obj$XY_errvar_draw[,,k,p,s])
								
					# env_obj$SigY[[ s ]][,,y,k,p] <- keep_finite(as.matrix(Matrix::nearPD(keep_finite(Hx_tmp %*% env_obj$Pk_prev[,,k,p,s] %*% t(Hx_tmp)  + (j_tmp[ y ] * env_obj$XY_errvar_draw[,,k,p,s])), ensureSymmetry=TRUE)$mat))
					env_obj$SigY[[ s ]][,,y,k,p] <- keep_finite(as.matrix(Matrix::nearPD(keep_finite(keep_finite(keep_finite(Hx_tmp %*% Pk_tmp) %*% t(Hx_tmp))  + (j_tmp[ y ] * env_obj$XY_errvar_draw[,,k,p,s])), ensureSymmetry=TRUE)$mat))

					
					
					# Fx_tmp <- Fx(mk=mk_prev[,k,p,s], dtprev=j_list[[ s ]][[ i ]][ y ]*env_obj$reg_dt)
					# Pk_prev_interp[[ s ]][,,y,k,p] <- as.matrix(Matrix::nearPD(Fx_tmp%*%Pk_actual[,,p,s]%*%t(Fx_tmp) + (j_list[[ s ]][[ i ]][ y ]^2)*Qt[,,k,p], ensureSymmetry=TRUE)$mat) #R_{t+1}
								
					# MuY[[ s ]][,y,k,p] <- keep_finite(h(mk=Xpart[p,,k,"curr",s], dtprev=j_list[[ s ]][[ i ]][ y ]*env_obj$reg_dt))
					# Hx_tmp <- Hx(mk=Xpart[p,,k,"curr",s], dtprev=j_list[[ s ]][[ i ]][ y ]*env_obj$reg_dt)
					# SigY[[ s ]][,,y,k,p] <- keep_finite(as.matrix(Matrix::nearPD(Hx_tmp%*%Pk_prev_interp[[ s ]][,,y,k,p]%*%t(Hx_tmp)  + (j_list[[ s ]][[ i ]][ y ]^2)*XY_errvar_draw[,,k,p,s], ensureSymmetry=TRUE)$mat))
				
				}
			}
		}
		

	}
	
	invisible(NULL)

}
	