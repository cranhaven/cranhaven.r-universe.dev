sharks_with_obs_propagate_EKF_1d_interp_joint <- function(env_obj) {

	#propagation of unobserved X particles using resampled particles
	num_reject <- c()

	env_obj$Pk[,,,,] <- 0
	env_obj$mk[,,,] <-0 				
	
	for (s in env_obj$sharks_with_obs) {
		
		#print(lambda_matrix[,i,s])
		#print(j_list[[ s ]][[ i ]])
		
		j_tmp <- diff(c(0, env_obj$j_list[[ s ]][[ env_obj$i ]]))
		
		for (p in 1:env_obj$npart) {
			z <- env_obj$lambda_matrix[p,env_obj$i,s] 
			
			#move everything to the first column
			#Xpart[p,,"state1","curr",s] <- Xpart[p,,z,"curr",s]
		
			#draw values for xt|x_{t-1}, lambda_t.  We are not here drawing values for y_t

			#update sufficient statistics for each particle
			#new estimates
	
			for (y in 1:env_obj$yobs_sharks[ s ]) {	
				#Hx_tmp <- Hx(mk=mk_prev[[ p ]][[ k ]], dtprev=j_list[[ i ]][ y ]*reg_dt)
				
				if (y==1) {  mk_tmp <- env_obj$mk_prev[,z,p,s] }		 
				else { 	mk_tmp <- c(env_obj$MuY[[ s ]][y-1,z,p], env_obj$mk_prev[2,z,p,s]) }	
				
				Hx_tmp <- keep_finite(env_obj$Hx(mk=mk_tmp, dtprev=j_tmp[ y ] * env_obj$reg_dt))	
				
				#print(Pk_prev_interp[[ s ]][,,y,k,p])
				#print(Hx_tmp)
				#print(ginv(SigY[[ s ]][,,y,k,p]))
				env_obj$Kgain[[ s ]][,y,z,p] <- keep_finite(keep_finite(env_obj$Pk_prev[,,z,p,s] %*% t(Hx_tmp)) %*% keep_finite(MASS::ginv(env_obj$SigY[[ s ]][y,z,p])))
				
				#Kgain[[ s ]][,y,k,p] <-  (Pk_prev_interp[[ s ]][,,y,k,p]%*%t(Hx_tmp))%*%ginv(SigY[[ s ]][y,k,p])
				#print(Pk_prev_interp[[ s ]][,,y,k,p] - Kgain[[ s ]][,,y,k,p]%*%SigY[[ s ]][,,y,k,p]%*%t(Kgain[[ s ]][,,y,k,p]))
				#print(Kgain[[ s ]][,,y,k,p])
				#print(Pk_prev_interp[[ s ]][,,y,k,p])
				#print(SigY[[ s ]][,,y,k,p])
				#print(Pk_prev_interp[[ s ]][,,y,k,p] - Kgain[[ s ]][,,y,k,p]%*%SigY[[ s ]][,,y,k,p]%*%t(Kgain[[ s ]][,,y,k,p]))	
				
				
				ktmp <- as.matrix(env_obj$Kgain[[ s ]][,y,z,p], ncol=1)
			
				#Pk[,,z,p,s] <- as.matrix(Matrix::nearPD(Pk[,,z,p,s] + Pk_prev_interp[[ s ]][,,y,k,p] - ktmp%*%SigY[[ s ]][y,k,p]%*%t(ktmp), ensureSymmetry=TRUE)$mat)
			
				env_obj$Pk[,,z,p,s] <- keep_finite(env_obj$Pk[,,z,p,s] - keep_finite(keep_finite(ktmp %*% env_obj$SigY[[ s ]][y,z,p]) %*% t(ktmp)))
					
				env_obj$mk[,z,p,s] <- keep_finite(env_obj$mk[,z,p,s] + keep_finite(ktmp %*% keep_finite(t(env_obj$ynext[ which(rownames(env_obj$ynext)==s)[ y ], "X", drop=FALSE] - env_obj$h(mk=mk_tmp, dtprev=j_tmp[ y ] * env_obj$reg_dt)))))
					
				#mk[,z,p,s] <- mk[,z,p,s] + (ktmp%*%t(ynext[ which(rownames(ynext)==s)[ y ],"X", drop=FALSE] - h(mk=mk_prev[,z,p,s], dtprev=j_list[[ s ]][[ i ]][ y ]*reg_dt)))
				#mk[4,z,p,s] <- normalize_angle( mk[4,z,p,s])
				
				
			}
			
			env_obj$Pk[,,z,p,s] <- keep_finite(as.matrix(Matrix::nearPD(keep_finite(env_obj$Pk[,,z,p,s]  + env_obj$Pk_prev[,,z,p,s]), ensureSymmetry=TRUE)$mat))	
			#do this because have to add mk_prev at the end because loop over yobs
			env_obj$mk[,z,p,s] <- keep_finite(env_obj$mk_prev[,z,p,s] + env_obj$mk[,z,p,s])
			#finish updating the sufficient statistics		
			env_obj$Pk_actual[,,p,s] <- env_obj$Pk[,,z, p,s]
			env_obj$mk_actual[,p,s] <- env_obj$mk[,z,p,s]
		

		 
			env_obj$Xpart_history[env_obj$i, c("X","velocity"),p,s]  <- keep_finite(mvtnorm::rmvnorm(n=1, mean=env_obj$mk_actual[,p,s], sigma=env_obj$Pk_actual[,,p,s]))  
			
									
		}
		
		# store history
		env_obj$mk_actual_history[env_obj$i,,,s] <- env_obj$mk_actual[,,s]

	}#loop over particles and sharks


	if (env_obj$show_prints) print("recalculating x dist and propagating")

	#recalculate turn angle at time t-1 to get to locations at time t
	#Xpart_history[i,"logv",,sharks_with_obs] <- normalize_logv(Xpart_history[i,"logv",,sharks_with_obs])
	#bearing is in radians
	#print(Xpart[,"bearing_rad",1,"curr",sharks_with_obs])
		
				
	#print("tmp")
	#print(Xpart_history[i,c("X","Y","logv","bearing_rad"),,sharks_with_obs])
	
	
	env_obj$Xpart_history[env_obj$i,"lambda",,env_obj$sharks_with_obs] <- env_obj$lambda_matrix[,env_obj$i, env_obj$sharks_with_obs ]
	
	#print(Xpart_history[(i-1):i, "lambda", c(1,80), sharks_with_obs])
	#print(shark_intervals)

	for (s in env_obj$sharks_with_obs) {
		
		#store the estimates
		#Xpart_history[i,c("X","logv"),,s] <- t(Xpart[,,1,"next_t", s])
		
		if (env_obj$nstates > 1) {
			#if (any(is.na(Xpart_history[i-1, "lambda",,s]))) { print(s) ; print(shark_intervals[[ s ]]); print(i); print(Xpart_history[1:i,"lambda",,s]) }
			same_state <- env_obj$Xpart_history[env_obj$i, "lambda",,s] == env_obj$Xpart_history[env_obj$i-1, "lambda",,s]
				
			env_obj$Xpart_history[env_obj$i, "time_in_state",same_state,s] <- env_obj$Xpart_history[env_obj$i-1, "time_in_state", same_state,s] +1
			env_obj$Xpart_history[env_obj$i, "time_in_state",same_state==FALSE,s] <- 1
			env_obj$Xpart_history[env_obj$i, "state_change",,s] <- as.numeric(apply(env_obj$Xpart_history[(env_obj$i-1):env_obj$i,"lambda",,s],2,function(x) paste0(x, collapse="")))
								
		}	
		#sum covariance components across observations, for each particle	
		
		for (p in 1:env_obj$npart) {
			z <- env_obj$Xpart_history[env_obj$i, "lambda", p, s]
			env_obj$errs[p, s] <- sum(env_obj$SigY[[ s ]][,z,p])
		}
		
		
	}#end looping over sharks
	
	invisible(NULL)

	
}