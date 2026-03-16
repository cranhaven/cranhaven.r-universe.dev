sharks_with_obs_propagate_EKF_interp_joint <- function(env_obj) {

	env_obj$Pk[,,,,env_obj$sharks_with_obs] <- 0
	env_obj$mk[,,,env_obj$sharks_with_obs] <- 0 				
	
	for (s in env_obj$sharks_with_obs) {
		
		#print(env_obj$lambda_matrix[,env_obj$i,s])
		#print(env_obj$j_list[[ s ]][[ env_obj$i ]])
		
		j_tmp <- diff(c(0, env_obj$j_list[[ s ]][[ env_obj$i ]]))
		
		for (p in 1:env_obj$npart) {
			z <- env_obj$lambda_matrix[p,env_obj$i,s] 
			
			#move everything to the first column
			#Xpart[p,,"state1","curr",s] <- Xpart[p,,z,"curr",s]
		
			#draw values for xt|x_{t-1}, lambda_t.  We are not here drawing values for y_t

			#update sufficient statistics for each particle
			#new estimates
							
			for (y in 1:env_obj$yobs_sharks[ s ]) {	
				#Hx_tmp <- Hx(mk=env_obj$mk_prev[[ p ]][[ k ]], dtprev=env_obj$j_list[[ env_obj$i ]][ y ]*env_obj$reg_dt)
				#mk_tmp is the location and bearing and log at each obs time step
				
				if (y==1) {  mk_tmp <- env_obj$mk_prev[,z,p,s] }		 
				else { 	mk_tmp <- c(env_obj$MuY[[ s ]][,y-1,z,p], env_obj$mk_prev[3:4,z,p,s]) }	
				
				Hx_tmp <- keep_finite(env_obj$Hx(mk=mk_tmp, dtprev=j_tmp[ y ] * env_obj$reg_dt))	

				env_obj$Kgain[[ s ]][,,y,z,p] <- keep_finite(keep_finite(env_obj$Pk_prev[,,z,p,s] %*% t(Hx_tmp)) %*% keep_finite(MASS::ginv(env_obj$SigY[[ s ]][,,y,z,p])))
				
				
				#env_obj$Pk[,,z,p,s] <- as.matrix(Matrix::nearPD(env_obj$Pk[,,z,p,s] + env_obj$Pk_prev_interp[[ s ]][,,y,z,p] - Kgain[[ s ]][,,y,z,p]%*%env_obj$SigY[[ s ]][,,y,z,p]%*%t(Kgain[[ s ]][,,y,z,p]), ensureSymmetry=TRUE)$mat)
				#sequentially add to env_obj$Pk variance
				env_obj$Pk[,,z,p,s] <- as.matrix(keep_finite(env_obj$Pk[,,z,p,s] - keep_finite(keep_finite(env_obj$Kgain[[ s ]][,,y,z,p] %*% env_obj$SigY[[ s ]][,,y,z,p]) %*% t(env_obj$Kgain[[ s ]][,,y,z,p]))))	
				#env_obj$mk[,z,p,s] <- env_obj$mk[,z,p,s] + (Kgain[[ s ]][,,y,z,p]%*%t(ynext[ which(rownames(ynext)==s)[ y ],c("X","Y"), drop=FALSE] - h(mk=env_obj$mk_prev[,z,p,s], dtprev=env_obj$j_list[[ s ]][[ env_obj$i ]][ y ]*env_obj$reg_dt)))
				env_obj$mk[,z,p,s] <- keep_finite(env_obj$mk[,z,p,s] + keep_finite(env_obj$Kgain[[ s ]][,,y,z,p] %*% keep_finite(t(env_obj$ynext[ which(rownames(env_obj$ynext)==s)[ y ], c("X","Y"), drop=FALSE] - env_obj$h(mk=mk_tmp, dtprev=j_tmp[ y ] * env_obj$reg_dt)))))
			
				env_obj$mk[4,z,p,s] <- normalize_angle(env_obj$mk[4,z,p,s])
				
			}
			
			
			
			env_obj$Pk[,,z,p,s] <- keep_finite(as.matrix(Matrix::nearPD(keep_finite(env_obj$Pk[,,z,p,s] + env_obj$Pk_prev[,,z,p,s]), ensureSymmetry=TRUE)$mat))	
			
			#do this because have to add env_obj$mk_prev at the end because loop over yobs
			#mk is the cumulative  sum of Kalman gains
		
			
			env_obj$mk[,z,p,s] <- keep_finite(env_obj$mk_prev[,z,p,s] + env_obj$mk[,z,p,s])
			env_obj$mk[4,z,p,s] <- normalize_angle(env_obj$mk[4,z,p,s])
			
			#finish updating the sufficient statistics		
			env_obj$Pk_actual[,, p,s] <- env_obj$Pk[,,z, p,s]
			env_obj$mk_actual[,p,s] <- env_obj$mk[,z,p,s]

						
			if (env_obj$truncate_to_map) {   
				
				tmp <- reject_sampling(mu=env_obj$mk_actual[,p,s], cmat=env_obj$Pk_actual[,,p,s], prev_val=env_obj$Xpart_history[env_obj$i-1, c("X","Y","log_speed","bearing_rad"),p,s], obj=env_obj)
			
				env_obj$Xpart_history[env_obj$i, c("X","Y","log_speed","bearing_rad"),p,s] <- keep_finite(tmp$val)
				env_obj$num_reject[ p ] <- tmp$iter
			}
			else {   
				#Xpart[p,,1,"next_t",s]  <- mvtnorm::rmvnorm(n=1, mean=env_obj$mk_actual[,p,s], sigma=env_obj$Pk_actual[,,p,s])  
				env_obj$Xpart_history[env_obj$i, c("X","Y","log_speed","bearing_rad"),p,s]  <- keep_finite(mvtnorm::rmvnorm(n=1, mean=env_obj$mk_actual[,p,s], sigma=env_obj$Pk_actual[,,p,s]))  
				#Xpart_history[ env_obj$i,c("X","Y","logv","bearing_rad"),p,s]  <- mvtnorm::rmvnorm(n=1, mean=h(mk=Xpart_history[i-1,c("X","Y","logv","bearing_rad"),p,s], dtprev=env_obj$reg_dt), sigma=Qt[,,z,p,s])  
			}

		}
		
		# store history
		env_obj$mk_actual_history[env_obj$i,,,s] <- env_obj$mk_actual[,,s]

		
		
		# restrict logvelocity to range so variance doesn't blow up
		env_obj$Xpart_history[env_obj$i, "log_speed",, env_obj$s] <- pmin(pmax(env_obj$logvelocity_truncate[1], env_obj$Xpart_history[env_obj$i, "log_speed",, env_obj$s]), env_obj$logvelocity_truncate[2])
	
		
		if (env_obj$truncate_to_map) {
			env_obj$reject_samp_hist[env_obj$i,,s] <- c(mean(env_obj$num_reject), median(env_obj$num_reject))
		}
		
	}#loop over particles and sharks
	

	#print(paste("recalculating x dist and propagating",round(times[9]-times[8], digits=3),"sec"))

	#recalculate turn angle at time t-1 to get to locations at time t
	#Xpart[,"logv",1,"curr",sharks_with_obs] <- normalize_logv(Xpart[,"logv",1,"curr",sharks_with_obs])
	

	#bearing is in radians
	
	env_obj$Xpart_history[env_obj$i,"bearing_rad",,env_obj$sharks_with_obs] <- normalize_angle(env_obj$Xpart_history[ env_obj$i,"bearing_rad",,env_obj$sharks_with_obs])
	env_obj$Xpart_history[env_obj$i,"turn_rad",, env_obj$sharks_with_obs] <- normalize_angle(env_obj$Xpart_history[env_obj$i,"bearing_rad",,env_obj$sharks_with_obs] - env_obj$Xpart_history[env_obj$i-1,"bearing_rad",,env_obj$sharks_with_obs])
	
	# env_obj$mk_actual[,,env_obj$sharks_with_obs] <- env_obj$Xpart_history[env_obj$i, c("X","Y","logv","bearing_rad"),, env_obj$sharks_with_obs]
	
			
	
	#print("tmp")
	#print(Xpart_history[ env_obj$i,c("X","Y","logv","bearing_rad"),,sharks_with_obs])
	
	env_obj$Xpart_history[ env_obj$i, "lambda",, env_obj$sharks_with_obs] <- env_obj$lambda_matrix[,env_obj$i, env_obj$sharks_with_obs ]
	
	#env_obj$Xpart_history[ env_obj$i,"turn_rad",,sharks_with_obs] <- new_turn_rad   
	#env_obj$Xpart_history[ env_obj$i,"turn_log",,sharks_with_obs] <- rad2log(new_turn_rad)
	
	#print(Xpart_history[(i-1):i, "lambda", c(1,80), sharks_with_obs])
	#print(shark_intervals)

	for (s in env_obj$sharks_with_obs) {
		
		region <- apply(env_obj$Xpart_history[env_obj$i, c("X","Y"),,s], 2, function(x) which_region(newcoord=x, centroid=env_obj$centroids)) 	
		env_obj$Xpart_history[env_obj$i, "region",,s] <- region 	
	 
		env_obj$region_counts[ ,, s ][ cbind(1:env_obj$npart, region) ] <- env_obj$region_counts[ ,, s ][ cbind(1:env_obj$npart, region) ] + 1 
		
		if (env_obj$nstates > 1) {
			if (any(is.na(env_obj$Xpart_history[env_obj$i-1, "lambda",,s]))) { print(s) ; print(env_obj$shark_intervals[[ s ]]); print(env_obj$ii); print(env_obj$Xpart_history[1:env_obj$i,"lambda",,s]) }
			same_state <- env_obj$Xpart_history[env_obj$i, "lambda",,s] == env_obj$Xpart_history[env_obj$i-1, "lambda",,s]
	
		
			env_obj$Xpart_history[env_obj$i, "time_in_state",same_state,s] <- env_obj$Xpart_history[env_obj$i-1, "time_in_state",same_state,s] + 1
			env_obj$Xpart_history[env_obj$i, "time_in_state",same_state==FALSE,s] <- 1
			env_obj$Xpart_history[env_obj$i, "state_change",,s] <- as.numeric(apply(env_obj$Xpart_history[(env_obj$i-1):env_obj$i,"lambda",,s],2,function(x) paste0(x, collapse="")))
								
		}	
		
		#sum errors 
		
		for (p in 1:env_obj$npart) {
			z <- env_obj$Xpart_history[env_obj$i, "lambda",p,s]
			env_obj$errs[, p, s] <- apply(env_obj$SigY[[ s ]][,,,z,p], 1, sum)[c(1,2,4)]
		}
	}#end looping over sharks
	
	invisible(NULL)
}
						