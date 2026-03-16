plotting_EKF_1d_interp_joint <- function(env_obj) {

	 
	lgd <- format(Sys.time(), "%Y_%m_%d_%H_%M_%S") 

	if (env_obj$output_plot) {
		# make sure filename comes out valid
		default_prefix = "EKF_1D"
		if (is.null(env_obj$pdf_prefix) | env_obj$pdf_prefix =="") {
			env_obj$pdf_prefix = default_prefix
		}
		else if (! (substr(env_obj$pdf_prefix, 1, 1) %in% c(letters, LETTERS))) {
			env_obj$pdf_prefix = paste("EKF", env_obj$pdf_prefix, sep="_")
		}
		
		
		grDevices::pdf(paste(env_obj$output_dir, "/", env_obj$pdf_prefix, "_", lgd, ".pdf", sep=""))
	}
	
	half <- ceiling(env_obj$N/2)
	
	env_obj$d <- as.data.frame(env_obj$d)
	env_obj$d$tag <- as.factor(env_obj$tags)
	
	old_pars <- par(mfrow=par()$mfrow, mfcol=par()$mfcol, las=par()$las)
	on.exit(expr=par(old_pars))

	par(mfrow=c(ifelse(env_obj$compare_with_known, 2, 3), 1), las=1)

	#select time gaps within limit
	for (s in env_obj$shark_names) {
	
		#select time gaps within limit
		
		a <- env_obj$d[ env_obj$tags==s & env_obj$d[,"time_to_next"] < env_obj$reg_dt * env_obj$max_int_wo_obs & ! is.na(env_obj$d[,"time_to_next"]),"time_to_next"]
		
		if (length(a) > 1) {
		
			dm <- density_min2(x=a, m1=min(a), m2=max(a))
			
			plot(dm, ylim=c(0, max(dm$y)*1.1), main=paste("Distribution of observed time steps, shark ",s, " (N=", length(a), ")", sep=""), xlab="Seconds", las=1)
			abline(v=c(env_obj$reg_dt, median(a), mean(a)), lwd=2, lty=1:3, col="red")
			legend("topright", legend=c("reg dt","median(obs dt)","mean(obs dt)"), lwd=2, lty=1:3, col="red")
		}
	}
	

	matplot(env_obj$resample_history, type="b", col=1:env_obj$nsharks, xlim=c(1, env_obj$N), ylim=c(0, 1), main="Fraction of particles resampled at each step", xlab="Step", ylab="Fraction", pch=19, cex=0.7, las=1)
	legend("bottomleft", lty=1, col=1:env_obj$nsharks, legend=env_obj$shark_names, cex=0.5, pch=19, pt.cex=0.7)
			
	
	# matplot(env_obj$eff_size_hist/env_obj$npart, type="b", col=1:env_obj$nsharks, xlim=c(1,env_obj$N), ylim=c(0,1), main="Effective sample size / #particles", xlab="Step", ylab="Fraction", pch=19, cex=0.7, las=1)
	# legend("bottomleft", lty=1, col=1:env_obj$nsharks, legend=env_obj$shark_names, cex=0.5)
	# abline(h=env_obj$neff_sample, lty=3)

	plot(x=-5, y=-5, xlim=c(1, env_obj$N), ylim=c(0, 1), main="Effective sample size / #particles", xlab="Step", ylab="Fraction", las=1)
	abline(h=env_obj$neff_sample, lty=3)
	pch_mat <- matrix(1, ncol=env_obj$nsharks, nrow=env_obj$N)
	pch_mat[ env_obj$eff_size_hist <= env_obj$neff_sample * env_obj$npart ] <- 19
	pch_mat[ is.na(env_obj$eff_size_hist) ] <- NA
	colnames(pch_mat) <- env_obj$shark_names
	
	for (ss in 1:env_obj$nsharks) {
		points(x=1:env_obj$N, y=env_obj$eff_size_hist[,ss] / env_obj$npart, pch=pch_mat[,ss], type="b", cex=0.7, col=ss)
	}
	legend("bottomleft", lty=1, col=1:env_obj$nsharks, legend=env_obj$shark_names, cex=0.5, pch=19, pt.cex=0.7)
		
		
	
	
	state_colors <- env_obj$states
	
	if (env_obj$nstates > 1) {
	
		pct_foraging <- apply(env_obj$lambda_matrix, c(2,3), function(x) sum(x==1, na.rm=FALSE)/env_obj$npart)
		pct_foraging <- apply(pct_foraging, 2, function(x) cumsum(x)/pmax(cumsum(! is.na(x)),1))

		colnames(pct_foraging) <- env_obj$shark_names
		
		#agree_table <- matrix(0, ncol=nsharks, nrow=1+nstates+
		
		
		matplot(pct_foraging, type="b", col=1:env_obj$nsharks, xlim=c(1, env_obj$N), ylim=c(0, 1), main="Cumulative fraction of regular steps simulated to be state 1 (foraging)", xlab="Step", ylab="Fraction", pch=19, cex=0.7, las=1)
		legend("bottomleft", lty=1, col=1:env_obj$nsharks, legend=env_obj$shark_names, cex=0.5)
		

		
		print("d")
		print(env_obj$d)
		
				
		par(mfrow=c(ifelse(env_obj$smoothing, 3, 2), 1), las=1)
		
		#agreement with only observed states
		for (s in env_obj$shark_names) {
		
			ids <- which(env_obj$tags == s)
			obs_lambda <- env_obj$states[ ids ]
			reg_intervals <- env_obj$d[ ids,"t_intervals"]
			#rows are indices of observations
			
			#each observation may may to one regular step, but a regular step may have more than one observation in it.
			#reg_intervals <- d_tmp[ ids,"t_intervals"]
			
			#create mapping between regular intervals and the observations
			#want to group together observed steps that are in the same interval
			
			
			reg_int_list <- matrix(NA, ncol=2, nrow=max(unique(reg_intervals)))
			colnames(reg_int_list) <- c("first","last")
			#find the starting and ending observation in each regular interval that has one.
			for (tt in unique(reg_intervals)) {
				
				reg_int_list[tt,] <- range(which(reg_intervals == tt))
			}
			reg_int_list <- na.omit(reg_int_list)
			#ignore if only one observation
						

			reg_int_list <- reg_int_list[ reg_int_list[,"last"] > reg_int_list[,"first"],, drop=FALSE]
			#subtract one to give left edge of box (bar 1 goes from 0 to 1, for instance)
			reg_int_list[ ,"first"] <- reg_int_list[ ,"first"]-1
			
			
			reg2obs_as <- matrix(env_obj$lambda_matrix[,reg_intervals,s], nrow=env_obj$npart, ncol=length(ids))
			obs_lambda_mat <- matrix(obs_lambda, ncol=length(ids), nrow=env_obj$npart, byrow=TRUE)
			agree_final <- apply(reg2obs_as == obs_lambda_mat, 2, mean)

			reg2obs_bs <- matrix(env_obj$lambda_matrix_beforesamp[,reg_intervals,s], nrow=env_obj$npart, ncol=length(ids))
			agree_bs <- apply(reg2obs_bs == obs_lambda_mat, 2, mean)

			#before resampling
			barplot(agree_bs, col=obs_lambda, main=paste("Fraction of regular step particles agreeing in state to observed\nblack=1, red=2 (before resampling), shark", s), 
					ylim=c(-0.1,1), border=NA, space=0, xlab="Observed steps", ylab="Fraction", las=1)
			if (nrow(reg_int_list)) { 
				rect(xleft=reg_int_list[,"first"], xright=reg_int_list[,"last"], ytop=rep(1.1, ncol(reg_int_list)), ybottom=rep(0, ncol(reg_int_list)), border="green")
				legend("bottomleft", legend=">1 obs. in single regular interval, so height of bar is constant here", pch=0, col="green", cex=0.8, bg="white")
			}
			loc_tmp <- round(seq(1, length(ids), length.out=min(length(ids),10)))
			axis(1, loc_tmp, at=loc_tmp - 0.5)
			
			#after resampling
			barplot(agree_final, col=obs_lambda, main=paste("Fraction of regular step particles agreeing in state to observed,\nblack=1, red=2 (final), shark",s), 
					ylim=c(-0.1,1), border=NA, space=0, xlab="Observed steps", ylab="Fraction", las=1)
			
			axis(1, loc_tmp, at=loc_tmp)
			if (nrow(reg_int_list)) { 
				rect(xleft=reg_int_list[,"first"], xright=reg_int_list[,"last"], ytop=rep(1.1, ncol(reg_int_list)), ybottom=rep(0, ncol(reg_int_list)), border="green")
				legend("bottomleft", legend=">1 obs. in single regular interval, so height of bar is constant here", pch=0, col="green", cex=0.8, bg="white")
			}
			
			env_obj$agree_table["overall",s] <- mean(reg2obs_as == obs_lambda_mat, na.rm=TRUE)
			
			print(paste("Shark", s, "proportion state agreement:", round(env_obj$agree_table["overall",s], digits=3)))
			
			for (k in 1:env_obj$nstates) {
				
				env_obj$agree_table[paste("state",k),s] <- mean(reg2obs_as[, obs_lambda==k] == obs_lambda_mat[, obs_lambda==k], na.rm=TRUE)
				print(paste("Shark",s,"proportion state", k, "agreement:", round(env_obj$agree_table[paste("state",k),s], digits=3)))
			}	
			

			if (env_obj$compare_with_known) {
				ids_true <- which(env_obj$known_regular_step_ds$tag == s & env_obj$known_regular_step_ds$t_intervals <= max(env_obj$shark_valid_steps[[ s ]]) + 1) 
				true_lambda <- as.numeric(env_obj$known_regular_step_ds$state.guess2[ ids_true ])
				true_lambda_mat <-  matrix(true_lambda, ncol=length(ids_true), nrow=env_obj$npart, byrow=TRUE)
					
					
				reg2true <- matrix(env_obj$lambda_matrix[, env_obj$known_regular_step_ds$t_intervals[ ids_true ], s], nrow=env_obj$npart, ncol=length(ids_true))
				env_obj$agree_table["true overall",s] <- mean(reg2true == true_lambda_mat, na.rm=TRUE)
				
				print(paste("Shark",s,"proportion state agreement with true:", round(env_obj$agree_table["true overall",s], digits=3)))
				
				for (k in 1:env_obj$nstates) {
				
					env_obj$agree_table[paste("true state",k),s] <- mean(reg2true[, true_lambda==k] == true_lambda_mat[, true_lambda==k], na.rm=TRUE)
					print(paste("Shark",s,"proportion state", k, "agreement with true:", round(env_obj$agree_table[paste("true state",k),s], digits=3)))
				}
				
				agree_true <- apply(reg2true == true_lambda_mat, 2, mean)

			
				barplot(agree_true, col=obs_lambda, main=paste("Fraction of regular step particles agreeing in state to TRUE\nblack=1, red=2 (final), shark", s), 
					ylim=c(-0.1,1.1), border=NA, space=0, xlab="True steps", ylab="Fraction", las=1)
				loc_tmp_true <- round(seq(1, length(ids_true), length.out=min(length(ids_true),10)))
				axis(1, loc_tmp_true, at=loc_tmp_true - 0.5)
				
			}	
				
				
				
			single_obs_intervals <- table(reg_intervals)
			single_obs_intervals <- as.numeric(names(single_obs_intervals[ single_obs_intervals==1]))
			
			if (length(single_obs_intervals)) { 
				#print(single_obs_intervals)
				reg2obs_one <- matrix(env_obj$lambda_matrix[,single_obs_intervals,s], nrow=env_obj$npart, ncol=length(single_obs_intervals))
				obs_lambda_one <- matrix(obs_lambda[ reg_intervals %in% single_obs_intervals  ], ncol=length(single_obs_intervals), nrow=env_obj$npart, byrow=TRUE)
				
				#print("reg")
				#print(reg_intervals)
				#print("reg in single")
				#print(reg_intervals[ reg_intervals %in% single_obs_intervals ])
				#agree_one <- apply(reg2obs_one == obs_lambda_mat, 2, mean)

				env_obj$agree_table["single",s] <- 	mean(reg2obs_one==obs_lambda_one, na.rm=TRUE)
				print(paste("Shark",s,"proportion state agreement for single observations:", round(env_obj$agree_table["single",s], digits=3)))
			}
			
			
			#smoothing
			if (env_obj$smoothing & (env_obj$fix_smoothed_behaviors==FALSE)) {
			
				smooth2obs <- matrix(t(env_obj$Xpart_history_smoothed[reg_intervals,"lambda",,s]), nrow=env_obj$npart, ncol=length(reg_intervals))
				tmp <- apply( smooth2obs, 1, function(x) x==obs_lambda)
				
				agree_smoothed <- apply(tmp , 1, mean)
			
				env_obj$agree_table_smoothed["overall",s] <- mean(tmp)
			
				print(paste("Shark",s,"smoothed proportion state agreement:", round(env_obj$agree_table_smoothed["overall",s], digits=3)))
			
				
				for (k in 1:env_obj$nstates) {
				
					env_obj$agree_table_smoothed[paste("state",k),s] <- mean( apply(smooth2obs[, obs_lambda==k, drop=FALSE], 1, function(x) x == obs_lambda[ obs_lambda==k]))
					print(paste("Shark",s,"smoothed proportion state", k, "agreement:", round(env_obj$agree_table_smoothed[paste("state",k),s], digits=3)))
				}	
			
							
				#after resampling
				barplot(agree_smoothed, col=obs_lambda, main=paste("Fraction of smoothed step particles agreeing in state to observed\nblack=1, red=2, shark", s), 
						ylim=c(-0.1,1.1), border=NA, space=0, xlab="Observed steps", ylab="Fraction", las=1, xaxt="n" )
				
				if (nrow(reg_int_list)) { 
					rect(xleft=reg_int_list[,"first"], xright=reg_int_list[,"last"], ytop=rep(1.1, ncol(reg_int_list)), ybottom=rep(0, ncol(reg_int_list)), border="green")
					legend("bottomleft", legend=">1 obs. in single regular interval, so height of bar is constant here", pch=0, col="green", cex=0.8, bg="white")
				}
				
				loc_tmp <- round(seq(1, length(ids), length.out=min(length(ids),10)))
				axis(1, loc_tmp, at=loc_tmp - 0.5)
			
				if (env_obj$compare_with_known) {
				
				
									
					smooth2true <- matrix(t(env_obj$Xpart_history_smoothed[env_obj$known_regular_step_ds$t_intervals[ ids_true ],"lambda",,s]), nrow=env_obj$npart, ncol=length(ids_true))
					env_obj$agree_table_smoothed["true overall",s] <- mean(apply(smooth2true, 1, function(x) x == true_lambda))
					
					
					print(paste("Shark",s,"proportion smoothed state agreement with true:", round(env_obj$agree_table_smoothed["true overall",s], digits=3)))
					
					for (k in 1:env_obj$nstates) {
					
						env_obj$agree_table_smoothed[paste("true state",k),s] <- mean(apply(smooth2true[, true_lambda==k, drop=FALSE], 1, function(x) x == true_lambda[ true_lambda==k ]))
						print(paste("Shark",s,"proportion smoothed state", k, "agreement with true:", round(env_obj$agree_table_smoothed[paste("true state",k),s], digits=3)))
					}
				
					smooth_agree_true <- apply(apply(smooth2true, 1, function(x) x== true_lambda), 1, mean)

			
					barplot(smooth_agree_true, col=true_lambda, main=paste("Fraction of smoothed step particles agreeing in state to TRUE,\nblack=1, red=2, shark", s), 
						ylim=c(-0.1,1.1), border=NA, space=0, xlab="True steps", ylab="Fraction", las=1, xaxt="n")
					axis(1, loc_tmp_true, at=loc_tmp_true - 0.5)

				}
				
				
			}	
			
		}#loop over sharks	
		

			

	}#if multiple states
	
	
	#errors

	env_obj$error_final_allpart <- array(NA, dim=c(env_obj$N, env_obj$npart, env_obj$nsharks), dimnames=list(env_obj$Nnames, env_obj$pnames, env_obj$shark_names))
	
	if (env_obj$smoothing) { 
		env_obj$error_smoothed_allpart <-  array(NA, dim=c(env_obj$N, env_obj$smooth_iter, env_obj$nsharks), dimnames=list(env_obj$Nnames, 1:env_obj$smooth_iter, env_obj$shark_names)) 
		env_obj$error_smoothed_quantiles <- env_obj$error_final_quantiles
	}	
	
	for (s in env_obj$shark_names) {
		
		for (i in env_obj$shark_intervals[[ s ]]) {
		
			obs <- env_obj$d[ env_obj$tags==s , "X"][ env_obj$d[env_obj$tags==s,"t_intervals"]==i ]
			
			env_obj$error_final_allpart[i,,s] <- apply(env_obj$Xpart_history[ i, c("X","velocity"),,s], 2, function(x) sum(abs(env_obj$h(mk=x, dtprev=env_obj$j_list[[ s ]][[ i ]] * env_obj$reg_dt) - obs)))		
			env_obj$error_final_quantiles[i,,s] <- quantile(env_obj$error_final_allpart[i,,s], p=c(.1, .5, .9), na.rm=TRUE)
			
			if (env_obj$smoothing) {

				#print(apply(env_obj$Xpart_history_smoothed[ i, c("X","logv"),,s], 2, function(x) sum(abs(as.vector(env_obj$h(mk=x, dtprev=env_obj$j_list[[ s ]][[ i ]] * env_obj$reg_dt)) - obs)))		)
				env_obj$error_smoothed_allpart[i,,s] <- apply(env_obj$Xpart_history_smoothed[ i, c("X","velocity"),,s], 2, function(x) sum(abs(env_obj$h(mk=x, dtprev=env_obj$j_list[[ s ]][[ i ]] * env_obj$reg_dt) - obs)))		
				
				env_obj$error_smoothed_quantiles[i,,s] <- quantile(env_obj$error_smoothed_allpart[i,,s], p=c(.1, .5, .9), na.rm=TRUE)
			}
	
		}#loop over i
	}#loop over sharks	  
	
	par(mfrow=c(ifelse(env_obj$smoothing,3,2),1))
	legend_quantiles <- function() legend("topleft", legend=c("Q90","Q50","Q10"), lty=3:1, cex=0.7, col=3:1, pch=1)
	pretty_axis <- pretty(x=1:env_obj$N, n=6)
	pretty_axis_intervals <- function() axis(side=1, at=pretty_axis, labels=pretty_axis)


	for (s in env_obj$shark_names) {	
		
		tmp <- cbind(env_obj$error_beforesamp_quantiles[,,s], env_obj$error_final_quantiles[,,s])
		if (env_obj$smoothing) { tmp <- cbind(tmp, env_obj$error_smoothed_quantiles[,,s]) }
		yrange <- range(tmp, na.rm=TRUE)
		yrange[1] <- min(yrange[1],0)
		
		#print(yrange)
		
		boxplot(t(env_obj$error_beforesamp_allpart[,,s]), xaxt="n", las=1, main=paste("Distance from observed (filtered before resampling), shark",s),
			ylab="distance", xlab="timestep", ylim=yrange, outline=FALSE, staplewex=0.2, boxfill="white", medlwd=2)
		pretty_axis_intervals()

		
		boxplot(t(env_obj$error_final_allpart[,,s]), xaxt="n", las=1, main=paste("Distance from observed (final filtered), shark",s),
			ylab="distance", xlab="timestep", ylim=yrange, outline=FALSE, staplewex=0.2, boxfill="white", medlwd=2)
		pretty_axis_intervals()

		# matplot(env_obj$error_beforesamp_quantiles[,,s], type="b", lty=1:3, main=paste("Distance from observed (filtered before resampling), shark",s), ylab="distance", xlab="timestep", ylim=yrange, cex=0.5, pch=1, las=1)
		# legend_quantiles()
		
		# matplot(env_obj$error_final_quantiles[,,s], type="b", lty=1:3, main=paste("Distance from observed (final filtered), shark",s), ylab="distance", xlab="timestep", ylim=yrange, cex=0.5, pch=1, las=1)
		# legend_quantiles()
		
		if (env_obj$smoothing) {
			
			boxplot(t(env_obj$error_smoothed_quantiles[,,s]), xaxt="n", las=1, main=paste("Distance from observed (smoothed)), shark",s),
				ylab="distance", xlab="timestep", ylim=yrange, outline=FALSE, staplewex=0.2,
				boxcol=2, whiskcol=2, outcol=2, medcol=2, staplecol=2, boxfill="white", medlwd=2)
			pretty_axis_intervals()
			
			# matplot(env_obj$error_smoothed_quantiles[,,s], type="b", lty=1:3, main=paste("Distance from observed (smoothed), shark", s), ylab="distance", xlab="timestep", ylim=yrange, cex=0.5, pch=1, las=1)
			# legend_quantiles()		
		}
		
	}
	#covariance components

	if (env_obj$compare_with_known) {

		
		print(env_obj$shark_intervals)
	
		par(mfrow=c(2 + env_obj$smoothing,1))
		env_obj$error_final_true_allpart <- array(NA, dim=c(env_obj$N, env_obj$npart, env_obj$nsharks), dimnames=list(env_obj$Nnames, env_obj$pnames, env_obj$shark_names)) 
		env_obj$error_final_true_quantiles <- array(NA, dim=c(env_obj$N, 3, env_obj$nsharks), dimnames=list(env_obj$Nnames, 1:3, env_obj$shark_names))
		env_obj$error_euclidean_estimate_true_from_obs <- env_obj$euclidean_estimate_true_from_obs <- array(NA, dim=c(env_obj$N, env_obj$nsharks), dimnames=list(env_obj$Nnames, env_obj$shark_names)) 

		if (env_obj$smoothing) { 
			env_obj$error_smoothed_true_allpart <- array(NA, dim=c(env_obj$N, env_obj$smooth_iter, env_obj$nsharks), dimnames=list(env_obj$Nnames, 1:env_obj$smooth_iter, env_obj$shark_names)) 
			env_obj$error_smoothed_true_quantiles <- env_obj$error_final_true_quantiles
		}

	
		for (s in env_obj$shark_names) {
		
			d_of_shark <- env_obj$d[env_obj$d$tag == s,]
			obs_d_time_range <- range(d_of_shark$date_as_sec, na.rm=TRUE)
			
			true_shark_ds <- env_obj$known_regular_step_ds[env_obj$known_regular_step_ds$tag == s,]
			true_shark_ds <- true_shark_ds[true_shark_ds$date_as_sec >= obs_d_time_range[1] & true_shark_ds$date_as_sec <= obs_d_time_range[2],]
			
			shark_intervals_to_use <- env_obj$shark_valid_steps[[ s ]][ env_obj$shark_valid_steps[[ s ]] %in% true_shark_ds$t_intervals[true_shark_ds$t_intervals < env_obj$N]]
			
		
			for (i in shark_intervals_to_use) {
			
				ids <- which(true_shark_ds$t_intervals==i)
				# if it's the first interval, there will be two
				ids <- ids[length(ids)]
				
				obs <- true_shark_ds$X[ ids ]
										
				env_obj$error_final_true_allpart[i+1,,s] <- abs(env_obj$Xpart_history[i+1 , "X",,s] - obs)
				env_obj$error_final_true_quantiles[i+1,,s] <- quantile(env_obj$error_final_true_allpart[i+1,,s], p=c(.1, .5, .9), na.rm=TRUE)					
				
				if (env_obj$smoothing) {
					
					env_obj$error_smoothed_true_allpart[i+1,,s] <- abs(env_obj$Xpart_history_smoothed[ i+1, "X",,s] - obs)		
					env_obj$error_smoothed_true_quantiles[i+1,,s] <- quantile(env_obj$error_smoothed_true_allpart[i+1,,s], p=c(.1, .5, .9), na.rm=TRUE)
				}
					
    	
				#get indices of rows of nearest previous observations
				
				nearby_obs_indices <- c(max(which(d_of_shark$date_as_sec <= env_obj$t_reg[i+1])), min(which(d_of_shark$date_as_sec >= env_obj$t_reg[i+1])))
				
				if (diff(nearby_obs_indices) == 0) {
					euc_true_pred <- d_of_shark$X[ nearby_obs_indices[1] ]
				}
				else {
					#interpolate observed velocity
					
					obs_velocity <- diff(d_of_shark$X[nearby_obs_indices])/diff(d_of_shark$date_as_sec[nearby_obs_indices])
					
					euc_true_pred <- d_of_shark$X[nearby_obs_indices[1]] + obs_velocity * (env_obj$t_reg[i+1] - d_of_shark$date_as_sec[nearby_obs_indices[1]])
				}
				
				
				env_obj$euclidean_estimate_true_from_obs[i+1,s] <- euc_true_pred
				env_obj$error_euclidean_estimate_true_from_obs[i+1,s] <- sum(abs(euc_true_pred - obs))

			}#loop over i
		
			
			tmp <- cbind(env_obj$error_final_true_quantiles[,,s], env_obj$error_euclidean_estimate_true_from_obs[,s])
			
			if (env_obj$smoothing) { tmp <- cbind(tmp, env_obj$error_smoothed_true_quantiles[,,s]) }
			
			yrange <- range(tmp, na.rm=TRUE)
			yrange[1] <- min(yrange[1],0)
			
			boxplot(t(env_obj$error_final_true_quantiles[,,s]), xaxt="n", las=1, main=paste("Distance from TRUE (final filtered), shark",s),
				ylab="distance", xlab="timestep", ylim=yrange, outline=FALSE, staplewex=0.2, boxfill="white", medlwd=2)
			pretty_axis_intervals()
			
			# matplot(env_obj$error_final_true_quantiles[,,s], type="b", lty=1:3, main=paste("Distance from TRUE (final filtered), shark", s), ylab="distance", xlab="timestep", ylim=yrange, cex=0.5, pch=1, las=1)
			# legend_quantiles()
		
			if (env_obj$smoothing) {
				boxplot(t(env_obj$error_smoothed_true_quantiles[,,s]), xaxt="n", las=1, main=paste("Distance from TRUE (smoothed), shark",s),
					ylab="distance", xlab="timestep", ylim=yrange, outline=FALSE, staplewex=0.2,
					boxcol=2, whiskcol=2, outcol=2, medcol=2, staplecol=2, boxfill="white", medlwd=2)
				pretty_axis_intervals()
				
				# matplot(env_obj$error_smoothed_true_quantiles[,,s], type="b", lty=1:3, main=paste("Distance from TRUE (smoothed), shark", s), ylab="distance", xlab="timestep", ylim=yrange, cex=0.5, pch=1, las=1)
				# legend_quantiles()
			}
			
			plot(env_obj$error_euclidean_estimate_true_from_obs[,s], type="b", ylab="distance", xlab="timestep", ylim=yrange, cex=0.5, pch=1, las=1, main=paste("Distance from TRUE (Euclidean estimate from observed), shark", s), xaxt="n")
			pretty_axis_intervals()
			
		}#loop over sharks
	
	}#if compare

	
	#print estimates of alpha and beta densities
	
	nresamp <- 500*env_obj$npart
	
	env_obj$param_draws <- env_obj$variance_draws <- velocity_draws <- array(NA, dim=c(nresamp, env_obj$nstates, env_obj$nsharks), dimnames=list(1:nresamp, env_obj$state_names, env_obj$shark_names))
	param_sampling_weights <- rep(1, env_obj$npart)

	if (env_obj$smooth_parameters) {
		env_obj$param_draws_smoothed <- env_obj$param_draws
		env_obj$variance_draws_smoothed <- env_obj$variance_draws
		velocity_draws_smoothed <- velocity_draws
	 }
	
	
	par(mfrow=c(2,1))
	
	for (s in env_obj$shark_names) {
	#par(mfrow=c(3,1))
		ids <- rep(1:env_obj$npart, nresamp/env_obj$npart) #low_var_sample(wts=param_sampling_weights, M=resamp)
		  
		tab <- table(factor(ids, levels=1:env_obj$npart))

		for (p in which(tab > 0)) {
					
			 for (k in 1:env_obj$nstates) {

				env_obj$variance_draws[ ids==p,k,s] <- MCMCpack::rinvgamma(n=tab[ p ], shape=env_obj$sigma_pars[p,2*k-1,s], scale=env_obj$sigma_pars[p,2*k,s])
				env_obj$param_draws[ ids==p, k, s] <- rnorm(n=tab[ p ], mean=env_obj$mu[k, "mu", p, s], sd=sqrt(env_obj$mu[k, "V", p, s] * env_obj$variance_draws[ ids==p,k,s]))
			
				if (env_obj$smooth_parameters) {
					
					env_obj$variance_draws_smoothed[ ids==p,k,s] <- MCMCpack::rinvgamma(n=tab[ p ], shape=env_obj$sigma_pars_smoothed[p,2*k-1,s], scale=env_obj$sigma_pars_smoothed[p,2*k,s])			
					env_obj$param_draws_smoothed[ ids==p,k, s] <- rnorm(n=tab[ p ], mean=env_obj$mu_smoothed[k,"mu",p,s], sd=sqrt(env_obj$mu_smoothed[k, "V", p, s] * env_obj$variance_draws_smoothed[ ids==p,k,s])) 
					
				}
			
			
			}	
	
		}
		
		for (k in 1:env_obj$nstates) {
			
			velocity_draws[,k,s] <- rnorm(n=nresamp, mean=env_obj$param_draws[,k,s], sd=sqrt(env_obj$variance_draws[,k,s]))
	
			if (env_obj$smooth_parameters) {
				
				velocity_draws_smoothed[,k,s] <- rnorm(n=nresamp, mean=env_obj$param_draws_smoothed[,k,s], sd=sqrt(env_obj$variance_draws_smoothed[,k,s]))	
			
			}
		}
		
		
	}	
	
	
	logv_modeled_densities <- lapply(env_obj$shark_names, function(ss) lapply(1:env_obj$nstates, function(k) density(x=velocity_draws[ ,k,ss])))
	#lapply(env_obj$shark_names, function(ss) apply(velocity_draws[,,ss,drop=FALSE], 2, density_min2))
	#print(modeled_densities)
	logv_obs_densities <- lapply(env_obj$shark_names, function(ss) lapply(1:env_obj$nstates, function(k) density_min2(x=env_obj$d[ env_obj$tags==ss & env_obj$d[,"state.guess2"]==k, "velocity"])))
	#print(observed_densities)
	names(logv_modeled_densities) <- names(logv_obs_densities) <- env_obj$shark_names
	#print(sapply(modeled_densities, function(x) sapply(x, function(ii) ii$y)))
	
	stab_N <- array("", dim=c(env_obj$nsharks, 4, env_obj$nstates), dimnames=list(env_obj$shark_names, c("part.","obs.","smooth.","true"), 1:env_obj$nstates))	
	stab_N[,"obs.",] <- paste("N=", t(sapply(env_obj$shark_names, function(ss) table(factor(env_obj$d[env_obj$tags==ss,"state.guess2"][ env_obj$tags ==ss ], levels=1:env_obj$nstates)))))

	legend_labels <- c("part.", "obs.")
	legend_lwd <- c(1,2)
	legend_lty <- c(1,2)
	
	logv_obs_x <- sapply(logv_obs_densities, function(x) sapply(x, function(ii) ii$x))
	logv_obs_y <- sapply(logv_obs_densities, function(x) sapply(x, function(ii) ii$y))
	
	
	ylim_logv <- max(c(sapply(logv_modeled_densities, function(x) sapply(x, function(ii) ii$y)), logv_obs_y), na.rm=TRUE)
	xlim_logv <- matrix(quantile(x=c(sapply(logv_modeled_densities, function(z) sapply(z, function(ii) ii$x)), logv_obs_x[! is.na(logv_obs_y)]), probs=c(0.025, 0.975), na.rm=TRUE), ncol=2)

	
	if (env_obj$smooth_parameters) {
		logv_modeled_smoothed_densities <- lapply(env_obj$shark_names, function(ss) lapply(1:env_obj$nstates, function(k) density(x=env_obj$param_draws_smoothed[ ,k, ss])))
		names(logv_modeled_smoothed_densities) <- env_obj$shark_names
		
		
		ylim_logv <- max(ylim_logv, sapply(logv_modeled_smoothed_densities, function(x) sapply(x, function(ii) ii$y)), na.rm=TRUE)
		xlim_logv <- rbind(xlim_logv, quantile(x=sapply(logv_modeled_smoothed_densities, function(x) sapply(x, function(ii) ii$x)), probs=c(0.025, 0.975), na.rm=TRUE))
		
		legend_labels <- c(legend_labels, "smooth.")
		legend_lwd <- c(legend_lwd, 2)
		legend_lty <- c(legend_lty, 3)
	}
	
	
	if (env_obj$compare_with_known) {
		logv_true_densities <- lapply(env_obj$shark_names, function(ss) lapply(1:env_obj$nstates, function(k) density_min2(x=env_obj$known_regular_step_ds[env_obj$known_regular_step_ds$tag==ss & env_obj$known_regular_step_ds$state.guess2==k, "velocity"])))
		names(logv_true_densities) <- env_obj$shark_names

		stab_N[,"true",] <- paste("N=", t(sapply(env_obj$shark_names, function(ss) table(factor(env_obj$known_regular_step_ds[env_obj$known_regular_step_ds$tag==ss,"state.guess2"], levels=1:env_obj$nstates)))), sep="")
		
		legend_labels <- c(legend_labels, "true")
		legend_lwd <- c(legend_lwd, 1)
		legend_lty <- c(legend_lty, 2)
		
		logv_true_x <- sapply(logv_true_densities, function(x) sapply(x, function(ii) ii$x))
		logv_true_y <- sapply(logv_true_densities, function(x) sapply(x, function(ii) ii$y))
		
		ylim_logv <- max(c(ylim_logv, logv_true_y), na.rm=TRUE)
		xlim_logv <- rbind(xlim_logv, quantile(x=logv_true_x[! is.na(logv_true_y)], probs=c(0.025, 0.975), na.rm=TRUE))

	}
	
	legend_col <- rep(1:env_obj$nstates, each=length(legend_labels))
	legend_txt <- rep(legend_labels, env_obj$nstates)
	
	if (env_obj$nstates > 1) {
		legend_labels_txt <- paste(legend_txt, ", state ", legend_col, sep="")
	}
	else {
		legend_labels_txt <- legend_txt
	} 
		
	
	legend_lty <- rep(legend_lty, env_obj$nstates)
	legend_lwd <- rep(legend_lwd, env_obj$nstates)
	xlim_logv <- c(min(xlim_logv[,1], na.rm=TRUE), max(xlim_logv[,2], na.rm=TRUE))

	
		
	for (s in env_obj$shark_names) {
	
		legend_N <- stab_N[s,legend_labels,]
		legend_tmp <- function() { legend("topleft", col=legend_col, lty=legend_lty, lwd=legend_lwd, cex=0.7, ncol=env_obj$nstates, legend=paste(legend_labels_txt, legend_N)) } 

	
		plot(x=-10, y=-10, xlim=xlim_logv, ylim=c(0, ylim_logv), xlab="alpha", ylab="Density", main=paste("Density of velocity (per second), shark",s), las=1)
		for (k in 1:env_obj$nstates) {
			
			lines(logv_modeled_densities[[ s ]][[ k ]], col=k, lty=1)
	
			if (env_obj$smooth_parameters) {
				lines(logv_modeled_smoothed_densities[[ s ]][[ k ]], col=k, lty=3, lwd=2)
			
			}
			
			lines(logv_obs_densities[[ s ]][[ k ]], col=k, lty=2, lwd=2)
				
			if (env_obj$compare_with_known) {
				lines(logv_true_densities[[ s ]][[ k ]], col=k, lty=2, lwd=1)

			}
			
		
		}
	
		legend_tmp()
		
	}
				
	if (env_obj$nstates > 1) {
	
				
		if (env_obj$time_dep_trans==FALSE) {
		
			
			
			env_obj$region_trans_draws <-		array(NA, dim=c(nresamp, env_obj$nstates, env_obj$nstates, env_obj$nsharks), dimnames=list(1:nresamp, env_obj$state_names, env_obj$state_names, env_obj$shark_names))
				
			env_obj$region_foraging_draws <-	array(NA, dim=c(nresamp, env_obj$nsharks), dimnames=list(1:nresamp, env_obj$shark_names))
			
						
			
			
			for (s in env_obj$shark_names) {
					
				#regions <- apply(d[tags==s,c("X","Y")], 1, function(x) which_region(x, centroid=centroids))
				trans_actual <- table(factor(env_obj$states[ env_obj$tags==s ], levels=1:env_obj$nstates), factor(env_obj$next_states[ env_obj$tags==s ], levels=1:env_obj$nstates))
				
				trans_actual <- matrix(trans_actual, ncol=1)	
				
				rownames(trans_actual) <- c("1to1","2to1","1to2","2to2")
			
				if (env_obj$compare_with_known==FALSE || is.null(env_obj$known_trans_prob))  {
					trans_prob_actual <- matrix(NA, nrow=2, ncol=1)
					cs1 <- colSums(trans_actual[c(1,3),,drop=FALSE])
					trans_prob_actual[1,cs1> 0] <- trans_actual[3,cs1> 0]/cs1[ cs1> 0]
					cs2 <- colSums(trans_actual[c(2,4),,drop=FALSE])
					trans_prob_actual[2,cs2> 0] <- trans_actual[2,cs2> 0]/cs2[ cs2> 0]
					print("transition probabilities:")
					print(trans_prob_actual)
				}
				else {
					
					trans_prob_actual <- env_obj$known_trans_prob[ rownames(env_obj$known_trans_prob)==s,]
					print(trans_prob_actual)
				}
				
				
				par(mfrow=c(2,1))
				matplot(t(env_obj$trans_mean[,,s]), type="l", col=1:2, lty=1, ylim=c(0,1), main=paste("Mean state-to-state transition probabilities, shark",s), ylab="probability", xlab="timestep", lwd=1.5)
			 
				#polygon(c(1:(N-1), (N-1):1), c(trans_median[,1], rev(trans_median[,3])), border=NA, col=rgb(169,169,169,alpha=50, max=255))
				#polygon(c(1:(N-1), (N-1):1), c(trans_median[,4], rev(trans_median[,6])), border=NA, col=rgb(250,128,114,alpha=50, max=255))

				if (env_obj$compare_with_known & ! is.null(env_obj$known_trans_prob)) {
					abline(h=trans_prob_actual[,"all",drop=FALSE], col=1:2, lty=2)
				}	
				else {
					abline(h=sum(trans_actual[3,])/sum(trans_actual[c(1,3),]), col=1, lty=2)
					abline(h=sum(trans_actual[2,])/sum(trans_actual[c(2,4),]), col=2, lty=2)
				}
				legend("topright", col=1:2, lty=1, legend=c("Pr(1 -> 2)","Pr(2 -> 1)"), lwd=1.5, cex=0.7)

				region_tab <- 100
				
				

				#convergence diagnostics

				#fraction each particle was in a region
				#frac_part_by_region <- apply(region_counts,2, function(x) x/sum(x))

				param_sampling_weights <- rep(1, env_obj$npart)
				plot(x=-4, y=-4, xlim=c(0,1), ylim=c(0,2), main=paste("CIs for behavior transition probability, shark",s), xlab="probability", ylab="", las=1, yaxt="n")
				
				ids <- rep(1:env_obj$npart, nresamp/env_obj$npart) #low_var_sample(wts=param_sampling_weights, M=resamp)
				  
				tab <- table(factor(ids, levels=1:env_obj$npart))
				for (p in which(tab > 0)) {
					dp <- env_obj$transition_mat[[ s ]][[ p ]]$dirichlet_pars[ 1,]
					
					tc <- env_obj$transition_mat[[ s ]][[ p ]]$counts[1, ]
					
					#if no observed transitions
					
					if (sum(tc) > 0) {
					
						for (k in 1:env_obj$nstates) {
							
							env_obj$region_trans_draws[ids==p,k,, s] <- MCMCpack::rdirichlet(n=tab[ p ], alpha=dp[ (env_obj$nstates*k-1):(env_obj$nstates*k) ])
							
						}
						for (ii in which(ids==p)) {	
							
							mat_tmp <- env_obj$region_trans_draws[ ii,,,s]
							eig_tmp <- eigen(t(mat_tmp))$vectors[,1]
							eig_tmp <- eig_tmp/sum(eig_tmp)
							env_obj$region_foraging_draws[ii, s] <- eig_tmp[ 1 ]
							#env_obj$region_foraging_draws[ids==p, s] <- (sum(tc[1:2])*rbeta(n=tab[ p ], shape1=dp[1], shape2=dp[2]) + sum(tc[3:4])*rbeta(n=tab[ p ], shape1=dp[3], shape2=dp[4]))/sum(tc)
						}
					}
				
				}

				emp_q <- HDInterval::hdi(env_obj$region_foraging_draws[,s, drop=FALSE], na.rm=TRUE) #quantile(env_obj$region_foraging_draws[,s, drop=FALSE], p=c(0.025, 0.975), na.rm=TRUE)
				
				lines(x=emp_q, y=c(0, 0), lty=1, type="b", col=1, pch=19, cex=.8)
				
				for (k in 1:env_obj$nstates) {
					emp_q <- HDInterval::hdi(env_obj$region_trans_draws[,k,which((1:env_obj$nstates) !=k) , s, drop=FALSE], na.rm=TRUE) #quantile(env_obj$region_trans_draws[,k, s, drop=FALSE], p=c(0.025, 0.975))
					lines(x=emp_q, y=c(k, k), lty=2, type="b", col=k, pch=19, cex=.8, lwd=1.5)
				}
				
				points(x=trans_prob_actual[,1], y=1:env_obj$nstates, col=1:env_obj$nstates, pch=1) 	
				legend("topright", col=c(1,1:env_obj$nstates), lty=c(1,2,2), legend=c("Pr(foraging)","Pr(1 -> 2)", "Pr(2 -> 1)"), cex=.5)
		
			}#end looping over sharks
			
			
			transition_densities <- lapply(env_obj$shark_names, function(ss) lapply(1:env_obj$nstates, function(k) density_min2(x=env_obj$region_trans_draws[,k,which((1:env_obj$nstates) !=k) , ss], m1=0, m2=1)))
			names(transition_densities) <- env_obj$shark_names
			
			foraging_densities <- lapply(env_obj$shark_names, function(ss) density_min2(x=env_obj$region_foraging_draws[,ss], m1=0, m2=1))
			names(foraging_densities) <- env_obj$shark_names
			
			ylim_mu <- max(sapply(transition_densities, function(x) sapply(x, function(ii) ii$y)), sapply(foraging_densities, function(ii) ii$y))
			
			
			for (ss in env_obj$shark_names) {
			
				#density probability
				plot(foraging_densities[[ ss ]], ylim=c(0,ylim_mu), xlim=c(0,1), col=1, lty=1, main=paste("Density of foraging and transition probabilities, shark",ss), xlab="Probability", ylab="density", las=1)
			
				if (! is.null(env_obj$known_foraging_prob)) { abline(v=env_obj$known_foraging_prob[ ss,1,drop=FALSE], col=1:env_obj$nregions, lty=1) }
				#legend("topright",col=1:env_obj$nregions, lty=1, legend=paste(1:env_obj$nregions,":",region_tab,"%"), cex=.5)
				
				for (k in 1:env_obj$nstates) {
					lines(transition_densities[[ ss ]][[ k ]], col=k, lty=2)
				}
				abline(v=trans_prob_actual[,1]+runif(2, min=-0.001, max=0.001), col=1:env_obj$nstates, lty=2)
							
				legend("topright",col=c(1,1:env_obj$nstates), lty=c(1,2,2), legend=c("Pr(foraging)","Pr(1 -> 2)", "Pr(2 -> 1)"), cex=.5)
		
			
			}#loop over sharks

		}#not time dependent
		else {

			tis <- 1:20
			env_obj$region_trans_draws <-	array(NA, dim=c(nresamp, length(tis), 2, env_obj$nsharks), dimnames=list(1:nresamp, tis, rep(env_obj$rnames, each=2), env_obj$shark_names))
		
			for (s in env_obj$shark_names) {
				par(mfrow=c(2,1))
				
				#r_with_obs <- which(r_tab==FALSE)
				
				ids <- rep(1:env_obj$npart, nresamp/env_obj$npart) #low_var_sample(wts=param_sampling_weights, M=resamp)
			  
				#ids <- sample(1:env_obj$npart, prob=region_counts[,r,drop=FALSE], replace=TRUE, size=resamp)
				tab <- table(factor(ids, levels=1:env_obj$npart))
					
				for (p in which(tab > 0)) {
							
					
					matrix(env_obj$transition_mat[[ s ]][[ p ]]$dirichlet_pars[ , c(1,4) ]/env_obj$transition_mat[[ s ]][[ p ]]$dirichlet_pars[, c(2,3) ], ncol=2)
					
					gamma_ratio <- cbind(rgamma(n=tab[ p ], shape=env_obj$transition_mat[[ s ]][[ p ]]$dirichlet_pars[1,2], rate=env_obj$transition_mat[[ s ]][[ p ]]$dirichlet_pars[1,1]),
											 rgamma(n=tab[ p ], shape=env_obj$transition_mat[[ s ]][[ p ]]$dirichlet_pars[1,3], rate=env_obj$transition_mat[[ s ]][[ p ]]$dirichlet_pars[1,4]))
					
						for (tt in tis) {
						
							env_obj$region_trans_draws[ ids==p,tt, ,s] <- 1-t(cbind(rbeta(n=tab[ p ], shape1=gamma_ratio[,1]*tt, shape2=1),
																				         rbeta(n=tab[ p ], shape1=gamma_ratio[,2]/tt, shape2=1)))
						}															
					
			
				}
				plot(x=-1, y=-1, xlim=range(tis), ylim=c(0,1), main=paste("Time-dependent median Pr(1 -> 2), shark",s), xlab="Number of steps in state", ylab="Probability", las=1)
					
										
				lines(apply(env_obj$region_trans_draws[,,1,s],2,mean), col=1, type="b")
						
				plot(x=-1, y=-1, xlim=range(tis), ylim=c(0,1), main=paste("Time-dependent median Pr(2 -> 1), shark",s), xlab="Number of steps in state", ylab="Probability", las=1)
					
										
				lines(apply(env_obj$region_trans_draws[,,2,s],2,mean), col=1, type="b")
				
				
				
			}
			
	
			
		}#time dependency
	
		
		if (env_obj$interact) {
		
	
			env_obj$interact_mu_draws <-  array(NA, dim=c(nresamp, env_obj$nstates-1, env_obj$nsharks), dimnames=list(1:nresamp, env_obj$state_names[ -env_obj$nstates ], env_obj$shark_names))
			pvals <- seq(0,1,by=0.1)
			env_obj$interact_intensity_draws <- array(NA, dim=c(nresamp, env_obj$nstates-1, length(pvals), env_obj$nsharks), 
												  dimnames=list(1:nresamp, env_obj$state_names[ -env_obj$nstates ], as.character(pvals), env_obj$shark_names))
			
			if (env_obj$smooth_parameters) {
				 env_obj$interact_mu_draws_smoothed <- env_obj$interact_mu_draws
				 env_obj$interact_intensity_draws_smoothed <- env_obj$interact_intensity_draws
			}
			
			
			
			for (s in env_obj$shark_names) {
				
			
				ids <- rep(1:env_obj$npart, nresamp/env_obj$npart) 
		  
				tab <- table(factor(ids, levels=1:env_obj$npart))
			
				for (p in which(tab > 0)) {
					#draw interaction parameter based on above
				
					env_obj$interact_mu_draws[ids==p,, s] <- matrix(rnorm(n=tab[ p ]*(env_obj$nstates-1), mean=env_obj$spatial_interact_pars[p, env_obj$mu_names,s], 
																		  sd=1/sqrt(env_obj$spatial_interact_pars[p, env_obj$prec_names,s])), ncol=env_obj$nstates-1, byrow=TRUE)
													
					for (pv in 1:length(pvals)) {
						for (k in 1:(env_obj$nstates-1)) {
							env_obj$interact_intensity_draws[ids==p,k,pv,s] <- rlnorm(n=tab[ p ], meanlog=env_obj$interact_mu_draws[ids==p,k,s]*pvals[ pv ], sdlog=1/sqrt(rep(env_obj$tau_vals[p,k], tab[ p ])))
						}
					}															
				}
				
				#print("interact mu summary")
				#print(apply(env_obj$interact_mu_draws[,,s, drop=FALSE], 2, summary))
				
				
				if (env_obj$smooth_parameters) {
					for (p in which(tab > 0)) {
						#draw interaction parameter based on above
					
						env_obj$interact_mu_draws_smoothed[ids==p,, s] <- matrix(rnorm(n=tab[ p ]*(env_obj$nstates-1), mean=env_obj$spatial_interact_pars_smoothed[p,env_obj$mu_names,s], 
																				 sd=1/sqrt(env_obj$spatial_interact_pars_smoothed[p, env_obj$prec_names,s])), ncol=env_obj$nstates-1, byrow=TRUE)
														
						for (pv in 1:length(pvals)) {
							for (k in 1:(env_obj$nstates-1)) {
								env_obj$interact_intensity_draws_smoothed[ids==p,k,pv,s] <- rlnorm(n=tab[ p ], meanlog=env_obj$interact_mu_draws_smoothed[ids==p,k,s]*pvals[ pv ], sdlog=1/sqrt(rep(env_obj$tau_vals[p,k], tab[ p ])))
							}
						}															
					}
				
				}
				
				
			}#loop over sharks to sample

			interact_legend <- function(lty=2) legend("bottomright", cex=0.8, lty=lty, legend="value if no interaction") 
			
					
			interact_mu_densities <- lapply(env_obj$shark_names, function(ss) density_min2(env_obj$interact_mu_draws[,1,ss]))
			names(interact_mu_densities) <- env_obj$shark_names
			
			ylim_mu <- max(sapply(interact_mu_densities, function(z) z$y))
			xlim_mu <- range(sapply(interact_mu_densities, function(z) z$x))
			

			par(mfrow=c(2,1))
			#par(mfrow=c(ifelse(env_obj$smoothing, 2, 3),1))			
			#basically we want to see when eta is bigger than -1/(2*tau*p_k) if so then it is spatially clustered.
			plot(x=-10, y=-10, xlim=xlim_mu, ylim=c(0, ylim_mu), lty=2, main="Density of foraging interaction mu", xlab="Value", ylab="Density", las=1)
			abline(h=0, v=0)
			for (s in 1:env_obj$nsharks) {
				lines(interact_mu_densities[[ s ]], col=s, lty=(s%%3)+1, lwd=1.5)
				
			}
			
			legend("topleft", lty=(1:env_obj$nsharks)%%3 +1, col=1:env_obj$nsharks, legend=env_obj$shark_names, lwd=1.5)
			
					
			plot(x=-4, y=-4, xlim=xlim_mu, ylim=c(1,env_obj$nsharks), main="CIs for interaction mu", xlab="Value", ylab="Shark", las=1, yaxt="n")
			abline(v=0, lty=2)
			axis(side=2, labels=env_obj$shark_names, at=1:env_obj$nsharks)
			for (s in 1:env_obj$nsharks) {
				
				emp_q <- HDInterval::hdi(env_obj$interact_mu_draws[,1,env_obj$shark_names[ s ]])
				#emp_q <- quantile(env_obj$interact_mu_draws[,1,env_obj$shark_names[ s ]], p=c(0.025, 0.975))
				lines(x=emp_q, y=c(s, s), lty=1, type="b", col=1, pch=19, cex=.8, lwd=1.5)
				
			}
			interact_legend()
		
			
			if (env_obj$smooth_parameters) {
			
				interact_mu_densities_smoothed <- lapply(env_obj$shark_names, function(ss) density_min2(env_obj$interact_mu_draws_smoothed[,1,ss]))
				names(interact_mu_densities_smoothed) <- env_obj$shark_names
				ylim_mu_sm <- max(sapply(interact_mu_densities_smoothed, function(z) z$y))
				xlim_mu_sm <- range(sapply(interact_mu_densities_smoothed, function(z) z$x))
			
				plot(x=-10, y=-10, xlim=xlim_mu_sm, ylim=c(0, ylim_mu_sm), lty=2, main="Density of foraging interaction mu (smoothed)", xlab="Value", ylab="Density", las=1)
				abline(h=0, v=0)

				for (s in 1:env_obj$nsharks) {
					lines(interact_mu_densities_smoothed[[ s ]], col=s, lty=(s%%3) + 1, lwd=1.5)
					
				}
				
				legend("topleft", lty=(1:env_obj$nsharks)%%3 + 1, col=1:env_obj$nsharks, legend=env_obj$shark_names, lwd=1.5)
			
				plot(x=-4, y=-4, xlim=xlim_mu_sm, ylim=c(1,env_obj$nsharks), main="CIs for interaction mu (smoothed)", xlab="Value", ylab="Shark", las=1, yaxt="n")
				abline(v=0, lty=2)
				axis(side=2, labels=env_obj$shark_names, at=1:env_obj$nsharks)
				for (s in 1:env_obj$nsharks) {
					
					emp_q <- HDInterval::hdi(env_obj$interact_mu_draws_smoothed[,1,env_obj$shark_names[ s ]])
					#emp_q <- quantile(env_obj$interact_mu_draws[,1,env_obj$shark_names[ s ]], p=c(0.025, 0.975))
					lines(x=emp_q, y=c(s, s), lty=1, type="b", col=1, pch=19, cex=.8, lwd=1.5)
					
				}
				interact_legend()
			
			
			
			}
			
			
		
		
			
		
		
			#CIs for interactions
			xlim_mu_ns <- quantile(env_obj$interact_intensity_draws[,1,,], probs=c(0.01, 0.99))
			xlim_mu_ns[1] <- min(0, xlim_mu_ns[1]) 

			for (s in env_obj$shark_names) {
				plot(x=-4, y=-4, xlim=xlim_mu_ns, ylim=c(0, length(pvals)+1), main=paste("CIs for interaction scale, shark", s, "by proportion of neib."), xlab="Value", ylab="proportion of neighborhood", las=1, yaxt="n")
				abline(v=1, lty=2)
				axis(side=2, labels=pvals, at=seq(0.5, 0.5+length(pvals)-1, by=1) )
				for (pv in 1:length(pvals)) {
						emp_q <- HDInterval::hdi(env_obj$interact_intensity_draws[,1,pv,s])
						#emp_q <- quantile(env_obj$interact_intensity_draws[,1,pv,s], p=c(0.025, 0.975))
						lines(x=emp_q, y=rep(pv, 2), lty=1, type="b", col=1, pch=19, cex=.8, lwd=1.5)
				}					
				interact_legend()
			}
			
			
			if ( any(! is.na(env_obj$spatial_interact_mu_history))) {
				matplot(env_obj$spatial_interact_mu_history[,1,], type="b", lwd=1.5, pch=19, cex=0.4, col=1:env_obj$nsharks, lty = (1:env_obj$nsharks)%%3 + 1, main="Median historical simulated interaction mu",  ylab="mu value")
				legend("bottomleft",  col=1:env_obj$nsharks, lwd=1.5, legend=env_obj$shark_names, bg="white", lty = (1:env_obj$nsharks)%%3 + 1)
			}
			
			if ( any(! is.na(env_obj$spatial_interact_intensity_history))) {
				matplot(env_obj$spatial_interact_intensity_history[,1,], type="b", lwd=1.5, pch=19, cex=0.4, col=1:env_obj$nsharks, lty = (1:env_obj$nsharks)%%3 + 1, main="Median historical interaction value (simulated over all neighborhood proportions)",  ylab="interaction value")
				legend("bottomleft",  col=1:env_obj$nsharks, lwd=1.5, legend=env_obj$shark_names, bg="white", lty = (1:env_obj$nsharks)%%3 + 1)
				
			}
					
		
			
			
		}#end interact

	
	
	
	
	}#if multiple states
	 


	closest_num <- min(10, env_obj$npart)
	nbins <- 100
	legend_closest <- function() legend("topright", pch=c(1, 124), col=c(1,2), cex=0.5, legend=c("part.", "obs."))
	intensity_gray_scale <- rev(gray.colors(n=nbins, start=0.025, end=0.975))

	legend_intensity <- function() legend("topright", pch=c(124, 22, 22), col=c(2,1,1), cex=0.5, pt.bg=c(NA, intensity_gray_scale[c(nbins, 1)]), legend=c("obs.", "max. dens.", "min. dens."), ncol=1)

	par(mfcol=c(2,1))
	for (s in env_obj$shark_names) {  

		all_x <- c(env_obj$Xpart_history[,"X",,s], env_obj$d[,"X"])
		dfrompts <- colSums(env_obj$error_final_allpart[,,s], na.rm=TRUE)
		closest <- order(dfrompts[1:closest_num])
		rx <- range(c(env_obj$Xpart_history[,"X",closest,s], env_obj$d[,"X"]), na.rm=TRUE)*c(1, 1.1)

		plot(x=-10, y=-10, ylim=c(0, closest_num), xlim=rx, main=paste("Closest trajectories to observed (final filtered), shark", s), xlab="X", yaxt="n", ylab="particle", las=1)
		abline(v=env_obj$d[env_obj$tags==s,"X"], col="red", lwd=1)

		
		for (jj in 1:length(closest)) {
			points(x=env_obj$Xpart_history[,"X", closest[ jj ],s], y=rep(jj, env_obj$N), cex=0.5)
		}
		
		legend_closest()
		
		if (env_obj$smoothing) {
			
			all_x <- c(all_x, env_obj$Xpart_history_smoothed[,"X",,s])
			dfrompts <- colSums(env_obj$error_smoothed_allpart[,,s], na.rm=TRUE)
			closest <- order(dfrompts[1:closest_num])
			rx <- range(c(env_obj$Xpart_history_smoothed[,"X",closest,s], env_obj$d[,"X"]), na.rm=TRUE)*c(1, 1.1)

		
			plot(x=-10, y=-10, ylim=c(0, closest_num), xlim=rx, main=paste("Closest trajectories to observed (smoothed), shark", s), xlab="X", yaxt="n", ylab="particle", las=1)
			abline(v=env_obj$d[env_obj$tags==s,"X"], col="red", lwd=1)
			for (jj in 1:length(closest)) {
				points(x=env_obj$Xpart_history_smoothed[,"X", closest[ jj ],s], y=rep(jj, env_obj$N), cex=0.5)
			}
			
			legend_closest()
		}
		
		rxi <- range(all_x, na.rm=TRUE)
		brks <- seq(rxi[1], rxi[2], length.out=nbins)
		 
		
		#print(brks)
		intens <- table(cut(env_obj$Xpart_history[,"X",,s], breaks=brks, right=FALSE))
		intens_level <- cut(intens, breaks=seq(from=0,to=max(intens, na.rm=TRUE),length.out=nbins + 1),right=FALSE,levels=1:nbins)
		#print(intens)
		#print(brks)

		
		plot(x=-10, y=-10, ylim=c(0,1), xlim=rxi*c(1, 1.15), main=paste("Density of predictions of observed loc. (final), shark", s), yaxt="n", xlab="X", ylab="")	
		rect(xleft=brks[-length(brks)], xright=brks[-1], ybottom=rep(0, nbins), ytop=rep(1,nbins), border=NA, col=intensity_gray_scale[ intens_level ])
		abline(v=env_obj$d[ env_obj$tags==s,"X"], col="red", lwd=1)
		legend_intensity()
		
		if (env_obj$smoothing) {
		
			intens <- table(cut(env_obj$Xpart_history_smoothed[,"X",,s], breaks=brks, right=FALSE))
			intens_level <- cut(intens, breaks=seq(from=0,to=max(intens, na.rm=TRUE), length.out=nbins + 1),right=FALSE,levels=1:nbins)
			plot(x=-10, y=-10, ylim=c(0,1), xlim=rxi*c(1, 1.15), main=paste("Density of predictions  of observed loc. (smoothed), shark", s), yaxt="n", xlab="X", ylab="")	
			rect(xleft=brks[-length(brks)], xright=brks[-1], ybottom=rep(0, nbins), ytop=rep(1, nbins), border=NA, col=intensity_gray_scale[ intens_level ])
			abline(v=env_obj$d[env_obj$tags==s,"X"], col="red", lwd=1)
			legend_intensity()
		}
	}	

	if (env_obj$compare_with_known) {
		legend_closest_true <- function() legend("topright", pch=c(1, 124), col=c(1,2), cex=0.5, legend=c("part.", "true loc."))

		legend_intensity_true <- function() legend("topright", pch=c(124, 22, 22), col=c(2,1,1), cex=0.5, pt.bg=c(NA, intensity_gray_scale[c(nbins, 1)]), legend=c("true loc.", "max. dens.", "min. dens."), ncol=1)


		for (s in env_obj$shark_names) {  

			all_x <- as.vector(env_obj$Xpart_history[,"X",,s])
			dfrompts <- colSums(env_obj$error_final_true_allpart[,,s], na.rm=TRUE)
			closest <- order(dfrompts[1:closest_num])
			rx <- range(c(env_obj$Xpart_history[,"X",closest,s], env_obj$known_regular_step_ds[env_obj$known_regular_step_ds$tag==s,"X"]), na.rm=TRUE)*c(1, 1.15)


			plot(x=-10, y=-10, ylim=c(0,closest_num), xlim=rx, main=paste("Closest trajectories to TRUE (final filtered), shark", s), xlab="X", yaxt="n", ylab="")
			abline(v=env_obj$known_regular_step_ds[env_obj$known_regular_step_ds$tag==s,"X"], col="red", lwd=1)
			for (jj in 1:length(closest)) {
				points(x=env_obj$Xpart_history[,"X", closest[ jj ],s], y=rep(jj, env_obj$N), cex=0.7)
			}
			legend_closest_true()
			
			if (env_obj$smoothing) {
			
				dfrompts <- colSums(env_obj$error_smoothed_true_allpart[,,s], na.rm=TRUE)
				closest <- order(dfrompts[1:closest_num])

				all_x <- c(all_x, env_obj$Xpart_history_smoothed[,"X",,s])
				rx <- range(c(env_obj$Xpart_history_smoothed[,"X",closest,s], env_obj$known_regular_step_ds[env_obj$known_regular_step_ds$tag==s,"X"]), na.rm=TRUE)*c(1, 1.15)

			
				plot(x=-10, y=-10, ylim=c(0,closest_num), xlim=rx, main=paste("Closest trajectories to TRUE (smoothed), shark", s), xlab="X", yaxt="n", ylab="")
				abline(v=env_obj$known_regular_step_ds[env_obj$known_regular_step_ds$tag==s,"X"], col="red", lwd=1)
				for (jj in 1:length(closest)) {
					points(x=env_obj$Xpart_history_smoothed[,"X", closest[ jj ],s], y=rep(jj, env_obj$N), cex=0.7)
				}
				
				legend_closest_true()
			}

			rxi <- range(all_x, na.rm=TRUE)
			brks <- seq(rxi[1], rxi[2], length.out=nbins)
			#print(brks)
			intens <- table(cut(env_obj$Xpart_history[,"X",,s], breaks=brks, right=FALSE))
			intens_level <- cut(intens, breaks=seq(from=0,to=max(intens, na.rm=TRUE), length.out=nbins + 1),right=FALSE,levels=1:nbins)
			#print(intens)
			#print(brks)
			
			
			plot(x=-10, y=-10, ylim=c(0,1), xlim=rxi*c(1, 1.15), main=paste("Density of predictions (final filtered), with TRUE, shark", s), yaxt="n", xlab="X", ylab="")	
			rect(xleft=brks[-length(brks)], xright=brks[-1], ybottom=rep(0,nbins), ytop=rep(1,nbins), border=NA, col=intensity_gray_scale[ intens_level ])
			abline(v=env_obj$known_regular_step_ds[ env_obj$known_regular_step_ds$tag==s,"X"], col="red", lwd=1)
			legend_intensity_true()
			
			if (env_obj$smoothing) {
			
				intens <- table(cut(env_obj$Xpart_history_smoothed[,"X",,s], breaks=brks, right=FALSE))
				intens_level <- cut(intens, breaks=seq(from=0,to=max(intens, na.rm=TRUE),length.out=nbins+ 1),right=FALSE,levels=1:nbins)
				plot(x=-10, y=-10, ylim=c(0,1), xlim=rxi*c(1, 1.15), main=paste("Density of predictions (smoothed), with TRUE, shark", s), yaxt="n", xlab="X", ylab="")	
				rect(xleft=brks[-length(brks)], xright=brks[-1], ybottom=rep(0,nbins), ytop=rep(1,nbins), border=NA, col=intensity_gray_scale[ intens_level ])
				abline(v=env_obj$known_regular_step_ds[env_obj$known_regular_step_ds$tag==s,"X"], col="red", lwd=1)
				legend_intensity_true()
				
			}
		}	
	}	
		
		
	if (env_obj$output_plot) {	
		grDevices::dev.off()
	}
	invisible(NULL)
}	