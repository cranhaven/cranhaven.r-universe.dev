plotting_EKF_interp_joint <- function(env_obj) {

	
	lgd <- format(Sys.time(), "%Y_%m_%d_%H_%M_%S") 
	
	
	if (env_obj$output_plot) {
		# make sure filename comes out valid
		default_prefix = "EKF_2D"
		if (is.null(env_obj$pdf_prefix) | env_obj$pdf_prefix =="") {
			env_obj$pdf_prefix = default_prefix
		}
		else if (! (substr(env_obj$pdf_prefix, 1, 1) %in% c(letters, LETTERS))) {
			env_obj$pdf_prefix = paste("EKF", env_obj$pdf_prefix, sep="_")
		}	
		
		print("printing pdf")
		grDevices::pdf(paste(env_obj$output_dir, "/", env_obj$pdf_prefix, "_", lgd, ".pdf", sep=""))
	}
	
	half <- ceiling(env_obj$N/2)

	old_pars <- par(mfrow=par()$mfrow, mfcol=par()$mfcol, las=par()$las)
	on.exit(expr=par(old_pars))
	
	par(mfrow=c(ifelse(env_obj$compare_with_known,2,3),1), las=1)

	
	#if we actually have the original regular observations from which the irregular one comes from (the one we are trying to interpolate), use that to calculate densities
	
	env_obj$d <- as.data.frame(env_obj$d)
	env_obj$d$tag <- as.factor(env_obj$tags) 
	
	
	print(head(env_obj$d))
	print(dim(env_obj$d))
	print(table(env_obj$d$tag))
	


	#print observed paths for all sharks, and the intervals they occur at
	obs_x_range <- range(env_obj$d$X, na.rm=TRUE)
	obs_y_range <- range(env_obj$d$Y, na.rm=TRUE)
	if (env_obj$nregions > 1) {  
		vortess <- deldir::deldir(x=env_obj$centroids[,1], y=env_obj$centroids[,2], wlines="tess", plotit=FALSE, suppressMsge=TRUE) 
	}
	
	obs_title <- "Observed shark locations for simulating (number=regular interval)"
	if (env_obj$interact) { obs_title <- paste(obs_title, "\ndashed circles are size of interaction spatial neighborhood", sep="") }
	
	par(mfcol=c(1,1))
	
	sp::plot(env_obj$area_map, xlim=obs_x_range, ylim=obs_y_range, main=obs_title, asp=1, xlab="X", ylab="Y", las=0, axes=TRUE)
	
	if (env_obj$nregions > 1 ) { deldir::plot.deldir(vortess, wlines="tess", cex=0, add=TRUE) }
	text(env_obj$centroids[,1,drop=FALSE], env_obj$centroids[,2,drop=FALSE], labels=1:nrow(env_obj$centroids))
	
	for (ss in 1:env_obj$nsharks) {
		
		di <- c(0, cumsum(diff(env_obj$shark_intervals[[ env_obj$shark_names[ss] ]] > env_obj$max_int_wo_obs)))
		interval_groups <- split(x=env_obj$shark_intervals[[ env_obj$shark_names[ss] ]], f=di)
		
		for (gg in 1:length(interval_groups)) {
			shark_obs <- which(env_obj$d$tag == env_obj$shark_names[ss] & env_obj$d$t_intervals %in% interval_groups[[gg]])
			if (env_obj$interact) {
				symbols(x=env_obj$d$X[ shark_obs[1]], y=env_obj$d$Y[ shark_obs[1]], circles=env_obj$spat_radius, 
				lty=2, col= ss + 1, add=TRUE, inches=FALSE)
			}
			
			lines(Y ~ X, data=env_obj$d[shark_obs,], type="l", col=ss + 1)
			text(env_obj$d$X[ shark_obs ], env_obj$d$Y[ shark_obs ], col=ss + 1, labels = env_obj$d$t_intervals[ shark_obs ])	
		}
	
	}
		
	legend("topright", legend=env_obj$shark_names, lty=1, col=2:(env_obj$nsharks + 1), cex=0.8, pch="3")



	par(mfrow=c(2,1), las=1)


	state_colors <- env_obj$states
		
		
	#agreement with only observed states

	
	for (s in env_obj$shark_names) {
		
		#select time gaps within limit
		
		a <- env_obj$d[ env_obj$tags==s & env_obj$d[,"time_to_next"] < env_obj$reg_dt * env_obj$max_int_wo_obs & ! is.na(env_obj$d[,"time_to_next"]),"time_to_next"]
		
		if (length(a) > 1) {
			
			dm <- density_min2(x=a, m1=min(a), m2=max(a))
			plot(dm, ylim=c(0, max(dm$y)*1.1), main=paste("Distribution of observed time steps, shark ",s, " (N=", length(a), ")", sep=""), xlab="Seconds", las=1)
			abline(v=c(env_obj$reg_dt, median(a), mean(a)), lwd=2, lty=1:3, col="red")
			legend("topright", legend=c("reg dt","median(obs dt)","mean(obs dt)"), lwd=2, lty=1:3, col="red")
		}
	
		if (env_obj$nstates > 1) {
			
			ids <- which(env_obj$tags==s)
			obs_lambda <- env_obj$states[ ids ]
			reg_intervals <- env_obj$d[ ids,"t_intervals"]	
					
			
			
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
			
			#rows are indices of observations
			reg2obs_as <- matrix(env_obj$lambda_matrix[,reg_intervals,s], nrow=env_obj$npart, ncol=length(ids))
			obs_lambda_mat <- matrix(obs_lambda, ncol=length(ids), nrow=env_obj$npart, byrow=TRUE)
			agree_final <- apply(reg2obs_as == obs_lambda_mat, 2, mean)

			reg2obs_bs <- matrix(env_obj$lambda_matrix_beforesamp[,reg_intervals,s], nrow=env_obj$npart, ncol=length(ids))
			agree_bs <- apply(reg2obs_bs == obs_lambda_mat, 2, mean)

			#before resampling
			barplot(agree_bs, col=obs_lambda, main=paste("Fraction of regular step particles agreeing in state to observed,\nblack=1, red=2 (before resampling), shark", s), 
					ylim=c(-0.1,1.1), border=NA, space=0, xlab="Observed steps", ylab="Fraction", las=1)
			
			if (nrow(reg_int_list)) { 
				rect(xleft=reg_int_list[,"first"], xright=reg_int_list[,"last"], ytop=rep(1.1, ncol(reg_int_list)), ybottom=rep(0, ncol(reg_int_list)), border="green")
				legend("bottomleft", legend=">1 obs. in single regular interval, so height of bar is constant here", pch=0, col="green", cex=0.8, bg="white")
			}
			
			loc_tmp <- round(seq(1, length(ids), length.out=min(length(ids),10)))
			axis(1, loc_tmp, at=loc_tmp - 0.5)
			
			#after resampling
			barplot(agree_final, col=obs_lambda, main=paste("Fraction of regular step particles agreeing in state to observed,\nblack=1, red=2 (after resampling), shark", s), 
					ylim=c(-0.1,1.1), border=NA, space=0, xlab="Observed steps", ylab="Fraction", las=1)
			if (nrow(reg_int_list)) { 
				rect(xleft=reg_int_list[,"first"], xright=reg_int_list[,"last"], ytop=rep(1.1, ncol(reg_int_list)), ybottom=rep(0, ncol(reg_int_list)), border="green")
				legend("bottomleft", legend=">1 obs. in single regular interval, so height of bar is constant here", pch=0, col="green", cex=0.8, bg="white")
			}
			axis(1, loc_tmp, at=loc_tmp - 0.5)
			
			env_obj$agree_table["overall",s] <- mean(reg2obs_as == obs_lambda_mat, na.rm=TRUE)
			
			print(paste("Shark",s,"proportion state agreement:", round(env_obj$agree_table["overall",s], digits=3)))
			
			
			for (k in 1:env_obj$nstates) {
				
				env_obj$agree_table[paste("state",k),s] <- mean(reg2obs_as[, obs_lambda==k] == obs_lambda_mat[, obs_lambda==k], na.rm=TRUE)
				print(paste("Shark",s,"proportion state", k, "agreement:", round(env_obj$agree_table[paste("state",k),s], digits=3)))
			}	
			

		
			if (env_obj$compare_with_known) {
				ids_true <- which(env_obj$known_regular_step_ds$tag == s & env_obj$known_regular_step_ds$t_intervals <= max(env_obj$shark_valid_steps[[ s ]]) )
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
			
				barplot(agree_true, col=true_lambda, main=paste("Fraction of regular step particles agreeing in state to TRUE,\nblack=1, red=2 (after resampling), shark", s), 
					ylim=c(-0.1,1.1), border=NA, space=0, xlab="True steps", ylab="Fraction", las=1)
				loc_tmp_true <- round(seq(1, length(ids_true), length.out=min(length(ids_true),10)))
				axis(1, loc_tmp_true, at=loc_tmp_true - 0.5)
			
				
				
			}	
			
			
			single_obs_intervals <- table(reg_intervals)
			single_obs_intervals <- as.numeric(names(single_obs_intervals[ single_obs_intervals==1]))
			if (length(single_obs_intervals)) { 
				#print("single")
				#print(single_obs_intervals)
				reg2obs_one <- matrix(env_obj$lambda_matrix[,single_obs_intervals,s], nrow=env_obj$npart, ncol=length(single_obs_intervals))
				obs_lambda_one <- matrix(obs_lambda[ reg_intervals %in% single_obs_intervals  ], ncol=length(single_obs_intervals), nrow=env_obj$npart, byrow=TRUE)
				
			
				env_obj$agree_table["single",s] <- 	mean(reg2obs_one==obs_lambda_one, na.rm=TRUE)
				print(paste("Shark",s,"proportion state agreement for single observations:", round(env_obj$agree_table["single",s], digits=3)))
			}
			
			
		
			if (env_obj$smoothing & (env_obj$fix_smoothed_behaviors==FALSE)) {
			
				smooth2obs <- matrix(t(env_obj$Xpart_history_smoothed[reg_intervals,"lambda",,s]), nrow=env_obj$npart, ncol=length(reg_intervals))
				
				tmp <- apply( smooth2obs , 1, function(x) x==obs_lambda)
				
				agree_smoothed <- apply(tmp , 1, mean)

				env_obj$agree_table_smoothed["overall",s] <- mean(tmp)
			
				print(paste("Shark",s,"smoothed proportion state agreement:", round(env_obj$agree_table_smoothed["overall",s], digits=3)))
			
				
				for (k in 1:env_obj$nstates) {
				
					env_obj$agree_table_smoothed[paste("state",k),s] <- mean( apply(smooth2obs[, obs_lambda==k, drop=FALSE], 1, function(x) x == obs_lambda[ obs_lambda==k]))
					print(paste("Shark",s,"smoothed proportion state", k, "agreement:", round(env_obj$agree_table_smoothed[paste("state",k),s], digits=3)))
				}	
			
							
				#after resampling
				barplot(agree_smoothed, col=obs_lambda, main=paste("Fraction of smoothed step particles agreeing in state to observed,\nblack=1, red=2, shark", s), 
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
					loc_tmp_true <- round(seq(1, length(ids_true), length.out=min(length(ids_true),10)))
					axis(1, loc_tmp_true, at=loc_tmp_true - 0.5)
			
				
				}
				
				
			}	
			
		}#if env_obj$nstates
		
	}#end loop over sharks
	

	
	
	if (env_obj$nstates > 1) {
		
		pct_foraging <- apply(env_obj$lambda_matrix, c(2,3), function(x) sum(x==1, na.rm=FALSE)/env_obj$npart)
		#print(pct_foraging)
		pct_foraging <- apply(pct_foraging, 2, function(x) { y <- x; y[ is.na(y) ] <- 0 ; a <- cumsum(y) ; a <- a/pmax(cumsum(! is.na(x)),1); a[ is.na(x) ] <- NA ; a})
		colnames(pct_foraging) <- env_obj$shark_names
		#print(pct_foraging)
		
		matplot(pct_foraging, type="b", col=1:env_obj$nsharks, xlim=c(1,env_obj$N), ylim=c(0,1), main="Cumulative fraction of regular steps simulated to be foraging", xlab="Step", ylab="Fraction", pch=19, cex=0.7, las=1)
		legend("topright", lty=1, col=1:env_obj$nsharks, legend=env_obj$shark_names, cex=0.5)
	}
	
	if (env_obj$truncate_to_map) {
		for (s in env_obj$shark_names) {

			matplot(env_obj$reject_samp_hist[,,s], type="l", col=1:2, lty=1, main=paste("Number of rejecting samples required to be in boundary, shark",s), ylab="Number", xlab="timestep", lwd=1.5, las=1, ylim=c(0, max(env_obj$reject_samp_hist, na.rm=TRUE)))
			legend("topright", col=1:2, lty=1, legend=c("mean","median"), lwd=1.5, cex=0.7)
		}
	}		
	
	# resample history
	par(mfrow=c(2,1))

	matplot(env_obj$resample_history, type="b", col=1:env_obj$nsharks, xlim=c(1,env_obj$N), ylim=c(0,1), main="Fraction of particles resampled at each step", xlab="Step", ylab="Fraction", pch=19, cex=0.7, las=1)
	legend("bottomleft", lty=1, col=1:env_obj$nsharks, legend=env_obj$shark_names, cex=0.5, pch=19, pt.cex=0.7)
	
	
	# effective size
	plot(x=-5, y=-5, xlim=c(1,env_obj$N), ylim=c(0,1), main="Effective sample size / #particles", xlab="Step", ylab="Fraction", las=1)
	abline(h=env_obj$neff_sample, lty=3)
	pch_mat <- matrix(1, ncol=env_obj$nsharks, nrow=env_obj$N)
	pch_mat[ env_obj$eff_size_hist <= env_obj$neff_sample * env_obj$npart ] <- 19
	pch_mat[ is.na(env_obj$eff_size_hist) ] <- NA
	colnames(pch_mat) <- env_obj$shark_names
	
	for (ss in 1:env_obj$nsharks) {
		points(x=1:env_obj$N, y=env_obj$eff_size_hist[,ss] / env_obj$npart, pch=pch_mat[,ss], type="b", cex=0.7, col=ss)
	}
	legend("bottomleft", lty=1, col=1:env_obj$nsharks, legend=env_obj$shark_names, cex=0.5, pch=19, pt.cex=0.7)
		
		
	
	
	par(mfrow=c(ifelse(env_obj$smoothing,3,2),1))
	#errors from observed

	env_obj$error_final_allpart <- array(NA, dim=c(env_obj$N, env_obj$npart, env_obj$nsharks), dimnames=list(env_obj$Nnames, env_obj$pnames, env_obj$shark_names))
		
	legend_quantiles <- function() legend("topleft", legend=c("Q90","Q50","Q10"), lty=3:1, cex=0.7, col=3:1, pch=1)

	pretty_axis <- pretty(x=1:env_obj$N, n=6)
	pretty_axis_intervals <- function() axis(side=1, at=pretty_axis, labels=pretty_axis)


	for (s in env_obj$shark_names) {

		
		env_obj$error_final_allpart[env_obj$shark_intervals[[ s ]],,s] <- 0
		if (env_obj$smoothing) {	env_obj$error_smoothed_allpart[env_obj$shark_intervals[[ s ]],,s] <- 0 }
		
		for (i in env_obj$shark_intervals[[ s ]]) { 
			obs <- env_obj$d[ env_obj$tags==s & env_obj$d[,"t_intervals"]==i, c("X","Y"), drop=FALSE]
		
			for (jj in 1:nrow(obs)) {
				
				#error from predictions
				env_obj$error_final_allpart[i,,s] <- env_obj$error_final_allpart[i,,s] + 
					as.vector(dist_func(center=obs[jj,], otherXY=t(apply(env_obj$Xpart_history[ i, c("X","Y","log_speed","bearing_rad"),,s], 2, function(x) env_obj$h(mk=x, dtprev=env_obj$j_list[[ s ]][[ i ]][ jj ] * env_obj$reg_dt)))))
				
			}			
			
			env_obj$error_final_quantiles[i,,s] <- quantile(env_obj$error_final_allpart[i,,s], p=c(.1, .5, .9), na.rm=TRUE)

			if (env_obj$smoothing) {
				
				for (jj in 1:nrow(obs)) {
				
				#error from predictions
				env_obj$error_smoothed_allpart[i,,s] <- env_obj$error_smoothed_allpart[i,,s] + 
					as.vector(dist_func(center=obs[jj,], otherXY=t(apply(env_obj$Xpart_history_smoothed[ i, c("X","Y","log_speed","bearing_rad"),,s], 2, function(x) env_obj$h(mk=x, dtprev=env_obj$j_list[[ s ]][[ i ]][ jj ] * env_obj$reg_dt)))))
				
				}			
			
				env_obj$error_smoothed_quantiles[i,,s] <- quantile(env_obj$error_smoothed_allpart[i,,s], p=c(.1, .5, .9), na.rm=TRUE)
			}



		}#loop over i
		
		tmp <- cbind(env_obj$error_beforesamp_quantiles[,,s], env_obj$error_final_quantiles[,,s])
		if (env_obj$smoothing) { tmp <- cbind(tmp, env_obj$error_smoothed_quantiles[,,s]) }
		yrange <- range(tmp, na.rm=TRUE)
		yrange[1] <- min(yrange[1],0)
		
		
		boxplot(t(env_obj$error_beforesamp_allpart[,,s]), xaxt="n", las=1, main=paste("Distance from observed (filtered before resampling), shark",s),
			ylab="distance", xlab="timestep", ylim=yrange, outline=FALSE, staplewex=0.2, boxfill="white", medlwd=2)
		pretty_axis_intervals()
		# matplot(env_obj$error_beforesamp_quantiles[,,s], type="b", lty=1:3, main=paste("Distance from observed (filtered before resampling), shark",s), ylab="distance", xlab="timestep", ylim=yrange, cex=0.5, pch=1, las=1)
		# legend_quantiles()
		
		boxplot(t(env_obj$error_final_allpart[,,s]), xaxt="n", las=1, main=paste("Distance from observed (final filtered), shark",s),
			ylab="distance", xlab="timestep", ylim=yrange, outline=FALSE, staplewex=0.2, boxfill="white", medlwd=2)
		pretty_axis_intervals()
		
		# matplot(env_obj$error_final_quantiles[,,s], type="b", lty=1:3, main=paste("Distance from observed (final filtered), shark",s), ylab="distance", xlab="timestep", ylim=yrange, cex=0.5, pch=1, las=1)
		# legend_quantiles()
		
		if (env_obj$smoothing) {
		
			boxplot(t(env_obj$error_smoothed_quantiles[,,s]), xaxt="n", las=1, main=paste("Distance from observed (smoothed)), shark",s),
				ylab="distance", xlab="timestep", ylim=yrange, outline=FALSE, staplewex=0.2,
				boxcol=2, whiskcol=2, outcol=2, medcol=2, staplecol=2, boxfill="white", medlwd=2)
			pretty_axis_intervals()
			#matplot(env_obj$error_smoothed_quantiles[,,s], type="b", lty=1:3, main=paste("Distance from observed (smoothed), shark",s), ylab="distance", xlab="timestep", ylim=yrange, cex=0.5, pch=1, las=1)
			# legend_quantiles()
		}
	
		
	}#loop over sharks	  

	if (env_obj$compare_with_known) {
		

		par(mfrow=c(2 + env_obj$smoothing,1))

		env_obj$error_final_true_allpart <- array(NA, dim=c(env_obj$N, env_obj$npart, env_obj$nsharks), dimnames=list(env_obj$Nnames, env_obj$pnames, env_obj$shark_names)) 
		env_obj$error_final_true_quantiles <- array(NA, dim=c(env_obj$N, 3, env_obj$nsharks), dimnames=list(env_obj$Nnames, 1:3, env_obj$shark_names))
		env_obj$error_euclidean_estimate_true_from_obs <- array(NA, dim=c(env_obj$N, 2, env_obj$nsharks), dimnames=list(env_obj$Nnames, c("euclidean", "bezier"), env_obj$shark_names)) 
		env_obj$euclidean_estimate_true_from_obs <- array(NA, dim=c(env_obj$N, 2, 2, env_obj$nsharks), dimnames=list(env_obj$Nnames, c("X","Y"), c("euclidean", "bezier"), env_obj$shark_names)) 

		
		if (env_obj$smoothing) { 
			env_obj$error_smoothed_true_allpart <- array(NA, dim=c(env_obj$N, env_obj$smooth_iter, env_obj$nsharks), 
														 dimnames=list(env_obj$Nnames, 1:env_obj$smooth_iter, env_obj$shark_names)) 
			env_obj$error_smoothed_true_quantiles <- env_obj$error_final_true_quantiles 
		}

		
		for (s in env_obj$shark_names) {
			
			env_obj$error_final_true_allpart[env_obj$shark_intervals[[ s ]],,s] <- 0
			d_of_shark <- env_obj$d[env_obj$d$tag == s,]
			
			obs_d_time_range <- range(d_of_shark$date_as_sec, na.rm=TRUE)
			
			true_shark_ds <- env_obj$known_regular_step_ds[env_obj$known_regular_step_ds$tag == s,]
			true_shark_ds <- true_shark_ds[true_shark_ds$date_as_sec >= obs_d_time_range[1] & true_shark_ds$date_as_sec <= obs_d_time_range[2],]
			
			shark_intervals_to_use <- env_obj$shark_valid_steps[[ s ]][ env_obj$shark_valid_steps[[ s ]] %in% true_shark_ds$t_intervals[true_shark_ds$t_intervals < env_obj$N]]

			for (i in shark_intervals_to_use) {

				ids <- which(true_shark_ds$t_intervals==i)
				# if it's the first interval, there will be two
				ids <- ids[length(ids)]
				
				obs <- true_shark_ds[ ids, c("X","Y"), drop=FALSE]
										
				env_obj$error_final_true_allpart[i+1,,s] <- dist_func(center=obs, otherXY=t(env_obj$Xpart_history[i+1 , c("X","Y"),,s]))
				env_obj$error_final_true_quantiles[i+1,,s] <- quantile(env_obj$error_final_true_allpart[i+1,,s], p=c(.1, .5, .9), na.rm=TRUE)					

				
				if (env_obj$smoothing) {
						
					env_obj$error_smoothed_true_allpart[i+1,,s] <- dist_func(center=obs, otherXY=t(env_obj$Xpart_history_smoothed[i+1 , c("X","Y"),,s]))
					env_obj$error_smoothed_true_quantiles[i+1,,s] <- quantile(env_obj$error_smoothed_true_allpart[i+1,,s], p=c(.1, .5, .9), na.rm=TRUE)
				}

				#get indices of rows of nearest previous observations

					
				nearby_obs_indices <- c(max(which(d_of_shark$date_as_sec <= env_obj$t_reg[i+1])), min(which(d_of_shark$date_as_sec >= env_obj$t_reg[i+1])))
					
				#here is the problem: if d_of_shark is a data frame
				obs_xy <- d_of_shark[nearby_obs_indices, c("X","Y"), drop=FALSE]
				obs_dxy <- apply(obs_xy, 2, diff)
				obs_dist <- sqrt(sum(obs_dxy^2))
				obs_bear <- atan2(x=obs_dxy["X"], y=obs_dxy["Y"])
				obs_timediff <- diff(d_of_shark$date_as_sec[nearby_obs_indices])
				
				if (obs_timediff == 0) {
					euc_pred <- as.matrix(obs_xy[1,])
				}
				else {
					euc_pred <- env_obj$h(mk=c(obs_xy[1,], log_safe(obs_dist/obs_timediff), obs_bear), dtprev=(env_obj$t_reg[i+1] - d_of_shark$date_as_sec[nearby_obs_indices[1]]))
				}
				
				colnames(euc_pred) <- c("X", "Y")
				rownames(euc_pred) <- paste("N", i+1, sep="")
				
		
				env_obj$euclidean_estimate_true_from_obs[i+1,c("X","Y"),"euclidean",s] <- euc_pred						
				env_obj$error_euclidean_estimate_true_from_obs[i+1,"euclidean",s] <- sum(dist_func(center=euc_pred, otherXY=obs))

			}#loop over i
			
			t_reg_in_shark_intervals <- as.integer(round(true_shark_ds$date_as_sec)) %in% as.integer(round(env_obj$t_reg[ shark_intervals_to_use + 1]))
	
			
			spline_estimates <- spline_interp(di=d_of_shark, area_map=env_obj$area_map, t_reg=true_shark_ds$date_as_sec,
			                                  max_dt_wo_obs=(env_obj$N + 1)*env_obj$reg_dt, maxStep=env_obj$N + 1,
			                                  centroids=env_obj$centroids, nstates=env_obj$nstates, spline_deg=3, split_logv=-3)$d_ds

			spline_estimates <- as.matrix(spline_estimates[t_reg_in_shark_intervals, c("X","Y"), drop=FALSE], ncol=2, nrow=length(t_reg_in_shark_intervals))
			rownames(spline_estimates) <- paste("N", shark_intervals_to_use + 1, sep="")			
			spline_errors <- apply(spline_estimates - true_shark_ds[t_reg_in_shark_intervals, c("X","Y"), drop=FALSE], 1, function(x) sqrt(sum(x^2)))
	
			env_obj$error_euclidean_estimate_true_from_obs[shark_intervals_to_use + 1,"bezier",s] <- spline_errors
			env_obj$euclidean_estimate_true_from_obs[shark_intervals_to_use + 1,c("X","Y"),"bezier",s] <- spline_estimates						

			#spline_distances <- apply(1:nrow(spline_estimates), function(jj) sum(dist_func(center=spline_estimates[jj, c("X","Y")], otherXY=)))		
				
			
			tmp <- cbind(env_obj$error_final_true_quantiles[,,s], env_obj$error_euclidean_estimate_true_from_obs[,,s])
			
			if (env_obj$smoothing) { tmp <- cbind(tmp, env_obj$error_smoothed_true_quantiles[,,s]) }
			yrange <- range(tmp, na.rm=TRUE)
			yrange[1] <- min(yrange[1],0)
			
			
			boxplot(t(env_obj$error_final_true_quantiles[,,s]), xaxt="n", las=1, main=paste("Distance from TRUE (final filtered), shark",s),
				ylab="distance", xlab="timestep", ylim=yrange, outline=FALSE, staplewex=0.2, boxfill="white", medlwd=2)
			pretty_axis_intervals()
			
			#matplot(env_obj$error_final_true_quantiles[,,s], type="b", lty=1:3, main=paste("Distance from TRUE (final filtered), shark",s), ylab="distance", xlab="timestep", ylim=yrange, cex=0.5, pch=1, las=1)
			# legend_quantiles()
		
			if (env_obj$smoothing) {
			
				boxplot(t(env_obj$error_smoothed_true_quantiles[,,s]), xaxt="n", las=1, main=paste("Distance from TRUE (smoothed), shark",s),
					ylab="distance", xlab="timestep", ylim=yrange, outline=FALSE, staplewex=0.2,
					boxcol=2, whiskcol=2, outcol=2, medcol=2, staplecol=2, boxfill="white", medlwd=2)
				pretty_axis_intervals()
			
				#matplot(env_obj$error_smoothed_true_quantiles[,,s], type="b", lty=1:3, main=paste("Distance from TRUE (smoothed), shark",s), ylab="distance", xlab="timestep", ylim=yrange, cex=0.5, pch=1, las=1)
				#legend_quantiles()
			}
		
			matplot(env_obj$error_euclidean_estimate_true_from_obs[,,s], type="b", ylab="distance", xlab="timestep", ylim=yrange, cex=0.5, pch=1, col=1:2, lty=1:2, las=1,
				main=paste("Distance from TRUE (estimated from observed), shark", s), xaxt="n")
			pretty_axis_intervals()
			
			legend("topleft", col=1:2, lty=1:2, cex=0.7, pch=1, legend=c("Euclidean", "Bezier spline"))

		
		}#loop over sharks

	}#if compare




	#error covariances
	par(mfcol=c(2,1))
	env_obj$cov_err_hist <- sign(env_obj$cov_err_hist)*sqrt(abs(env_obj$cov_err_hist))

	for (s in env_obj$shark_names) {
		
		cov_hist <- apply(env_obj$cov_err_hist[,"particle",,,s,], c(1,3,4), function(x) quantile(x, prob=c(.1, .5, .9), na.rm=TRUE))
		cov_hist[,,env_obj$first_intervals[[ s ]],] <- NA
				
		if (any (! is.na(cov_hist))) { 
		
			ylim_cov <- range(cov_hist, na.rm=TRUE)
			if (all(ylim_cov >= 0)) { ylim_cov[ 1 ] <- 0 }
			else if (all(ylim_cov <= 0)) { ylim_cov[ 2 ] <- 0 }
			
		
			matplot(t(cov_hist[,"X",,"orig"]), col=1:3, type="b", main=paste("Shark",s,"Particle X_t to X_{t+1} X error std. dev. (orig)"), ylim=ylim_cov, pch=1,cex=0.4, ylab="meters")
			legend_quantiles()
			abline(v=env_obj$shark_intervals[[ s ]], lty=3)
			matplot(t(cov_hist[,"X",,"resamp"]), col=1:3, type="b", main=paste("Shark",s,"Particle X_t to X_{t+1} X error std. dev. (final)"), ylim=ylim_cov, pch=1,cex=0.4, ylab="meters")
			legend_quantiles()
			abline(v=env_obj$shark_intervals[[ s ]], lty=3)
			matplot(t(cov_hist[,"Y",,"orig"]), col=1:3, type="b", main=paste("Shark",s,"Particle X_t to X_{t+1} Y error std. dev. (orig)"), ylim=ylim_cov, pch=1,cex=0.4, ylab="meters")
			legend_quantiles()
			abline(v=env_obj$shark_intervals[[ s ]], lty=3)
			matplot(t(cov_hist[,"Y",,"resamp"]), col=1:3, type="b", main=paste("Shark",s,"Particle X_t to X_{t+1} Y error std. dev. (final)"), ylim=ylim_cov, pch=1,cex=0.4, ylab="meters")
			legend_quantiles()
			abline(v=env_obj$shark_intervals[[ s ]], lty=3)
			matplot(t(cov_hist[,"cov",,"orig"]), col=1:3, type="b", main=paste("Shark",s,"Particle X_t to X_{t+1} error co-std. dev. (orig)"), ylim=ylim_cov, pch=1,cex=0.4, ylab="meters")
			legend_quantiles()
			abline(v=env_obj$shark_intervals[[ s ]], lty=3)
			matplot(t(cov_hist[,"cov",,"resamp"]), col=1:3, type="b", main=paste("Shark",s,"Particle X_t to X_{t+1} error co-std. dev. (final)"), ylim=ylim_cov, pch=1,cex=0.4, ylab="meters")
			legend_quantiles()
			abline(v=env_obj$shark_intervals[[ s ]], lty=3)
		}
		
	
		
		for (k in 1:env_obj$nstates) {		
			cov_hist <- apply(env_obj$cov_err_hist[,k+1,,,s,], c(1,3,4), function(x) quantile(x, prob=c(.1, .5, .9), na.rm=TRUE))
			cov_hist[,,env_obj$first_intervals[[ s ]],] <- NA
		
			if (any (! is.na(cov_hist))) { 
		
				ylim_cov <- range(cov_hist, na.rm=TRUE)
				if (all(ylim_cov >= 0)) { ylim_cov[ 1 ] <- 0 }
				else if (all(ylim_cov <= 0)) { ylim_cov[ 2 ] <- 0 }
				
		
				matplot(t(cov_hist[,"X",,"orig"]), col=1:3, type="b", main=paste("Shark",s,"Observation X error std. dev. (orig), behavior", k), ylim=ylim_cov, pch=1,cex=0.4, ylab="meters")
				legend_quantiles()
				#abline(v=env_obj$shark_intervals[[ s ]], lty=3)
				matplot(t(cov_hist[,"X",,"resamp"]), col=1:3, type="b", main=paste("Shark",s,"Observation X error std. dev. (resamp), behavior", k), ylim=ylim_cov, pch=1,cex=0.4, ylab="meters")
				legend_quantiles()
				#abline(v=env_obj$shark_intervals[[ s ]], lty=3)
				matplot(t(cov_hist[,"Y",,"orig"]), col=1:3, type="b", main=paste("Shark",s,"Observation Y error std. dev. (resamp), behavior", k), ylim=ylim_cov, pch=1,cex=0.4, ylab="meters")
				legend_quantiles()
				#abline(v=env_obj$shark_intervals[[ s ]], lty=3)
				matplot(t(cov_hist[,"Y",,"resamp"]), col=1:3, type="b", main=paste("Shark",s,"Observation Y error std. dev. (resamp), behavior", k), ylim=ylim_cov, pch=1,cex=0.4, ylab="meters")
				legend_quantiles()
				#abline(v=env_obj$shark_intervals[[ s ]], lty=3)
				matplot(t(cov_hist[,"cov",,"orig"]), col=1:3, type="b", main=paste("Shark",s,"Observation error co-std. dev. (orig), behavior", k), ylim=ylim_cov, pch=1, cex=0.4,ylab="meters")
				legend_quantiles()
				#abline(v=env_obj$shark_intervals[[ s ]], lty=3)
				matplot(t(cov_hist[,"cov",,"resamp"]), col=1:3, type="b", main=paste("Shark",s,"Observation error co-std. dev. (resamp), behavior", k), ylim=ylim_cov, pch=1, cex=0.4, ylab="meters")
				legend_quantiles()
				#abline(v=env_obj$shark_intervals[[ s ]], lty=3)
			}
		}
		
		

	}#end looping over sharks	



	#print estimates of alpha and beta densities

	nresamp <- 100 * env_obj$npart

	env_obj$param_draws <- env_obj$variance_draws <- logv_turn_draws <- array(NA, dim=c(nresamp, 2, env_obj$nstates, env_obj$nsharks), dimnames=list(1:nresamp, c("logv", "turn"), env_obj$state_names, env_obj$shark_names))
	param_sampling_weights <- rep(1, env_obj$npart)

	if (env_obj$smooth_parameters) {
		env_obj$param_draws_smoothed <- env_obj$param_draws
		env_obj$variance_draws_smoothed <- env_obj$variance_draws
		logv_turn_draws_smoothed <- logv_turn_draws
	 }


	for (s in env_obj$shark_names) {
		par(mfrow=c(2,1))
		ids <- rep(1:env_obj$npart, nresamp/env_obj$npart) #low_var_sample(wts=param_sampling_weights, M=nresamp)
		  
		tab <- table(factor(ids, levels=1:env_obj$npart))
				
		for (p in which(tab > 0)) {
		
			
		
			 for (k in 1:env_obj$nstates) {
			 
				env_obj$variance_draws[ ids==p,"logv",k,s] <-  MCMCpack::rinvgamma(n=tab[ p ], env_obj$sigma_pars[p,2*k-1,s], env_obj$sigma_pars[p,2*k,s])
				env_obj$variance_draws[ ids==p,"turn",k,s] <- MCMCpack::rinvgamma(n=tab[ p ], env_obj$tau_pars[p,2*k-1,s], env_obj$tau_pars[p,2*k,s])
				
				env_obj$param_draws[ ids==p,"logv", k, s] <-  rnorm(n=tab[ p ], mean=env_obj$mu[k,"alpha","mu",p,s], sd=sqrt(env_obj$mu[k,"alpha","V",p,s] * env_obj$variance_draws[ ids==p,"logv",k,s])) 
				env_obj$param_draws[ ids==p,"turn", k, s] <-  normalize_angle(rnorm(n=tab[ p ], mean=env_obj$mu[k,"beta", "mu",p,s], sd=sqrt(env_obj$mu[k,"beta", "V",p,s] * env_obj$variance_draws[ ids==p,"turn",k,s])))	
				
				
				if (env_obj$smooth_parameters) {
					env_obj$variance_draws_smoothed[ ids==p,"logv",k,s] <-  MCMCpack::rinvgamma(n=tab[ p ], env_obj$sigma_pars_smoothed[p,2*k-1,s], env_obj$sigma_pars_smoothed[p,2*k,s])
					env_obj$variance_draws_smoothed[ ids==p,"turn",k,s] <-  MCMCpack::rinvgamma(n=tab[ p ], env_obj$tau_pars_smoothed[p,2*k-1,s], env_obj$tau_pars_smoothed[p,2*k,s])
				
					env_obj$param_draws_smoothed[ ids==p,"logv", k, s] <-  rnorm(n=tab[ p ], mean=env_obj$mu_smoothed[k,"alpha","mu",p,s], sd=sqrt(env_obj$mu_smoothed[k,"alpha","V",p,s] * env_obj$variance_draws_smoothed[ ids==p,"logv",k,s])) 
					env_obj$param_draws_smoothed[ ids==p,"turn", k, s] <-  normalize_angle(rnorm(n=tab[ p ], mean=env_obj$mu_smoothed[k,"beta", "mu",p,s], sd=sqrt(env_obj$mu_smoothed[k,"beta", "V",p,s] * env_obj$variance_draws_smoothed[ ids==p,"turn",k,s])))	
				
				}
			}

		}
		env_obj$param_draws[ ,"turn", , s] <- normalize_angle(env_obj$param_draws[ ,"turn", , s])
		
		for (vv in c("logv", "turn")) {
			for (k in 1:env_obj$nstates) {
			
				logv_turn_draws[,vv, k,s] <- rnorm(n=nresamp, mean=env_obj$param_draws[,vv,k,s], sd=sqrt(env_obj$variance_draws[,vv,k,s]))
	
				if (env_obj$smooth_parameters) {
				
					logv_turn_draws_smoothed[,vv,k,s] <- rnorm(n=nresamp, mean=env_obj$param_draws_smoothed[,vv,k,s], sd=sqrt(env_obj$variance_draws_smoothed[,vv,k,s]))	
					
				}
			}
		}
		logv_turn_draws[,"turn",,ss] <- normalize_angle(logv_turn_draws[,"turn",,ss])
		if (env_obj$smooth_parameters) {
			logv_turn_draws_smoothed[,"turn",,ss] <- normalize_angle(logv_turn_draws_smoothed[,"turn",,ss])		
		}
		
	}#finish draws of parameters

	#calculate densities
	
	logv_modeled_densities <- lapply(env_obj$shark_names, function(ss) lapply(1:env_obj$nstates, function(k) density(x=logv_turn_draws[ ,"logv", k, ss])))
	turn_modeled_densities <- lapply(env_obj$shark_names, function(ss) lapply(1:env_obj$nstates, function(k) density(x=logv_turn_draws[ ,"turn", k, ss], from=-pi, to=pi)))
	names(logv_modeled_densities) <- names(turn_modeled_densities) <- env_obj$shark_names
	
	stab_N <- array("", dim=c(env_obj$nsharks, 4, env_obj$nstates), dimnames=list(env_obj$shark_names, c("part.","obs.","smooth.","true"), 1:env_obj$nstates))	
		
	logv_obs_densities <- lapply(env_obj$shark_names, function(ss) lapply(1:env_obj$nstates, function(k) density_min2(x=env_obj$d[ env_obj$d$tag==ss & env_obj$d$state.guess2==k,"log_speed"])))
	turn_obs_densities <- lapply(env_obj$shark_names, function(ss) lapply(1:env_obj$nstates, function(k) density_min2(x=env_obj$d[ env_obj$d$tag==ss & env_obj$d$state.guess2==k,"turn.angle.rad"], m1=-pi, m2=pi)))
	names(logv_obs_densities) <- names(turn_obs_densities) <- env_obj$shark_names
	
	stab_N[,"obs.",] <- paste("N=", t(sapply(env_obj$shark_names, function(ss) table(factor(env_obj$d[env_obj$tags==ss,"state.guess2"][ env_obj$tags ==ss ], levels=1:env_obj$nstates)))))
	

	logv_obs_x <- sapply(logv_obs_densities, function(x) sapply(x, function(ii) ii$x))
	logv_obs_y <- sapply(logv_obs_densities, function(x) sapply(x, function(ii) ii$y))
	
	turn_obs_x <- sapply(turn_obs_densities, function(x) sapply(x, function(ii) ii$x))
	turn_obs_y <- sapply(turn_obs_densities, function(x) sapply(x, function(ii) ii$y))
	


	ylim_logv <- max(c(sapply(logv_modeled_densities, function(x) sapply(x, function(ii) ii$y)), logv_obs_y), na.rm=TRUE)
	ylim_turn <- max(c(sapply(turn_modeled_densities, function(x) sapply(x, function(ii) ii$y)), turn_obs_y), na.rm=TRUE)
	xlim_logv <- matrix(quantile(x=c(sapply(logv_modeled_densities, function(x) sapply(x, function(ii) ii$x)), logv_obs_x[! is.na(logv_obs_y)]), probs=c(0.025, 0.975), na.rm=TRUE), ncol=2)
	
	legend_labels <- c("part.", "obs.")
	legend_lwd <- c(1,2)
	legend_lty <- c(1,2)

	if (env_obj$smooth_parameters) {
		logv_modeled_smoothed_densities <- lapply(env_obj$shark_names, function(ss) lapply(1:env_obj$nstates, function(k) density(x=logv_turn_draws_smoothed[ ,"logv", k, ss])))
		turn_modeled_smoothed_densities <- lapply(env_obj$shark_names, function(ss) lapply(1:env_obj$nstates, function(k) density(x=logv_turn_draws_smoothed[ ,"turn", k, ss], from=-pi, to=pi)))
		names(logv_modeled_smoothed_densities) <- names(turn_modeled_smoothed_densities) <- env_obj$shark_names
		
		
		ylim_logv <- max(ylim_logv, sapply(logv_modeled_smoothed_densities, function(x) sapply(x, function(ii) ii$y)), na.rm=TRUE)
		ylim_turn <- max(ylim_turn, sapply(turn_modeled_smoothed_densities, function(x) sapply(x, function(ii) ii$y)), na.rm=TRUE)
		xlim_logv <- rbind(xlim_logv, quantile(x=sapply(logv_modeled_smoothed_densities, function(x) sapply(x, function(ii) ii$x)), probs=c(0.025, 0.975), na.rm=TRUE))
		
		legend_labels <- c(legend_labels, "smooth.")
		legend_lwd <- c(legend_lwd, 2)
		legend_lty <- c(legend_lty, 3)
	}
		
	
	if (env_obj$compare_with_known) {
		logv_true_densities <- lapply(env_obj$shark_names, function(ss) lapply(1:env_obj$nstates, function(k) density_min2(x=env_obj$known_regular_step_ds[ env_obj$known_regular_step_ds$tag==ss & env_obj$known_regular_step_ds$state.guess2==k,"log_speed"])))
		turn_true_densities <- lapply(env_obj$shark_names, function(ss) lapply(1:env_obj$nstates, function(k) density_min2(x=env_obj$known_regular_step_ds[ env_obj$known_regular_step_ds$tag==ss & env_obj$known_regular_step_ds$state.guess2==k,"turn.angle.rad"], m1=-pi, m2=pi)))
		names(logv_true_densities) <- names(turn_true_densities) <- env_obj$shark_names
		
		stab_N[,"true",] <- paste("N=", t(sapply(env_obj$shark_names, function(ss) table(factor(env_obj$known_regular_step_ds[env_obj$known_regular_step_ds$tag==ss,"state.guess2"], levels=1:env_obj$nstates)))), sep="")
		
		logv_true_x <- sapply(logv_true_densities, function(x) sapply(x, function(ii) ii$x))
		logv_true_y <- sapply(logv_true_densities, function(x) sapply(x, function(ii) ii$y))
	
		turn_true_x <- sapply(turn_true_densities, function(x) sapply(x, function(ii) ii$x))
		turn_true_y <- sapply(turn_true_densities, function(x) sapply(x, function(ii) ii$y))
	
		
		
		ylim_logv <- max(c(ylim_logv, logv_true_y), na.rm=TRUE)
		ylim_turn <- max(c(ylim_turn, turn_true_y), na.rm=TRUE)
		xlim_logv <- rbind(xlim_logv, quantile(x=logv_true_x[! is.na(logv_true_y)], probs=c(0.025, 0.975), na.rm=TRUE))
	
		
		legend_labels <- c(legend_labels, "true")
		legend_lwd <- c(legend_lwd, 1)
		legend_lty <- c(legend_lty, 2)
	
	}	

	
	xlim_logv <- c(max(min(xlim_logv[,1], na.rm=TRUE), env_obj$logvelocity_truncate[1]),
				   min(max(xlim_logv[,2], na.rm=TRUE), env_obj$logvelocity_truncate[2]))

	
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

	
	for (ss in env_obj$shark_names) {
		
		legend_N <- stab_N[ss,legend_labels,]
		legend_tmp <- function() { legend("topleft", col=legend_col, lty=legend_lty, lwd=legend_lwd, cex=.7, ncol=env_obj$nstates, legend=paste(legend_labels_txt, legend_N)) } 

		
		plot(x=-10, y=-10, xlim=xlim_logv, ylim=c(0,ylim_logv), xlab="alpha", ylab="Density", main=paste("Density of log-speed (per second), shark",ss), las=1)
		for (k in 1:env_obj$nstates) {
			
			lines(logv_modeled_densities[[ ss ]][[ k ]], col=k, lty=1)
			
			if (env_obj$smooth_parameters) {
				lines(logv_modeled_smoothed_densities[[ ss ]][[ k ]], col=k, lty=3, lwd=2)
			
			}
			
			lines(logv_obs_densities[[ ss ]][[ k ]], col=k, lty=2, lwd=2)
				
			if (env_obj$compare_with_known) {
				lines(logv_true_densities[[ ss ]][[ k ]], col=k, lty=2)

			}
		
		}


		legend_tmp()

		
		plot(x=-10, y=-10, xlim=c(-pi,pi), ylim=c(0,ylim_turn), xaxt="n", xlab="beta", ylab="Density", main=paste("Density of turn angle (radians), shark",s), las=1)
		axis(side=1, at=seq(-pi,pi, by=pi/2), labels=c(expression(-pi), expression(paste(-pi, "/", 2)), 0, expression(paste(pi, "/", 2)), expression(pi)))
		
		for (k in 1:env_obj$nstates) {
			
			lines(turn_modeled_densities[[ ss ]][[ k ]], col=k, lty=1)
			
			if (env_obj$smooth_parameters) {
				lines(turn_modeled_smoothed_densities[[ ss ]][[ k ]], col=k, lty=3, lwd=2)
			
			}
			
			lines(turn_obs_densities[[ ss ]][[ k ]], col=k, lty=2, lwd=2)
					

			if (env_obj$compare_with_known) {
				lines(turn_true_densities[[ ss ]][[ k ]], col=k, lty=2)
			}
		
		}

		
		legend_tmp()
		
				
	}#loop over sharks
	

	#multiple states
	
	if (env_obj$nstates > 1) {

				
		if (env_obj$time_dep_trans==FALSE) {
			
			env_obj$region_trans_draws <- array(NA, dim=c(nresamp, env_obj$nregions, env_obj$nstates, env_obj$nstates, env_obj$nsharks), 
													dimnames=list(1:nresamp, env_obj$rnames, env_obj$state_names, env_obj$state_names, env_obj$shark_names))
				
			env_obj$region_foraging_draws <- array(NA, dim=c(nresamp, env_obj$nregions, env_obj$nsharks), dimnames=list(1:nresamp, env_obj$rnames, env_obj$shark_names))
			
			if (env_obj$smooth_parameters) {
				env_obj$region_trans_draws_smoothed <- env_obj$region_trans_draws	
				env_obj$region_foraging_draws_smoothed <- env_obj$region_foraging_draws
			
			}
						
			
			for (s in env_obj$shark_names) {
					
				regions <- apply(env_obj$d[env_obj$tags==s,c("X","Y"), drop=FALSE], 1, function(x) which_region(x, centroid=env_obj$centroids))
				trans_actual <- table(factor(env_obj$states[ env_obj$tags==s ], levels=1:env_obj$nstates), 
									  factor(env_obj$next_states[ env_obj$tags==s ], levels=1:env_obj$nstates), factor(regions, levels=1:env_obj$nregions))
					
				dim(trans_actual) <- c(env_obj$nstates^2, env_obj$nregions)
				colnames(trans_actual) <- paste("region", 1:env_obj$nregions, sep="")
				rownames(trans_actual) <- c("1to1","2to1","1to2","2to2")
			
				if (env_obj$compare_with_known==FALSE || is.null(env_obj$known_trans_prob))  {
					trans_prob_actual <- matrix(NA, nrow=2, ncol=env_obj$nregions)
					cs1 <- colSums(trans_actual[c(1,3),,drop=FALSE])
					trans_prob_actual[1,cs1>0] <- trans_actual[3,cs1>0]/cs1[ cs1>0]
					cs2 <- colSums(trans_actual[c(2,4),,drop=FALSE])
					trans_prob_actual[2,cs2>0] <- trans_actual[2,cs2>0]/cs2[ cs2>0]
					#print(trans_prob_actual)
				}
				else {
					trans_prob_actual <- env_obj$known_trans_prob[ rownames(env_obj$known_trans_prob)==s,]
				
				}
				
				par(mfrow=c(2,1))
				matplot(t(env_obj$trans_mean[,,s]), type="l", col=1:2, lty=1, ylim=c(0,1), main=paste("Mean state-to-state transition probabilities, shark",s), ylab="probability", xlab="timestep", lwd=1.5)
			 
				
				if (env_obj$compare_with_known & ! is.null(env_obj$known_trans_prob)) {
					abline(h=trans_prob_actual[,"all",drop=FALSE] + runif(nrow(trans_prob_actual), min=-0.001, max=0.001), col=1:2, lty=2)
				}	
				else {
					abline(h=sum(trans_actual[3,])/sum(trans_actual[c(1,3),]), col=1, lty=2)
					abline(h=sum(trans_actual[2,])/sum(trans_actual[c(2,4),]), col=2, lty=2)
				}
				legend("topright", col=1:2, lty=1, legend=c("Pr(1 -> 2)","Pr(2 -> 1)"), lwd=1.5, cex=0.7)

				region_tab <- 100
				
				if (env_obj$nregions > 1) {
				
					region_tab <- round(100*table(factor(env_obj$d[ env_obj$tags==s,"region"], levels=1:env_obj$nregions))/sum(env_obj$tags==s)) 
					
					for (r in 1:env_obj$nregions) {
					 
						matplot(t(env_obj$trans_mean_byregion[,,r,s]), type="l", col=1:2, lty=1, ylim=c(0,1), main=paste("Mean transition probabilities (shark ",s," region ",r,", frac=",region_tab[ r ],"%)",sep=""),
								ylab="probability", xlab="timestep", lwd=1.5, las=1)
									  
						abline(h=trans_prob_actual[1:2,r] + runif(2, min=-0.001, max=0.001), col=1:2, lty=2)
						#print(trans_prob_actual[1:2,r])
						legend("topright", col=1:2, lty=1, legend=c("Pr(1 -> 2)","Pr(2 -> 1)"), lwd=1.5, cex=0.7)

					}
			 
				}#if more than one region


				print("trans prob actual")
				print(trans_prob_actual)
				#convergence diagnostics

				#fraction each particle was in a region
				#frac_part_by_region <- apply(region_counts,2, function(x) x/sum(x))
				
				#param_sampling_weights[ param_sampling_weights <0 ] <- 0
				#param_sampling_weights <- rep(1,env_obj$npart) 
				
				for (r in 1:env_obj$nregions) {
					
					if (any(env_obj$region_counts[,r,s,drop=FALSE] > 0)) {
						#sample by probability spent there.
						#ids <- rep(1:env_obj$npart, nresamp/env_obj$npart)
						ids <- rep(1:env_obj$npart, nresamp/env_obj$npart) #low_var_sample(wts=param_sampling_weights, M=nresamp)
					  
						#ids <- sample(1:env_obj$npart, prob=region_counts[,r,drop=FALSE], replace=TRUE, size=nresamp)
						tab <- table(factor(ids, levels=1:env_obj$npart))
						
						for (p in which(tab > 0)) {
							dp <- env_obj$transition_mat[[ s ]][[ p ]]$dirichlet_pars[ r,]
							tc <- env_obj$transition_mat[[ s ]][[ p ]]$counts[r, ]
							
							
							
							#if no observed transitions
							
							if (sum(tc) >0) {

								#draw transition matrix tab[ p ] times
								for (k in 1:env_obj$nstates) {					
								
									env_obj$region_trans_draws[ids==p,r,k,, s] <- MCMCpack::rdirichlet(n=tab[ p ], alpha=dp[ (env_obj$nstates*k-1):(env_obj$nstates*k) ])
								}
								
								for (ii in which(ids==p)) {
									
									mat_tmp <- env_obj$region_trans_draws[ ii,r,,,s]
									eig_tmp <- eigen(t(mat_tmp))$vectors[,1]
									eig_tmp <- eig_tmp/sum(eig_tmp)
									
									env_obj$region_foraging_draws[ii,r,s] <- eig_tmp[ 1 ]
									
								}	
												
													
							}
							
																
						}#loop over particles
					
					
					}	
				}#loop over regions
			

				if (env_obj$smooth_parameters) {
				
					for (r in 1:env_obj$nregions) {
					
						if (any(env_obj$region_counts[,r,s,drop=FALSE] > 0)) {
							#sample by probability spent there.
							#ids <- rep(1:env_obj$npart, nresamp/env_obj$npart)
							ids <- rep(1:env_obj$npart, nresamp/env_obj$npart) #low_var_sample(wts=param_sampling_weights, M=nresamp)
						  
							#ids <- sample(1:env_obj$npart, prob=env_obj$region_counts[,r,drop=FALSE], replace=TRUE, size=nresamp)
							tab <- table(factor(ids, levels=1:env_obj$npart))
							
							for (p in which(tab > 0)) {
								dp <- env_obj$transition_mat_smoothed[[ s ]][[ p ]]$dirichlet_pars[ r,]
								tc <- env_obj$transition_mat_smoothed[[ s ]][[ p ]]$counts[r, ]
								
								
								
								#if no observed transitions
								
								if (sum(tc) > 0) {

									#draw transition matrix tab[ p ] times
									for (k in 1:env_obj$nstates) {					
									
										env_obj$region_trans_draws_smoothed[ids==p,r,k,, s] <- MCMCpack::rdirichlet(n=tab[ p ], alpha=dp[ (env_obj$nstates*k-1):(env_obj$nstates*k) ])
									}
									
									for (ii in which(ids==p)) {
										
										mat_tmp <- env_obj$region_trans_draws_smoothed[ ii,r,,,s]
										eig_tmp <- eigen(t(mat_tmp))$vectors[,1]
										eig_tmp <- eig_tmp/sum(eig_tmp)
										
										env_obj$region_foraging_draws_smoothed[ii,r,s] <- eig_tmp[ 1 ]
										
									}	
													
								
								}
								
																	
							}#loop over particles
						
						}	
					}#loop over regions
				
				}#if env_obj$smoothing
			
			}



			#foraging CIs
			
			par(mfcol=c(2,1))

			for (ss in env_obj$shark_names) {
			
				plot(x=-4, y=-4, xlim=c(0,1), ylim=c(1, env_obj$nregions + 1), main=paste("CIs for foraging probability by region, shark", ss), xlab="probability", ylab="region", las=1, yaxt="n")
				axis(side=2, labels=1:env_obj$nregions, at=1:env_obj$nregions)
				for (rr in 1:env_obj$nregions) {			
					emp_q <- as.numeric(HDInterval::hdi(env_obj$region_foraging_draws[,rr, ss]))
					lines(x=emp_q, y=c(rr, rr), lty=1, type="b", col=rr, pch=19, cex=.8)
					
				}
				if (! is.null(env_obj$known_foraging_prob)) { 
					points(x=env_obj$known_foraging_prob[ ss,1:env_obj$nregions], y=1:env_obj$nregions, col=1:env_obj$nregions, pch="|") 
				}
				
				if (env_obj$smooth_parameters) {
					for (rr in 1:env_obj$nregions) {			
						emp_q <- as.numeric(HDInterval::hdi(env_obj$region_foraging_draws_smoothed[,rr, ss]))
						lines(x=emp_q, y=c(rr, rr) + 0.2, lty=2, type="b", col=rr, pch=8, cex=.8)
						
					}
				legend("topleft", lty=1:2, pch=c(19,8), legend=c("regular","smoothed"))
				}
			

			}
			
			#P1to2
			for (ss in env_obj$shark_names) {
			
				plot(x=-4, y=-4, xlim=c(0,1), ylim=c(1, env_obj$nregions + 1), main=paste("CIs for Pr(1 -> 2) transition by region, shark", ss), xlab="probability", ylab="region", las=1, yaxt="n")
				axis(side=2, labels=1:env_obj$nregions, at=1:env_obj$nregions)
				for (rr in 1:env_obj$nregions) {			
					emp_q <- as.numeric(HDInterval::hdi(env_obj$region_trans_draws[,rr,1,2, ss]))
					lines(x=emp_q, y=c(rr, rr), lty=1, type="b", col=rr, pch=19, cex=.8)
					
				}
				if (! is.null(env_obj$known_trans_prob)) { 
					points(x=env_obj$known_trans_prob[rownames(env_obj$known_trans_prob)== ss, 1:env_obj$nregions, drop=FALSE][1,], y=1:env_obj$nregions, col=1:env_obj$nregions, pch="|") 
				}
				if (env_obj$smooth_parameters) {
					for (rr in 1:env_obj$nregions) {			
						emp_q <- as.numeric(HDInterval::hdi(env_obj$region_trans_draws_smoothed[,rr,1,2, ss]))
						lines(x=emp_q, y=c(rr, rr) + 0.2, lty=2, type="b", col=rr, pch=8, cex=.8)
						
					}
				legend("topleft", lty=1:2, pch=c(19,8), legend=c("regular","smoothed"))
				}

			}
			
			#P2to1
			for (ss in env_obj$shark_names) {
			
				plot(x=-4, y=-4, xlim=c(0,1), ylim=c(1, env_obj$nregions + 1), main=paste("CIs for Pr(2 -> 1) transition by region, shark", ss), xlab="probability", ylab="region", las=1, yaxt="n")
				axis(side=2, labels=1:env_obj$nregions, at=1:env_obj$nregions)
				for (rr in 1:env_obj$nregions) {			
					emp_q <- as.numeric(HDInterval::hdi(env_obj$region_trans_draws[,rr,2,1, ss]))
					lines(x=emp_q, y=c(rr, rr), lty=1, type="b", col=rr, pch=19, cex=.8)
					
				}
				if (! is.null(env_obj$known_trans_prob)) { 
					points(x=env_obj$known_trans_prob[ rownames(env_obj$known_trans_prob)== ss, 1:env_obj$nregions, drop=FALSE][2,], y=1:env_obj$nregions, col=1:env_obj$nregions, pch="|") 
				}
				if (env_obj$smooth_parameters) {
					for (rr in 1:env_obj$nregions) {			
						emp_q <- as.numeric(HDInterval::hdi(env_obj$region_trans_draws_smoothed[,rr,2,1, ss]))
						lines(x=emp_q, y=c(rr, rr) + 0.2, lty=2, type="b", col=rr, pch=8, cex=.8)
						
					}
				legend("topleft", lty=1:2, pch=c(19,8), legend=c("regular","smoothed"))
				}

			}
			#foraging density
			foraging_density_byregion <- lapply(env_obj$shark_names, function(ss) lapply(1:env_obj$nregions, function(r) density_min2(x=env_obj$region_foraging_draws[ ,r,ss], m1=0, m2=1)))
			names(foraging_density_byregion) <- env_obj$shark_names
			ylim_foraging <- max(sapply(foraging_density_byregion, function(x) sapply(x, function(ii) ii$y)), na.rm=TRUE)

			if (env_obj$smooth_parameters) {
				foraging_density_byregion_smoothed <- lapply(env_obj$shark_names, function(ss) lapply(1:env_obj$nregions, function(r) density_min2(x=env_obj$region_foraging_draws_smoothed[ ,r,ss], m1=0, m2=1)))
				names(foraging_density_byregion_smoothed) <- env_obj$shark_names
				ylim_foraging <- max(ylim_foraging, sapply(foraging_density_byregion_smoothed, function(x) sapply(x, function(ii) ii$y)), na.rm=TRUE)

			}
						
						
			for (ss in env_obj$shark_names) {
				
				plot(foraging_density_byregion[[ ss ]][[ 1 ]], ylab="density", xlim=c(0,1), ylim=c(0,ylim_foraging),col=1, lty=2, main=paste("Density of Pr(foraging) by region, shark",ss), xlab="Probability", las=1)
				
				if (env_obj$nregions > 1) {
					for ( r in 2:env_obj$nregions) {
					
						lines(foraging_density_byregion[[ ss ]][[ r ]], col=r, lty=(r %% 3)+1)
					 
					}
				
				}
				if (! is.null(env_obj$known_foraging_prob)) { 
					abline(v=env_obj$known_foraging_prob[ ss,1:env_obj$nregions] + runif(env_obj$nregions, min=-0.001, max=0.001), col=1:env_obj$nregions, lty=(1:env_obj$nregions)%%3 +1) 
				}	
				legend("topright",col=1:env_obj$nregions, legend=paste(1:env_obj$nregions, ":", region_tab, "%"), cex=0.5, lty=(1:env_obj$nregions %% 3) + 1)
				
				if (env_obj$smooth_parameters) {
					plot(foraging_density_byregion_smoothed[[ ss ]][[ 1 ]], ylab="density", xlim=c(0,1), ylim=c(0,ylim_foraging),col=1, lty=2, main=paste("Density of Pr(foraging) by region, shark",ss, "(smoothed)"), xlab="Probability", las=1)
				
					if (env_obj$nregions > 1) {
						for ( r in 2:env_obj$nregions) {
						
							lines(foraging_density_byregion_smoothed[[ ss ]][[ r ]], col=r, lty=(r %% 3)+1)
						 
						}
					
					}
					if (! is.null(env_obj$known_foraging_prob)) { 
						abline(v=env_obj$known_foraging_prob[ ss,1:env_obj$nregions] + runif(env_obj$nregions, min=-0.001, max=0.001), col=1:env_obj$nregions, lty=(1:env_obj$nregions)%%3 +1) 
					}	
					legend("topright",col=1:env_obj$nregions, legend=paste(1:env_obj$nregions, ":", region_tab, "%"), cex=0.5, lty=(1:env_obj$nregions %% 3) + 1)
				}	
					
			}#end over sharks		
					
			#transition densities
			p1to2_density_byregion <- lapply(env_obj$shark_names, function(ss) lapply(1:env_obj$nregions, function(r) density_min2(x=env_obj$region_trans_draws[,r,1,2, ss], m1=0, m2=1)))
			p2to1_density_byregion <- lapply(env_obj$shark_names, function(ss) lapply(1:env_obj$nregions, function(r) density_min2(x=env_obj$region_trans_draws[,r,2,1, ss], m1=0, m2=1)))

			names(p1to2_density_byregion) <- names(p2to1_density_byregion) <- env_obj$shark_names
			ylim_trans <- max(sapply(p1to2_density_byregion, function(x) sapply(x, function(ii) ii$y)), sapply(p2to1_density_byregion, function(x) sapply(x, function(ii) ii$y)), na.rm=TRUE)
			
			if (env_obj$smooth_parameters) {
				p1to2_density_byregion_smoothed <- lapply(env_obj$shark_names, function(ss) lapply(1:env_obj$nregions, function(r) density_min2(x=env_obj$region_trans_draws_smoothed[,r,1,2, ss], m1=0, m2=1)))
				p2to1_density_byregion_smoothed <- lapply(env_obj$shark_names, function(ss) lapply(1:env_obj$nregions, function(r) density_min2(x=env_obj$region_trans_draws_smoothed[,r,2,1, ss], m1=0, m2=1)))

				names(p1to2_density_byregion_smoothed) <- names(p2to1_density_byregion_smoothed) <- env_obj$shark_names
				ylim_trans <- max(ylim_trans, sapply(p1to2_density_byregion_smoothed, function(x) sapply(x, function(ii) ii$y)), sapply(p2to1_density_byregion_smoothed, function(x) sapply(x, function(ii) ii$y)), na.rm=TRUE)
			
			}	

			
			#P1t2
			for (ss in env_obj$shark_names) {
				
				plot(p1to2_density_byregion[[ ss ]][[ 1 ]], ylab="density", xlim=c(0,1), ylim=c(0,ylim_trans),col=1, lty=2, main=paste("Density of Pr(1 -> 2) by region, shark", ss), xlab="Probability", las=1)
				
				if (env_obj$nregions > 1) {
					for ( r in 2:env_obj$nregions) {
					
						lines(p1to2_density_byregion[[ ss ]][[ r ]], col=r, lty=(r %% 3)+1)
					 
					}
				
				}
				if (! is.null(env_obj$known_trans_prob)) { 
					abline(v=env_obj$known_trans_prob_prob[ rownames(env_obj$known_trans_prob)== ss, 1:env_obj$nregions, drop=FALSE][1,] + runif(env_obj$nregions, min=-0.001, max=0.001), col=1:env_obj$nregions, lty=(1:env_obj$nregions)%%3 +1) 
				}	
				legend("topright",col=1:env_obj$nregions, legend=paste(1:env_obj$nregions, ":", region_tab, "%"), cex=0.5, lty=(1:env_obj$nregions %% 3) + 1)
				
				if (env_obj$smooth_parameters) {
					plot(p1to2_density_byregion_smoothed[[ ss ]][[ 1 ]], ylab="density", xlim=c(0,1), ylim=c(0,ylim_trans),col=1, lty=2, main=paste("Density of Pr(1 -> 2) by region, shark",ss, "(smoothed)"), xlab="Probability", las=1)
				
					if (env_obj$nregions > 1) {
						for ( r in 2:env_obj$nregions) {
						
							lines(p1to2_density_byregion_smoothed[[ ss ]][[ r ]], col=r, lty=(r %% 3)+1)
						 
						}
					
					}
					if (! is.null(env_obj$known_trans_prob)) { 
						abline(v=env_obj$known_trans_prob_prob[ rownames(env_obj$known_trans_prob)== ss, 1:env_obj$nregions, drop=FALSE][1,] + runif(env_obj$nregions, min=-0.001, max=0.001), col=1:env_obj$nregions, lty=(1:env_obj$nregions)%%3 +1) 
					}	
					legend("topright",col=1:env_obj$nregions, legend=paste(1:env_obj$nregions, ":", region_tab, "%"), cex=0.5, lty=(1:env_obj$nregions %% 3) + 1)
				}	
					
			}#end over sharks				


			#P2t1
			for (ss in env_obj$shark_names) {
				
				plot(p2to1_density_byregion[[ ss ]][[ 1 ]], ylab="density", xlim=c(0,1), ylim=c(0,ylim_trans),col=1, lty=2, main=paste("Density of Pr(2 -> 1) by region, shark", ss), xlab="Probability", las=1)
				
				if (env_obj$nregions > 1) {
					for ( r in 2:env_obj$nregions) {
					
						lines(p2to1_density_byregion[[ ss ]][[ r ]], col=r, lty=(r %% 3)+1)
					 
					}
				
				}
				if (! is.null(env_obj$known_trans_prob)) { 
					abline(v=env_obj$known_trans_prob[ rownames(env_obj$known_trans_prob)== ss, 1:env_obj$nregions, drop=FALSE][2,] + runif(env_obj$nregions, min=-0.001, max=0.001), col=1:env_obj$nregions, lty=(1:env_obj$nregions)%%3 +1) 
				}	
				legend("topright",col=1:env_obj$nregions, legend=paste(1:env_obj$nregions, ":", region_tab, "%"), cex=0.5, lty=(1:env_obj$nregions %% 3) + 1)
				
				if (env_obj$smooth_parameters) {
					plot(p2to1_density_byregion_smoothed[[ ss ]][[ 1 ]], ylab="density", xlim=c(0,1), ylim=c(0,ylim_trans),col=1, lty=2, main=paste("Density of Pr(foraging) by region, shark",ss, "(smoothed)"), xlab="Probability", las=1)
				
					if (env_obj$nregions > 1) {
						for ( r in 2:env_obj$nregions) {
						
							lines(p2to1_density_byregion_smoothed[[ ss ]][[ r ]], col=r, lty=(r %% 3)+1)
						 
						}
					
					}
					if (! is.null(env_obj$known_trans_prob)) { 
						abline(v=env_obj$known_trans_prob[ rownames(env_obj$known_trans_prob)== ss, 1:env_obj$nregions, drop=FALSE][2,] + runif(env_obj$nregions, min=-0.001, max=0.001), col=1:env_obj$nregions, lty=(1:env_obj$nregions)%%3 +1) 
					}	
					legend("topright",col=1:env_obj$nregions, legend=paste(1:env_obj$nregions, ":", region_tab, "%"), cex=0.5, lty=(1:env_obj$nregions %% 3) + 1)
				}	
					
			}#end over sharks	


			
		}#not time dependent
		else {

			tis <- 1:10
			env_obj$region_trans_draws <-	array(NA, dim=c(nresamp, length(tis), env_obj$nregions*2, env_obj$nsharks), 
												  dimnames=list(1:nresamp, tis, rep(env_obj$rnames, each=2), env_obj$shark_names))
		
			for (s in env_obj$shark_names) {
				par(mfrow=c(2,1))
				r_tab <- apply(env_obj$region_counts[,,s, drop=FALSE], 2, function(x) all(x==0))
				if (any(r_tab)) { 
					noobs <- which(r_tab==TRUE)
					env_obj$region_trans_draws[,,c(2*noobs-1, 2*noobs) ,s] <- 0
				}
				r_with_obs <- which(r_tab==FALSE)
				
				ids <- rep(1:env_obj$npart, nresamp/env_obj$npart) #low_var_sample(wts=param_sampling_weights, M=nresamp)
			  
				#ids <- sample(1:env_obj$npart, prob=env_obj$region_counts[,r,drop=FALSE], replace=TRUE, size=nresamp)
				tab <- table(factor(ids, levels=1:env_obj$npart))
					
				for (p in which(tab > 0)) {
							
					
					for (r in r_with_obs) {
						for (tt in tis) {
						
							env_obj$region_trans_draws[ ids==p,tt, c(2*r-1, 2*r),s] <- t(cbind(rbeta(n=tab[ p ], shape1=env_obj$transition_mat[[ s ]][[ p ]]$dirichlet_pars[r, 1]*tt, shape2=1),
																							   rbeta(n=tab[ p ], shape1=env_obj$transition_mat[[ s ]][[ p ]]$dirichlet_pars[r, 2]*tt, shape2=1)))
						}															
					}#loop over regions		
			
				}
				plot(x=-1, y=-1, xlim=range(tis), ylim=c(0,1), main=paste("Time-dependent CI of regional Pr(1 -> 2), shark",s), xlab="Number of steps in state", ylab="Probability", las=1)
					
				for (r in 1:env_obj$nregions) {
					for (tt in tis) {
						tmp <- env_obj$region_trans_draws[,tt,2*r-1,s]
						tmp <- tmp[ ! is.na(tmp) ]
						
						lines(x=rep(tt,2), y=as.numeric(HDInterval::hdi(tmp)), col=r, type="b", pch=19, cex=0.3)
						#lines(x=rep(tt,2), y=quantile(tmp, probs=c(0.025, 0.975)), col=r, type="b", pch=19, cex=0.3)
					}	
				}
				legend("topright",col=1:env_obj$nregions, lty=1, legend=paste("region", 1:env_obj$nregions), cex=.5)	
				plot(x=-1, y=-1, xlim=range(tis), ylim=c(0,1), main=paste("Time-dependent CI of regional Pr(2 -> 1), shark",s), xlab="Number of steps in state", ylab="Probability", las=1)
					
				for (r in 1:env_obj$nregions) {
					for (tt in tis) {
						tmp <- env_obj$region_trans_draws[,tt,2*r,s]
						tmp <- tmp[ ! is.na(tmp) ]
					
						lines(x=rep(tt,2), y=as.numeric(HDInterval::hdi(tmp)), col=r, type="b", pch=19, cex=0.3)

						#lines(x=rep(tt,2), y=quantile(tmp, probs=c(0.025, 0.975)), col=r, type="b", pch=19, cex=0.3)
					}	
				}	
				legend("topright",col=1:env_obj$nregions, lty=1, legend=paste("region", 1:env_obj$nregions), cex=.5)	
			
			
			}#loop over sharks
				
			
			
		}#time dependency

		
		#interaction density
		
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
				
				
			}	
				
			xlim_mu <- c(-3,3)
	
						
			if (env_obj$nstates > 2) {
				for (s in env_obj$shark_names) {
					par(mfrow=c(3,1))
					#par(mfrow=c(ifelse(env_obj$smoothing,2,3),1))
					
					#basically we want to see when eta is bigger than -1/(2*tau*p_k) if so then it is spatially clustered.
					plot(x=-8, y=-8, xlim=xlim_mu, ylim=c(0,4), lty=2, main=paste("Density of interaction mu, shark",s), xlab="Value", ylab="Density", las=1)
					
					for (k in 1:(env_obj$nstates-1)) {
						
						lines(density(env_obj$interact_mu_draws[,k,s], from=xlim_mu[ 1 ], to=xlim_mu[ 2 ], na.rm=TRUE), col=k, lty=(k%%3)+1, lwd=1.5)

					}
					
					legend("bottomleft", lty=(1:env_obj$nstates)%%3 +1, col=1:env_obj$nstates, legend=paste("eta",1:env_obj$nstates,sep="_"), lwd=1.5)
					
				
					
					plot(x=-4, y=-4, xlim=xlim_mu, ylim=c(1,env_obj$nstates+1), main=paste("CIs for interaction mu, shark", s), xlab="Value", ylab="Density", las=1, yaxt="n")
					axis(side=2, labels=1:env_obj$nstates, at=1:env_obj$nstates)
					
					for (k in 1:(env_obj$nstates-1)) {
						emp_q <- as.numeric(HDInterval::hdi(env_obj$interact_mu_draws[,k,s])) #quantile(env_obj$interact_mu_draws[,k,s], p=c(0.025, 0.975))
						lines(x=emp_q, y=c(k, k), lty=1, type="b", col=k, pch=19, cex=.8, lwd=1.5)
						#rug(side=3, ticksize=0.1, x=rug_tmp, fg=k, quiet=TRUE,lwd=1)
						
						#text(x=rug_tmp, y=rep(0.75+k, length(pk)), labels=pk, col=k, cex=0.7)
					}
					legend("bottomleft", lwd=1.5, col=1:env_obj$nstates, legend=paste("eta",1:env_obj$nstates,sep="_"))
			
			
			
					#CIs for interactions
					
					
					plot(x=-4, y=-4, xlim=c(0,15), ylim=c(0, length(pvals)+1), main=paste("CIs for interaction scale, shark", s, "by proportion of neib."), xlab="Value", ylab="proportion of neighborhood", las=1, yaxt="n")
					axis(side=2, labels=pvals, at=seq(0.5, 0.5+length(pvals)-1, by=1) )
					abline(v=1, lty=3)
					
					
					for (pv in 1:length(pvals)) {
						for (k in 1:(env_obj$nstates-1)) {
							emp_q <- as.numeric(HDInterval::hdi(env_obj$interact_intensity_draws[,k,pv,s])) #quantile(env_obj$interact_intensity_draws[,k,pv,s], p=c(0.025, 0.975))
							lines(x=emp_q, y=rep(pv + k/(env_obj$nstates+1), 2), lty=1, type="b", col=k, pch=19, cex=.8, lwd=1.5)
						}					
					}
				
					legend("bottomright", lwd=1.5, pch=19, pt.cex=0.8, col=1:env_obj$nstates, legend=paste("rho",1:env_obj$nstates,sep="_"))
				
				
				
					par(mfrow=c(2,1))
				
					if (nrow(na.omit(env_obj$spatial_interact_mu_history[,,s, drop=FALSE]))) {
						matplot(env_obj$spatial_interact_mu_history[,,s], type="b", lwd=1.5, pch=19, cex=0.4, col=1:(env_obj$nstates-1), main=paste("Median historical simulated interaction mu, shark",s),  ylab="mu value")
						legend("bottomleft",  col=1:(env_obj$nstates-1), lwd=1.5, legend=paste("eta",1:(env_obj$nstates-1),sep="_"))
					}
					if (nrow(na.omit(env_obj$spatial_interact_intensity_history[,,s, drop=FALSE]))) {
						matplot(env_obj$spatial_interact_intensity_history[,,s], type="b", lwd=1.5, pch=19, cex=0.4, col=1:(env_obj$nstates-1), main=paste("Median historical interaction value (simulated over all neighborhoods), shark",s),  ylab="interaction value")
						legend("bottomleft",  col=1:(env_obj$nstates-1), lwd=1.5, legend=paste("rho",1:(env_obj$nstates-1),sep="_"))
					}
		
				}#loop over sharks
			
			}#more than 2 states
			else {
				par(mfrow=c(2 + env_obj$interact,1))
				#par(mfrow=c(ifelse(env_obj$smoothing, 2, 3),1))			
				#basically we want to see when eta is bigger than -1/(2*tau*p_k) if so then it is spatially clustered.
				
				foraging_interaction_mu_density <- lapply(env_obj$shark_names, function(ss) density_min2(env_obj$interact_mu_draws[,1,ss]))
				names(foraging_interaction_mu_density) <- env_obj$shark_names
				ylim_mu <- max(sapply(foraging_interaction_mu_density, function(ss) max(ss$y)))
				xlim_mu_ns <- quantile(env_obj$interact_mu_draws[,1,], probs=c(0.005, 0.995))


				plot(x=-10, y=-10, xlim=xlim_mu_ns, ylim=c(0, ylim_mu), lty=2, main="Density of foraging interaction mu", xlab="Value", ylab="Density", las=1)
				abline(h=0, v=0)
				for (s in 1:env_obj$nsharks) {
					lines(foraging_interaction_mu_density[[ env_obj$shark_names[ s ] ]], col=s, lty=(s%%3)+1, lwd=1.5)
					
				}
				
				legend("topright", lty=(1:env_obj$nsharks)%%3 +1, col=1:env_obj$nsharks, legend=env_obj$shark_names, lwd=1.5)
				
				plot(x=-4, y=-4, xlim=xlim_mu_ns, ylim=c(1,env_obj$nsharks), main="CIs for interaction mu", xlab="Value", ylab="Shark", las=1, yaxt="n")
				abline(v=0, lty=2)

				axis(side=2, labels=env_obj$shark_names, at=1:env_obj$nsharks)
				for (s in 1:env_obj$nsharks) {
					
					emp_q <- HDInterval::hdi(env_obj$interact_mu_draws[,1,env_obj$shark_names[ s ]])
					#emp_q <- quantile(env_obj$interact_mu_draws[,1,env_obj$shark_names[ s ]], p=c(0.025, 0.975))
					lines(x=emp_q, y=c(s, s), lty=1, type="b", col=1, pch=19, cex=.8, lwd=1.5)
					
				}
				
				
				if (env_obj$smooth_parameters) {
				
				
					foraging_interaction_mu_smoothed_density <- lapply(env_obj$shark_names, function(ss) density_min2(env_obj$interact_mu_draws_smoothed[,1,ss]))
					names(foraging_interaction_mu_smoothed_density) <- env_obj$shark_names
					ylim_mu_sm <- max(sapply(foraging_interaction_mu_smoothed_density, function(ss) max(ss$y)))
					xlim_mu_sm <- quantile(env_obj$interact_mu_draws_smoothed[,1,], probs=c(0.005, 0.995))

				
					plot(x=-10, y=-10, xlim=xlim_mu_sm, ylim=c(0, ylim_mu_sm), lty=2, main="Density of foraging interaction mu (smoothed)", xlab="Value", ylab="Density", las=1)
					abline(h=0, v=0)

					for (s in 1:env_obj$nsharks) {
						lines(foraging_interaction_mu_smoothed_density[[ env_obj$shark_names[ s ] ]], col=s, lty=(s%%3)+1, lwd=1.5)

					}
					
					legend("topleft", lty=(1:env_obj$nsharks)%%3 +1, col=1:env_obj$nsharks, legend=env_obj$shark_names, lwd=1.5)
					
					plot(x=-4, y=-4, xlim=xlim_mu_sm, ylim=c(1,env_obj$nsharks), main="CIs for interaction mu (smoothed)", xlab="Value", ylab="Shark", las=1, yaxt="n")
					abline(v=0, lty=2)

					axis(side=2, labels=env_obj$shark_names, at=1:env_obj$nsharks)
					for (s in 1:env_obj$nsharks) {
					
						emp_q <- HDInterval::hdi(env_obj$interact_mu_draws_smoothed[,1,env_obj$shark_names[ s ]])
						#emp_q <- quantile(env_obj$interact_mu_draws[,1,env_obj$shark_names[ s ]], p=c(0.025, 0.975))
						lines(x=emp_q, y=c(s, s), lty=1, type="b", col=1, pch=19, cex=.8, lwd=1.5)
						
					}
				}
				
				
				

				
				#CIs for interactions
				
				for (s in env_obj$shark_names) {
					plot(x=-4, y=-4, xlim=c(0,15), ylim=c(0, length(pvals)+1), main=paste("CIs for interaction scale, shark", s, "by proportion of neib."), xlab="Value", ylab="proportion of neighborhood", las=1, yaxt="n")
					abline(v=1, lty=2)

					axis(side=2, labels=pvals, at=seq(0.5, 0.5+length(pvals)-1, by=1) )
					for (pv in 1:length(pvals)) {
							emp_q <- HDInterval::hdi(env_obj$interact_intensity_draws[,1,pv,s])
							#emp_q <- quantile(env_obj$interact_intensity_draws[,1,pv,s], p=c(0.025, 0.975))
							lines(x=emp_q, y=rep(pv, 2), lty=1, type="b", col=1, pch=19, cex=.8, lwd=1.5)
					}					
				}
				
				if ( any(! is.na(env_obj$spatial_interact_mu_history))) {
					matplot(env_obj$spatial_interact_mu_history[,1,], type="b", lwd=1.5, pch=19, cex=0.4, col=1:env_obj$nsharks, lty = (1:env_obj$nsharks)%%3 + 1, main="Median historical simulated interaction mu",  ylab="mu value")
					abline(h=0, lty=2)
					legend("bottomleft",  col=1:env_obj$nsharks, lwd=1.5, legend=env_obj$shark_names, bg="white", lty = (1:env_obj$nsharks)%%3 + 1)
				}
				
				if ( any(! is.na(env_obj$spatial_interact_intensity_history))) {
					matplot(env_obj$spatial_interact_intensity_history[,1,], type="b", lwd=1.5, pch=19, cex=0.4, col=1:env_obj$nsharks, lty = (1:env_obj$nsharks)%%3 + 1, main="Median historical interaction value (simulated over all neighborhood proportions)",  ylab="interaction value")
					abline(h=0, lty=2)
					legend("bottomleft",  col=1:env_obj$nsharks, lwd=1.5, legend=env_obj$shark_names, bg="white", lty = (1:env_obj$nsharks)%%3 + 1)
					
				}
						
			
			}

			
			
		}#end interact

	}#if multiple states


	par(mfcol=c(1,1))




	for (s in env_obj$shark_names) {

		par(mfcol=c(1,1))
		dfrompts <- colSums(env_obj$error_final_allpart[,,s], na.rm=TRUE)
		
		#add in total distance traveled between particles, want to ideally minimize this
		
		reg_int_noobs <- env_obj$shark_valid_steps[[ s ]]
		reg_int_noobs <- reg_int_noobs[ ! (reg_int_noobs %in% env_obj$shark_intervals[[ s ]]) ]
		#print(reg_int_noobs)
		if (length(reg_int_noobs)) {
		
			dfrompts <- dfrompts + colSums(apply((env_obj$Xpart_history[reg_int_noobs,c("X","Y"),,s, drop=FALSE] - env_obj$Xpart_history[reg_int_noobs + 1,c("X","Y"),,s, drop=FALSE])^2, c(1,3), function(x) sqrt(sum(x, na.rm=TRUE))), na.rm=TRUE)
		}
		
		#print(env_obj$error_final_allpart[,,s])
		#print(dfrompts)
		closest <- order(dfrompts)[1]
		nsteps <- length(env_obj$shark_valid_steps[[ s ]])
		r1 <- range(c(env_obj$XY[env_obj$tags==s,"X"], env_obj$Xpart_history[,"X",closest,s]), na.rm=TRUE)
		r2 <- range(c(env_obj$XY[env_obj$tags==s,"Y"], env_obj$Xpart_history[,"Y",closest,s]), na.rm=TRUE)
	
	
		cex_sim <- rep(1, env_obj$N+1)
		cex_sim[ env_obj$first_intervals[[ s ]] ] <- 2

		
		plot(Y ~ X, data=env_obj$XY[env_obj$tags==s,,drop=FALSE], type="b", col="red", main=paste("Red=observed locs., black=closest trajectory, shark",s), pch=19, asp=1, xlim=r1, ylim=r2, cex=c(2, rep(1, sum(env_obj$tags==s)-1)))
		#plot(Y ~ X, data=env_obj$XY[env_obj$tags==s,], type="p", col="red", main=paste("Red=observed positions, shark",s,", black=closest trajectory"), pch=19, asp=1, xlim=r1, ylim=r2, cex=c(2, rep(1, sum(env_obj$tags==s)-1)))
		#xspline(env_obj$XY[env_obj$tags==s,"X"], env_obj$XY[env_obj$tags==s,"X"], border="red", lwd=2, shape=1)
	
		
		lines(Y ~ X, data=env_obj$Xpart_history[, c("X","Y"),closest,s], type="b", col="black", pch=19, asp=1, cex=cex_sim)
		#points(Y ~ X, data=env_obj$Xpart_history[, c("X","Y"),closest,s], type="p", col="black", pch=19, asp=1, cex=cex_sim)
		#xspline(env_obj$Xpart_history[, "X",closest,s], env_obj$Xpart_history[, "Y",closest,s], border="black", lwd=2, shape=1)
		
		sp::plot(env_obj$area_map, border="green", lwd=2, add=TRUE, axes=TRUE, las=0)
		
		if (env_obj$nregions > 1 ) { deldir::plot.deldir(vortess, wlines="tess", cex=0, add=TRUE) }
		text(env_obj$centroids[,1,drop=FALSE], env_obj$centroids[,2,drop=FALSE], labels=1:nrow(env_obj$centroids))

		if (env_obj$smoothing) {
			
			dfrompts <- colSums(env_obj$error_smoothed_allpart[,,s], na.rm=TRUE)
		
			if (length(reg_int_noobs)) {
				
				tmp <- (env_obj$Xpart_history_smoothed[reg_int_noobs,c("X","Y"),,s, drop=FALSE] - env_obj$Xpart_history_smoothed[reg_int_noobs + 1,c("X","Y"),,s, drop=FALSE])^2

				dfrompts <- dfrompts + colSums(apply(tmp^2, c(1,3), function(x) sqrt(sum(x, na.rm=TRUE))), na.rm=TRUE)
			}
			#print(env_obj$error_final_allpart[,,s])
			#print(dfrompts)
			closest <- order(dfrompts)[1]
			
			r1 <- range(c(env_obj$XY[env_obj$tags==s,"X"], env_obj$Xpart_history_smoothed[,"X",closest,s]), na.rm=TRUE)
			r2 <- range(c(env_obj$XY[env_obj$tags==s,"Y"], env_obj$Xpart_history_smoothed[,"Y",closest,s]), na.rm=TRUE)
			
			cex_sim <- rep(1, env_obj$N+1)
			cex_sim[ env_obj$first_intervals[[ s ]] ] <- 2
			
			plot(Y ~ X, data=env_obj$XY[env_obj$tags==s,,drop=FALSE], type="b", col="red", main=paste("Red=observed locs., black=closest trajectory (smoothed), shark", s),
			pch=19, asp=1, xlim=r1, ylim=r2, cex=c(2, rep(1, sum(env_obj$tags==s)-1)))
			#plot(Y ~ X, data=env_obj$XY[env_obj$tags==s,], type="p", col="red", main=paste("Red=observed positions, shark",s,", black=closest trajectory (smoothed)"), pch=19, asp=1, xlim=r1, ylim=r2, cex=c(2, rep(1, sum(env_obj$tags==s)-1)))
			
			#xspline(env_obj$XY[env_obj$tags==s,"X"], env_obj$XY[env_obj$tags==s,"X"], border="red", lwd=2, shape=1)
		
			lines(Y ~ X, data=env_obj$Xpart_history_smoothed[, c("X","Y"),closest,s], type="b", col="black", pch=19, asp=1, cex=cex_sim)
			#xspline(env_obj$Xpart_history_smoothed[, "X",closest,s], env_obj$Xpart_history_smoothed[, "Y",closest,s], border="black", lwd=2, shape=1)
		
			sp::plot(env_obj$area_map, border="green", lwd=2, add=TRUE, axes=TRUE, las=0)
			
			if (env_obj$nregions > 1 ) { deldir::plot.deldir(vortess, wlines="tess", cex=0, add=TRUE) }
			text(env_obj$centroids[,1,drop=FALSE], env_obj$centroids[,2,drop=FALSE], labels=1:nrow(env_obj$centroids))

		
		}
		
		xall <- env_obj$Xpart_history[,"X",,s]
		yall <- env_obj$Xpart_history[,"Y",,s]
		
		xobs <- env_obj$d[ env_obj$tags==s,"X"]
		yobs <- env_obj$d[ env_obj$tags==s,"Y"]
		

		median_position <- matrix(NA, ncol=2, nrow=env_obj$N)
		colnames(median_position) <- c("X","Y")
		
		for (i in env_obj$shark_valid_steps[[ s ]]) {
			median_position[i,] <- apply(env_obj$Xpart_history[i, c("X","Y"),,s], 1, median)
		}	
		
		r1 <- range(c(xobs, median_position[,"X"]), na.rm=TRUE)
		r2 <- range(c(yobs, median_position[,"Y"]), na.rm=TRUE)

		plot(Y ~ X, data=env_obj$d[ env_obj$tags==s,c("X","Y"),drop=FALSE], type="b", col="red", 
		main=paste("Red=observed locs., black=median position, shark",s), pch=19, asp=1, xlim=r1, ylim=r2, cex=c(2, rep(1, sum(env_obj$tags==s)-1)))
		#xspline(env_obj$d[ env_obj$tags==s,"X"], d[ env_obj$tags==s,"Y"], border="red", lwd=2, shape=1)
		
		
		lines(Y ~ X, data=median_position, type="b", col="black", pch=19, asp=1, cex=cex_sim)
		#points(Y ~ X, data=median_position, type="p", col="black", pch=19, asp=1, cex=cex_sim)
		#xspline(median_position[,"X"], median_position[,"Y"], border="black", lwd=2, shape=1)
		
		sp::plot(env_obj$area_map, border="green", lwd=2, add=TRUE, axes=TRUE, las=0)
		if (env_obj$nregions > 1 ) { deldir::plot.deldir(vortess, wlines="tess", cex=0, add=TRUE) }
		text(env_obj$centroids[,1], env_obj$centroids[,2], labels=1:nrow(env_obj$centroids))

		
		
		if (env_obj$smoothing) {
		
			median_position <- matrix(NA, ncol=2, nrow=env_obj$N)
			colnames(median_position) <- c("X","Y")
		
			for (i in env_obj$shark_valid_steps[[ s ]]) {
				median_position[i,] <- apply(env_obj$Xpart_history_smoothed[i, c("X","Y"),,s], 1, median)
			}	
			
			r1 <- range(c(xobs, median_position[,"X"]), na.rm=TRUE)
			r2 <- range(c(yobs, median_position[,"Y"]), na.rm=TRUE)

			plot(Y ~ X, data=env_obj$d[ env_obj$tags==s,c("X","Y"),drop=FALSE], type="b", col="red",
			main=paste("Red=observed locs., black=median position (smoothed), shark",s), pch=19, asp=1, xlim=r1, ylim=r2, cex=c(2, rep(1, sum(env_obj$tags==s)-1)))
			#xspline(env_obj$d[ env_obj$tags==s,"X"], d[ env_obj$tags==s,"Y"], border="red", lwd=2, shape=1)
		
			#points(Y ~ X, data=median_position, type="p", col="black", pch=19, asp=1, cex=cex_sim)
			#xspline(median_position[,"X"], median_position[,"Y"], border="black", lwd=2, shape=1)
		
			lines(Y ~ X, data=median_position, type="b", col="black", pch=19, asp=1, cex=cex_sim)
			sp::plot(env_obj$area_map, border="green", lwd=2, add=TRUE, axes=TRUE, las=0)
			if (env_obj$nregions > 1 ) { deldir::plot.deldir(vortess, wlines="tess", cex=0, add=TRUE) }
			text(env_obj$centroids[,1], env_obj$centroids[,2], labels=1:nrow(env_obj$centroids))

	
		
		}
		
		#overall positions

		r1 <- range(c(xobs, xall), na.rm=TRUE)
		r2 <- range(c(yobs, yall), na.rm=TRUE)

		
		plot(x=xall , y=yall, type="p", col="black", main=paste("All particle positions (red=observed locs.), shark",s), pch=1, asp=1, 
			 xlim=r1, ylim=r2, cex=.7, xlab="X", ylab="Y")
		lines(Y ~ X, data=env_obj$XY[env_obj$tags==s,,drop=FALSE], type="b", col="red", pch=19, cex=c(2, rep(1, sum(env_obj$tags==s)-1))) 
		sp::plot(env_obj$area_map, border="green", lwd=2, add=TRUE, axes=TRUE, las=0)
		if (env_obj$nregions > 1 ) { deldir::plot.deldir(vortess, wlines="tess", cex=0, add=TRUE) }
		text(env_obj$centroids[,1], env_obj$centroids[,2], labels=1:nrow(env_obj$centroids))


		# extract these so we have the same xlimits in ggplot
		ggxlim <- par()$usr[1:2]
		ggylim <- par()$usr[3:4]

		#density plot
		particle_locs <- do.call(rbind, lapply(1:env_obj$npart, function(pp) env_obj$Xpart_history[,c("X","Y"),pp,s]))
		particle_locs <- na.omit(as.data.frame(particle_locs))
		rownames(particle_locs) <- NULL

		
		#g <- ggplot(particle_locs, aes(x=X, y=Y)) + coord_fixed(ratio=1, xlim=ggxlim, ylim=ggylim) + theme(axis.text.x = element_text(size=14), axis.text.y = element_text(size=14))
		#g <- g + theme_bw() + theme(panel.background = element_rect(fill="white"))		
		
		g <- ggplot(particle_locs, aes(x=.data$X, y=.data$Y)) + theme_bw() + theme(aspect.ratio=1, axis.text.x = element_text(size=14), axis.text.y = element_text(size=14))
		g <- g + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
		g <- g + ggtitle(paste("All particle positions density (red=observed locs.), shark",s)) + theme(plot.title=element_text(face="bold"))
		g <- g + stat_density2d(aes(fill=after_stat(!!str2lang("density"))), geom="tile", contour=FALSE)
		g <- g + scale_fill_gradient(low="white", high="black", na.value="white") + theme(legend.position="right")
		g <- g + geom_path(data=make_segments(xy=env_obj$XY[env_obj$tags==s, c("X","Y"),drop=FALSE]), aes(x=.data$X, y=.data$Y), colour="red", lwd=1)
		print(g)
		rm(g)



		if (env_obj$compare_with_known) {
		
			true_locs_in_time_range <- (env_obj$known_regular_step_ds$tag==s) & (env_obj$known_regular_step_ds$t_intervals <= max(env_obj$shark_valid_steps[[s]]))
		
			plot(x=xall , y=yall, type="p", col="black", main=paste("All particle positions (red=TRUE locs.), shark",s), pch=1, asp=1, 
			 xlim=r1, ylim=r2, cex=.7, xlab="X", ylab="Y")
			lines(Y ~ X, data=env_obj$known_regular_step_ds[true_locs_in_time_range,,drop=FALSE], type="b", col="red", pch=19, cex=c(2, rep(1, sum(env_obj$tags==s)-1))) 
			sp::plot(env_obj$area_map, border="green", lwd=2, add=TRUE, axes=TRUE, las=0)
			if (env_obj$nregions > 1 ) { deldir::plot.deldir(vortess, wlines="tess", cex=0, add=TRUE) }
			text(env_obj$centroids[,1], env_obj$centroids[,2], labels=1:nrow(env_obj$centroids))
		
			ggxlim <- par()$usr[1:2]
			ggylim <- par()$usr[3:4]
		
			g <- ggplot(particle_locs, aes(x=.data$X, y=.data$Y)) + theme_bw() + theme(aspect.ratio=1, axis.text.x = element_text(size=14), axis.text.y = element_text(size=14))

			#g <- ggplot(particle_locs, aes(x=X, y=Y)) + coord_fixed(ratio=1, xlim=ggxlim, ylim=ggylim) + theme(axis.text.x = element_text(size=14), axis.text.y = element_text(size=14))
			g <- g + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
			g <- g + ggtitle(paste("All particle positions density (red=TRUE locs.), shark",s)) + theme(plot.title=element_text(face="bold"))
			g <- g + stat_density2d(aes(fill=after_stat(!!str2lang("density"))), geom="tile", contour=FALSE)
			g <- g + scale_fill_gradient(low="white", high="black", na.value="white") + theme(legend.position="right")
			g <- g + geom_path(data=make_segments(xy=env_obj$known_regular_step_ds[true_locs_in_time_range, c("X","Y"), drop=FALSE]), aes(x=.data$X, y=.data$Y), colour="red", lwd=1)
			print(g)
			rm(g)

		}
		
		
		
		if (env_obj$smoothing) {
		
			xall_smoothed <- env_obj$Xpart_history_smoothed[,"X",,s]
			yall_smoothed <- env_obj$Xpart_history_smoothed[,"Y",,s]
		
			r1 <- range(c(xobs, xall_smoothed), na.rm=TRUE)
			r2 <- range(c(yobs, yall_smoothed), na.rm=TRUE)

			particle_locs_smoothed <- do.call(rbind, lapply(1:(dim(env_obj$Xpart_history_smoothed)[3]), function(pp) env_obj$Xpart_history_smoothed[,c("X","Y"),pp,s]))
			particle_locs_smoothed <- na.omit(as.data.frame(particle_locs_smoothed))
			rownames(particle_locs_smoothed) <- NULL
				
			plot(x=xall_smoothed , y=yall_smoothed, type="p", col="black", main=paste("All particle positions (smoothed, red=observed locs.), shark",s), pch=1, asp=1, 
				 xlim=r1, ylim=r2, cex=.7, xlab="X", ylab="Y")
			lines(Y ~ X, data=env_obj$XY[env_obj$tags==s,,drop=FALSE], type="b", col="red", pch=19, cex=c(2, rep(1, sum(env_obj$tags==s)-1))) 
			sp::plot(env_obj$area_map, border="green", lwd=2, add=TRUE, axes=TRUE, las=0)
			if (env_obj$nregions > 1 ) { deldir::plot.deldir(vortess, wlines="tess", cex=0, add=TRUE) }
			text(env_obj$centroids[,1], env_obj$centroids[,2], labels=1:nrow(env_obj$centroids))	
		
			ggxlim <- par()$usr[1:2]
			ggylim <- par()$usr[3:4]
		
			g <- ggplot(particle_locs_smoothed, aes(x=.data$X, y=.data$Y)) + theme_bw() + theme(aspect.ratio=1, axis.text.x = element_text(size=14), axis.text.y = element_text(size=14))

			#g <- ggplot(particle_locs_smoothed, aes(x=X, y=Y)) + coord_fixed(ratio=1, xlim=ggxlim, ylim=ggylim) + theme(axis.text.x = element_text(size=14), axis.text.y = element_text(size=14))
			g <- g + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
			g <- g + ggtitle(paste("All particle positions density (smoothed, red=observed locs.), shark",s)) + theme(plot.title=element_text(face="bold"))
			g <- g + stat_density2d(aes(fill=after_stat(!!str2lang("density"))), geom="tile", contour=FALSE)
			g <- g + scale_fill_gradient(low="white", high="black", na.value="white") + theme(legend.position="right")
			g <- g + geom_path(data=make_segments(xy=env_obj$XY[env_obj$tags==s, c("X","Y"),drop=FALSE]), aes(x=.data$X, y=.data$Y), colour="red", lwd=1)
			print(g)
			rm(g)

		
		
			if (env_obj$compare_with_known) {
			
				plot(x=xall_smoothed , y=yall_smoothed, type="p", col="black", main=paste("All particle positions (smoothed, red=TRUE locs.), shark",s), pch=1, asp=1, 
				 xlim=r1, ylim=r2, cex=.7, xlab="X", ylab="Y")
				lines(Y ~ X, data=env_obj$known_regular_step_ds[true_locs_in_time_range,,drop=FALSE], type="b", col="red", pch=19, cex=c(2, rep(1, sum(env_obj$tags==s)-1))) 
				sp::plot(env_obj$area_map, border="green", lwd=2, add=TRUE, axes=TRUE, las=0)
				if (env_obj$nregions > 1 ) { deldir::plot.deldir(vortess, wlines="tess", cex=0, add=TRUE) }
				text(env_obj$centroids[,1], env_obj$centroids[,2], labels=1:nrow(env_obj$centroids))
			
				ggxlim <- par()$usr[1:2]
				ggylim <- par()$usr[3:4]
			

				#g <- ggplot(particle_locs_smoothed, aes(x=X, y=Y)) + coord_fixed(ratio=1, xlim=ggxlim, ylim=ggylim) + theme(axis.text.x = element_text(size=14), axis.text.y = element_text(size=14))
				#g <- g + theme_bw() + theme(panel.background = element_rect(fill="white"))
				g <- ggplot(particle_locs_smoothed, aes(x=.data$X, y=.data$Y)) + theme_bw() + theme(aspect.ratio=1, axis.text.x = element_text(size=14), axis.text.y = element_text(size=14))
				g <- g + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

				g <- g + ggtitle(paste("All particle positions density (smoothed, red=TRUE locs.), shark",s)) + theme(plot.title=element_text(face="bold"))
				g <- g + stat_density2d(aes(fill=after_stat(!!str2lang("density"))), geom="tile", contour=FALSE)
				g <- g + scale_fill_gradient(low="white", high="black", na.value="white") + theme(legend.position="right")
				g <- g + geom_path(data=make_segments(xy=env_obj$known_regular_step_ds[true_locs_in_time_range, c("X","Y"), drop=FALSE]), aes(x=.data$X, y=.data$Y), colour="red", lwd=1)
				print(g)
				rm(g)

			
			}
		
		
		}
		


	}#loop over sharks	

	if (env_obj$output_plot) {	
		grDevices::dev.off()
	}
	
	invisible(NULL)	
}