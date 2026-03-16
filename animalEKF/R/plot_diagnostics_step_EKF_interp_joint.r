plot_diagnostics_step_EKF_interp_joint <- function(env_obj) {


	if (env_obj$other_type == "obs") {
		other_sharks <- env_obj$sharks_with_obs
	}
	else if (env_obj$other_type == "sim") {
		other_sharks <- env_obj$sharks_to_sim
	}
	
	type_title <- ifelse(env_obj$before_samp, "Pre-resamp: ", "Post-resamp: ")
	
	#bounds for logv
	
	q1 <- qnorm(p=0.01, mean=env_obj$mu[,"alpha","mu",,other_sharks], sd=sqrt(env_obj$sigma_draw[,,other_sharks]))
	q99 <- qnorm(p=0.99, mean=env_obj$mu[,"alpha","mu",,other_sharks], sd=sqrt(env_obj$sigma_draw[,,other_sharks]))
	
	
	# pl_logv <- c(seq(from=max(env_obj$logvelocity_truncate[1], min(q1, na.rm=TRUE)), to=min(env_obj$logvelocity_truncate[2], max(q99, na.rm=TRUE)), length.out=100), env_obj$mu[,"alpha","mu",,other_sharks])
	# pl_logv <- c(seq(from=max(min(q1, na.rm=TRUE), env_obj$logvelocity_truncate[1]),  to=min(max(q99, na.rm=TRUE), env_obj$logvelocity_truncate[2]), length.out=100), env_obj$mu[,"alpha","mu",,other_sharks])
	logv_truncate_bounds <- env_obj$logvelocity_truncate + c(-1/3,1/3)*diff(env_obj$logvelocity_truncate)
	# make sure the mean (the max value) ends in the vector
	pl_logv <- c(seq(from=max(min(q1, na.rm=TRUE), logv_truncate_bounds[1]),  to=min(max(q99, na.rm=TRUE), logv_truncate_bounds[2]), length.out=100), env_obj$mu[,"alpha","mu",,other_sharks])

	#pl_logv <- c(seq(from=min(q1, na.rm=TRUE),  to=max(q99, na.rm=TRUE), length.out=100), env_obj$mu[,"alpha","mu",,other_sharks])


	pl_logv <- sort(pl_logv[ ! is.na(pl_logv) ])
	
	pl_turn <- seq(-pi, pi, .1)	

	ymax_logv <- min(1e50, max(dnorm(x=env_obj$mu[,"alpha","mu",,other_sharks], mean=env_obj$mu[,"alpha","mu",,other_sharks], sd=sqrt(env_obj$sigma_draw[,,other_sharks])), na.rm=TRUE))
	ymax_turn <- min(1e50, max(dwrpnorm_tmp(x=env_obj$mu[,"beta","mu",,other_sharks], mean=env_obj$mu[,"beta","mu",,other_sharks], sd=sqrt(env_obj$tau_draw[,,other_sharks])), na.rm=TRUE))

	
			  
	for (ss in other_sharks) {

		#plot of ellipses
		sp::plot(env_obj$area_map, border="green", asp=1, xlim=env_obj$obs_XY_bbox[,"X"], ylim=env_obj$obs_XY_bbox[,"Y"], 
			 main=paste(type_title,ss," t=", env_obj$i, sep=""), xlab="X", ylab="Y", cex.main=0.8, las=0, axes=TRUE) 
	
	
		if (env_obj$other_type == "sim") {
	
			for (p in 1:env_obj$npart) {
				for (k in 1:env_obj$nstates) {
					
					lines(ellipse::ellipse(x=env_obj$Pk_prev[,,k,p,ss], centre=env_obj$mk_prev[1:2,k,p,ss], level=env_obj$loc_pred_plot_conf), lwd=.25, col=k, type="l")
					
				}		
			}
		}
		else {
			for (p in 1:env_obj$npart) {
				for (k in 1:env_obj$nstates) {
					for (y in 1:env_obj$yobs_sharks[ ss ]) {
					   lines(ellipse::ellipse(x=env_obj$SigY[[ ss ]][,,y,k,p], centre=env_obj$MuY[[ ss ]][,y,k,p], level=env_obj$loc_pred_plot_conf), lwd=.25, col=k, type="l")
					}
				}		
			}
			points(x=env_obj$ynext[ ,"X"], y=env_obj$ynext[ ,"Y"], col=env_obj$ynext[ ,"state.guess2"] + 4, 
				   pch=env_obj$shark_symbols[ rownames(env_obj$ynext) ], lwd=2, cex=2)

		}

	
		#logv density
		plot(x=-5, y=-5, xlim=range(pl_logv), ylim=c(0, ymax_logv), xlab="alpha", ylab="density", 
			 main=paste("Log-speed dens, ", ss, ", t=", env_obj$i, sep=""), las=1, cex.main=0.8)
		abline(h=0, lwd=0.25)

		for (k in 1:env_obj$nstates) {
			
			for (p in 1:env_obj$npart) {
			   lines(x=pl_logv, y=dnorm(x=pl_logv, mean=env_obj$mu[k,"alpha","mu",p,ss], sd=sqrt(env_obj$sigma_draw[p,k,ss])), lwd=.5, type="l", lty=1, col=k)
			}
		}
		if (env_obj$nstates > 1) { 
			legend("topleft", col=1:2, lty=1, legend=paste("type", 1:2), cex=0.8)		
		}	
	
		
		#turn density
		plot(x=-5, y=-5, xlim=c(-pi, pi), ylim=c(0, ymax_turn),
			 xlab="beta", ylab="density", main=paste("Turn (rad) dens, ", ss, ", t=", env_obj$i, sep=""), xaxt="n", las=1, cex.main=0.8)
		abline(h=0, lwd=0.25)
		axis(side=1, at=seq(-pi,pi, by=pi/2), labels=c(expression(-pi), expression(paste(-pi, "/", 2)), 0, expression(paste(pi, "/", 2)), expression(pi)))
		
		for (k in 1:env_obj$nstates) {
				
			for (p in 1:env_obj$npart) {

				lines(x=pl_turn, y=dwrpnorm_tmp(x=pl_turn, mean=env_obj$mu[k,"beta","mu",p,ss], sd=sqrt(env_obj$tau_draw[p,k,ss])), lwd=.5, type="l", lty=1, col=k)
			}
		}
		
		if (env_obj$nstates > 1) { 
			legend("topleft", col=1:2, lty=1, legend=paste("type", 1:2), cex=0.8)		
		}
	
	}#end of sharks

	invisible(NULL)
}