plot_diagnostics_step_EKF_1d_interp_joint <- function(env_obj) {


	if (env_obj$other_type == "obs") {
		other_sharks <- env_obj$sharks_with_obs
	}
	else if (env_obj$other_type == "sim") {
		other_sharks <- env_obj$sharks_to_sim
	}
	
	type_title <- ifelse(env_obj$before_samp, "Pre-resamp: ", "Post-resamp: ")
	

	#density plots:
	#only print every few to be efficient
	
	for (ss in other_sharks) {
	
		plot(x=-5, y=-5, xlim=range(env_obj$d[,"X"]), ylim=c(0, env_obj$npart + 1), xlab="X", ylab="particle", yaxt="n", 
			 main=paste(type_title, ss, "particle loc., step", env_obj$i), las=1, cex.main=0.8)

		
		if (env_obj$other_type == "sim") {

			for (p in 1:env_obj$npart) {
				
				for (k in 1:env_obj$nstates) {
				
					lines(x=env_obj$mk_prev[1,k,p,ss] + c(-1, 1)*env_obj$loc_pred_plot_conf_constant*sqrt(env_obj$Pk_prev[1,1,k,p,ss]), y=rep(p+(k-1)*0.5,2), type="l", col=k, lwd=0.1)
				}				
			}	
		}
		else {	
				
			for (p in 1:env_obj$npart) {

				for (k in 1:env_obj$nstates) {
				
					for (y in 1:env_obj$yobs_sharks[ ss ]) {
					
						lines(x=env_obj$MuY[[ ss ]][y,k,p] + c(-1, 1)*env_obj$loc_pred_plot_conf_constant*sqrt(env_obj$SigY[[ ss ]][y,k,p]), y=rep(p+(k-1)*0.5,2), type="l", col=k, lwd=0.1)
			
					}
								
				}		
			}
			
			r <- rownames(env_obj$ynext) == ss
			abline(v=env_obj$ynext[r,"X"], col=env_obj$ynext[r,"state.guess2"], lwd=2)
			if (env_obj$i > 1) { 
				points(x=env_obj$Xpart_history[env_obj$i-1,"X",,ss], y=1:env_obj$npart, pch=19, cex=0.1, col="green")
			}
		
		}

	}#end looping over ss

	#pl <- seq(env_obj$mu0_range[1], env_obj$mu0_range[2], 0.1)
	#alpha by state	
	q1 <- qnorm(p=0.01, mean=env_obj$mu[,"mu",,other_sharks], sd=sqrt(env_obj$sigma_draw[,,other_sharks]))
	q99 <- qnorm(p=0.99, mean=env_obj$mu[,"mu",,other_sharks], sd=sqrt(env_obj$sigma_draw[,,other_sharks]))

	
	pl <- c(seq(from=min(q1, na.rm=TRUE), to=max(q99, na.rm=TRUE), length.out=100), env_obj$mu[,"mu",,other_sharks])
	pl <- sort(pl[ ! is.na(pl) ])
	
	#axis(1, seq(env_obj$mu0_range[1], env_obj$mu0_range[2], by=1), at=seq(env_obj$mu0_range[1], env_obj$mu0_range[2], by=1))
	
	for (ss in other_sharks) {

		
		plot(x=pl, y=rep(-5, length(pl)), xlim=range(pl), ylim=c(0, min(1e10, max(dnorm(x=env_obj$mu[,"mu",,ss], mean=env_obj$mu[,"mu",,ss], sd=sqrt(env_obj$sigma_draw[,,ss])), na.rm=TRUE))),
		 xlab="velocity", ylab="density", main=paste("Density of velocity, shark", ss, "step=", env_obj$i), cex.main=0.8, las=1)
	
		for (p in 1:env_obj$npart) {
			for (k in 1:env_obj$nstates) {
			   lines(x=pl, y=dnorm(x=pl, mean=env_obj$mu[k, "mu", p, ss], sd=sqrt(env_obj$sigma_draw[p,k,ss])), lwd=.5, type="l", lty=1, col=k)
			}
		}
		abline(h=0, lwd=0.5)

		if (env_obj$nstates > 1) { 
			legend("topleft", col=1:2, lty=1, legend=paste("type", 1:2), cex=0.8)		
		}		
	}#end printing velocity
	invisible(NULL)
}
