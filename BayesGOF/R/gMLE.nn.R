gMLE.nn <- 
function(value, se, fixed = FALSE, method = c("DL","SJ","REML","MoM")){
		### Univariate meta-analysis combiner function 
		### Input is 2 vectors of length k (where k is number of partitions)  
		### 	1) value is the vector of predicted values for a specified test value
		### 	2) se is the standard error for the predicted values in value
		### 	3) fixed == FALSE  Treat it as a random effects model; if TRUE, fixed effects
		###		4) method = "DL" Uses DerSimonian and Lard for tausq; REML ; BROWN
		### Output is the estimated population parameter and the between-group variance (tao sq)
		### Verified equations with 20SEP16 with Marin-Martinez/Sanchez-Mecca(2010) and Higgens et al (2009)
		method <- match.arg(method)
		out <- list()
		nn1 <- length(value)
		var.v <- se^2
		fe.weight <- 1/var.v
		pop.est.fe <- (fe.weight %*% value) / sum( fe.weight )
		c.value <- sum(fe.weight) - (sum (fe.weight^2 ) / sum(fe.weight) )
		Q.value <- sum(fe.weight * (value - as.vector(pop.est.fe))^2 )
		tau.sq.DL <- max(0,( Q.value - (length(value) - 1) ) / c.value )
		if(fixed == TRUE){
			out$mu.hat <- (fe.weight %*% value) / sum (fe.weight)
			} else {
		switch(method, 
			"DL" = { out$tau.sq <- tau.sq.DL
				   hv.weight <- 1/ (out$tau.sq + var.v)
				   out$mu.hat <- (hv.weight %*% value) / sum (hv.weight)
				   out$estimate <- c(out$mu.hat, out$tau.sq)
				   out$method <- method
			},
			"SJ" = { tau.not <- mean( (value - mean(value))^2)
					r.hat.i <- var.v / tau.not
					v.hat.i <- r.hat.i + 1
					theta.hat.v.hat <- sum(value/v.hat.i)/ sum(1/v.hat.i)
					out$tau.sq <- sum((value - theta.hat.v.hat)^2/v.hat.i)/(nn1 - 1)
					hv.weight <- 1/ (out$tau.sq + var.v)
				   out$mu.hat <- (hv.weight %*% value) / sum (hv.weight)
				   out$estimate <- c(out$mu.hat, out$tau.sq)
				   out$method <- method
			},
			"REML" = {
				nloop = 0
				absch = 1 # absolute change in tauR2 value
				while ( absch > 10^(-5) ) {
					nloop = nloop + 1
						if ( nloop > 10^5 ) {
							stop("tauREML2 via REML method does not converge.")
						} else {
						tauR2O <- tau.sq.DL # tauR2Old
						# update thetaR, wR
						wR <- 1/(var.v + tauR2O)
						thetaR <- sum(wR*value) / sum(wR)
						# update tauR
						tau.sq.DL <- sum(wR^2*(nn1/(nn1-1)*(value-thetaR)^2 - var.v)) / sum(wR^2)
						absch <- abs(tau.sq.DL - tauR2O)
						}
					}
				# return
				out$tau.sq <- max(0, tau.sq.DL)
				hv.weight <- 1/ (out$tau.sq + var.v)
				out$mu.hat <- (hv.weight %*% value) / sum (hv.weight)
				out$estimate <- c(out$mu.hat, out$tau.sq)
				out$method <- "REML"
				},
			"MoM" = {
				hv.weight <- 1/ (tau.sq.DL + var.v)
				mu.brn <- (hv.weight %*% value) / sum (hv.weight)
				tau.sq.brn <- (sum((value-mu.brn)^2)-((length(value)-1)/length(value))*sum(var.v))/(length(value)-1)
				hv.2 <- 1/(tau.sq.brn + var.v)
				out$mu.hat <- (hv.2%*%value)/sum(hv.2)
				out$tau.sq <- max(0,(sum((value-out$mu.hat)^2)-((length(value)-1)/length(value))*sum(var.v))/(length(value)-1))
				out$estimate <- c(out$mu.hat, out$tau.sq)
				out$method <- "MoM"
			})
		}		
		return(out)
		}