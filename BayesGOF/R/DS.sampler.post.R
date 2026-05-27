DS.sampler.post <-
function(k, g.par, LP.par, y.0, n.0 = NULL, con.prior = c("Normal", "Beta", "Gamma"), 
			LP.type = c("L2","MaxEnt"), B = 250){
	fam = match.arg(con.prior)
	LPt = match.arg(LP.type)
	switch(fam,
		"Normal" = {
			lambda.i <- function(s.i, tau.2){s.i^2/(s.i^2+tau.2)}
			y.i <- y.0
			se.i <- n.0
			if(LPt == "L2"){
				m <- length(LP.par)
				post.mu.i <- lambda.i(se.i, g.par[2]) * g.par[1] +
					(1-lambda.i(se.i, g.par[2]))* y.i
				post.tau2.i <- (1-lambda.i(se.i, g.par[2]))*se.i^2 #output is VARIANCE
				x.samp <- NULL
				for(i in 1:k){
					DG.val <- 0
					DU.val <- 1
					unit.vec <- seq(1/B, 1-1/B, length = B)
					#d.u <- 1+LP.basis.beta(unit.vec, c(1,1), m)%*%LP.par 
					d.u <- 1 + gLP.basis(unit.vec, c(1,1), m, con.prior = "Beta")%*%LP.par 
					d.max <- max(d.u)
					while(DG.val < DU.val){
						y <- rnorm(1, post.mu.i, sd = sqrt(post.tau2.i)) 
						#Leg.G <- LP.basis.norm(y, g.par, m)
						Leg.G <- gLP.basis(y, g.par, m, con.prior = "Normal")
						DG.val <- 1+Leg.G%*%LP.par 
						DU.val <- runif(1) * d.max 
					}
				x.samp[i] <- y
				}
			return(x.samp)
			} else {
				m <- length(LP.par)-1
				post.mu.i <- lambda.i(se.i, g.par[2]) * g.par[1] +
					(1-lambda.i(se.i, g.par[2]))* y.i
				post.tau2.i <- (1-lambda.i(se.i, g.par[2]))*se.i^2 #output is VARIANCE
				x.samp <- NULL
				for(i in 1:k){
					DG.val <- 0
					DU.val <- 1
					unit.vec <- seq(1/B, 1-1/B, length = B)
					#d.u <- exp(cbind(1,LP.basis.beta(unit.vec, c(1,1), m))%*%LP.par.ME) 
					d.u <- exp(cbind(1,gLP.basis(unit.vec, c(1,1), m, con.prior = "Beta"))%*%LP.par)
					d.max <- max(d.u)
					while(DG.val < DU.val){
						y <- rnorm(1, post.mu.i, sd = sqrt(post.tau2.i)) 
						#Leg.G <- LP.basis.norm(y, g.par, m)
						Leg.G <- gLP.basis(y, g.par, m, con.prior = "Normal")
						DG.val <- exp(cbind(1,Leg.G)%*%LP.par) 
						DU.val <- runif(1) * d.max 
					}
					x.samp[i] <- y
				}
				return(x.samp)
			}
		 },
		 "Beta" = {
			y.i <- y.0
			n.i <- n.0
			if(LPt == "L2"){
				m <- length(LP.par)
				post.alpha <- g.par[1] + y.i
				post.beta <- n.i - y.i + g.par[2]
				x.samp <- NULL
				cd.samp <- NULL
				for(i in 1:k){
					DG.val <- 0
					DU.val <- 1
					unit.vec <- seq(1/B, 1-1/B, length = B)
					#d.u <- 1+LP.basis.beta(unit.vec, c(1,1), m)%*%LP.par 
					d.u <- 1 + gLP.basis(unit.vec, c(1,1), m, con.prior = "Beta")%*%LP.par
					d.max <- max(d.u)
					while(DG.val < DU.val){
						y <- rbeta(1,post.alpha,post.beta) 
						#Leg.G <- LP.basis.beta(y, g.par, m)
						Leg.G <- gLP.basis(y, g.par, m, con.prior = "Beta")
						DG.val <- 1+Leg.G%*%LP.par 
						DU.val <- runif(1) * d.max 
					}
					x.samp[i] <- y
				}
				return(x.samp)
			} else {
				m <- length(LP.par)-1
				post.alpha <- g.par[1] + y.i
				post.beta <- n.i - y.i + g.par[2]
				x.samp <- NULL
				cd.samp <- NULL
				for(i in 1:k){
					DG.val <- 0
					DU.val <- 1
					unit.vec <- seq(1/B, 1-1/B, length = B)
					#d.u <- exp(cbind(1,LP.basis.beta(unit.vec, c(1,1), m))%*%LP.par.ME) 
					d.u <- exp(cbind(1,gLP.basis(unit.vec, c(1,1), m, con.prior = "Beta"))%*%LP.par)
					d.max <- max(d.u)
					while(DG.val < DU.val){
						y <- rbeta(1,post.alpha,post.beta) 
						#Leg.G <- LP.basis.beta(y, g.par, m)
						Leg.G <- gLP.basis(y, g.par, m, con.prior = "Beta")
						DG.val <-  exp(cbind(1,Leg.G)%*%LP.par)  
						DU.val <- runif(1) * d.max 
						}
					x.samp[i] <- y
					}
				return(x.samp)
			}
		 },
		 "Gamma" = {
			if(LPt == "L2"){
				x.i <- y.0
				m <- length(LP.par)
				post.alpha <- g.par[1] + x.i
				post.beta <- g.par[2]/(1+g.par[2])
				x.samp <- NULL
				cd.samp <- NULL
				for(i in 1:k){
					DG.val <- 0
					DU.val <- 1
					unit.vec <- seq(1/B, 1-1/B, length = B)
					d.u <- 1 + gLP.basis(unit.vec, c(1,1), m, con.prior = "Beta")%*%LP.par
					#d.u <- 1+LP.basis.beta(unit.vec, c(1,1), m)%*%LP.par 
					d.max <- max(d.u)
					while(DG.val < DU.val){
						y <- rgamma(1,post.alpha, scale = post.beta) 
						#Leg.G <- LP.basis.gamma(y, g.par, m)
						Leg.G <- gLP.basis(y, g.par, m, con.prior = "Gamma")
						DG.val <- 1+Leg.G%*%LP.par 
						DU.val <- runif(1) * d.max 
						}
					x.samp[i] <- y
					}
				return(x.samp)
			} else {
				x.i <- y.0
				m <- length(LP.par)-1
				post.alpha <- g.par[1] + x.i
				post.beta <- g.par[2]/(1+g.par[2])
				x.samp <- NULL
				cd.samp <- NULL
				for(i in 1:k){
					DG.val <- 0
					DU.val <- 1
					unit.vec <- seq(1/B, 1-1/B, length = B)
					#d.u <- exp(cbind(1,LP.basis.beta(unit.vec, c(1,1), m))%*%LP.par.ME)
					d.u <- exp(cbind(1,gLP.basis(unit.vec, c(1,1), m, con.prior = "Beta"))%*%LP.par)					
					d.max <- max(d.u)
					while(DG.val < DU.val){
						y <- rgamma(1,post.alpha, scale = post.beta) 
						#Leg.G <- LP.basis.gamma(y, g.par, m)
						Leg.G <- gLP.basis(y, g.par, m, con.prior = "Gamma")
						DG.val <- exp(cbind(1,Leg.G)%*%LP.par) 
						DU.val <- runif(1) * d.max 
						}
					x.samp[i] <- y
					}
				return(x.samp)
			}
			 }
		)
	}