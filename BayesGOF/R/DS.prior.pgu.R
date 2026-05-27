DS.prior.pgu <- 
function(vec.counts, max.m = 8, start.par, iter.c = 200,
						B = 1000, smooth.crit = "BIC", 
						LP.type = c("L2","MaxEnt"), max.theta = NULL){
			LP.type <- match.arg(LP.type)
			fam <- "Poisson"
			out <- list()
			#Format the input for efficiency
			k <- length(vec.counts)
			tbl <- table(vec.counts)
			cnt.vec <- as.integer(unlist(dimnames(tbl)))
			freq.vec <- as.vector(tbl)
			max.cnt <- ifelse(is.null(max.theta)==TRUE, max(cnt.vec),max.theta)
			#PEB g
			if(dgamma(0, start.par[1], scale = start.par[2]) != Inf){
				theta.vals <- seq(0,max.cnt, length.out = B)
				}else{
				theta.vals <- seq(1/B,max.cnt, length.out = B)
				}
			PEB.g <- dgamma(theta.vals, start.par[1], scale = start.par[2]) #g instead of d
			u.grid <- pgamma(theta.vals, start.par[1], scale = start.par[2])
			#u.grid <- seq(1/B,1-(1/B), length.out = B)
			#Check for m = 0
			if(max.m == 0){
				out$g.par <- start.par
				out$LP.par <- 0
				out$prior.fit <- data.frame(theta.vals = theta.vals,
									   parm.prior = PEB.g)
				out$UF.data <- data.frame(UF.x = u.grid, UF.y = rep(1,length(u.grid)))
				out$obs.data <- vec.counts
				out$dev.df <- data.frame(m = 0, dev = 0)
				out$LP.type <- "L2"
				out$fam <- fam
				class(out) <- "DS_GF"
				return(out)
				}
			#Initialize for LP means
			c.vec <- NULL
			dev.m <- NULL
			B.loop <- 150
			u.loop <- seq(1/B.loop,1-(1/B.loop), length.out = B.loop) ##unit interval, 0 to 1
			## Generate Posterior Values for Weight Function
			post.alpha.i <- start.par[1] + cnt.vec
			post.beta.i <- rep(start.par[2]/(1+start.par[2]),length(cnt.vec))
			post.mat <- data.frame(al = post.alpha.i, be = post.beta.i)
			## Generate Weight Function Matrix for Conditional Expectation
			wght.loop <- apply(post.mat, 1, function(x) weight.fun.univ(u.loop, start.par[1], start.par[2], x[1], x[2], family = fam))
			###Determine LP coefficients
			if(smooth.crit == "BIC"){
				out$sm.crit <- "BIC"
				for(j in 1:max.m){
					c.vec[j]<-0 #Force jth entry to be zero
					cutoff.c <- 0 ###uses cutoff in base file
					leg.mat.j <- gLP.basis(u.loop, c(1,1), length(c.vec), con.prior = "Beta")
					for(i in 1:iter.c){ ##uses just iter in base file
						c.L.new <- Reduce.LP.coef.univ(wght.mat=wght.loop, c.vec, leg.mat.j, freq.vec = freq.vec, k.tot = k)
						cutoff.c[i+1] <- sqrt(sum((c.L.new - c.vec)^2))
						c.vec[j] <- c.L.new[j] #replaces old jth entry with new jth entry
					if (cutoff.c[i+1] < 0.006 | abs(cutoff.c[i+1] - cutoff.c[(i)]) < 0.000006 ) {break}
					}
					dev.m[j] <- max(0,sum(c.vec^2)-(log(k))*(j/k))
					}
				} else {
				out$sm.crit <- "AIC"
				for(j in 1:max.m){
					c.vec[j]<-0 #Force jth entry to be zero
					cutoff.c <- 0 ###uses cutoff in base file
					leg.mat.j <- gLP.basis(u.loop, c(1,1), length(c.vec), con.prior = "Beta")
					for(i in 1:iter.c){ ##uses just iter in base file
						c.L.new <- Reduce.LP.coef.univ(wght.mat=wght.loop, c.vec, leg.mat.j, freq.vec = freq.vec, k.tot = k)
						cutoff.c[i+1] <- sqrt(sum((c.L.new - c.vec)^2))
						c.vec[j] <- c.L.new[j] #replaces old jth entry with new jth entry
						if (cutoff.c[i+1] < 0.006 | abs(cutoff.c[i+1] - cutoff.c[(i)]) < 0.000006 ) {break}
						}
					dev.m[j] <- max(0,sum(c.vec^2) - (2 * (j/k)))
					}
				}
			###Determine best m
			m.vec <- c(1:max.m)
			if(sum(dev.m)==0){
				out$m.val <- 0
				out$g.par <- start.par
				names(out$g.par) <- c("alpha","beta")
				out$LP.par <- 0
				out$LP.max.uns <- c.vec
				out$LP.max.smt <- LP.smooth(c.vec,k, method = smooth.crit)
				out$prior.fit <- data.frame(theta.vals = theta.vals,
									   parm.prior = PEB.g)
				out$UF.data <- data.frame(UF.x = u.grid, UF.y = rep(1,length(u.grid)))
				out$obs.data <- vec.counts
				out$dev.df <- data.frame(  m = m.vec, dev = dev.m)
				out$LP.type <- "L2"
				out$fam <- fam
				class(out) <- "DS_GF"
				return(out)
				}else{
				out$m.val <- m.vec[which.max(dev.m)]
				out$LP.max.smt <- LP.smooth(c.vec,k, method = smooth.crit)
				out$LP.par <- out$LP.max.smt[1:out$m.val]
				out$LP.max.uns <- c.vec 
				out$dev.df <- data.frame(  m = m.vec, dev = dev.m)
				out$g.par <- c(start.par[1], start.par[2])
				names(out$g.par) <- c("alpha","beta")
				names(out$LP.par) <- paste("LP",1:length(out$LP.par),sep = "")
				##### Generate data frame of values to plot both DS and PEB priors, Ufunction
				d.u <- 1 + gLP.basis(u.grid,c(1,1),out$m.val, con.prior = "Beta")%*%out$LP.par
				Leg.mat <- gLP.basis(theta.vals, start.par, out$m.val, con.prior = "Gamma")
				d.G <- 1+Leg.mat%*%out$LP.par
				DS.sm <- PEB.g*d.G
				DS.sm[DS.sm<0]<-0.0000000001
				d.u[d.u<0] <- 0.0000000001
				area <- sintegral(u.grid, d.u)$int
				out$prior.fit <- data.frame(theta.vals = theta.vals,
								   parm.prior = PEB.g,
								   ds.prior = DS.sm/area)
				out$UF.data <- data.frame(UF.x = u.grid, UF.y = d.u)
				out$obs.data <- vec.counts
				out$fam <- fam
				if(LP.type == "L2"){
					out$LP.type <- "L2"
					} else {
					out <- maxent.obj.convert(out)
					out$LP.type <- "MaxEnt"
					}
				class(out) <- "DS_GF"
				return(out)
				}
			}