DS.Finite.Bayes <- 
function(DS.GF.obj, y.0, n.0 = NULL, cred.interval = 0.90, iters = 25){
	#Converter Function
	LP.post.conv <- function(theta.set, DS.GF.obj, y.0, n.0 = NULL, e.0 = NULL){
		fam = DS.GF.obj$fam
		out <- list()
		lambda.i <- function(s.i, tau.2){s.i^2/(s.i^2+tau.2)}
		switch(fam,
			"Normal" = {
				prior.type = "Normal"
				se.0 <- n.0
				post.mu.i <- lambda.i(se.0, DS.GF.obj$g.par[2]) * DS.GF.obj$g.par[1] +
					(1-lambda.i(se.0, DS.GF.obj$g.par[2]))* y.0
				post.tau2.i <- (1-lambda.i(se.0, DS.GF.obj$g.par[2]))*se.0^2 #output is VARIANCE
				PEB.pos.den <- dnorm(theta.set, post.mu.i, sd = sqrt(post.tau2.i))
				if(sum(DS.GF.obj$LP.par^2)==0){
				post.fit <- data.frame(theta.vals = theta.set, parm.pos = PEB.pos.den)
					} else {
					unit.grid <- pnorm(theta.set, DS.GF.obj$g.par[1], sd = sqrt(DS.GF.obj$g.par[2]))
					wght.den <- weight.fun.univ(unit.grid, 
						DS.GF.obj$g.par[1], DS.GF.obj$g.par[2],
						post.mu.i, post.tau2.i, family = fam) # in terms of u
					if(DS.GF.obj$LP.type == "L2"){
						d.u <- 1 + gLP.basis(unit.grid,c(1,1), DS.GF.obj$m.val, 
								con.prior = "Beta")%*%DS.GF.obj$LP.par
						} else {
						d.u <- exp(cbind(1,gLP.basis(unit.grid, c(1, 1), DS.GF.obj$m.val, con.prior = "Beta")) %*% 
								DS.GF.obj$LP.par)
						}
					denom <- sintegral(unit.grid, d.u*wght.den)$int # in terms of u
					post.fit <- data.frame(theta.vals = theta.set,
									   parm.pos = PEB.pos.den,
								       ds.pos = PEB.pos.den * (d.u/denom))
				}
			return(post.fit)
		 },
		 "Binomial" = {
			prior.type = "Beta"
			post.alph.i <- y.0 + DS.GF.obj$g.par[1]
			post.beta.i <- n.0 - y.0 + DS.GF.obj$g.par[2]
			PEB.pos.den <- dbeta(theta.set,post.alph.i, post.beta.i) #in terms of theta
			if(sum(DS.GF.obj$LP.par^2)==0){
				post.fit <- data.frame(theta.vals = theta.set,
										   parm.pos = PEB.pos.den)
				} else {
					unit.grid <- pbeta(theta.set, DS.GF.obj$g.par[1], DS.GF.obj$g.par[2])
					wght.den <- weight.fun.univ(unit.grid, 
									DS.GF.obj$g.par[1], DS.GF.obj$g.par[2],
									post.alph.i, post.beta.i, family = fam) # in terms of u
					if(DS.GF.obj$LP.type == "L2"){
					d.u <- 1 + gLP.basis(unit.grid, c(1,1), DS.GF.obj$m.val, 
										 con.prior = "Beta")%*%DS.GF.obj$LP.par
					} else {
					d.u <- exp(cbind(1,gLP.basis(unit.grid, c(1, 1), DS.GF.obj$m.val, con.prior = "Beta")) %*% 
								DS.GF.obj$LP.par)
					}
					denom <- sintegral(unit.grid, d.u*wght.den)$int # in terms of u
					post.fit <- data.frame(theta.vals = theta.set,
												parm.pos = PEB.pos.den,
												ds.pos = PEB.pos.den * (d.u/denom))
					}
			return(post.fit)
		 },
		 "Poisson" = {
			prior.type = "Gamma"
			if(is.null(e.0) == TRUE){
				post.alph.i <- y.0 + DS.GF.obj$g.par[1]
				post.beta.i <- DS.GF.obj$g.par[2]/(1+DS.GF.obj$g.par[2])
				} else {
				post.alph.i <- y.0 + DS.GF.obj$g.par[1]
				post.beta.i <- DS.GF.obj$g.par[2]/(1 + e.0*DS.GF.obj$g.par[2])
				}
			PEB.pos.den <- dgamma(theta.set,post.alph.i, scale = post.beta.i) #in terms of theta
			if(sum(DS.GF.obj$LP.par^2)==0){
				post.fit <- data.frame(theta.vals = theta.set,
												parm.pos = PEB.pos.den)
				} else {
				unit.grid <- pgamma(theta.set, DS.GF.obj$g.par[1], scale = DS.GF.obj$g.par[2])
				wght.den <- weight.fun.univ(unit.grid, 
									DS.GF.obj$g.par[1], DS.GF.obj$g.par[2],
									post.alph.i, post.beta.i, family = fam) # in terms of u
				if(DS.GF.obj$LP.type == "L2"){
					d.u <- 1 + gLP.basis(unit.grid, c(1,1), DS.GF.obj$m.val, 
									 con.prior = "Beta")%*%DS.GF.obj$LP.par
					} else {
					d.u <- exp(cbind(1,gLP.basis(unit.grid, c(1, 1), DS.GF.obj$m.val, con.prior = "Beta")) %*% 
								DS.GF.obj$LP.par)
					}
				denom <- sintegral(unit.grid, d.u*wght.den)$int # in terms of u
				post.fit <- data.frame(theta.vals = theta.set,
												parm.pos = PEB.pos.den,
												ds.pos = PEB.pos.den * (d.u/denom))
				}
			return(post.fit)
			}
		)
	}
####
	out <- list()
	fam = DS.GF.obj$fam
	LP.type = DS.GF.obj$LP.type
	DS.objt.list <- list()
	#DS.post.list <- list()
	switch(fam,
		"Normal" = {
		out$study <- c(y.0, n.0)
		if(LP.type == "MaxEnt"){
			#MaxEnt
			for(i in 1:iters){
				L2.norm.thres <- 1.1*sqrt(sum((DS.GF.obj$LP.max.smt[1:DS.GF.obj$m.val]^2)))
				L2.norm <- L2.norm.thres + 1
				while(L2.norm > L2.norm.thres){
					par.g <- c(NA, NA)
					while (!is.finite(par.g[1]) == TRUE | !is.finite(par.g[2]) == TRUE) {
						new.df <- rPPD.ds(DS.GF.obj, 1, pred.type = "prior")$first.set
						possibleError <- tryCatch(par.g <- gMLE.nn(new.df$y,new.df$se)$estimate, error = function(e) e)
						er.check <- !inherits(possibleError, "error")
						if (er.check == TRUE) {
							par.g <- gMLE.nn(new.df$y, new.df$se)$estimate
						} else {
							par.g <- c(NA, NA)
						}	
					}
				new.ds.me <- DS.prior(new.df, max.m = DS.GF.obj$m.val, g.par = par.g, family = "Normal", LP.type = "MaxEnt")
				L2.norm <- sqrt(sum((new.ds.me$LP.par)^2))
				}
			DS.objt.list[[i]] <- new.ds.me
			#DS.post.list[[i]] <- DS.micro.inf.nnu(new.ds.me, y.0 = y.0, se.0 = n.0)
			}
			} else {
			#L2
			for(i in 1:iters){
				par.g <- c(NA,NA)
				while(!is.finite(par.g[1])==TRUE | !is.finite(par.g[2])==TRUE){
					new.df <- rPPD.ds(DS.GF.obj,1, pred.type = "prior")$first.set
					possibleError <- tryCatch(par.g <- gMLE.nn(new.df$y, new.df$se)$estimate,
											  error = function(e) e )
					er.check <- !inherits(possibleError, "error")
					if(er.check == TRUE){
						par.g <- gMLE.nn(new.df$y, new.df$se)$estimate
						} else {
						par.g <- c(NA, NA)
					}
				}				
				new.ds.L2 <- DS.prior.nnu(new.df, max.m = DS.GF.obj$m.val, start.par = par.g)
				DS.objt.list[[i]] <- new.ds.L2
				#DS.post.list[[i]] <-DS.micro.inf.nnu(new.ds.L2, y.0 = y.0, se.0 = n.0)
				}
			}
		},
		"Binomial" = {
		out$study <- c(y.0, n.0)
		if(LP.type == "MaxEnt"){
			#MaxEnt
			for(i in 1:iters){
				L2.norm.thres <- 1.1*sqrt(sum((DS.GF.obj$LP.max.smt[1:DS.GF.obj$m.val]^2)))
				L2.norm <- L2.norm.thres + 1
				while(L2.norm > L2.norm.thres){
					par.g <- c(NA, NA)
					while (!is.finite(par.g[1]) == TRUE | !is.finite(par.g[2]) == TRUE) {
						new.df <- rPPD.ds(DS.GF.obj, 1, pred.type = "prior")$first.set
						possibleError <- tryCatch(par.g <- gMLE.bb(new.df$y,new.df$n)$estimate, error = function(e) e)
						er.check <- !inherits(possibleError, "error")
						if (er.check == TRUE) {
							par.g <- gMLE.bb(new.df$y, new.df$n)$estimate
						} else {
							par.g <- c(NA, NA)
						}	
					}
				new.ds.me <- DS.prior(new.df, max.m = DS.GF.obj$m.val, g.par = par.g, family = "Binomial", LP.type = "MaxEnt")
				L2.norm <- sqrt(sum((new.ds.me$LP.par)^2))
				}
			DS.objt.list[[i]] <- new.ds.me
			#DS.post.list[[i]] <- DS.micro.inf(new.ds.me, y.0 = y.0, n.0 = n.0)
			}
			} else {
			#L2
			for(i in 1:iters){
				par.g <- c(NA,NA)
				while(!is.finite(par.g[1])==TRUE | !is.finite(par.g[2])==TRUE){
					new.df <- rPPD.ds(DS.GF.obj,1, pred.type = "prior")$first.set
					possibleError <- tryCatch(par.g <- gMLE.bb(new.df$y, new.df$n)$estimate,
											  error = function(e) e )
					er.check <- !inherits(possibleError, "error")
					if(er.check == TRUE){
						par.g <- gMLE.bb(new.df$y, new.df$n)$estimate
						} else {
						par.g <- c(NA, NA)
					}
				}				
				new.ds.L2 <- DS.prior.bbu(new.df, max.m = DS.GF.obj$m.val, start.par = par.g)
				DS.objt.list[[i]] <- new.ds.L2
				#DS.post.list[[i]] <-DS.micro.inf(new.ds.L2, y.0 = y.0, n.0 = n.0)
				}
			}
		
		},
		"Poisson" = {
		out$study <- y.0
			if(LP.type == "MaxEnt"){
			#MaxEnt
				for(i in 1:iters){
					L2.norm.thres <- 1.1*sqrt(sum((DS.GF.obj$LP.max.smt[1:DS.GF.obj$m.val]^2)))
					L2.norm <- L2.norm.thres + 1
					while(L2.norm > L2.norm.thres){
						par.g <- c(NA, NA)
						while (!is.finite(par.g[1]) == TRUE | !is.finite(par.g[2]) == TRUE) {
							new.y <- rPPD.ds(DS.GF.obj, 1, pred.type = "prior")$first.set
							possibleError <- tryCatch(par.g <- gMLE.pg(new.y, start.par = DS.GF.obj$g.par), error = function(e) e)
						er.check <- !inherits(possibleError, "error")
						if (er.check == TRUE) {
							par.g <- gMLE.pg(new.y, start.par = DS.GF.obj$g.par)
						} else {
							par.g <- c(NA, NA)
							}	
						}
					new.ds.me <- DS.prior(new.y, max.m = DS.GF.obj$m.val, g.par = par.g, family = "Poisson", LP.type = "MaxEnt")
					L2.norm <- sqrt(sum((new.ds.me$LP.par)^2))
					}
				DS.objt.list[[i]] <- new.ds.me
				#DS.post.list[[i]] <- DS.micro.inf(new.ds.me, y.0 = y.0, n.0 = NULL)
				}
			} else {
			#L2
			for(i in 1:iters){
				par.g <- c(NA,NA)
				while(!is.finite(par.g[1])==TRUE | !is.finite(par.g[2])==TRUE){
					new.y <- rPPD.ds(DS.GF.obj,1, pred.type = "prior")$first.set
					possibleError <- tryCatch(par.g <- gMLE.pg(new.y, start.par = DS.GF.obj$g.par),
											  error = function(e) e )
					er.check <- !inherits(possibleError, "error")
					if(er.check == TRUE){
						par.g <- gMLE.pg(new.y, start.par = DS.GF.obj$g.par)
						} else {
						par.g <- c(NA, NA)
					}
				}				
				new.ds.L2 <- DS.prior.pgu(new.y, max.m = DS.GF.obj$m.val, start.par = par.g)
				DS.objt.list[[i]] <- new.ds.L2
				#DS.post.list[[i]] <-DS.micro.inf(new.ds.L2, y.0 = y.0, n.0 = NULL)
				}
			}
		
		}
	)
	#build finite prior
	finite.prior <- matrix(0, nrow = length(DS.objt.list), ncol = length(DS.GF.obj$prior.fit$theta.vals))
	for(i in 1:length(DS.objt.list)){
		if(sum(DS.objt.list[[i]]$LP.par^2) == 0){
			finite.prior[i,] <- DS.objt.list[[i]]$prior.fit$parm.prior
			} else {
			finite.prior[i,] <- DS.objt.list[[i]]$prior.fit$ds.prior
			}
		}
	if(sum(DS.GF.obj$LP.par^2) == 0){
			out$prior.fit <- data.frame(theta.vals = DS.GF.obj$prior.fit$theta.vals,
									parm.prior = DS.GF.obj$prior.fit$parm.prior,
									finite.prior = colMeans(finite.prior)
									)
			} else {
			out$prior.fit <- data.frame(theta.vals = DS.GF.obj$prior.fit$theta.vals,
									parm.prior = DS.GF.obj$prior.fit$parm.prior,
									ds.prior = DS.GF.obj$prior.fit$ds.prior,
									finite.prior = colMeans(finite.prior)
									)
			}
		#build posterior
	post.base <- DS.micro.inf(DS.GF.obj, y.0= y.0, n.0 = n.0)
	theta.post <- post.base$post.fit$theta.vals
	test.post <- lapply(DS.objt.list, function(x) LP.post.conv(theta.post, x, y.0 = y.0, n.0 = n.0) )
	finite.post <- matrix(0, nrow = length(DS.objt.list), ncol = length(post.base$post.fit$theta.vals))
	for(i in 1:length(DS.objt.list)){
		if(dim(test.post[[i]])[2] == 2){
			finite.post[i,] <- test.post[[i]]$parm.pos
			} else {
			finite.post[i,] <- test.post[[i]]$ds.pos
			}
		}
	finite.post[which(finite.post < 0)] <- 0.00001
	if(dim(post.base$post.fit)[2] == 2){
			out$post.fit <- data.frame(theta.vals = post.base$post.fit$theta.vals,
							   parm.post = post.base$post.fit$parm.pos,
							   finite.post =  colMeans(finite.post))
			} else {
			out$post.fit <- data.frame(theta.vals = post.base$post.fit$theta.vals,
							   parm.post = post.base$post.fit$parm.pos,
							   ds.post = post.base$post.fit$ds.pos,
							   finite.post =  colMeans(finite.post))
			}
	out$post.vec <- c(post.base$post.vec[1],
					  DS_MN = sintegral(out$post.fit$theta.vals, out$post.fit$theta.vals*out$post.fit$finite.post)$int,
					  post.base$post.vec[3],
					  DS_MD = out$post.fit$theta.vals[which.max(out$post.fit$finite.post)])
					  
	#credible interval
	dens.vec <- NULL
	for(i in 2:length(post.base$post.fit$theta.vals)){
		dens.vec[i] <- sintegral(post.base$post.fit$theta.vals[1:i],
								 out$post.fit$finite.post[1:i])$int
		}
	top.CI <- which.min(abs( dens.vec - (cred.interval + (1-cred.interval)/2) ))+1
	bot.CI <- which.min(abs( dens.vec - (1-cred.interval)/2))
	out$interval <- post.base$post.fit$theta.vals[c(bot.CI, top.CI)]
	class(out) <- "DS_FB_obj"
	return(out)
}