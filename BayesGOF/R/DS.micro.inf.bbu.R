DS.micro.inf.bbu <-
function(DS.GF.obj, y.0, n.0){
	fam <- "Binomial"
	out <- list()
	##### PEB Posterior and Family Calculations
	post.alph.i <- y.0 + DS.GF.obj$g.par[1]
	post.beta.i <- n.0 - y.0 + DS.GF.obj$g.par[2]
	###Set up conversion based on d(u)
	theta.conv <- qbeta(DS.GF.obj$UF.data$UF.x, DS.GF.obj$g.par[1], DS.GF.obj$g.par[2])
	PEB.pos.den <- dbeta(theta.conv,post.alph.i, post.beta.i) #in terms of theta
	out$PEB.mode <- theta.conv[which.max(PEB.pos.den)]
	out$PEB.mean <- post.alph.i /( post.alph.i + post.beta.i)
	if(sum(DS.GF.obj$LP.par^2)==0){
		out$DS.mean <- out$PEB.mean
		out$DS.mode <- out$PEB.mode
		out$post.vec <- c(out$PEB.mean, out$DS.mean, out$PEB.mode, out$DS.mode)
		out$post.fit <- data.frame(theta.vals = theta.conv,
							   parm.pos = PEB.pos.den)
		out$study <- c(y.0, n.0)
		} else {
		wght.den <- weight.fun.univ(DS.GF.obj$UF.data$UF.x, 
					DS.GF.obj$g.par[1], DS.GF.obj$g.par[2],
					post.alph.i, post.beta.i, family = fam) # in terms of u
		##### LP posterior calculations
		denom <- sintegral(DS.GF.obj$UF.data$UF.x,
				DS.GF.obj$UF.data$UF.y*wght.den)$int # in terms of u
		LP.pos.den <- PEB.pos.den * (DS.GF.obj$UF.data$UF.y/denom) #in terms of theta
		out$post.fit <- data.frame(theta.vals = theta.conv,
									parm.pos = PEB.pos.den,
									ds.pos = LP.pos.den)
		out$DS.mean <- sintegral(DS.GF.obj$UF.data$UF.x,
				   theta.conv*DS.GF.obj$UF.data$UF.y*wght.den)$int / denom #in terms of u
		out$DS.mode <- out$post.fit$theta.vals[which.max(out$post.fit$ds.pos)]
		out$post.vec <- c(out$PEB.mean, out$DS.mean, out$PEB.mode, out$DS.mode)
		out$study <- c(y.0, n.0)
	}
	class(out) <- "DS_GF_micro"
	names(out$post.vec) <- c("PEB_MN", "DS_MN", "PEB_MD", "DS_MD")
	names(out$PEB.mean) <- NULL; names(out$PEB.mode) <- NULL; names(out$DS.mean) <- NULL; names(out$DS.mode) <- NULL
	return(out)
}