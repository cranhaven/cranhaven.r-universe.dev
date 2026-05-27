maxent.obj.convert <- function(DS.obj){
	switch(DS.obj$fam,
		"Normal" = {prior.type = "Normal"},
		"Binomial" = {prior.type = "Beta"},
		"Poisson" = {prior.type = "Gamma"})
	DS.obj$LP.par <- maxent.LP.par(DS.obj$LP.par)
	u.grid <- DS.obj$UF.data$UF.x #test
	#u.grid <- seq(0, 1, length.out = length(DS.obj$prior.fit$theta.vals))
	Leg.mat <- gLP.basis(x =DS.obj$prior.fit$theta.vals, g.par = DS.obj$g.par, 
						 m = DS.obj$m.val, con.prior = prior.type)
	CDen.u <- exp(cbind(1,gLP.basis(u.grid, c(1, 1), DS.obj$m.val, con.prior = "Beta")) %*% 
					DS.obj$LP.par)
	CDen <- exp(cbind(1,Leg.mat) %*% DS.obj$LP.par)
	DS.sm <- DS.obj$prior.fit$parm.prior * CDen
	### Finalize output
	DS.obj$prior.fit <- data.frame(theta.vals = DS.obj$prior.fit$theta.vals, 
									parm.prior = DS.obj$prior.fit$parm.prior,
									ds.prior = DS.sm)
	DS.obj$UF.data <- data.frame(UF.x = u.grid, UF.y = CDen.u)
	return(DS.obj)
	}