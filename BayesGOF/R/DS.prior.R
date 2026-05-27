DS.prior <-
function(input, max.m = 8, g.par,  family = c("Normal","Binomial", "Poisson"),
			LP.type = c("L2","MaxEnt"), smooth.crit = "BIC", iters = 200, B = 1000, max.theta = NULL){
####iterates through given conditions to find c-vector
#### INPUTS		
#### yn.df 			dataframe with 1st column as predictions for X from each of k servers
#### 		   		and second column as number observations per y.i
#### g.par   	user-desired parameters for designated prior for g)
#### max.m			maximum order of legendre polynomials desired
#### iters			number of iterations desired for calculating LP coefficents
#### smooth.crit	Criteria for selecting optimal m and 
####				smoothing criteria for final c-vector; either BIC or AIC
#### family			Type of conjugate family: Normal-Normal, Binomial-Beta, Poisson-Gamma
#### OUTPUTS
#### $g.par			Starting parameter parameters for G
#### $LP.par		FINAL LP coefficients, smoothed and adjusted based on max deviance
#### $LPc.vec.smt	Smoothed vector of max.m c-values
#### $LPc.vec.uns	Unsmoothed vector of max.m c-values
#### $prior.fit    Information to plot both G and (if m >0) DS priors
#### $UF.data		Information to plot U-function
#### $obs.data		Original observed data
#### $cutoff		norm distance between old c.vec and new c.vec;
	fam = match.arg(family)
	meth = match.arg(LP.type)
	switch(fam,
		"Normal" = {
			DS.prior.nnu(yn.df = input, max.m = max.m , start.par = g.par, 
						 iter.c = iters, B = B, smooth.crit = smooth.crit,
						 LP.type = meth)
		 },
		 "Binomial" = {
			DS.prior.bbu(yn.df = input, max.m = max.m , start.par= g.par, 
						  iter.c = iters, B = B, smooth.crit = smooth.crit,
						  LP.type = meth)
		 },
		 "Poisson" = {
			DS.prior.pgu(vec.counts = input, max.m = max.m , start.par= g.par, 
						 iter.c = iters, B = B, smooth.crit = smooth.crit,
						 LP.type = meth, max.theta = max.theta)
				}
			 
		)
	}