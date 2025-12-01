gen.inits.single = function(data, a.init=NA, b.init=NA, cstar.init=NA, 
	logc.limits = c(-8.9, -1.9), sigma.upper=NA, stdadd = FALSE, offset = 1, calibration.only = FALSE) {
###################################################################
# 				Initial values                            #
# 	generate initial values for describeISE and analyse         #
# 	for single (gen.inits.single) or                            #
#		multiple (gen.inits.multiple) ISEs                    #
###################################################################
	
###
# Generates initial values for a single ISE model
# Data should be formatted using load.ISE.data
# If there are convergence problems, initial values
# for the non-linear regression may be altered
# by specifying a.init, b.init, and cstar.init;
# sigma.upper places an upper bound on sigma.init;
# stdadd specifies basic model (F) or standard addition model (T)
# offset determines how many points to use to estimate the slope; 
#	offset = 1 uses the last two points,
#	offset = 2 uses the n-2 and nth point, etc.
###
	# Generate initial values for calibration data
	x <- 10^data$log10x
	order.x = order(x)
	x = sort(x)
	log10x = log10(x)
	emf <- data$emf[order.x]
	
	# Assignments
	n = length(x)
	b = (emf[n] - emf[n-offset])/(log10x[n] - log10x[n-offset])	
	a = emf[n] - b*log10x[n]
	C = min(max(10^logc.limits[1], 10^((emf[1]-a)/b) - x[1]), 10^logc.limits[2])
	cstar = 10^(log10(C)/10)

	# over-ride if initial values are provided
	if(!is.na(a.init)) { a = a.init  }
	if(!is.na(b.init)) { b = b.init  }
	if(!is.na(cstar.init)) { cstar = cstar.init  }

	Preds = a + b*log10(x + C)
	Resids = emf - Preds
	logsigma.temp = log(sqrt( sum(Resids^2)/(length(Resids) - 3) ))
	logsigma <- logsigma.temp
	sigma = exp(logsigma)
	# If sigma is too large, it may be out of bounds.
	# Use sigma.upper to place an upper limit on sigma
	if(!is.na(sigma.upper)) {
		sigma = min(sigma.upper, exp(logsigma))
	}

	sigma.sq = sigma^2
	tau = 1/sigma.sq

	# Calculate LOD using s/n = 3
  	mu0 <- a + b*log10(cstar^10)
  	mu0.3sig <- mu0 + 3*sigma
  	LOD <- 10^( (mu0.3sig - a)/b ) - cstar^10

	# Generate inital values for experimental data
	# Values with ISEs below S/N=3 are set to E(ISE|x=0) + 3*sigma
   if (!calibration.only)	{
	x.exp = rep(NA, data$M)
	if (stdadd != T) {
		emf.tmp <- data$emf.exp
		emf.tmp2 <- pmax(emf.tmp, mu0.3sig)
		x.tmp <- 10^( (emf.tmp2 - a)/b ) - cstar^10
	}
	if (stdadd == TRUE) {
		delta.emf.tmp = data$delta.emf
		V.add.tmp = data$V.add
		conc.add.tmp = data$conc.add
		V.s.tmp = data$V.s
		x.tmp = pmax(10^logc.limits[1],
			(V.add.tmp*conc.add.tmp/V.s.tmp)/(10^(delta.emf.tmp/b)))
	}
		
	# Initial values are between 90% and 100% of the average estimate
	x.exp = x.tmp*runif(1, 0.9, 1)
	log10x.exp = log10(x.exp)
	
	INITS <- list(a=a, b=b, cstar=cstar, logsigma=logsigma)
	init.stretch = 0.01
	inits <- list(a=a*runif(1, 1 - init.stretch, 1 + init.stretch),
				b=b*runif(1, 1 - init.stretch, 1 + init.stretch),
				cstar=cstar*runif(1, 1 - init.stretch, 1 + init.stretch),
				sigma=sigma*runif(1, 1 - init.stretch, 1 + init.stretch),
				log10x.exp=log10x.exp)
   }
   if (calibration.only) {
	INITS <- list(a=a, b=b, cstar=cstar, logsigma=logsigma)
	init.stretch = 0.01
	inits <- list(a=a*runif(1, 1 - init.stretch, 1 + init.stretch),
				b=b*runif(1, 1 - init.stretch, 1 + init.stretch),
				cstar=cstar*runif(1, 1 - init.stretch, 1 + init.stretch),
				sigma=sigma*runif(1, 1 - init.stretch, 1 + init.stretch))
   }
	return(inits)
}