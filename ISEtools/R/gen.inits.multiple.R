gen.inits.multiple <- function(data, a.init= NA, b.init= NA, cstar.init= NA, offset = 1,
	logc.limits = c(-8.9, -1.9), sigma.upper = NA, stdadd = FALSE, calibration.only=FALSE) {
###
# Similar to gen.inits.single, but for multiple ISEs
# If initial values are specified, the should be vectors with length equal to the number of ISEs
###
	# Initialise vectors
	a <- rep(NA, data$R)
	b <- rep(NA, data$R)
	cstar <- rep(NA, data$R)
	logsigma <- rep(NA, data$R)
	sigma <- rep(NA, data$R)
	sigma.sq <- rep(NA, data$R)
	tau <- rep(NA, data$R)

	# Generate initial values for calibration data
	for (i in 1:data$R) {
		
		x <- 10^data$log10x[data$ISEID==i]
		order.x = order(x) 
		x = sort(x)  
		log10x = log10(x)
		emf <- data$emf[data$ISEID==i][order.x] 

		# Assignments
		n = length(x)
		b[i] = (emf[n] - emf[n-offset])/(log10x[n] - log10x[n-offset])	
		a[i] = emf[n] - b[i]*log10x[n]
		C = min(max(10^logc.limits[1], 10^((emf[1]-a[i])/b[i]) - x[1]), 10^logc.limits[2])
		cstar[i] = 10^(log10(C)/10)

		# over-ride if initial values are provided
		if(!is.na(a.init[1])) { a[i] = a.init[i]  }
		if(!is.na(b.init[1])) { b[i] = b.init[i]  }
		if(!is.na(cstar.init[1])) { cstar[i] = cstar.init[i]  }

		Preds = a[i] + b[i]*log10(x + C)
		Resids = emf - Preds
		logsigma.temp = log(sqrt( sum(Resids^2)/(length(Resids) - 3) ))

		logsigma[i] <- logsigma.temp
		sigma[i] = exp(logsigma[i])

		# If sigma is too large, it may be out of bounds.
		# Use sigma.upper to place an upper limit on sigma
		if(!is.na(sigma.upper)) {
			sigma[i] = min(sigma.upper, exp(logsigma[i]))
		}
		sigma.sq[i] = sigma[i]^2
		tau[i] = 1/sigma.sq[i]
	}

	# Calculate LOD using s/n = 3
  	mu0 <- a + b*log10(cstar^10)
  	mu0.3sig <- mu0 + 3*sigma*sign(b)
  	LOD <- 10^( (mu0.3sig - a)/b ) - cstar^10

   if (!calibration.only) {
	# Generate inital values for experimental data
	# Values with ISEs below S/N=3 are set to E(ISE|x=0) + 3*sigma
	x.exp = rep(NA, data$M)

	if (stdadd != TRUE) {
		for (i in 1:data$M) {
			emf.tmp <- data$emf.exp[data$xID.exp==i]
			ISEID.tmp <- data$ISEID.exp[data$xID.exp==i]
			emf.tmp2 <- pmax(emf.tmp, mu0.3sig[ISEID.tmp])
			x.tmp <- 10^( (emf.tmp2 - 
				a[ISEID.tmp])/b[ISEID.tmp] ) - 
				cstar[ISEID.tmp]^10
			# Initial values are between 90% and 100% of the average estimate
			# but at least equal to the lower limit for log c
			x.exp[i] = max(10^logc.limits[1], mean(x.tmp))*runif(1, 0.9, 1)
		}
	}

	if (stdadd == TRUE) {
		for (i in 1:data$M) {
			delta.emf.tmp = data$delta.emf[data$xID.exp==i]
			ISEID.tmp <- data$ISEID.exp[data$xID.exp==i]
			V.add.tmp = data$V.add[data$xID.exp==i]
			conc.add.tmp = data$conc.add[data$xID.exp==i]
			V.s.tmp = data$V.s[data$xID.exp==i]
			x.tmp = (V.add.tmp*conc.add.tmp/V.s.tmp)/(10^(delta.emf.tmp/b[ISEID.tmp]))
			# Initial values are between 90% and 100% of the average estimate
			x.exp[i] = mean(x.tmp)*runif(1, 0.9, 1)
		}
	}

	log10x.exp = log10(x.exp)

	INITS <- list(a=a, b=b, cstar=cstar, logsigma=logsigma)
	init.stretch = 0.01
	inits <- list(a=a*runif(data$R, 1 - init.stretch, 1 + init.stretch),
				b=b*runif(data$R, 1 - init.stretch, 1 + init.stretch),
				cstar=cstar*runif(data$R, 1 - init.stretch, 1 + init.stretch),
				sigma=sigma*runif(data$R, 1 - init.stretch, 1 + init.stretch),
				log10x.exp=log10x.exp)
   }
   if (calibration.only) {
	INITS <- list(a=a, b=b, cstar=cstar, logsigma=logsigma)
	init.stretch = 0.01
	inits <- list(a=a*runif(data$R, 1 - init.stretch, 1 + init.stretch),
				b=b*runif(data$R, 1 - init.stretch, 1 + init.stretch),
				cstar=cstar*runif(data$R, 1 - init.stretch, 1 + init.stretch),
				sigma=sigma*runif(data$R, 1 - init.stretch, 1 + init.stretch))
   }
	return(inits)
}