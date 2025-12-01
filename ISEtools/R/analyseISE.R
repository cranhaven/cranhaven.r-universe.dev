analyseISE = function(data, model.path=NA, model.name=NA, Z=NA, temperature = 21,
                      burnin=25000, iters = 50000, chains=4, thin = 1,
                      a.init= NA, b.init=NA, cstar.init=NA, logc.limits = c(-8.9, -1.9), sigma.upper = 5, diagnostic.print=FALSE, offset = 1,
                      alpha = 0.05, beta = 0.05, SN = NA, program="OpenBUGS") {
  
  ###################################################################
  # 				analyseISE::                                      #
  #   estimate ISE parameters and sample concentrations             #
  ###################################################################
  
  ###
  # Calculates calibration curve, and calibrates experimental data
  # Required inputs are:
  #  data:        Data formatted using load.ISE.data
  #  Z:		    Ionic valence
  #
  # Optional inputs that do not require specification: 
  #  model.path:  The directory where the BUGS model is located (e.g. "c:/myisefolder", but usually the location of ISEtools) 
  #  model.name:  The name of the BUGS model (e.g. "Single ISE model.txt"); allows the user to modify the supplied BUGS models
  #  burnin:      The number of initial parameter estimates to discard while tuning the system
  #  iters:       The total number of iterations
  #  chains:      The number of parallel chains to run (useful for diagnostics)
  #  ### Note:    The final estimates are based on (iters - burnin)*chains number of simulations
  #  a.init:      Initial value passed to gen.inits.single (scalar) or gen.inits.multiple (vector)
  #  b.init:      Similar
  #  cstar.init:  Similar
  #  logc.limits: Upper and lower climits for log(c) initial values (defaults to -8.9 and -1.9)
  #  sigma.upper: Upper limit for sigma.init
  #  offset:	The initial value for the slope uses the last and the last - offset point (defaults to 1).
  #  ### Note:    Default values may be altered if there are convergence problems in the 
  #               non-linear regression
  #  diagnostic.print:  Prints information used to determine convergence (OpenBUGS only)
  #  alpha:       False positive rate used for detection threshold (not output) to calculate LOD(alpha, beta) {only returned if SN = NA}
  #  beta:        False negative rate used to calculate LOD(alpha, beta) {only returned if SN = NA}
  #  SN:          Desired signal-to-noise ratio for LOD(S/N) calculations (default is to calculate the S/N equivalent based on alpha, beta)
  #  program      Choice of "OpenBUGS" or "jags" for MCMC sampler, defaults to "OpenBUGS"
  ###
  
  # Error checking
  if (is.na(Z)) {
    stop("Please specify the ionic valence using Z=<value>, e.g. describeISE(..., Z = 2,...)", call.=FALSE)
  }
  
  # Check if required packages are installed and loaded
  if (!(program %in% c("OpenBUGS", "jags"))) {
    stop("Exiting: program must equal OpenBUGS or jags.", call.=FALSE)
  }
  
  if (program == "OpenBUGS") {
    OB.x = !identical(find.package("R2OpenBUGS", quiet=TRUE), character(0))
    if (!OB.x) {
      stop("The R2OpenBUGS package must be installed to use OpenBUGS.", call.=FALSE)
    }
  }
  
  if (program == "jags") {
    j.x = !identical(find.package("rjags", quiet=TRUE), character(0))
    if (!j.x) {
      stop("The rjags package must be installed to use jags.", call.=FALSE)
    }
  }
  
  # One ISE or more than one?
  if (data$R == 1) { single.ISE = TRUE }
  if (data$R > 1) { single.ISE = FALSE }
  
  # Standard addition or not?
  stdadd = data$stdadd
  
  # If model.path is not specified, revert to appropriate defauls
  if (is.na(model.path)) { model.path = paste(path.package('ISEtools'), "/models", sep="") }
  
  
  # If model.name is not specified, revert to appropriate defaults
  if (is.na(model.name) & program == "OpenBUGS") {
    if (single.ISE) {
      if (!stdadd) { model.name = "Single_ISE_model.txt" }
      if (stdadd) { model.name = "Single_ISE_model_standard_addition.txt" }
    }
    if (!single.ISE) {
      if (!stdadd) { model.name = "Multiple_ISE_model.txt" }
      if (stdadd) { model.name = "Multiple_ISE_model_standard_addition.txt" }
    }
  }
  
  if (is.na(model.name) & program == "jags") {
    if (single.ISE) {
      if (!stdadd) { model.name = "Single_ISE_model_jags.txt" }
      if (stdadd) { model.name = "Single_ISE_model_standard_addition_jags.txt" }
    }
    if (!single.ISE) {
      if (!stdadd) { model.name = "Multiple_ISE_model_jags.txt" }
      if (stdadd) { model.name = "Multiple_ISE_model_standard_addition_jags.txt" }
    }
  }
  
  # Location of the BUGs model
  ISE.model <- file.path(model.path, model.name)
  
  # Reduce the dataset to those variables expected by BUGS
  alldata = data
  data = list()
  data$N = alldata$N; data$log10x = alldata$log10x; data$emf = alldata$emf; data$M = alldata$M
  if (single.ISE) {
    if (!stdadd) { data$emf.exp = alldata$emf.exp }
    if (stdadd) { data$delta.emf = alldata$delta.emf; data$V.s = alldata$V.s; data$V.add = alldata$V.add; data$conc.add = alldata$conc.add }
  }
  
  if (!single.ISE) { 
    data$R = alldata$R; data$ISEID = alldata$ISEID; data$M.obs = alldata$M.obs; data$ISEID.exp = alldata$ISEID.exp; data$xID.exp = alldata$xID.exp
    if (!stdadd) { data$emf.exp = alldata$emf.exp }
    if (stdadd) { data$delta.emf = alldata$delta.emf; data$V.s = alldata$V.s; data$V.add = alldata$V.add; data$conc.add = alldata$conc.add }
  }
  
  
  # BUGs model requires at least two samples.  
  # If only one exists, duplicate it.
  flag.datareplicated = 0
  if(data$M==1) {
    data$M=2
    if (!stdadd) { data$emf.exp=rep(data$emf.exp, 2) } 
    if (stdadd) { 
      data$delta.emf = rep(data$delta.emf, 2)
      data$V.s = rep(data$V.s, 2) 
      data$V.add = rep(data$V.add, 2) 
      data$conc.add = rep(data$conc.add, 2) 
    }
    flag.datareplicated = 1
  }
  
  # Initial values
  message("Generating initial values...")
  if (single.ISE==TRUE) {
    # Number of ISEs = 1
    Num.ISEs = 1
    # Generate initial values
    inits <- rep(list(NA), chains)
    for (i in 1:chains) {
      inits[[i]] <- gen.inits.single(data, a.init=a.init, b.init=b.init,
                                     cstar.init= cstar.init, logc.limits=logc.limits, sigma.upper=sigma.upper, stdadd = stdadd, offset=offset)
    }
  }
  if (single.ISE==FALSE) {
    # Generate initial values
    inits <- rep(list(NA), chains)
    # Number of ISEs given in data
    Num.ISEs = data$R
    for (i in 1:chains) {
      inits[[i]] <- gen.inits.multiple(data, a.init=a.init, b.init=b.init, 
                                       cstar.init= cstar.init, logc.limits=logc.limits, sigma.upper= sigma.upper, stdadd = stdadd, offset=offset)
    }
  }
  # Parameters to monitor
  parameters <- c("a", "b", "c", "cstar", "sigma", "log10x.exp")
  
  # Calculate theoretical slope at mV level
  data$mu.b = 1000*log(10)*8.31433*(temperature + 273.15)/(96487*Z)
  
  if(program=="OpenBUGS") {
    # Call BUGS
    message("Running Bayesian model using OpenBUGS...")
    # Using R2OpenBUGS
    ISE.Bayes <- R2OpenBUGS::bugs(data, inits, parameters, n.iter = iters, 
                                  model.file=ISE.model, n.chains = chains, n.burnin=burnin, 
                                  n.thin=thin, digits=9)
    
    # Print and plot diagnostics
    if(diagnostic.print==TRUE) {
      print(ISE.Bayes, digits=9)
    }
  }
  
  if(program=="jags") {
    # Call jags
    message("Running Bayesian model using jags...")
    ISE.Bayes.mod = rjags::jags.model(file = ISE.model, data=data, n.chains=chains, inits=inits, n.adapt=1000, quiet=TRUE)
    update(ISE.Bayes.mod, n.iter = burnin, progress.bar="none")
    my.coda = rjags::coda.samples(ISE.Bayes.mod, parameters, n.iter = (iters - burnin), thin = thin)
    
    # Align with OpenBUGS output
    ISE.Bayes = list()
    ISE.Bayes$sims.array = array(NA, c(chains, (iters - burnin)/thin, Num.ISEs*5 + data$M) )
    for (i in 1:chains) {
      ISE.Bayes$sims.array[i,,] = my.coda[[i]]
    }
    # R2OpenBUGS takes output based on parameter entry, rjags alphabetises, so re-order log10x and sigma
    for (i in 1:chains) {
      ISE.Bayes$sims.array[i,,] = cbind(ISE.Bayes$sims.array[i,,1:(Num.ISEs*4)], 
                                        ISE.Bayes$sims.array[i,, 1:Num.ISEs + Num.ISEs*4 + data$M], ISE.Bayes$sims.array[i,, Num.ISEs*4 + 1:data$M] )
    }
  }
  
  if(single.ISE==TRUE) {
    # Calculate quantiles for x
    log10x.exp <- matrix(NA, nrow=data$M, ncol=3)
    log10x.exp.IQ <- matrix(NA, nrow=data$M, ncol=2)
    colnames(log10x.exp)=c("Estimated concentration", "Lower limit", "Upper limit")
    colnames(log10x.exp.IQ)=c("1st quartile", "3rd quartile")
    for (i in 1:data$M) {
      tmp <- as.vector(ISE.Bayes$sims.array[,,i+5])
      quantile.tmp <- quantile(tmp, c(0.025, 0.5, 0.975, 0.25, 0.75))
      log10x.exp[i,] <- quantile.tmp[c(2,1,3)]
      log10x.exp.IQ[i,] <- quantile.tmp[c(4,5)]
    }
    
    # Other vars
    ahat.coda = as.vector(ISE.Bayes$sims.array[,,1])
    coda.length = length(ahat.coda)
    bhat.coda = as.vector(ISE.Bayes$sims.array[,,2])
    cstarhat.coda = as.vector(ISE.Bayes$sims.array[,,4])
    chat.coda = cstarhat.coda^10
    sigmahat.coda = as.vector(ISE.Bayes$sims.array[,,5])
    ahat = median(ahat.coda)
    bhat = median(bhat.coda)
    cstarhat = median(cstarhat.coda)
    chat = cstarhat^10
    sigmahat = median(sigmahat.coda)
    
    ahat.lcl = quantile(ahat.coda, 0.025)
    bhat.lcl = quantile(bhat.coda, 0.025)
    cstarhat.lcl = quantile(cstarhat.coda, 0.025)
    chat.lcl = cstarhat.lcl^10
    ahat.ucl = quantile(ahat.coda, 0.975)
    bhat.ucl = quantile(bhat.coda, 0.975)
    cstarhat.ucl = quantile(cstarhat.coda, 0.975)
    chat.ucl = cstarhat.ucl^10
    sigmahat.lcl = quantile(sigmahat.coda, 0.025)
    sigmahat.ucl = quantile(sigmahat.coda, 0.975)
    
    ###
    # Derived variables
    ###
    if(is.na(SN)){
      if(is.na(alpha)) alpha = 0.05 # Reset to default if needed
      if(is.na(beta)) beta = 0.05
      zscore = qnorm(alpha, lower.tail=FALSE) + qnorm(beta, lower.tail=FALSE)
      LOD.info = list(type = "alpha, beta", alpha = alpha, beta = beta, SN = zscore)
    }else{
      zscore = SN
      LOD.info = list(type = "S/N", alpha = NA, beta = NA, SN = zscore)
    }
    LOD.coda = 10*log10(cstarhat.coda) + log10(10^(zscore*sigmahat.coda /abs( bhat.coda ) ) - 1)
    LOD.hat = median(LOD.coda)
    LOD.Q1 = quantile(LOD.coda, 0.25)
    LOD.Q3 = quantile(LOD.coda, 0.75)
    LOD.lcl = quantile(LOD.coda, 0.025)
    LOD.ucl = quantile(LOD.coda, 0.975)
    
  }
  
  if (single.ISE == FALSE) {
    # Calculate quantiles for x
    log10x.exp <- matrix(NA, nrow=data$M, ncol=3)
    log10x.exp.IQ <- matrix(NA, nrow=data$M, ncol=2)
    colnames(log10x.exp)=c("Estimated concentration", "Lower limit", "Upper limit")
    colnames(log10x.exp.IQ)=c("1st quartile", "3rd quartile")
    for (i in 1:data$M) {
      tmp = as.vector(ISE.Bayes$sims.array[,,i+5*data$R])
      quantile.tmp <- quantile(tmp, c(0.025, 0.5, 0.975, 0.25, 0.75))
      log10x.exp[i,] <- quantile.tmp[c(2,1,3)]
      log10x.exp.IQ[i,] <- quantile.tmp[c(4,5)]
    }
    
    # Other vars
    coda.length = length(as.vector(ISE.Bayes$sims.array[,,1]))
    ahat.coda = matrix(NA, nrow = coda.length, ncol = data$R)
    bhat.coda <- cstarhat.coda <- chat.coda <- sigmahat.coda <- LOD.coda <- ahat.coda
    
    ahat = rep(NA, data$R)
    ahat.lcl = rep(NA, data$R)
    ahat.ucl = rep(NA, data$R)
    bhat = rep(NA, data$R)
    bhat.lcl = rep(NA, data$R)
    bhat.ucl = rep(NA, data$R)
    chat = rep(NA, data$R)
    chat.lcl = rep(NA, data$R)
    chat.ucl = rep(NA, data$R)
    cstarhat = rep(NA, data$R)
    cstarhat.lcl = rep(NA, data$R)
    cstarhat.ucl = rep(NA, data$R)
    sigmahat = rep(NA, data$R)
    sigmahat.lcl = rep(NA, data$R)
    sigmahat.ucl = rep(NA, data$R)
    
    LOD.hat = rep(NA, data$R)
    LOD.lcl = rep(NA, data$R)
    LOD.ucl = rep(NA, data$R)
    LOD.Q1 = rep(NA, data$R)
    LOD.Q3 = rep(NA, data$R)
    if(is.na(SN)){
      if(is.na(alpha)) alpha = 0.05 # Reset to default if needed
      if(is.na(beta)) beta = 0.05
      zscore = qnorm(alpha, lower.tail=FALSE) + qnorm(beta, lower.tail=FALSE)
      LOD.info = list(type = "alpha, beta", alpha = alpha, beta = beta, SN = zscore)
    }else{
      zscore = SN
      LOD.info = list(type = "S/N", alpha = NA, beta = NA, SN = zscore)
    }
    
    for (j in 1:data$R) {
      ahat.coda[,j] = as.vector(ISE.Bayes$sims.array[,,j])
      bhat.coda[,j] = as.vector(ISE.Bayes$sims.array[,,data$R + j])
      cstarhat.coda[,j] = as.vector(ISE.Bayes$sims.array[,,3*data$R + j])
      sigmahat.coda[,j] = as.vector(ISE.Bayes$sims.array[,,4*data$R + j])
      chat.coda[,j] = cstarhat.coda[,j]^10
      
      ahat.q = quantile(ahat.coda[,j], c(0.025, 0.5, 0.975))
      ahat[j] = ahat.q[2]
      ahat.lcl[j] = ahat.q[1]
      ahat.ucl[j] = ahat.q[3]
      bhat.q = quantile(bhat.coda[,j], c(0.025, 0.5, 0.975))
      bhat[j] = bhat.q[2]
      bhat.lcl[j] = bhat.q[1]
      bhat.ucl[j] = bhat.q[3]
      chat.q = quantile(chat.coda[,j], c(0.025, 0.5, 0.975))
      chat[j] = chat.q[2]
      chat.lcl[j] = chat.q[1]
      chat.ucl[j] = chat.q[3]
      cstarhat.q = quantile(cstarhat.coda[,j], c(0.025, 0.5, 0.975))
      cstarhat[j] = cstarhat.q[2]
      cstarhat.lcl[j] = cstarhat.q[1]
      cstarhat.ucl[j] = cstarhat.q[3]
      sigmahat.q = quantile(sigmahat.coda[,j], c(0.025, 0.5, 0.975))
      sigmahat[j] = sigmahat.q[2]
      sigmahat.lcl[j] = sigmahat.q[1]
      sigmahat.ucl[j] = sigmahat.q[3]
      
      # Derived variable
      LOD.coda[,j] = 10*log10(cstarhat.coda[,j]) + log10(10^(zscore*sigmahat.coda[,j] /abs( bhat.coda[,j] ) ) - 1)
      LOD.q = quantile(LOD.coda[,j], c(0.025, 0.25, 0.5, 0.75, 0.975))
      LOD.hat[j] = LOD.q[3]
      LOD.lcl[j] = LOD.q[1]
      LOD.ucl[j] = LOD.q[5]
      LOD.Q1[j] = LOD.q[2]
      LOD.Q3[j] = LOD.q[3]
    }	
  }
  
  ## Sample concentrations
  SampleID = seq(data$M)
  
  #   If flag.datareplicated == 1, the sample was replicated
  #   for the BUGs model, and the replicate is now removed
  if(flag.datareplicated==1) {
    SampleID = c(1)
    log10x.exp = log10x.exp[1,]
    log10x.exp.IQ = log10x.exp.IQ[1,]
  }
  
  ###
  # Relevant output
  ###
  results = list(SampleID = SampleID, log10x.exp=log10x.exp, log10x.exp.IQ= log10x.exp.IQ, 
                 ahat=ahat, bhat=bhat, chat=chat, cstarhat=cstarhat, sigmahat=sigmahat, 
                 ahat.lcl=ahat.lcl, ahat.ucl=ahat.ucl, bhat.lcl=bhat.lcl, bhat.ucl=bhat.ucl, 
                 chat.lcl=chat.lcl, chat.ucl=chat.ucl, sigmahat.lcl=sigmahat.lcl, sigmahat.ucl=sigmahat.ucl,
                 LOD.info = LOD.info, LOD.hat = LOD.hat, LOD.lcl = LOD.lcl, LOD.ucl = LOD.ucl,
                 LOD.Q1 = LOD.Q1, LOD.Q3 = LOD.Q3)
  class(results) = "analyseISE"
  return(results)	
}