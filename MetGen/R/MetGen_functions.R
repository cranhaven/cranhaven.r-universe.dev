encodeDay <- function(mydates){#d is a chron object
  n <- length(mydates)
  mydates.encoded <- rep(NA,n)
  for (d in 1:n){
    l <- month.day.year(as.numeric(mydates[d]))
    
    if (l$month > 1){
      if (leap.year(l$year)){
        daysInMonths <- c(31,29,31,30,31,30,31,31,30,31,30,31)
      } else {
        daysInMonths <- c(31,28,31,30,31,30,31,31,30,31,30,31)
      }
      
      
      mydates.encoded[d] <- sum(daysInMonths[1:(l$month-1)])+l$day
    } else {
      mydates.encoded[d] <- l$day
    }
  }
  mydates.encoded
}

pred.var <- function(fit, data){
  nvar.mu <- length(fit$var.mu)
  nvar.sigma <- length(fit$var.sigma)
  n <- nrow(data)
  pred <- matrix(nrow=n, ncol=2)
  fit.df <- na.omit(data.frame(xm=data[fit$var.mu],xs=data[fit$var.sigma]))
  subset <- 1:n
  subset <- subset[-attr(fit.df, "na.action")]
  pred[subset,] <- predict(fit, as.matrix(fit.df[,1:nvar.mu]),
                           as.matrix(fit.df[,(nvar.mu+1):(nvar.mu+nvar.sigma)]))
  pred
}

occint.trans <- function(var.mat, var.name, threshold=1){
  ## separate occurrence and intensity for a precipitation-like variable
  myvar <- as.matrix(var.mat[var.name])
  nobs <- length(myvar)
  ind <- which(myvar>threshold)
  
  int <- rep(NaN,nobs)
  int[ind] <- myvar[ind]
  
  occ <- rep(NA,nobs)
  occ[ind] <- 1
  ind0 <- which(!(myvar>threshold))
  occ[ind0] <- 0
  
  myvar.occint <- data.frame(occ,int)
  names(myvar.occint) <- c(paste0(var.name,".occ"),paste0(var.name,".int"))
  
  data.frame(var.mat,myvar.occint)
}

seasonal.effect <- function(var.mat, period = 365.25){
  ## var.mat contains a variable named "dates" in chron format, possibly including times
  nvar <- ncol(var.mat)
  np <- length(period)
  dsim <- encodeDay(var.mat[,"dates"])
  
  if (np > 1){
    dsim.mat <- matrix(dsim, nrow=length(dsim), ncol=np)
    period.mat <- matrix(period, nrow=length(dsim), ncol=np, byrow=TRUE)
    cycle.mat <- cbind(cos(2*pi*dsim.mat/period.mat), sin(2*pi*dsim.mat/period.mat))
    
    var.mat <- data.frame(var.mat,cycle.mat[,c(seq(1,2*np,by=2),seq(2,2*np,by=2))])
    names(var.mat)[(nvar+1):(nvar+2*np)] <- paste0(c("cos.","sin."),rep(period,each=2),"d")
  } else {
    var.mat <- data.frame(var.mat,cbind(cos(2*pi*dsim/period), sin(2*pi*dsim/period)))
    names(var.mat)[(nvar+1):(nvar+2*np)] <- paste0(c("cos.","sin."),period,"d")
  }
  var.mat
}


diurnal.effect <- function(var.mat, period = 24){
  ## var.mat contains a variable named "dates" in chron format, possibly including times
  nvar <- ncol(var.mat)
  np <- length(period)
  dsim <- hours(var.mat[,"dates"])+0.5
  
  if (np > 1){
    dsim.mat <- matrix(dsim, nrow=length(dsim), ncol=np)
    period.mat <- matrix(period, nrow=length(dsim), ncol=np, byrow=TRUE)
    cycle.mat <- cbind(cos(2*pi*dsim.mat/period.mat), sin(2*pi*dsim.mat/period.mat))
    var.mat <- data.frame(var.mat,cycle.mat[,c(seq(1,2*np,by=2),seq(2,2*np,by=2))])
    names(var.mat)[(nvar+1):(nvar+2*np)] <- paste0(c("cos.","sin."),rep(period,each=2),"h")
  } else {
    var.mat <- data.frame(var.mat,cbind(cos(2*pi*dsim/period), sin(2*pi*dsim/period)))
    names(var.mat)[(nvar+1):(nvar+2*np)] <- paste0(c("cos.","sin."),period,"h")
  }
  var.mat
}


lagged.effect <- function(var.mat, var.name, maxlag, nstat = NULL){
  ## var.name is the name of the variable to be picked up in data frame var.mat
  ## maxlag is the maximum amount of lag requested
  ## assumes that all time steps for all stations are included otherwise issues an error
  
  nvar0 <- ncol(var.mat) ## number of initial columns in data frame
  nobs <- nrow(var.mat) ## number of obs = time steps x stations
  ntstep <- length(unique(var.mat[,"dates"])) ## number of time steps
  if (is.null(nstat)){
    nstat <- nobs/ntstep
    print(paste0("Assuming there are ", nstat, " stations"))
  }
  
  nvar <- length(var.name) ## number of variables to lag
  myvar <- as.matrix(var.mat[var.name]) ## variables to lag
  
  if (maxlag > 1 | nvar > 1){
    myvar.lag <- matrix(nrow=nobs, ncol=(nvar*maxlag)) ## lagged variables
    for (l in 1:maxlag){
      myvar.lag[(l*nstat+1):nobs,(1+(l-1)*nvar):(l*nvar)] <- myvar[1:(nobs-l*nstat),]
    }
  } else {
    myvar.lag <- c(rep(NA, nstat), myvar[1:(nobs-nstat),])
  }
  if (any(is.numeric(match(paste0(var.name,".lag",rep(1:maxlag,each=nvar)), names(var.mat))))){
    ## variables already in data frame so replace
    var.mat[,paste0(var.name,".lag",rep(1:maxlag,each=nvar))] <- myvar.lag
  } else {
    var.mat <- data.frame(var.mat,myvar.lag)
    names(var.mat)[(nvar0+1):(nvar0+maxlag*nvar)] <- paste0(var.name,".lag",rep(1:maxlag,each=nvar))
  }
  var.mat
}

formating.var <- function(var.mat, var.name, nstat){
  ntimestep <- nrow(var.mat)/nstat
  myvar <- cbind(unique(var.mat[,"dates"]),matrix(var.mat[,var.name], nrow=ntimestep, ncol = nstat, byrow=TRUE))
  colnames(myvar) <- c("dates", paste0("s", 1:nstat))
  myvar
}

spatave.effect <- function(var.mat, var.name, nstat = NULL, na.proc = FALSE){
  ## var.name is the name of the variable to be picked up in data frame var.mat
  ## lag is the amount of lag requested
  ## assumes that all time steps for all stations are included otherwise issues an error
  
  nvar <- length(var.name) ## number of variables to lag
  if (nvar > 1)
    stop("A single variable can be processed at once")
  
  nvar0 <- ncol(var.mat) ## number of initial columns in data frame
  nobs <- nrow(var.mat) ## number of obs = time steps x stations
  ntstep <- length(unique(var.mat[,"dates"])) ## number of time steps
  if (is.null(nstat)){
    nstat <- nobs/ntstep
    print(paste0("Assuming there are ", nstat, " stations"))
  }
  myvar.spatave <- rep(apply(formating.var(var.mat, var.name, nstat)[,-1], 1, mean, na.rm= na.proc),
                       each = nstat) ## repeat for each station
  
  if (any(names(var.mat) == paste0(var.name,".spatave"))){ ## effect already in data frame so replace
    var.mat[[paste0(var.name,".spatave")]] <- myvar.spatave
  } else {
    var.mat <- data.frame(var.mat,myvar.spatave)
    names(var.mat)[nvar0+1] <- paste0(var.name,".spatave")
  }
  var.mat
}

movave.effect <- function(var.mat, var.name, bw, nstat = NULL, na.proc = FALSE){
  ## var.name is the name of the variable to be picked up in data frame var.mat
  ## lag is the amount of lag requested
  ## bw is the bandwidth of the moving average
  ## assumes that all time steps for all stations are included otherwise issues an error
  
  nvar <- length(var.name) ## number of variables to lag
  if (nvar > 1)
    stop("A single variable can be processed at once")
  
  nvar0 <- ncol(var.mat) ## number of initial columns in data frame
  nobs <- nrow(var.mat) ## number of obs = time steps x stations
  ntstep <- length(unique(var.mat[,"dates"])) ## number of time steps
  if (is.null(nstat)){
    nstat <- nobs/ntstep
    print(paste0("Assuming there are ", nstat, " stations"))
  }
  
  myvar <- formating.var(var.mat, var.name, nstat)[,-1]
  myvar.movav <- matrix(nrow=ntstep, ncol=nstat)
  
  for (t in bw:ntstep)
    myvar.movav[t,] <- apply(myvar[(t-bw+1):t,],2,mean,na.rm=na.proc)
  
  if (any(names(var.mat) == paste0(var.name,".movave",bw))){ ## effect already in data frame so replace
    var.mat[[paste0(var.name,".movave",bw)]] <- as.vector(t(myvar.movav))
  } else {
    var.mat <- data.frame(var.mat,as.vector(t(myvar.movav)))
    names(var.mat)[nvar0+1] <- paste0(var.name,".movave",bw)
  }
  var.mat
}

sim.glm <- function(fit, datapred, fam.glm = "gaussian", occ.cond = NULL){
  var.form <- all.vars(fit$formula)[-1]
  
  if (!all(var.form %in% names(datapred)))
    stop("Variables in FIT's formula not found in DATAPRED")
  
  if (fam.glm == "gaussian"){
    psim <- predict(fit, newdata = datapred, type="response")
    s <- summary(fit)$dispersion
    sim.newdata <- rep(NA, length(psim))
    sim.newdata[!is.na(psim)] <- rnorm(sum(!is.na(psim)), mean = psim[!is.na(psim)], sd = s)
  }
  
  if (fam.glm == "gaussian-hetero"){
    psim <- pred.var(fit, datapred)
    subset <- which(apply(!is.na(psim), 1, all))
    sim.newdata <- rep(NA, nrow(psim))
    sim.newdata[subset] <- rnorm(length(subset), mean = psim[subset, 1], sd= psim[subset, 2])
  }
  
  if (fam.glm == "binomial"){
    psim <- predict(fit, newdata = datapred, type="response")
    sim.newdata <- rep(NA, length(psim))
    sim.newdata[!is.na(psim)] <- rbinom(sum(!is.na(psim)), size=1, prob= psim[!is.na(psim)])
  }
  
  if (fam.glm == "Gamma" ){
    psim <- predict(fit, newdata = datapred, type="response")  ## !! Gamma expectation ??
    shape <-  MASS::gamma.shape(fit)$alpha
    rate <- shape/psim
    sim.newdata <- rep(NA, length(psim))
    sim.newdata[!is.na(psim)] <- rgamma(sum(!is.na(psim)), shape = shape, rate = rate[!is.na(psim)])
    
  }
  
  if (!is.null(occ.cond)) { # simulate conditionally on occurrence
    ind0 <- which(datapred[occ.cond] == 0)
    sim.newdata[ind0] <- 0
  }
  sim.newdata
}


rm.buffer <- function(simdata, nstat, bi.length=480){
  simdata[(1+bi.length*nstat):nrow(simdata),]
}

vec.effect.update <- function(t, ydf, nstat, na.proc, var.name, var.lag,
                              spatave, movave, bw, maxlag){
  ## update ydf of one time step t correponding to rows (1:nstat)+(t-1)*nstat
  
  if (t < 2)
    stop("T must be greater than 1")
  
  yvec <- ydf[(1:nstat)+(t-2)*nstat, var.name]
  
  if (spatave){
    y.spatave <- mean(yvec,  na.rm = na.proc)
    ydf[[paste0(var.name,".spatave")]][(1:nstat)+(t-2)*nstat]<-
      rep(y.spatave, each = nstat)
  }
  
  
  if (movave){
    nbw <- length(bw)
    ymat <- matrix(ydf[[var.name]][1:((t-1)*nstat)], ncol = nstat, byrow=TRUE)
    for (b in 1:nbw){
      myvar.movav <- apply(ymat[(t-bw[b]):(t-1),],2,mean,na.rm=na.proc)
      ydf[[paste0(var.name,".movave",bw[b])]][(1:nstat)+(t-2)*nstat]<-myvar.movav
      
      if (spatave){                
        myvar.movav <- mean(ydf[[paste0(var.name,".spatave")]][
          seq((t-bw[b])*nstat, (t-1)*nstat, by = nstat)],na.rm=na.proc)
        ydf[[paste0(var.name,".spatave.movave",bw[b])]][(1:nstat)+(t-2)*nstat]<-
          rep(myvar.movav, each = nstat)
      } ## spatave
    } ## for bw
  } ## movave
  
  if (maxlag){
    for (l in 1:maxlag){
      lag.var <- paste0(var.lag,".lag",l)
      ydf[(1:nstat)+(t-1)*nstat,lag.var] <-
        ydf[(1:nstat)+(t-1-l)*nstat,var.lag]
    }
    
  }
  
  ydf
}

imputation.lagged <- function(fit, var.name, maxlag, coord,cov=NULL,
                              seasonal = TRUE, speriod = 365.25, diurnal = TRUE,
                              dperiod = 24, spatave=TRUE, movave = TRUE, bw = 48,
                              na.proc=TRUE, fam.glm = "gaussian", occ.cond = NULL,
                              init.buff = 1440){
  ## imputation of missing values in fit$data from simulation of fitted model
  
  if (movave & init.buff < max(bw))
    stop("Increase init.buffer to cover moving average window ! ")
  
  
  nstat <- nrow(coord)
  nhist <- nrow(fit$data)
  dstart <- fit$data$dates[1]
  if (as.integer(dstart) == dstart){
    dstart <- chron(dstart, "00:00:00")
  } else
    dstart <- chron(dstart)
  dend <- chron(fit$data$dates[nhist])
  tstep <- seq(dstart-init.buff/48,dend,by=1/48)
  if (!is.null(cov)){## data frame already in place from previous imputation
    newsim.agg<- cov
  } else {
    newsim.agg <-  data.frame(
      dates=matrix(sapply(tstep,rep,nstat),length(tstep)*nstat,1),
      coord = apply(as.matrix(coord),2, rep,length(tstep)))
    
    if (seasonal)
      newsim.agg <- seasonal.effect(newsim.agg, period=speriod)
    
    if (diurnal)
      newsim.agg <- diurnal.effect(newsim.agg, period=dperiod)
    
  } # else : taking care of setting up data frame for imputation
  
  ## create buffer from simulation
  var.obs <- formating.var(fit$data,var.name, nstat)
  ylagged <-matrix(nrow=(nstat*init.buff),ncol=1)
  
  if (fam.glm == "gaussian" | fam.glm == "gaussian-hetero"){
    mean.var <- apply(var.obs[,2:(nstat+1)],2, mean, na.rm=TRUE)
    sd.var <- apply(var.obs[,2:(nstat+1)],2, sd, na.rm=TRUE)
    
    if (any(is.na(mean.var))){
      mean.var[is.na(mean.var)] <- sample(rep(mean.var[!is.na(mean.var)],nstat),sum(is.na(mean.var)),replace=TRUE)
      sd.var[is.na(sd.var)] <- sample(rep(sd.var[!is.na(sd.var)],nstat),sum(is.na(sd.var)),replace=TRUE)
    }
    
    for (s in 1:nstat)
      ylagged[seq(s,(nstat*init.buff), by=nstat),1] <-
      matrix(rnorm(init.buff, mean=mean.var[s], sd = sd.var[s]),
             nrow=init.buff, ncol=1)
    
  } else {
    
    if (fam.glm == "binomial") {
      prob.var <- apply(var.obs[,2:(nstat+1)],2, mean, na.rm=TRUE)
      
      if (any(is.na(prob.var))){
        prob.var[is.na(prob.var)] <- sample(rep(prob.var[!is.na(prob.var)],nstat),sum(is.na(prob.var)),replace=TRUE)
      }
      
      
      for (s in 1:nstat)
        ylagged[seq(s,(nstat*init.buff), by=nstat),1] <-
          matrix(rbinom(init.buff, size = 1, prob = prob.var[s]),
                 nrow=init.buff, ncol=1)
      
    } else {
      
      if (fam.glm == "Gamma") {
        var.obs <- apply(var.obs[,-1], 2, na.omit)
        ind.null <- which(sapply(var.obs, length) == 0)
        if (length(ind.null) > 0){
          gamm.est <- matrix(nrow=2, ncol=nstat)
          gamm.est[,-ind.null] <- sapply(var.obs[-ind.null], function(x) {fitdistr(x,"gamma")$estimate}) } else {
            gamm.est <- sapply(var.obs, function(x) {fitdistr(x,"gamma")$estimate})
          }
        
        if (any(is.na(gamm.est))){
          gamm.est[1,is.na(gamm.est[1,])] <- sample(rep(gamm.est[1,!is.na(gamm.est[1,])],nstat),sum(is.na(gamm.est[1,])),replace=TRUE)
          gamm.est[2,is.na(gamm.est[2,])] <- sample(rep(gamm.est[2,!is.na(gamm.est[2,])],nstat),sum(is.na(gamm.est[2,])),replace=TRUE)
        }
        
        for (s in 1:nstat)
          ylagged[seq(s,(nstat*init.buff), by=nstat),1] <-
          matrix(rgamma(init.buff, shape = gamm.est[1,s],
                        rate = gamm.est[2,s]),nrow=maxlag,ncol=1)
      } else {
        stop("GLM family unrecognized")
      }
    }
  }
  
  if (!is.null(occ.cond)){
    if (is.na(match(occ.cond, names(cov)) ))
      stop('For intensity variable, the corresponding occurrence variable must be in the data used to fit the GLM')
    var.name <- strsplit(var.name,".", fixed=TRUE)[[1]][1]
    ind0 <- which(cov[[occ.cond]][1:(nstat*init.buff)] == 0)
    ylagged[ind0] <- 0
  }
  
  ylagged <- data.frame(dates=newsim.agg$dates[1:(nstat*init.buff)], ylagged)
  names(ylagged)[2] <- var.name
  
  ## number of seasonal and diurnal effects + eventual covariates
  
  nvar.cycles <- ncol(newsim.agg)
  
  ## determine all variable names of memory effects
  if (spatave){
    ylagged <- spatave.effect(ylagged, var.name, nstat, na.proc)
  }
  if (movave){
    for (bb in bw)
      ylagged <- movave.effect(ylagged, var.name, bb, nstat, na.proc)
    if (spatave){
      for (bb in bw)
        ylagged <- movave.effect(ylagged, paste0(var.name,".spatave"), bb, nstat, na.proc)
    }
  }
  lag.names <- names(ylagged)[2:ncol(ylagged)]
  
  if (maxlag){
    nvar.lag <- length(lag.names)
    ylagged <- lagged.effect(ylagged, lag.names,maxlag, nstat)
    lag.names <- names(ylagged)[2:ncol(ylagged)]
  }
  
  ## start filling memory effects in the buffer of newsim.agg using simulated values
  newsim.agg <- data.frame(newsim.agg,
                           matrix(nrow=nrow(newsim.agg), ncol = length(lag.names)))
  names(newsim.agg)[(nvar.cycles+1):ncol(newsim.agg)] <-lag.names
  newsim.agg[1:(init.buff*nstat),lag.names] <- ylagged[,lag.names]
  
  ##browser()
  for (t in (init.buff+1):length(tstep)){
    newsim.agg <- vec.effect.update(t, newsim.agg, nstat, na.proc,
                                    var.name,lag.names[1:nvar.lag],
                                    spatave, movave, bw, maxlag)
    
    
    newsim.agg[((1:nstat)+(t-1)*nstat),var.name] <- fit$data[(1:nstat)+(t-1-init.buff)*nstat, var.name]
    
    if (any(is.na(fit$data[(1:nstat)+(t-1-init.buff)*nstat, var.name]))){
      ind.na <- which(is.na(fit$data[(1:nstat)+(t-1-init.buff)*nstat, var.name]))
      sim.t <- sim.glm(fit, newsim.agg[(1:nstat)+(t-1)*nstat,], fam.glm, occ.cond)
      newsim.agg[((1:nstat)+(t-1)*nstat)[ind.na],var.name] <- sim.t[ind.na]
    }
    
    ## newsim.agg[(1:nstat)+(t-1)*nstat,var.name] <-
    ##     sim.glm(fit, newsim.agg[(1:nstat)+(t-1)*nstat,], fam.glm, occ.cond)
  }
  
  newsim.agg
}

projection.lagged <- function(dstart, dend, fit, var.name, maxlag, coord,cov=NULL,
                              seasonal = TRUE, speriod = 365.25, diurnal = TRUE,
                              dperiod = 24, spatave=TRUE, movave = TRUE, bw = 48,
                              na.proc=TRUE, fam.glm = "gaussian", occ.cond = NULL,
                              init.buff = 1440){
  ## projection on period dstart to dend based on simulation from fitted model
  
  if (movave & init.buff < max(bw))
    stop("Increase init.buffer to cover moving average window ! ")
  
  
  nstat <- nrow(coord)
  nhist <- nrow(fit$data)
  if (as.integer(dstart) == dstart){
    dstart <- chron(dstart, "00:00:00")
  } else
    dstart <- chron(dstart)
  dend <- chron(dend)
  
  tstep <- seq(dstart-init.buff/48,dend,by=1/48)
  
  if (!is.null(cov)){## data frame already in place from previous imputation
    newsim.agg <- cov
  } else {
    
    newsim.agg <- data.frame(
      dates=matrix(sapply(tstep,rep,nstat),length(tstep)*nstat,1),
      coord = apply(as.matrix(coord),2, rep,length(tstep)))
    
    if (seasonal)
      newsim.agg <- seasonal.effect(newsim.agg, period=speriod)
    
    if (diurnal)
      newsim.agg <- diurnal.effect(newsim.agg, period=dperiod)
    
    
  } # else : taking care of setting up data frame for imputation
  
  
  ## create buffer from simulation
  var.obs <- formating.var(fit$data,var.name, nstat)
  ylagged <-matrix(nrow=(nstat*init.buff),ncol=1)
  
  if (fam.glm == "gaussian" | fam.glm == "gaussian-hetero"){
    mean.var <- apply(var.obs[,2:(nstat+1)],2, mean, na.rm=TRUE)
    sd.var <- apply(var.obs[,2:(nstat+1)],2, sd, na.rm=TRUE)
    
    if (any(is.na(mean.var))){
      mean.var[is.na(mean.var)] <- sample(rep(mean.var[!is.na(mean.var)],nstat),sum(is.na(mean.var)),replace=TRUE)
      sd.var[is.na(sd.var)] <- sample(rep(sd.var[!is.na(sd.var)],nstat),sum(is.na(sd.var)),replace=TRUE)
    }
    
    for (s in 1:nstat)
      ylagged[seq(s,(nstat*init.buff), by=nstat),1] <-
      matrix(rnorm(init.buff, mean=mean.var[s], sd = sd.var[s]),
             nrow=init.buff, ncol=1)
    
  } else {
    
    if (fam.glm == "binomial") {
      prob.var <- apply(var.obs[,2:(nstat+1)],2, mean, na.rm=TRUE)
      
      if (any(is.na(prob.var))){
        prob.var[is.na(prob.var)] <- sample(rep(prob.var[!is.na(prob.var)],nstat),sum(is.na(prob.var)),replace=TRUE)
      }
      
      
      for (s in 1:nstat)
        ylagged[seq(s,(nstat*init.buff), by=nstat),1] <-
          matrix(rbinom(init.buff, size = 1, prob = prob.var[s]),
                 nrow=init.buff, ncol=1)
      
    } else {
      
      if (fam.glm == "Gamma") {
        var.obs <- apply(var.obs[,-1], 2, na.omit)
        ind.null <- which(sapply(var.obs, length) == 0)
        if (length(ind.null) > 0){
          gamm.est <- matrix(nrow=2, ncol=nstat)
          gamm.est[,-ind.null] <- sapply(var.obs[-ind.null], function(x) {fitdistr(x,"gamma")$estimate}) } else {
            gamm.est <- sapply(var.obs, function(x) {fitdistr(x,"gamma")$estimate})
          }
        
        if (any(is.na(gamm.est))){
          gamm.est[1,is.na(gamm.est[1,])] <- sample(rep(gamm.est[1,!is.na(gamm.est[1,])],nstat),sum(is.na(gamm.est[1,])),replace=TRUE)
          gamm.est[2,is.na(gamm.est[2,])] <- sample(rep(gamm.est[2,!is.na(gamm.est[2,])],nstat),sum(is.na(gamm.est[2,])),replace=TRUE)
        }
        
        for (s in 1:nstat)
          ylagged[seq(s,(nstat*init.buff), by=nstat),1] <-
          matrix(rgamma(init.buff, shape = gamm.est[1,s],
                        rate = gamm.est[2,s]),nrow=maxlag,ncol=1)
      } else {
        stop("GLM family unrecognized")
      }
    }
  }
  
  if (!is.null(occ.cond)){
    if (is.na(match(occ.cond, names(cov)) ))
      stop('For intensity variable, the corresponding occurrence variable must be in the data used to fit the GLM')
    var.name <- strsplit(var.name,".", fixed=TRUE)[[1]][1]
    ind0 <- which(cov[[occ.cond]][1:(nstat*init.buff)] == 0)
    ylagged[ind0] <- 0
  }
  
  ylagged <- data.frame(dates=newsim.agg$dates[1:(nstat*init.buff)], ylagged)
  names(ylagged)[2] <- var.name
  
  ## number of seasonal and diurnal effects + eventual covariates
  nvar.cycles <- ncol(newsim.agg)
  
  ## determine all variable names of memory effects
  if (spatave){
    ylagged <- spatave.effect(ylagged, var.name, nstat, na.proc)
  }
  if (movave){
    for (bb in bw)
      ylagged <- movave.effect(ylagged, var.name, bb, nstat, na.proc)
    if (spatave){
      for (bb in bw)
        ylagged <- movave.effect(ylagged, paste0(var.name,".spatave"), bb, nstat, na.proc)
    }
  }
  lag.names <- names(ylagged)[2:ncol(ylagged)]
  
  if (maxlag){
    nvar.lag <- length(lag.names)
    ylagged <- lagged.effect(ylagged, lag.names,maxlag, nstat)
    lag.names <- names(ylagged)[2:ncol(ylagged)]
  }
  
  
  
  ## start filling memory effects in the buffer of newsim.agg using simulated values
  newsim.agg <- data.frame(newsim.agg,
                           matrix(nrow=nrow(newsim.agg), ncol = length(lag.names)))
  names(newsim.agg)[(nvar.cycles+1):ncol(newsim.agg)] <-lag.names
  newsim.agg[1:(init.buff*nstat),lag.names] <- ylagged[,lag.names]
  
  for (t in (init.buff+1):length(tstep)){
    newsim.agg <- vec.effect.update(t, newsim.agg, nstat, na.proc,
                                    var.name,lag.names[1:nvar.lag],
                                    spatave, movave, bw, maxlag)
    
    
    newsim.agg[((1:nstat)+(t-1)*nstat),var.name] <- sim.glm(fit, newsim.agg[(1:nstat)+(t-1)*nstat,], fam.glm, occ.cond)
    
  }
  
  newsim.agg
}


fit.glm <- function(var.name, dep.var=NULL, geocov=TRUE, large.var, seasonal = TRUE, speriod = 365.25, 
                    diurnal = TRUE, dperiod = 24, spatave=TRUE, lagspat, movave = TRUE, bwM = 48, lagmov, spatmovave= TRUE, 
                    bwSM = 48, lagspatmov, lagvar, add.cov= FALSE, others=NULL,  fam.glm = "gaussian", data){
  
  formule <- paste0(var.name , "~", paste0(c(dep.var, large.var), collapse = " + "))
  if (geocov)
    formule=paste0(c(formule, paste0(c("coord.x", "coord.y"),collapse = " + ")), collapse = " + ")
  if (seasonal)
    formule <- paste0(c(formule, paste0(c("cos.","sin."),rep(speriod,each=2), "d", collapse = " + ")), collapse = " + ")
  if (diurnal)
    formule <- paste0(c(formule, paste0(c("cos.","sin."),rep(dperiod,each=2) ,"h", collapse = " + ")), collapse = " + ")
  
  if (var.name=="Precip.int")
     var.name="Precip"
  if (var.name=="Rg.int")
    var.name="Rg"
  if (spatave)
    formule <- paste0(c(formule,paste0(var.name,".spatave.lag", lagspat, collapse = " + ")), collapse = " + ")
  if (movave)
    for (i in 1:length(bwM)){
      formule <- paste0(c(formule,paste0(var.name,".movave", bwM[i], ".lag", lagmov, collapse = " + ")), collapse = " + ")}
  if (spatmovave)
    for (i in 1:length(bwSM)){
      formule <- paste0(c(formule,paste0(var.name,".spatave.movave", bwSM[i], ".lag", lagspatmov, collapse = " + ")), collapse = " + ")}
  if(lagvar)
    formule <- paste0(c(formule,paste0(var.name,".lag", lagvar, collapse = " + ")), collapse = " + ")
  if (add.cov)
    formule <- paste0(c(formule,paste0(others, collapse = " + ")), collapse = " + ")
  Var.fitted <- glm(as.formula(formule), fam.glm, data)
  Var.fitted
}

fit.lasso <- function(cov.mat, var.name){ ##cov.mat data frame that contain only var to fit and covariates
  cov.mat=na.omit(cov.mat)
  ind=which(names(cov.mat)==var.name)
  var=cov.mat[,ind]
  cov.mat=as.matrix(cov.mat[-ind])
  fit = cv.glmnet(cov.mat, var, family="gaussian", alpha = 1)
  coef(fit, s= fit$lambda.min)
}

UV2VitDir=function(U,V){
  Vit=sqrt(U^2+V^2)
  Dir= atan2(U,V)*(180/pi)+180
  
  cbind(Vit,Dir)  
  
}