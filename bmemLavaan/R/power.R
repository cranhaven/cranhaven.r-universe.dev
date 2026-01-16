## Monte Calor based power analysis for indirect effect using bootstrap
## by Zhiyong Zhang
summary.power<-function(object, ...) {
  
  # lavaan slots
  lavoptions <- lavInspect(object$out, "options")
  lavtest    <- lavInspect(object$out, "test")
  
  # Show some basic about the simulation methods
  
  # main part: parameter estimates
  cat("Basic information:\n\n")
  if(lavoptions$test %in% c("satorra.bentler", "yuan.bentler",
                            "mean.var.adjusted",
                            "scaled.shifted") &&
     length(lavtest) > 1L) {
    scaled <- TRUE
    if(lavoptions$test == "scaled.shifted")
      shifted <- TRUE
    else
      shifted <- FALSE
  } else {
    scaled <- FALSE
    shifted <- FALSE
  }
  
  t0.txt <- sprintf("  %-40s", "Esimation method")
  t1.txt <- sprintf("  %10s", lavoptions$estimator)
  t2.txt <- ifelse(scaled, 
                   sprintf("  %10s", "Robust"), "")
  cat(t0.txt, t1.txt, t2.txt, "\n", sep="")
  
  t0.txt <- sprintf("  %-40s", "Standard error")
  t1.txt <- sprintf("  %10s", lavoptions$se)
  cat(t0.txt, t1.txt, "\n", sep="")
  
  if (!is.null(object$info$bootstrap)){
    t0.txt <- sprintf("  %-40s", "Number of requested bootstrap")
    t1.txt <- sprintf("  %10i", object$info$bootstrap)
    cat(t0.txt, t1.txt, "\n", sep="")
  }
  
  t0.txt <- sprintf("  %-40s", "Number of requested replications")
  t1.txt <- sprintf("  %10i", object$info$nrep)
  cat(t0.txt, t1.txt, "\n", sep="")
  t0.txt <- sprintf("  %-40s", "Number of successful replications")
  t1.txt <- sprintf("  %10i", nrow(object$results$estimates))
  cat(t0.txt, t1.txt, "\n", sep="")
  
  cat("\n")
  
  # local print function
  print.estimate <- function(name="ERROR", i=1, z.stat=TRUE) {       
    # cut name if (still) too long
    name <- strtrim(name, width=15L)
    txt <- sprintf("    %-15s %8.3f %8.3f %8.3f %8.3f %8.3f %8.3f %8.3f\n",
                   name, pop.value[i], est[i], mse[i], sd.est[i], power[i], power.se[i], cvg[i])
    cat(txt)
  }
  
  est <- apply(object$results$estimates, 2, mean, na.rm=TRUE)
  sd.est  <- apply(object$results$estimates, 2, sd, na.rm=TRUE)
  mse <- apply(object$results$se, 2, mean, na.rm=TRUE)
  power <- object$power
  power.se<-object$power.se
  pop.value<-object$pop.value
  cvg<-object$coverage
  
  object<-object$out
  ngroups <- lavInspect(object, "ngroups")
  group.label <- lavInspect(object, "group.label")
  lavpartable <- parTable(object)
  
  for(g in 1:ngroups) {
    ov.names <- lavaanNames(object, group = g) 
    lv.names <- lavaanNames(object, type = "lv", group = g) 
    
    # group header
    if(ngroups > 1) {
      if(g > 1) cat("\n\n")
      cat("Group ", g, 
          " [", group.label[g], "]:\n\n", sep="")
    }
    
    # estimates header
    #txt <- sprintf("%-13s %12s %12s %8s %8s %8s\n", "", "True", "Estimate", "MSE", "SD", "Power")
    #cat(txt)
    cat("                       True  Estimate      MSE      SD     Power Power.se Coverage\n")
    
    makeNames <- function(NAMES, LABELS) {
      multiB <- FALSE
      if(any(nchar(NAMES) != nchar(NAMES, "bytes")))
        multiB <- TRUE
      if(any(nchar(LABELS) != nchar(LABELS, "bytes")))
        multiB <- TRUE
      # labels?
      l.idx <- which(nchar(LABELS) > 0L)
      if(length(l.idx) > 0L) {
        if(!multiB) {
          LABELS <- abbreviate(LABELS, 4)
          LABELS[l.idx] <- paste(" (", LABELS[l.idx], ")", sep="")
          MAX.L <- max(nchar(LABELS))
          NAMES <- abbreviate(NAMES, minlength = (13 - MAX.L), 
                              strict = TRUE)
        } else {
          # do not abbreviate anything (eg in multi-byte locales)
          MAX.L <- 4L
        }
        NAMES <- sprintf(paste("%-", (13 - MAX.L), "s%", MAX.L, "s",
                               sep=""), NAMES, LABELS)
      } else {
        if(!multiB) {
          NAMES <- abbreviate(NAMES, minlength = 13, strict = TRUE)
        } else {
          NAMES <- sprintf(paste("%-", 13, "s", sep=""), NAMES)
        }
      }
      
      NAMES
    }
    
    NAMES <- lavpartable$rhs
    
    # 1a. indicators ("=~") (we do show dummy indicators)
    mm.idx <- which( lavpartable$op == "=~" & 
                       !lavpartable$lhs %in% ov.names &
                       lavpartable$group == g)
    if(length(mm.idx)) {
      cat("Latent variables:\n")
      lhs.old <- ""
      NAMES[mm.idx] <- makeNames(  lavpartable$rhs[mm.idx],
                                   lavpartable$label[mm.idx])
      for(i in mm.idx) {
        lhs <- lavpartable$lhs[i]
        if(lhs != lhs.old) cat("  ", lhs, " =~\n", sep="")
        print.estimate(name=NAMES[i], i)
        lhs.old <- lhs
      }
      cat("\n")
    }
    
    # 1b. formative/composites ("<~")
    fm.idx <- which( lavpartable$op == "<~" &
                       lavpartable$group == g)
    if(length(fm.idx)) {
      cat("Composites:\n")
      lhs.old <- ""
      NAMES[fm.idx] <- makeNames(  lavpartable$rhs[fm.idx],
                                   lavpartable$label[fm.idx])
      for(i in fm.idx) {
        lhs <- lavpartable$lhs[i]
        if(lhs != lhs.old) cat("  ", lhs, " <~\n", sep="")
        print.estimate(name=NAMES[i], i)
        lhs.old <- lhs
      }
      cat("\n")
    }
    
    # 2. regressions
    eqs.idx <- which(lavpartable$op == "~" & lavpartable$group == g)
    if(length(eqs.idx) > 0) {
      cat("Regressions:\n")
      lhs.old <- ""
      NAMES[eqs.idx] <- makeNames(  lavpartable$rhs[eqs.idx],
                                    lavpartable$label[eqs.idx])
      for(i in eqs.idx) {
        lhs <- lavpartable$lhs[i]
        if(lhs != lhs.old) cat("  ", lhs, " ~\n", sep="")
        print.estimate(name=NAMES[i], i)
        lhs.old <- lhs
      }
      cat("\n")
    }
    
    # 3. covariances
    cov.idx <- which(lavpartable$op == "~~" & 
                       !lavpartable$exo &
                       lavpartable$lhs != lavpartable$rhs &
                       lavpartable$group == g)
    if(length(cov.idx) > 0) {
      cat("Covariances:\n")
      lhs.old <- ""
      NAMES[cov.idx] <- makeNames(  lavpartable$rhs[cov.idx],
                                    lavpartable$label[cov.idx])
      for(i in cov.idx) {
        lhs <- lavpartable$lhs[i]
        if(lhs != lhs.old) cat("  ", lhs, " ~~\n", sep="")
        print.estimate(name=NAMES[i], i)
        lhs.old <- lhs
      }
      cat("\n")
    }
    
    # 4. intercepts/means
    ord.names <- lavaanNames(object, type="ov.ord") 
    int.idx <- which(lavpartable$op == "~1" & 
                       !lavpartable$lhs %in% ord.names &
                       !lavpartable$exo &
                       lavpartable$group == g)
    if(length(int.idx) > 0) {
      cat("Intercepts:\n")
      NAMES[int.idx] <- makeNames(  lavpartable$lhs[int.idx],
                                    lavpartable$label[int.idx])
      for(i in int.idx) {
        print.estimate(name=NAMES[i], i)
      }
      cat("\n")
    }
    
    # 4b thresholds
    th.idx <- which(lavpartable$op == "|" &
                      lavpartable$group == g)
    if(length(th.idx) > 0) {
      cat("Thresholds:\n")
      NAMES[th.idx] <- makeNames(  paste(lavpartable$lhs[th.idx],
                                         "|",
                                         lavpartable$rhs[th.idx],
                                         sep=""),
                                   lavpartable$label[th.idx])
      for(i in th.idx) {
        print.estimate(name=NAMES[i], i)
      }
      cat("\n")
    }
    
    # 5. (residual) variances
    var.idx <- which(lavpartable$op == "~~" &
                       !lavpartable$exo &
                       lavpartable$lhs == lavpartable$rhs &
                       lavpartable$group == g)
    if(length(var.idx) > 0) {
      cat("Variances:\n")
      NAMES[var.idx] <- makeNames(  lavpartable$rhs[var.idx],
                                    lavpartable$label[var.idx])
      for(i in var.idx) {
        if(lavoptions$mimic == "lavaan") {
          print.estimate(name=NAMES[i], i, z.stat=FALSE)
        } else {
          print.estimate(name=NAMES[i], i, z.stat=TRUE)
        }
      }
      cat("\n")
    }
    
    # 6. latent response scales
    delta.idx <- which(lavpartable$op == "~*~" &
                         lavpartable$group == g)
    if(length(delta.idx) > 0) {
      cat("Scales y*:\n")
      NAMES[delta.idx] <- makeNames(  lavpartable$rhs[delta.idx],
                                      lavpartable$label[delta.idx])
      for(i in delta.idx) {
        print.estimate(name=NAMES[i], i, z.stat=TRUE)
      }
      cat("\n")
    }
    
  } # ngroups
  
  # 6. variable definitions
  def.idx <- which(lavpartable$op == ":=")
  if(length(def.idx) > 0) {
    if(ngroups > 1) cat("\n")
    cat("Indirect/Mediation effects:\n")
    NAMES[def.idx] <- makeNames(  lavpartable$lhs[def.idx], "")
    for(i in def.idx) {
      print.estimate(name=NAMES[i], i)
    }
    cat("\n")
  }
  
}

## bootstrap confidence intervals
popPar<-function(object){
  
  lavpartable <- parTable(object)
  
  par.value<-lavpartable$ustart
  for (i in 1:length(par.value)){
    if (is.na(par.value[i])){
      if (lavpartable$op[i]=="~~"){
        if (lavpartable$rhs[i]==lavpartable$lhs[i]){
          par.value[i]<-1
        }else{
          par.value[i]<-0
        }
      }else{
        par.value[i]<-0
      }
    }
  }
  names(par.value)<-lavpartable$label
  ## for indirect effec defined here
  for (i in 1:length(par.value)){
    if (lavpartable$op[i]==":="){
      temp<-lavpartable$rhs[i]
      temp<-gsub(' +','',temp)
      temp<-gsub('-','+', temp, fixed=TRUE)
      temp<-unlist(strsplit(temp, '+', fixed=TRUE))
      m<-length(temp)
      temp.est<-0
      par<-NULL
      for (j in 1:m){
        temp1<-unlist(strsplit(temp[j], '*', fixed=TRUE))
        par<-c(par, temp1)
      }
      ind.exp<-parse(text=lavpartable$rhs[i])
      par.list<-as.list(par.value[par])
      par.value[i]<-eval(ind.exp, par.list)
    }
  }
  par.value
}

## power function for analysis
power.basic<-function(model, nobs=100, nrep=1000, alpha=.95, skewness=NULL, kurtosis=NULL, ovnames=NULL, se="default", estimator="default", parallel=FALSE, ncore=1, verbose=TRUE, ...){
  if (missing(model)) stop("Error: missing value 'model' is needed")
  
  ngroups <- length(nobs)
  ## Initial analysis for some model information
  
  ## checking for error
  error <- 0
  while (error == 0){
    newdata<-try(simulateData(model,sample.nobs=nobs,skewness=0,kurtosis=0, ...))
    if (!inherits(newdata, "try-error")) error <- 1
  }
  
  if (ngroups > 1){
    error <- 0
    while (error == 0){
      temp.res<-try(lavaan::cfa(model, data=newdata, se=se, estimator=estimator, group='group', ...))
      if (!inherits(temp.res, "try-error")) error <- 1
    }
  }else{
    error <- 0
    while (error == 0){
      temp.res<-try(lavaan::cfa(model, data=newdata, se=se, estimator=estimator, ...))
      if (!inherits(temp.res, "try-error")) error <- 1
    }
  }
  par.value<-popPar(temp.res)
  
  temp.res.lavpartable <- parTable(temp.res)
  idx <- 1:length( temp.res.lavpartable$lhs )
  #cnames<-paste(temp.res.lavpartable$lhs[idx], temp.res.lavpartable$op[idx], temp.res.lavpartable$rhs[idx])
  ov<-lavaanNames(temp.res,type="ov")
  ## dealing with skewness and kurtosis
  if (length(skewness) > 1){
    if (length(skewness)!=length(ovnames)) stop("Error: the number of 'skewness' is not equal to the number observed variables")	
    index<-match(ov, ovnames)
    skewness<-skewness[index]
  }
  
  if (length(kurtosis) > 1){
    if (length(kurtosis)!=length(ovnames)) stop("Error: the number of 'kurtosis' is not equal to the number observed variables")	
    index<-match(ov, ovnames)
    kurtosis<-kurtosis[index]
  }
  
  cnames<-paste(temp.res.lavpartable$lhs[idx], temp.res.lavpartable$op[idx], temp.res.lavpartable$rhs[idx])
  out<-temp.res
  runonce<-function(i){
    ## Step 1: generate data
    error <- 0
    while (error == 0){
      newdata<-try(lavaan::simulateData(model,sample.nobs=nobs,skewness=skewness,kurtosis=kurtosis, ...)	)
      if (!inherits(newdata, "try-error")) error <- 1
    }
    
    ## Step 2: fit the model 		
    if (ngroups > 1){
      temp.res<-try(lavaan::cfa(model, data=newdata, se=se, estimator=estimator, group='group', warn=FALSE, ...))
    }else{
      temp.res<-try(lavaan::cfa(model, data=newdata, se=se, estimator=estimator, warn=FALSE, ...))
    }
    
    ## Step 3: Check significance
    
    if (!inherits(temp.res, "try-error")){
      temp.res.lavpartable <- lavaan::parTable(temp.res)
      idx <- 1:length( temp.res.lavpartable$lhs )
      temp.est<-temp.res.lavpartable$est[idx]
      temp.se<-temp.res.lavpartable$se[idx]
      temp.sig<-temp.est/temp.se
      crit<-qnorm(1-(1-alpha)/2)
      temp.sig.ind<-abs(temp.sig)>crit
      temp.sig.ind[!is.finite(temp.sig)]<-NA
      
      ## Step 4: Check the coverage
      ci.u<-temp.est+crit*temp.se
      ci.l<-temp.est-crit*temp.se
      temp.cvg<- (ci.u>par.value[idx]) & (ci.l<par.value[idx])
    }else{
      nna<-length(idx)
      temp.est<-rep(NA, nna)
      temp.sig.ind<-rep(NA, nna)
      temp.se<-rep(NA, nna)
      temp.cvg<-rep(NA, nna)
    }		
    return(list(temp.sig.ind=temp.sig.ind, temp.est=temp.est, temp.se=temp.se, temp.cvg=temp.cvg))
  }
  
  ## run parallel or not
  # this is from the boot function in package boot
  suppressWarnings(
  {RR <- nrep
  if (ncore > 1L && parallel) {
    snowfall::sfInit( parallel=TRUE, cpus=ncore )
    snowfall::sfLibrary("lavaan", character.only = TRUE)
    res <- snowfall::sfLapply(seq_len(RR), runonce)
    snowfall::sfStop()
  } else res <- lapply(seq_len(RR), runonce)
  })
  
  all.sig<-do.call(rbind, lapply(res, "[[", 'temp.sig.ind'))
  
  all.par<-do.call(rbind, lapply(res, "[[", 'temp.est'))
  all.se<-do.call(rbind, lapply(res, "[[", 'temp.se'))
  all.cvg<-do.call(rbind, lapply(res, "[[", 'temp.cvg'))
  
  colnames(all.sig)<-cnames
  power<-apply(all.sig, 2, mean, na.rm=TRUE)
  power.se<-sqrt(power*(1-power)/apply(!is.na(all.sig), 2, sum))
  cvg<-apply(all.cvg, 2, mean, na.rm=TRUE)
  info<-list(nobs=nobs, nrep=nrep, alpha=alpha, method="Normal", bootstrap=NULL)
  if(verbose){
    print(power)
  }
  object<-list(power=power, power.se=power.se, coverage=cvg, pop.value=par.value, results=list(estimates=all.par, se=all.se, all=res), info=info, out=out, data=newdata)
  class(object)<-'power'
  return(object)
}

power.robust<-function(model, nobs=100, nrep=1000, alpha=.95, skewness=NULL, kurtosis=NULL, ovnames=NULL, parallel=FALSE, ncore=1, verbose=TRUE, ...){
  if (missing(model)) stop("Error: missing value 'model' is needed")
  se="robust.huber.white"; estimator="MLR"
  
  ngroups <- length(nobs)
  ## Initial analysis for some model information
  
  ## checking for error
  error <- 0
  while (error == 0){
    newdata<-try(simulateData(model,sample.nobs=nobs,skewness=0,kurtosis=0, ...))
    if (!inherits(newdata, "try-error")) error <- 1
  }
  
  if (ngroups > 1){
    error <- 0
    while (error == 0){
      temp.res<-try(lavaan::cfa(model, data=newdata, se=se, estimator=estimator, group='group', ...))
      if (!inherits(temp.res, "try-error")) error <- 1
    }
  }else{
    error <- 0
    while (error == 0){
      temp.res<-try(lavaan::cfa(model, data=newdata, se=se, estimator=estimator, ...))
      if (!inherits(temp.res, "try-error")) error <- 1
    }
  }
  par.value<-popPar(temp.res)
  
  temp.res.lavpartable <- parTable(temp.res)
  idx <- 1:length( temp.res.lavpartable$lhs )
  #cnames<-paste(temp.res.lavpartable$lhs[idx], temp.res.lavpartable$op[idx], temp.res.lavpartable$rhs[idx])
  ov<-lavaanNames(temp.res,type="ov")
  ## dealing with skewness and kurtosis
  if (length(skewness) > 1){
    if (length(skewness)!=length(ovnames)) stop("Error: the number of 'skewness' is not equal to the number observed variables")	
    index<-match(ov, ovnames)
    skewness<-skewness[index]
  }
  
  if (length(kurtosis) > 1){
    if (length(kurtosis)!=length(ovnames)) stop("Error: the number of 'kurtosis' is not equal to the number observed variables")	
    index<-match(ov, ovnames)
    kurtosis<-kurtosis[index]
  }	
  
  cnames<-paste(temp.res.lavpartable$lhs[idx], temp.res.lavpartable$op[idx], temp.res.lavpartable$rhs[idx])
  out<-temp.res
  runonce<-function(i){
    ## Step 1: generate data
    error <- 0
    while (error == 0){
      newdata<-try(lavaan::simulateData(model,sample.nobs=nobs,skewness=skewness,kurtosis=kurtosis, ...)	)
      if (!inherits(newdata, "try-error")) error <- 1
    }
    
    ## Step 2: fit the model 		
    if (ngroups > 1){
      temp.res<-try(lavaan::cfa(model, data=newdata, se=se, estimator=estimator, group='group', warn=FALSE, ...))
    }else{
      temp.res<-try(lavaan::cfa(model, data=newdata, se=se, estimator=estimator, warn=FALSE, ...))
    }
    
    ## Step 3: Check significance
    
    if (!inherits(temp.res, "try-error")){
      temp.res.lavpartable <- lavaan::parTable(temp.res)
      idx <- 1:length( temp.res.lavpartable$lhs )
      temp.est<-temp.res.lavpartable$est[idx]
      temp.se<-temp.res.lavpartable$se[idx]
      temp.sig<-temp.est/temp.se
      crit<-qnorm(1-(1-alpha)/2)
      temp.sig.ind<-abs(temp.sig)>crit
      temp.sig.ind[!is.finite(temp.sig)]<-NA
      
      ## Step 4: Check the coverage
      ci.u<-temp.est+crit*temp.se
      ci.l<-temp.est-crit*temp.se
      temp.cvg<- (ci.u>par.value[idx]) & (ci.l<par.value[idx])
    }else{
      nna<-length(idx)
      temp.est<-rep(NA, nna)
      temp.sig.ind<-rep(NA, nna)
      temp.se<-rep(NA, nna)
      temp.cvg<-rep(NA, nna)
    }		
    return(list(temp.sig.ind=temp.sig.ind, temp.est=temp.est, temp.se=temp.se, temp.cvg=temp.cvg))
  }
  
  ## run parallel or not
  # this is from the boot function in package boot
  suppressWarnings(
  {RR <- nrep
  if (ncore > 1L && parallel) {
    snowfall::sfInit( parallel=TRUE, cpus=ncore )
    snowfall::sfLibrary("lavaan", character.only = TRUE)
    res <- snowfall::sfLapply(seq_len(RR), runonce)
    snowfall::sfStop()
  } else res <- lapply(seq_len(RR), runonce)
  })
  
  all.sig<-do.call(rbind, lapply(res, "[[", 'temp.sig.ind'))
  
  all.par<-do.call(rbind, lapply(res, "[[", 'temp.est'))
  all.se<-do.call(rbind, lapply(res, "[[", 'temp.se'))
  all.cvg<-do.call(rbind, lapply(res, "[[", 'temp.cvg'))
  
  colnames(all.sig)<-cnames
  power<-apply(all.sig, 2, mean, na.rm=TRUE)
  power.se<-sqrt(power*(1-power)/apply(!is.na(all.sig), 2, sum))
  cvg<-apply(all.cvg, 2, mean, na.rm=TRUE)
  info<-list(nobs=nobs, nrep=nrep, alpha=alpha, method="Robust", bootstrap=NULL)
  if(verbose){
    print(power)
  }
  object<-list(power=power, power.se=power.se, coverage=cvg, pop.value=par.value, results=list(estimates=all.par, se=all.se, all=res), info=info, out=out, data=newdata)
  class(object)<-'power'
  return(object)
}


power.boot<-function(model, nobs=100, nrep=1000, nboot=1000, alpha=.95, skewness=NULL, kurtosis=NULL, ovnames=NULL, ci='perc', boot.type='default', se="default", estimator="default", parallel=FALSE, ncore=1, verbose=TRUE, ...){		
  if (missing(model)) stop("Error: missing value 'model' is needed")
  if (ci!='perc' & ci!='bc') stop("Error: 'ci' must be 'perc' or 'bc'")
  if (parallel) parallel_lavaan <- "multicore"
  else parallel_lavaan <- "no"
  
  ## internal function
  coef.new<-function(x,...){
    coef(x, type='user', ...)
  }
  
  ngroups <- length(nobs)
  
  ## Initial analysis for some model information
  ## checking for error
  error <- 0
  while (error == 0){
    newdata<-try(simulateData(model,sample.nobs=nobs,skewness=0,kurtosis=0, ...))
    if (!inherits(newdata, "try-error")) error <- 1
  }
  
  if (ngroups > 1){
    error <- 0
    while (error == 0){
      temp.res<-try(lavaan::cfa(model, data=newdata, se=se, estimator=estimator, group='group', ...))
      if (!inherits(temp.res, "try-error")) error <- 1
    }
  }else{
    error <- 0
    while (error == 0){
      temp.res<-try(lavaan::cfa(model, data=newdata, se=se, estimator=estimator, ...))
      if (!inherits(temp.res, "try-error")) error <- 1
    }
  }
  temp.res.lavpartable <- parTable(temp.res)
  par.value<-popPar(temp.res)
  idx <- 1:length( temp.res.lavpartable$lhs )
  #cnames<-paste(temp.res.lavpartable$lhs[idx], temp.res.lavpartable$op[idx], temp.res.lavpartable$rhs[idx])
  ov<-lavaanNames(temp.res,type="ov")
  ptype<-(temp.res.lavpartable$op == "~~") & (temp.res.lavpartable$lhs == temp.res.lavpartable$rhs)
  
  ## dealing with skewness and kurtosis
  if (length(skewness) > 1){
    if (length(skewness)!=length(ovnames)) stop("Error: the number of 'skewness' is not equal to the number observed variables")	
    index<-match(ov, ovnames)
    skewness<-skewness[index]
  }
  
  if (length(kurtosis) > 1){
    if (length(kurtosis)!=length(ovnames)) stop("Error: the number of 'kurtosis' is not equal to the number observed variables")	
    index<-match(ov, ovnames)
    kurtosis<-kurtosis[index]
  }
  
  cnames<-paste(temp.res.lavpartable$lhs[idx], temp.res.lavpartable$op[idx], temp.res.lavpartable$rhs[idx])
  
  ci.bc1<-function(x, b, cl=.95){
    n<-length(x)
    z0<-qnorm(sum(x<b, na.rm=TRUE)/n)
    alpha<-(1-cl)/2
    alpha<-c(alpha, 1-alpha)
    alpha<-sort(alpha)
    alpha1<-alpha
    alpha<-pnorm(2*z0+qnorm(alpha))
    dig <- max(2L, getOption("digits"))
    np<-length(alpha)
    qs<-quantile(x, alpha, na.rm=TRUE)
    names(qs) <- paste(if (np < 100) 
      formatC(100 * alpha1, format = "fg", width = 1, digits = dig)
      else format(100 * alpha1, trim = TRUE, digits = dig), 
      "%", sep = "")
    qs
  }
  
  ci.bc<-function(par.boot, par0, cl=.95){
    se.boot<-apply(par.boot, 2, sd, na.rm=TRUE)
    estimate<-par0
    p<-ncol(par.boot)
    ci<-NULL
    for (i in 1:p){
      ci<-rbind(ci, ci.bc1(par.boot[,i], par0[i], cl))
    }
    cbind(estimate, se.boot, ci)
  }
  
  ci.perc<-function(par.boot, par0, cl=.95){
    se.boot<-apply(par.boot, 2, sd, na.rm=TRUE)
    estimate<-par0
    alpha<-(1-cl)/2
    alpha<-c(alpha, 1-alpha)
    ci<-apply(par.boot, 2, quantile, prob=alpha) 
    cbind(estimate, se.boot, t(ci))
  }
  
  ## repeated the following for nrep times
  runonce<-function(i){
    # Step 1: generate data
    error <- 0
    while (error == 0){
      newdata<-try(simulateData(model,sample.nobs=nobs,skewness=skewness,kurtosis=kurtosis, ...)	)
      if (!inherits(newdata, "try-error")) error <- 1
    }
    
    ## Step 2: fit the model 		
    #temp.res<-sem(model, data=newdata, se=se, estimator=estimator, ...)
    if (ngroups > 1){
      temp.res<-try(lavaan::cfa(model, data=newdata, se=se, estimator=estimator, group='group', warn=FALSE, ...))
    }else{
      temp.res<-try(lavaan::cfa(model, data=newdata, se=se, estimator=estimator, warn=FALSE, ...))
    }
    
    
    ## Step 3: Conduct bootstrap analysis
    if (!inherits(temp.res, "try-error")){
      orig.res<-coef.new(temp.res)
      if (boot.type=="default"){
        boot.res<-bootstrapLavaan(temp.res, FUN=coef.new, R=nboot, parallel=parallel_lavaan, ncpus=ncore,warn=FALSE, ...)
      }else{
        boot.res<-bootstrapLavaan(temp.res, FUN=coef.new, R=nboot, type='bollen.stine', parallel=parallel_lavaan, ncpus=ncore,warn=FALSE, ...)
      }
      if (ci=='perc'){
        ci.res<-ci.perc(boot.res, orig.res, cl=alpha)
      }else{
        ci.res<-ci.bc(boot.res, orig.res, cl=alpha)
      }
      ## Step 4: Check the coverage		
      
      temp.cvg<- (ci.res[,4]>=par.value[idx]) & (ci.res[,3]<par.value[idx])
      
      ## Step 5: check significance
      temp.sig<-NULL
      temp.est<-coef.new(temp.res) 
      temp.se<-ci.res[,2]
      for (jj in 1:length(ptype)){
        if (ptype){
          crit<-qnorm(1-(1-alpha)/2)
          ci.temp<-ci.res[jj, 1] + c(-1,1)*ci.res[jj, 2]*crit
          temp.sig<-c(temp.sig, (ci.temp[1]>0 | ci.temp[2]<0))
        }else{
          temp.sig<-c(temp.sig, (ci.res[jj, 3]>0 | ci.res[jj, 4]<0))
        }
      }
    }else{
      nna<-length(idx)
      temp.est<-rep(NA, nna)
      temp.sig<-rep(NA, nna)
      temp.se<-rep(NA, nna)
      temp.cvg<-rep(NA, nna)
      boot.res<-NA
      ci.res<-NA
    }
    
    return(list(temp.sig=temp.sig, temp.est=temp.est, temp.se=temp.se, temp.cvg=temp.cvg, boot.res=boot.res, ci.res=ci.res))
  }
  
  ## run parallel or not
  suppressWarnings(
  {RR <- nrep
  if (ncore > 1L && parallel) {
    snowfall::sfInit( parallel=TRUE, cpus=ncore )
    snowfall::sfLibrary("lavaan", character.only = TRUE)
    res <- snowfall::sfLapply(seq_len(RR), runonce)
    snowfall::sfStop()
  } else res <- lapply(seq_len(RR), runonce)
  })
  
  all.sig<-do.call(rbind, lapply(res, "[[", 'temp.sig'))
  
  all.par<-do.call(rbind, lapply(res, "[[", 'temp.est'))
  all.se<-do.call(rbind, lapply(res, "[[", 'temp.se'))
  all.cvg<-do.call(rbind, lapply(res, "[[", 'temp.cvg'))

  colnames(all.sig)<-cnames
  power<-apply(all.sig, 2, mean, na.rm=TRUE)
  power.se<-sqrt(power*(1-power)/apply(!is.na(all.sig), 2, sum))
  cvg<-apply(all.cvg, 2, mean, na.rm=TRUE)
  info<-list(nobs=nobs, nrep=nrep, alpha=alpha, method="Bootstrap", bootstrap=nboot)
  if(verbose){
    print(power)
  }
  object<-list(power=power, power.se=power.se, coverage=cvg, pop.value=par.value, results=list(estimates=all.par, se=all.se, all=res), info=info, out=temp.res, data=newdata)
  class(object)<-'power'
  return(object)
}


power.curve<-function(model, nobs=seq(100, 2000, 200), method='normal', nrep=1000, nboot=1000, alpha=.95, skewness=NULL, kurtosis=NULL, ovnames=NULL,  ci='perc', boot.type='default', se="default", estimator="default", parallel=FALSE, ncore=1, interactive=TRUE, ...){		
  if (missing(model)) stop("Error: missing value 'model' is needed")
  
  allpower<-NULL
  
  ## check whether nobs is a vector or not
  if (is.vector(nobs)){	
    for (N in nobs){
      if (method == 'normal'){
        ## sobel test based analysis
        indpower <- power.basic(model=model, nobs=N, nrep=nrep, alpha=alpha, skewness=skewness, kurtosis=kurtosis, ovnames=ovnames, se=se, estimator=estimator, parallel=parallel, ncore=ncore,  ...)			
      }else{
        if (method == 'boot'){
          indpower <- power.boot(model=model, nobs=N, nrep=nrep, nboot=nboot, alpha=alpha, skewness=skewness, kurtosis=kurtosis, ovnames=ovnames, se=se, ci=ci, boot.type=boot.type, estimator=estimator, parallel=parallel, ncore=ncore,  ...)	
        }else{
          if (method == 'robust'){
            indpower <- power.robust(model=model, nobs=N, nrep=nrep, alpha=alpha, skewness=skewness, kurtosis=kurtosis, ovnames=ovnames, se=se, estimator=estimator, parallel=parallel, ncore=ncore,  ...)			
          }else{stop("Error: 'method' must be 'normal', 'robust' or 'boot'")}
        }
      }
      allpower<-rbind(allpower, indpower$power)
    }
  }else{
    for (k in 1:nrow(nobs)){
      N <- nobs[k]
      if (method == 'basic'){
        ## sobel test based analysis
        indpower <- power.basic(model=model, nobs=N, nrep=nrep, alpha=alpha, skewness=skewness, kurtosis=kurtosis, ovnames=ovnames, se=se, estimator=estimator, parallel=parallel, ncore=ncore,  ...)			
      }else{
        if (method == 'boot'){
          indpower <- power.boot(model=model, nobs=N, nrep=nrep, nboot=nboot, alpha=alpha, skewness=skewness, kurtosis=kurtosis, ovnames=ovnames, se=se, ci=ci, boot.type=boot.type, estimator=estimator, parallel=parallel, ncore=ncore,  ...)	
        }else{
          if (method == 'robust'){
            indpower <- power.robust(model=model, nobs=N, nrep=nrep, alpha=alpha, skewness=skewness, kurtosis=kurtosis, ovnames=ovnames, se=se, estimator=estimator, parallel=parallel, ncore=ncore,  ...)			
          }else{stop("Error: 'method' must be 'normal', 'robust' or 'boot'")}
        }
      }
      allpower<-rbind(allpower, indpower$power)
    }
  }
  
  if (interactive & .Platform$OS.type=="windows"){
    dev.new(record=TRUE)
    op <- par(ask=TRUE)
    on.exit(par(op))
  }
  pnames <- colnames(allpower)
  for (j in 1:ncol(allpower)){
    if (sum(is.nan(allpower[,j])) == 0){
      if (is.vector(nobs)){
        plot(nobs, allpower[, j], ylab=paste('Power of', pnames[j]), xlab='Sample size')
        lines(nobs, allpower[, j])
      }else{
        N<-apply(nobs, 1, sum)
        plot(N, allpower[, j], ylab=paste('Power of', pnames[j]), xlab='Sample size')
        lines(N, allpower[, j])
      }
    }
  }
  invisible(allpower)
}

power.bmem<-function(model, method="normal", nobs=100, nrep=1000, nboot=1000, alpha=.95, skewness=NULL, kurtosis=NULL, ovnames=NULL, ci='perc', boot.type='default', se="default", estimator="default", parallel=FALSE, ncore=1, verbose=TRUE, ...){		
  if (missing(model)) stop("Error: missing value 'model' is needed")	
  
  if (method == 'normal'){
    power.basic(model = model, nobs=nobs, nrep=nrep, alpha=alpha, skewness=skewness, kurtosis=kurtosis, ovnames=ovnames,parallel = parallel, se=se, estimator = estimator, ncore = ncore, verbose = verbose, ...)
  }else{
    if (method == 'robust'){
      power.robust(model = model, nobs = nobs, nrep = nrep,alpha = alpha, skewness = skewness, kurtosis = kurtosis, ovnames = ovnames, parallel = parallel, ncore = ncore, verbose = verbose, ...)
    }
    else{
      if (method == 'boot'){
        power.boot(model = model, nobs=nobs,nboot = nboot, nrep=nrep, alpha=alpha, skewness=skewness, kurtosis=kurtosis, ovnames=ovnames, ci=ci, boot.type = boot.type, parallel = parallel, se=se, estimator = estimator, ncore = ncore, verbose = verbose,...)
      }else{stop("Error: 'method' must be 'normal', 'robust' or 'boot'")}
    }
  }
  
}

