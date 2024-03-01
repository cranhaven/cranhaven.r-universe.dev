
eventdepenobs <- function(formula, indiv, astart, aend, aevent, adrug, aedrug, censor, expogrp = list(), 
                          washout = list(), sameexpopar = list(), agegrp = NULL, dataformat="stack", 
                          covariates=NULL, regress=F, initval=rep(0.1,7), data){
  
  if (isTRUE(regress) & length(initval)<7){
    
    stop("The number of initial values must be 7 for regress=T")
  }
  
  
  if (dataformat!="multi" & dataformat!="stack"){
    
    stop("Please input dataformat as multi or stack")
  }
  
  yon <- deparse(substitute(adrug)) 
  yon1 <- as.formula(paste("z", "~", yon)) 
  adrugcolnames <- all.vars(yon1, functions = FALSE, unique = TRUE)[-1] 
  # colname  <- deparse(substitute(adrug))
  adrug  <- eval(substitute(adrug), data, parent.frame())
  
  # Changing adrug to a list if given as cbind(adrug1, adrug2,...) or adrug not as a list
  
  if ((dataformat=="multi" & !is.null(ncol(adrug)))) {
    adrug <- data.frame(adrug)
    adrug <- list(adrug) 
  } else if (dataformat=="stack" & !is.null(ncol(adrug))){
    adrug <- data.frame(adrug)
    adrug1 <- list()
    for (i in 1:ncol(adrug)){
      adrug1[[i]] <- adrug[,i]
    }
    adrug <- adrug1
  } else if (length(adrugcolnames)==1 & length(adrug)!=1) {
    adrug <- list(adrug)
    
  } else {
    adrug <- adrug
  }
  
  
  for (i in 1:length(adrug)){
    adrug[[i]] <- data.frame(adrug[[i]])
  }
  
  ncoladrug <- NULL
  for (i in 1:length(adrug)){
    ncoladrug[i] <- ncol(adrug[[i]]) 
  }
  
  
  for (i in 1:length(adrug)) {
    colnames(adrug[[i]]) <- adrugcolnames[c(1, cumsum(ncoladrug)+1)[-(length(ncoladrug)+1)][i]:cumsum(ncoladrug)[i]]
  }
  
  colname  <- adrugcolnames
  
  # getting the arguments from the data 
  indiv  <- eval(substitute(indiv), data, parent.frame())
  astart <- eval(substitute(astart), data, parent.frame())
  aend   <- eval(substitute(aend), data, parent.frame())
  aevent <- eval(substitute(aevent), data, parent.frame())
  aedrug <- eval(substitute(aedrug), data, parent.frame())
  present<- 1- eval(substitute(censor), data, parent.frame())
  covariates<-eval(substitute(covariates), data, parent.frame())
  
  # Changing aedrug to a list if given as cbind(aedrug1, aedrug2,...) or aedrug not as a list
  
  if ((dataformat=="multi" & !is.null(ncol(aedrug)))) {
    aedrug <- data.frame(aedrug)
    aedrug <- list(aedrug) 
  } else if (dataformat=="stack" & !is.null(ncol(aedrug))){
    aedrug <- data.frame(aedrug)
    aedrug1 <- list()
    for (i in 1:ncol(aedrug)){
      aedrug1[[i]] <- aedrug[,i]
    }
    aedrug <- aedrug1
  } else if (length(adrugcolnames)==1 & length(aedrug)!=1) {
    aedrug <- list(aedrug)
    
  } else {
    aedrug <- aedrug
  }
  
  # data$present <- ifelse(data$aend>=data$aestudy, 1, 0)
  # data$aend<-ifelse(data$aend==data$aevent,data$aend+0.5, data$aend)
  aend<-ifelse(aend==aevent,aend+1, aend)
  
  # qq <- all.vars(as.formula(formula))[-c(which(all.vars(as.formula(formula))=="age"), which(all.vars(as.formula(formula))=="season"), which(all.vars(as.formula(formula))=="event"))]
  qq <- all.vars(as.formula(formula))[-c(which(all.vars(as.formula(formula))=="age"), which(all.vars(as.formula(formula))=="event"))]
  
  if (length(qq)==0) {
    cov <- cbind()
  }   else {
    cova <- qq[is.na(match(qq, colname))]
    cov <- data.frame(data[, cova])
    colnames(cov) <- cova
  }
  
  
  
  
  # Extruct a data frame of unique indiv, astart, aend, aevent and cen 
  
  data1 <- data.frame(unique(cbind(indiv, aevent, astart, aend, present, covariates)))
   # data1 sorted by indiv - 09-12-21
  data1 <- data1[order(data1$indiv), ]
  
  # COvariates from data1
  
  if (ncol(data1)>5){
    covariatesp <- data1[,6:ncol(data1)]
    
  } else {
    
    covariatesp <- NULL
  }
  
  #------------------------------------#
  # Design matrix involving covariates #
  #------------------------------------#
  
  # A function used to create a design matirix when there are no covariates
  # If there are no covariates the design matrix will have one column of 1's
  # if not the covariates matrix will be processed to create dummyvar variables.
  # If there are no covariates the covariates=NULL is given
  
  covariates1 <- cbind(covariatesp, rep(1, nrow(data1)))
  
  dd <- function(x,y) {
    if (ncol(x)==1) {
      rrr <- x
      return(rrr)
    }
    rrr1 <- y
    return(rrr1)
  }
  
  covariates2 <- data.frame(dd(covariates1, covariatesp))
  
  
  
  #-----------------new -----------------------------#
  # Design matrix of the covariates
  
  Dmatrix1 <- matrix(1, nrow=nrow(data1),ncol=1)
  
  Dmatrix2 <- list()
  
  for (j in 1:ncol(covariates2)) {
    Dmatrix2[[j]] <- Yproduct(Dmatrix1, dummyvar(covariates2[,j], data = NULL, sep = "", drop = TRUE, fun = as.integer, verbose = FALSE))
  }
  
  Dmatrix <-NULL
  for (i in 1:length(Dmatrix2)) {
    
    Dmatrix <- cbind(Dmatrix, Dmatrix2[[i]])
    
  }
  
  #-------------------------------------------------#
  
  
  # Number of parameters in the models
  
  # npar <- ncol(Dmatrix) + 3*2*(ncol(Dmatrix))
  
  
  #--------------------------------------------------------#
  #       Exponential- Weibull (Age) mixture Model         #
  #--------------------------------------------------------#
  
  mod_ewad2<-function(p, astart, aevent, aend, present, Dmatrix, regress){
    
    if (isTRUE(regress)){
      Dmatrixevent <- cbind(Dmatrix, Yproduct(Dmatrix, as.matrix(aevent)))
      Dmatrixeventlog <- cbind(Dmatrix, Yproduct(Dmatrix, as.matrix(log(aevent))))
      
      Dmatrixstart <- cbind(Dmatrix, Yproduct(Dmatrix, as.matrix(astart)))
      Dmatrixstartlog <- cbind(Dmatrix, Yproduct(Dmatrix, as.matrix(log(astart))))
      
      thetaA <- Dmatrix%*%p[1:ncol(Dmatrix)] #  (rho)
      thetaB <- Dmatrixstartlog%*%p[(ncol(Dmatrix)+1):(3*(ncol(Dmatrix)))] # log(u(t,y))
      eta    <- Dmatrixevent%*%p[((3*(ncol(Dmatrix))) + 1):(5*(ncol(Dmatrix)))]  # log(pi(t,y))
      gamma0 <- Dmatrixstartlog%*%p[((5*(ncol(Dmatrix))) + 1):(7*(ncol(Dmatrix)))]  # log(nu(t,y))
      
    } else {
      
      Dmatrixevent <- cbind(Dmatrix)
      Dmatrixeventlog <- cbind(Dmatrix)
      
      Dmatrixstart <- cbind(Dmatrix)
      Dmatrixstartlog <- cbind(Dmatrix)
      
      thetaA <- Dmatrix%*%p[1:ncol(Dmatrix)] #  (rho)
      thetaB <- Dmatrixstartlog%*%p[(ncol(Dmatrix)+1):(2*(ncol(Dmatrix)))] # log(u(t,y))
      eta    <- Dmatrixevent%*%p[((2*(ncol(Dmatrix))) + 1):(3*(ncol(Dmatrix)))]  # log(pi(t,y))
      gamma0 <- Dmatrixstartlog%*%p[((3*(ncol(Dmatrix))) + 1):(4*(ncol(Dmatrix)))]  # log(nu(t,y))
      
      
    }
    
    lamA<-exp(-thetaA)            # 1/rho in the paper
    lamB<-exp(-thetaB)            # 1/mu
    pi0 <-exp(eta)/(1+exp(eta))   # pi
    nu0<-exp(gamma0)              # nu
    
    
    
    lik<- ((1-present)*log(pi0*lamA*exp(-lamA*(aend-aevent))+
                             (1-pi0)*nu0*lamB*((aend*lamB)^(nu0-1))*exp(-((aend*lamB)^nu0-(aevent*lamB)^nu0))) +
             present *log(pi0*exp(-lamA*(aend-aevent))+
                            (1-pi0)*exp(-((aend*lamB)^nu0-(aevent*lamB)^nu0))))
    l<-(-2)*sum(lik)
    l
  }
  
  #--------------------------------------------------------#
  #       Exponential- Weibull (Interval) mixture Model    #
  #--------------------------------------------------------#
  # Put int inside the function
  mod_ewid2 <-function(p, aevent, aend, present, Dmatrix, regress){
    
    if (isTRUE(regress)) {
      
      Dmatrixevent <- cbind(Dmatrix, Yproduct(Dmatrix, as.matrix(aevent)))
      Dmatrixeventlog <- cbind(Dmatrix, Yproduct(Dmatrix, as.matrix(log(aevent))))
      
      thetaA <- Dmatrix%*%p[1:ncol(Dmatrix)] #  (rho)
      thetaB <- Dmatrixeventlog%*%p[(ncol(Dmatrix)+1):(3*(ncol(Dmatrix)))] # log(u(t,y))
      eta <- Dmatrixevent%*%p[((3*(ncol(Dmatrix))) + 1):(5*(ncol(Dmatrix)))]  # log(pi(t,y))
      gamma0 <- Dmatrixeventlog%*%p[((5*(ncol(Dmatrix))) + 1):(7*(ncol(Dmatrix)))]  # log(nu(t,y))
      
    } else {
      
      Dmatrixevent <- cbind(Dmatrix)
      Dmatrixeventlog <- cbind(Dmatrix)
      
      thetaA <- Dmatrix%*%p[1:ncol(Dmatrix)] #  (rho)
      thetaB <- Dmatrixeventlog%*%p[(ncol(Dmatrix)+1):(2*(ncol(Dmatrix)))] # log(u(t,y))
      eta <- Dmatrixevent%*%p[((2*(ncol(Dmatrix))) + 1):(3*(ncol(Dmatrix)))]  # log(pi(t,y))
      gamma0 <- Dmatrixeventlog%*%p[((3*(ncol(Dmatrix))) + 1):(4*(ncol(Dmatrix)))]  # log(nu(t,y))
      
    }
    
    
    lamA<-exp(-thetaA)            # 1/rho in the paper
    lamB<-exp(-thetaB)            # 1/mu
    pi0 <-exp(eta)/(1+exp(eta))   # pi
    nu0<-exp(gamma0)              # nu
    
    int <- aend-aevent
    lik<-
      
      ((1-present)*log(pi0*lamA*exp(-lamA*int)+
                         (1-pi0)*nu0*lamB*((int*lamB)^(nu0-1))*exp(-((int*lamB)^nu0))) +
         
         present *log(pi0*exp(-lamA*int)+
                        (1-pi0)*exp(-((int*lamB)^nu0))))
    l<-(-2)*sum(lik)
    l
  }
  
  #--------------------------------------------------------#
  #       Exponential- Gamma (Age) mixture Model           #
  #--------------------------------------------------------#
  
  mod_egad2<-function(p, astart, aevent, aend, present, Dmatrix, regress){
    
    if (isTRUE(regress)) {
      
      Dmatrixevent <- cbind(Dmatrix, Yproduct(Dmatrix, as.matrix(aevent)))
      Dmatrixeventlog <- cbind(Dmatrix, Yproduct(Dmatrix, as.matrix(log(aevent))))
      
      Dmatrixstart <- cbind(Dmatrix, Yproduct(Dmatrix, as.matrix(astart)))
      Dmatrixstartlog <- cbind(Dmatrix, Yproduct(Dmatrix, as.matrix(log(astart))))
      
      thetaA <- Dmatrix%*%p[1:ncol(Dmatrix)] #  (rho)
      thetaB <- Dmatrixstartlog%*%p[(ncol(Dmatrix)+1):(3*(ncol(Dmatrix)))] # log(u(t,y))
      eta    <- Dmatrixevent%*%p[((3*(ncol(Dmatrix))) + 1):(5*(ncol(Dmatrix)))]  # log(pi(t,y))
      gamma0 <- Dmatrixstartlog%*%p[((5*(ncol(Dmatrix))) + 1):(7*(ncol(Dmatrix)))]  # log(nu(t,y))
      
    } else {
      
      Dmatrixevent <- cbind(Dmatrix)
      Dmatrixeventlog <- cbind(Dmatrix)
      
      Dmatrixstart <- cbind(Dmatrix)
      Dmatrixstartlog <- cbind(Dmatrix)
      
      thetaA <- Dmatrix%*%p[1:ncol(Dmatrix)] #  (rho)
      thetaB <- Dmatrixstartlog%*%p[(ncol(Dmatrix)+1):(2*(ncol(Dmatrix)))] # log(u(t,y))
      eta    <- Dmatrixevent%*%p[((2*(ncol(Dmatrix))) + 1):(3*(ncol(Dmatrix)))]  # log(pi(t,y))
      gamma0 <- Dmatrixstartlog%*%p[((3*(ncol(Dmatrix))) + 1):(4*(ncol(Dmatrix)))]  # log(nu(t,y))
      
      
    }
    
    lamA <-exp(-thetaA)            # 1/rho in the paper
    lamB <-exp(-thetaB)            # 1/mu
    pi0  <-exp(eta)/(1+exp(eta))   # pi
    nu0  <-exp(gamma0)              # nu
    
    rate0 <-nu0*lamB
    
    
    # lik<-((1-present)*log(pi0*lamA*exp(-lamA*(aend-aevent))+
    #                 (1-pi0)*dgamma(aend,shape=nu0,rate=rate0)/pgamma(aevent,shape=nu0,rate=rate0,lower.tail=F)) +
    #                    present *log(pi0*exp(-lamA*(aend-aevent))+
    #                 (1-pi0)*pgamma(aend,shape=nu0,rate=rate0,lower.tail=F)/pgamma(aevent,shape=nu0,rate=rate0,lower.tail=F)))
    
    lik<-((1-present)*log(pi0*lamA*exp(-lamA*(aend-aevent))+
                            (1-pi0)*dgamma(aend,shape=nu0,rate=rate0)/ifelse(pgamma(aevent,shape=nu0,rate=rate0,lower.tail=F)==0,0.000000001, pgamma(aevent,shape=nu0,rate=rate0,lower.tail=F))) +
            present *log(pi0*exp(-lamA*(aend-aevent))+
                           (1-pi0)*pgamma(aend,shape=nu0,rate=rate0,lower.tail=F)/ifelse(pgamma(aevent,shape=nu0,rate=rate0,lower.tail=F)==0, 0.000000001, pgamma(aevent,shape=nu0,rate=rate0,lower.tail=F))))
    
    l <-(-2)*sum(lik)
    l
  }
  
  #--------------------------------------------------------#
  #       Exponential- Gamma (Interval) mixture Model      #
  #--------------------------------------------------------#
  
  # Put int inside the function
  mod_egid2<-function(p, aevent, aend, present, Dmatrix, regress){
    
    if (isTRUE(regress)) {
      
      
      Dmatrixevent <- cbind(Dmatrix, Yproduct(Dmatrix, as.matrix(aevent)))
      Dmatrixeventlog <- cbind(Dmatrix, Yproduct(Dmatrix, as.matrix(log(aevent))))
      
      thetaA <- Dmatrix%*%p[1:ncol(Dmatrix)] #  (rho)
      thetaB <- Dmatrixeventlog%*%p[(ncol(Dmatrix)+1):(3*(ncol(Dmatrix)))] # log(u(t,y))
      eta <- Dmatrixevent%*%p[((3*(ncol(Dmatrix))) + 1):(5*(ncol(Dmatrix)))]  # log(pi(t,y))
      gamma0 <- Dmatrixeventlog%*%p[((5*(ncol(Dmatrix))) + 1):(7*(ncol(Dmatrix)))]  # log(nu(t,y))
      
    } else {
      
      Dmatrixevent <- cbind(Dmatrix)
      Dmatrixeventlog <- cbind(Dmatrix)
      
      thetaA <- Dmatrix%*%p[1:ncol(Dmatrix)] #  (rho)
      thetaB <- Dmatrixeventlog%*%p[(ncol(Dmatrix)+1):(2*(ncol(Dmatrix)))] # log(u(t,y))
      eta <- Dmatrixevent%*%p[((2*(ncol(Dmatrix))) + 1):(3*(ncol(Dmatrix)))]  # log(pi(t,y))
      gamma0 <- Dmatrixeventlog%*%p[((3*(ncol(Dmatrix))) + 1):(4*(ncol(Dmatrix)))]  # log(nu(t,y))
      
    }
    
    lamA<-exp(-thetaA)            # 1/rho in the paper
    lamB<-exp(-thetaB)            # 1/mu
    pi0 <-exp(eta)/(1+exp(eta))   # pi
    nu0<-exp(gamma0)              # nu
    
    rate0 <-nu0*lamB
    
    int <- aend - aevent
    lik<-((1-present)*log(pi0*lamA*exp(-lamA*int)+
                            (1-pi0)*dgamma(int,shape=nu0,rate=rate0)) +
            present *log(pi0*exp(-lamA*int)+
                           (1-pi0)*pgamma(int,shape=nu0,rate=rate0,lower.tail=F)))
    
    
    l<-(-2)*sum(lik)
    l
  }
  #-----------------------------------#
  #    Fit the four mixture models    #
  #-----------------------------------#
  
  # data driven intial values
  
  ##if (is.null(initval)) {
  
  #  if (isTRUE(regress)) { 
  #    p01 <- c(log(mean(data1$aend[data1$present==0]-data1$aevent[data1$present==0])), 
  #             log(mean(data1$aend[data1$present==1]-data1$aevent[data1$present==1])),
  #             0,
  #             log(2),
  #             0,
  #             log((sum(1-data1$present))/sum(data1$present)),
  #             0)
  #  } else {
  
  #    p01 <- c(log(mean(data1$aend[data1$present==0]-data1$aevent[data1$present==0])), 
  #             log(mean(data1$aend[data1$present==1]-data1$aevent[data1$present==1])),
  #             
  #             log(2),
  #             
  #             log((sum(1-data1$present))/sum(data1$present)))
  #  }
  #  
  #} else {
  
  #  if (isTRUE(regress)) {
  #    p01 <- c(initval[1], initval[2], initval[5], initval[3], initval[6], initval[4], initval[7])
  
  #  } else {
  
  #   p01 <- initval[1:4]
  #  }
  #}
  # Defaults is rep(0.1, 7)
  
  if (isTRUE(regress)) {
    p01 <- c(initval[1], initval[2], initval[5], initval[3], initval[6], initval[4], initval[7])
    
  } else {
    
    p01 <- initval[1:4]
  }
  
  p0 <- rep(p01, each=ncol(Dmatrix))
  npar <- length(p0)
  
  #  p0 <- rep(0.1, times=npar)   # inital values
  options(warn=-1) 
  fit_ewad2 <- nlm(mod_ewad2, p=p0, astart=data1$astart/365.25, aevent=data1$aevent/365.25, aend=data1$aend/365.25, present=data1$present, Dmatrix=Dmatrix,
                   regress=regress, hessian = TRUE, iterlim=100)
  
  
  EWA <- c((fit_ewad2$minimum/-2), (2*npar + fit_ewad2$minimum))   # Loglikelihood and AIC values of the fited model
  AIC_EWA <- (2*npar + fit_ewad2$minimum)                        # AIC
  p_ewad2 <- fit_ewad2$estimate                                     # Parameter estimates
  
  
  fit_ewid2 <- nlm( mod_ewid2, p=p0, aevent=data1$aevent/365.25, aend=data1$aend/365.25, present=data1$present, Dmatrix=Dmatrix,
                    regress=regress, iterlim=100,  hessian = TRUE)
  
  EWI     <- c((fit_ewid2$minimum/-2), (2*npar + fit_ewid2$minimum))
  AIC_EWI <- (2*npar + fit_ewid2$minimum)
  p_ewid2 <- fit_ewid2$estimate
  
  
  fit_egad2 <- nlm(mod_egad2, p=p0, astart=data1$astart/365.25, aevent=data1$aevent/365.25, aend=data1$aend/365.25, present=data1$present, Dmatrix=Dmatrix,
                   regress=regress, iterlim=100,  hessian = TRUE)
  
  
  EGA <- c((fit_egad2$minimum/-2), (2*npar + fit_egad2$minimum))
  AIC_EGA <- (2*npar + fit_egad2$minimum)
  p_egad2 <- fit_egad2$estimate
  
  fit_egid2 <- nlm(mod_egid2, p=p0, aevent=data1$aevent/365.25, aend=data1$aend/365.25, present=data1$present, Dmatrix=Dmatrix,
                   regress=regress, iterlim=100,  hessian = TRUE)
  
  EGI <- c((fit_egid2$minimum/-2), (2*npar + fit_egid2$minimum))
  AIC_EGI <- (2*npar + fit_egid2$minimum)
  p_egid2 <- fit_egid2$estimate
  
  # Value <- c("Loglik", "AIC")
  modelfit <- cbind(EWA, EWI, EGA, EGI)    # Loglikelihood and AIC values of the four models
  rownames(modelfit) <- c("Loglik", "AIC")
  AICs <- c(AIC_EWA, AIC_EWI, AIC_EGA, AIC_EGI)
  
  #-----------------------------------------------------------#
  #  Formatting the data based on the age and exposure groups #
  #-----------------------------------------------------------#
  
  # chopdat <- formatdata(adrug, aedrug, agegrp, expogrp, washout, sameexpopar, data=data) # Expanded data based on the age and exposure groups
  chopdat <- formatdata(indiv=indiv, astart=astart, aend=aend, aevent=aevent, adrug=adrug, aedrug=aedrug, expogrp = expogrp, washout = washout , 
                        sameexpopar = sameexpopar, agegrp = agegrp, cov=cov, dataformat=dataformat, data=NULL)
  
  chopdat$present <- rep(data1$present, times = data.frame((table(chopdat$indivL)))$Freq)
  
  # --------------------------------------------------#
  # Design matrix of covariates in the chopped data   #
  # --------------------------------------------------#
  #----------------------------------new ----------------------------------#
  Dmatrix_weights1 <- matrix(1, nrow=nrow(chopdat),ncol=1)
  
  covariates_weight <-  data.frame(apply(covariates2, 2, function(x) rep(x, times = data.frame((table(chopdat$indivL)))$Freq)))
  
  Dmatrix_weights2 <- list()
  
  for (j in 1:ncol(covariates2)) {
    Dmatrix_weights2[[j]] <- Yproduct(Dmatrix_weights1, dummyvar(covariates_weight[,j], data = NULL, sep = "", drop = TRUE, fun = as.integer, verbose = FALSE))
  }
  
  Dmatrix_weights <- NULL
  
  for (i in 1:length(Dmatrix_weights2)){
    
    Dmatrix_weights <- cbind(Dmatrix_weights, Dmatrix_weights2[[j]])
  }
  
  
  #  Dmatrix_weights <- matrix(1, nrow=nrow(chopdat),ncol=1)
  
  #  covariates_weight <-  data.frame(apply(covariates2, 2, function(x) rep(x, times = data.frame((table(chopdat$indivL)))$Freq)))
  
  #  for (j in 1:ncol(covariates2)) {
  #    Dmatrix_weights <- Yproduct(Dmatrix_weights, dummyvar(covariates_weight[,j], data = NULL, sep = "", drop = TRUE, fun = as.integer, verbose = FALSE))
  #  }
  
  
  ### Functions used to calculate the weights to be used as offset in the new model ###
  
  #--------------------------------------------------------#
  #       Weight function for Exponential- Weibull (Age)   #
  #                    mixture Model                       #
  #--------------------------------------------------------#
  
  # p<-p_ewad2
  
  
  wsmall_ewad2<-function(t,p, present,astart,aend, Dmatrix, regress){
    
    if (isTRUE(regress)) { 
      thetaA  <- Dmatrix%*%p[1:length(Dmatrix)]
      thetaB  <- Dmatrix%*%p[((length(Dmatrix))+1):(2*(length(Dmatrix)))] +   (Dmatrix*(log(astart)))%*%p[(2*(length(Dmatrix))+ 1):(3*(length(Dmatrix)))]
      eta     <- Dmatrix%*%p[(3*(length(Dmatrix))+ 1):(4*(length(Dmatrix)))] + ((Dmatrix)%*%p[(4*(length(Dmatrix))+ 1):(5*(length(Dmatrix)))])*t
      gamma0  <- Dmatrix%*%p[(5*(length(Dmatrix))+ 1):(6*(length(Dmatrix)))] + (Dmatrix*log(astart))%*%p[(6*(length(Dmatrix))+ 1):(7*(length(Dmatrix)))]
      
    } else {
      
      thetaA  <- Dmatrix%*%p[1:length(Dmatrix)]
      thetaB  <- Dmatrix%*%p[((length(Dmatrix))+1):(2*(length(Dmatrix)))]
      eta     <- Dmatrix%*%p[(2*(length(Dmatrix))+ 1):(3*(length(Dmatrix)))] 
      gamma0  <- Dmatrix%*%p[(3*(length(Dmatrix))+ 1):(4*(length(Dmatrix)))]
      
    }
    
    
    lamA <-(exp(-thetaA))            # 1/rho in the paper
    lamB <-(exp(-thetaB))            # 1/mu
    pi0  <-(exp(eta)/(1+exp(eta)))   # pi
    nu0  <-(exp(gamma0))              # nu
    
    val <- ((1-present)*log(pi0*lamA*exp(-lamA*(aend-t))+
                              (1-pi0)*nu0*lamB*((aend*lamB)^(nu0-1))*exp(-((aend*lamB)^nu0-(t*lamB)^nu0))) +
              present *log(pi0*exp(-lamA*(aend-t))+
                             (1-pi0)*exp(-((aend*lamB)^nu0-(t*lamB)^nu0))))
    exp(val)
  }
  
  
  #--------------------------------------------------------------#
  #       Weight function for Exponential- Weibull (Interval)    #
  #                    mixture Model                             #
  #--------------------------------------------------------------#
  
  # p<-p_ewid2
  
  
  wsmall_ewid2<-function(t, p, present, aend, Dmatrix, regress){
    
    if (isTRUE(regress)) { 
      thetaA  <- Dmatrix%*%p[1:length(Dmatrix)]
      thetaB  <- Dmatrix%*%p[((length(Dmatrix))+1):(2*(length(Dmatrix)))] +   ((Dmatrix)%*%p[(2*(length(Dmatrix))+ 1):(3*(length(Dmatrix)))])*(log(t))
      eta     <- Dmatrix%*%p[(3*(length(Dmatrix))+ 1):(4*(length(Dmatrix)))] + ((Dmatrix)%*%p[(4*(length(Dmatrix))+ 1):(5*(length(Dmatrix)))])*t
      gamma0  <- Dmatrix%*%p[(5*(length(Dmatrix))+ 1):(6*(length(Dmatrix)))] + ((Dmatrix)%*%p[(6*(length(Dmatrix))+ 1):(7*(length(Dmatrix)))])*log(t)
      
    } else {
      
      thetaA  <- Dmatrix%*%p[1:length(Dmatrix)]
      thetaB  <- Dmatrix%*%p[((length(Dmatrix))+1):(2*(length(Dmatrix)))]
      eta     <- Dmatrix%*%p[(2*(length(Dmatrix))+ 1):(3*(length(Dmatrix)))] 
      gamma0  <- Dmatrix%*%p[(3*(length(Dmatrix))+ 1):(4*(length(Dmatrix)))]
      
    }
    
    lamA<-exp(-thetaA)            # 1/rho in the paper
    lamB<-exp(-thetaB)            # 1/mu
    pi0 <-exp(eta)/(1+exp(eta))   # pi
    nu0<-exp(gamma0)              # nu
    
    int<-aend-t
    
    val<- ((1-present)*log(pi0*lamA*exp(-lamA*int)+
                             (1-pi0)*nu0*lamB*((int*lamB)^(nu0-1))*exp(-((int*lamB)^nu0))) +
             
             present *log(pi0*exp(-lamA*int)+
                            (1-pi0)*exp(-((int*lamB)^nu0))))
    
    exp(val)
  }
  
  #--------------------------------------------------------#
  #       Weight function for Exponential- Gamma (Age)     #
  #                    mixture Model                       #
  #--------------------------------------------------------#
  
  # p<-p_egad2
  
  
  wsmall_egad2 <- function(t,p,present,astart,aend,Dmatrix, regress){
    
    if (isTRUE(regress)) { 
      thetaA  <- Dmatrix%*%p[1:length(Dmatrix)]
      thetaB  <- Dmatrix%*%p[((length(Dmatrix))+1):(2*(length(Dmatrix)))] +   (Dmatrix*(log(astart)))%*%p[(2*(length(Dmatrix))+ 1):(3*(length(Dmatrix)))]
      eta     <- Dmatrix%*%p[(3*(length(Dmatrix))+ 1):(4*(length(Dmatrix)))] + ((Dmatrix)%*%p[(4*(length(Dmatrix))+ 1):(5*(length(Dmatrix)))])*t
      gamma0  <- Dmatrix%*%p[(5*(length(Dmatrix))+ 1):(6*(length(Dmatrix)))] + (Dmatrix*log(astart))%*%p[(6*(length(Dmatrix))+ 1):(7*(length(Dmatrix)))]
      
    } else {
      
      thetaA  <- Dmatrix%*%p[1:length(Dmatrix)]
      thetaB  <- Dmatrix%*%p[((length(Dmatrix))+1):(2*(length(Dmatrix)))]
      eta     <- Dmatrix%*%p[(2*(length(Dmatrix))+ 1):(3*(length(Dmatrix)))] 
      gamma0  <- Dmatrix%*%p[(3*(length(Dmatrix))+ 1):(4*(length(Dmatrix)))]
      
    }
    
    lamA <-exp(-thetaA)            # 1/rho in the paper
    lamB <-exp(-thetaB)            # 1/mu
    pi0  <-exp(eta)/(1+exp(eta))   # pi
    nu0  <-exp(gamma0)             # nu
    
    rate0 <-nu0*lamB
    
    # val<- ((1-present)*log(pi0*lamA*exp(-lamA*(aend-t))+
    #                 (1-pi0)*dgamma(aend,shape=nu0,rate=rate0)/pgamma(t,shape=nu0,rate=rate0,lower.tail=F)) +
    #                    present*log(pi0*exp(-lamA*(aend-t))+
    #                 (1-pi0)*pgamma(aend,shape=nu0,rate=rate0,lower.tail=F)/pgamma(t,shape=nu0,rate=rate0,lower.tail=F)))
    
    val<- ((1-present)*log(pi0*lamA*exp(-lamA*(aend-t))+
                             (1-pi0)*dgamma(aend,shape=nu0,rate=rate0)/ifelse(pgamma(t,shape=nu0,rate=rate0,lower.tail=F)==0,0.000000001, pgamma(t,shape=nu0,rate=rate0,lower.tail=F))) +
             present *log(pi0*exp(-lamA*(aend-t))+
                            (1-pi0)*pgamma(aend,shape=nu0,rate=rate0,lower.tail=F)/ifelse(pgamma(t,shape=nu0,rate=rate0,lower.tail=F)==0, 0.000000001, pgamma(t,shape=nu0,rate=rate0,lower.tail=F))))
    
    
    
    exp(val)
  }
  
  #--------------------------------------------------------#
  #       Weight function for Exponential- Gamma (Interval)#
  #                    mixture Model                       #
  #--------------------------------------------------------#
  
  # p<-p_egid2
  
  wsmall_egid2 <- function(t,p,present,astart,aend,Dmatrix, regress) {
    
    if (isTRUE(regress)) { 
      thetaA  <- Dmatrix%*%p[1:length(Dmatrix)]
      thetaB  <- Dmatrix%*%p[((length(Dmatrix))+1):(2*(length(Dmatrix)))] +   ((Dmatrix)%*%p[(2*(length(Dmatrix))+ 1):(3*(length(Dmatrix)))])*(log(t))
      eta     <- Dmatrix%*%p[(3*(length(Dmatrix))+ 1):(4*(length(Dmatrix)))] + ((Dmatrix)%*%p[(4*(length(Dmatrix))+ 1):(5*(length(Dmatrix)))])*t
      gamma0  <- Dmatrix%*%p[(5*(length(Dmatrix))+ 1):(6*(length(Dmatrix)))] + ((Dmatrix)%*%p[(6*(length(Dmatrix))+ 1):(7*(length(Dmatrix)))])*log(t)
      
    } else {
      
      thetaA  <- Dmatrix%*%p[1:length(Dmatrix)]
      thetaB  <- Dmatrix%*%p[((length(Dmatrix))+1):(2*(length(Dmatrix)))]
      eta     <- Dmatrix%*%p[(2*(length(Dmatrix))+ 1):(3*(length(Dmatrix)))] 
      gamma0  <- Dmatrix%*%p[(3*(length(Dmatrix))+ 1):(4*(length(Dmatrix)))]
      
    }
    
    lamA<-exp(-thetaA)            # 1/rho in the paper
    lamB<-exp(-thetaB)            # 1/mu
    pi0 <-exp(eta)/(1+exp(eta))   # pi
    nu0<-exp(gamma0)              # nu
    
    rate0 <-nu0*lamB
    
    int <-aend-t
    
    val<- ((1-present)*log(pi0*lamA*exp(-lamA*int)+
                             (1-pi0)*dgamma(int,shape=nu0,rate=rate0)) +
             present *log(pi0*exp(-lamA*int)+
                            (1-pi0)*pgamma(int,shape=nu0,rate=rate0,lower.tail=F)))
    exp(val)
  }
  
  
  #-------------------------------------------------------------#
  # A function that calculates weights by selecting the weight  #
  #      function amodel with the minimum AIC                   #
  #-------------------------------------------------------------#
  
  calcweight <- function() {
    if (min(AICs)==AIC_EWA) {
      wlarge_ewa = NULL
      error_ewa = NULL
      for(i in 1:nrow(chopdat)){
        if (chopdat$upper[i]==chopdat$lower[i]) {
          
          wlarge_ewa[i] <- 1e-15 
        } else {
          
          integral <-integrate(wsmall_ewad2, lower=chopdat$lower[i]/365.25,upper=chopdat$upper[i]/365.25,p=p_ewad2, present=chopdat$present[i], astart=chopdat$astart[i]/365.25,aend=chopdat$aend[i]/365.25,Dmatrix=Dmatrix_weights[i,], regress=regress, rel.tol=.Machine$double.eps^0.5)
          wlarge_ewa[i]<-integral$value
          error_ewa[i]<-integral$abs.error
        }
      }
      #      return list("wlarge"=wlarge, "error"=error)
      return(wlarge_ewa)
    } else if (min(AICs)==AIC_EWI) {
      wlarge_ewi = NULL
      error_ewi = NULL
      for(i in 1:nrow(chopdat)){
        
        if (chopdat$upper[i]==chopdat$lower[i]) {
          
          wlarge_ewi[i] <- 1e-15 
        } else {
          
          integral<-integrate(wsmall_ewid2,  lower=chopdat$lower[i]/365.25,upper=chopdat$upper[i]/365.25,p=p_ewid2, present=chopdat$present[i], aend=chopdat$aend[i]/365.25,Dmatrix=Dmatrix_weights[i,], regress=regress,rel.tol=.Machine$double.eps^0.5)
          
          wlarge_ewi[i]<-integral$value
          error_ewi[i]<-integral$abs.error
        }
      }
      #     return list("wlarge"=wlarge, "error"=error)
      return(wlarge_ewi)
    } else if (min(AICs)==AIC_EGA) {
      wlarge_ega = NULL
      error_ega = NULL
      for(i in 1:nrow(chopdat)){
        if (chopdat$upper[i]==chopdat$aend[i] & chopdat$lower[i]==chopdat$aend[i]) {
          
          wlarge_ega[i] <- 1e-15 
        } else {
          
          integral<-integrate(wsmall_egad2, lower=chopdat$lower[i]/365.25,upper=chopdat$upper[i]/365.25,p=p_egad2, present=chopdat$present[i], astart=chopdat$astart[i]/365.25,aend=chopdat$aend[i]/365.25,Dmatrix=Dmatrix_weights[i,], regress=regress,rel.tol=.Machine$double.eps^0.5)
          wlarge_ega[i]<-integral$value
          error_ega[i]<-integral$abs.error
        }
      }
      #      return list("wlarge"=wlarge, "error"=error)
      return(wlarge_ega)
      
    } else {
      wlarge_egi = NULL
      error_egi = NULL
      for(i in 1:nrow(chopdat)){
        if (chopdat$upper[i]==chopdat$aend[i] & chopdat$lower[i]==chopdat$aend[i]) {
          
          wlarge_egi[i] <- 1e-15 
        } else {
          
          integral<-integrate(wsmall_egid2, lower=chopdat$lower[i]/365.25, upper=chopdat$upper[i]/365.25,p=p_egid2, present=chopdat$present[i], astart=chopdat$astart[i]/365.25,aend=chopdat$aend[i]/365.25, Dmatrix=Dmatrix_weights[i,], regress=regress, rel.tol=.Machine$double.eps^0.5)
          wlarge_egi[i]<-integral$value
          error_egi[i]<-integral$abs.error
        }
      }
      #      return list("wlarge"=wlarge, "error"=error)
      return(wlarge_egi)
    }
  }
  
  
  #--------------------------#
  #      Weights             #
  #--------------------------#
  
  weightt <- calcweight()
  weightt <- ifelse(weightt<1e-15, 1e-15, weightt)
  #logw <- log(weights)
  chopdat$logw <- log(weightt)
  
  #-------------------#
  # Fitting the model #
  #-------------------#
  #attach(chopdat)
  #mod<-clogit(formula, data=chopdat)
  #detach(chopdat)
  #return
  fmla <- paste(formula, "+", "strata(indivL)", "+", "offset(logw)")
  fmla1 <- as.formula(paste("event~", fmla[3]))
  mod <- clogit(formula = fmla1, data = chopdat)
  summary <- summary(mod)
  options(warn=0)
  return(list("modelfit"=modelfit, "summary"=summary(mod)))
  
}

