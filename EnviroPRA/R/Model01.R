sampler <- function ( n, a){
 
  if (length (a) > 1)
  s<- sample ( a, n, replace=T)
  else
    s <- a
  return (s)
}


condition <- function (n) {
  if (n> 0.05)
    return ("Significant")
  else
    return ("Not-significant")
}

sig <- function (n){
  
  if (is.na(n)== T)
    return ("Not applicable")
  else
    return (condition(n))
}



DWIR <- function ( CW=1, IRW=2, EF=350, ED=24, BW=80, AT=365*70){
  
  I = ( CW * IRW * EF * ED ) / ( BW * AT )
  return (I)
}


DWIRboot<- function (n=100, CW=1, IRW=2, EF=350, BW=80, ED=24, AT=365*70) {
  c <- sampler ( n, CW)
  ir <-  sampler (n, IRW)
  ef <- sampler ( n, EF)
  bw <- sampler ( n, BW)
  ed <- sampler ( n, ED)
  at <- sampler (n, AT)
  I <- DWIR (CW = c, IRW = ir, EF = ef, BW = bw, AT= at, ED = ed)
  return (I)
}


AD <- function ( CS=1, SA=2800, AF=0.2, ABS=0.001, EF=350, ED=24, BW=70, AT=365*70){
  CF = 1e-06
  I = ( CS * SA * AF* ABS* EF * ED * CF ) / ( BW * AT )
  return (I)
}

ADboot <- function ( n=100, CS=1, SA=2800, AF=0.2, ABS=0.001, EF=350, ED=24, BW=70, AT=365*70){
  c <- sampler ( n, CS)
  sa <- sampler (n, SA)
  af <- sampler (n, AF)
  abs <- sampler (n, ABS)
  ef <- sampler ( n, EF)
  bw <- sampler ( n, BW)
  ed <- sampler ( n, ED)
  at <- sampler (n, AT)
  I = AD( CS = c, SA= sa, AF= af, ABS = abs, EF = ef, BW =bw, AT= at )
  return (I)
}



SIR <- function ( CS=1, IR=100, FI=1, EF=350, ED=24, BW=80, AT=365*70){
  CF = 1e-06
  I = ( CS * IR * FI* CF* EF * ED ) / ( BW * AT )
  return (I)
}

SIRboot <- function ( n=100, CS=1, IR=100, FI=1, EF=350, ED=24, BW=80, AT=365*70){
  c <- sampler ( n, CS)
  ir <- sampler (n, IR)
  fi <- sampler (n, FI)
  ef <- sampler ( n, EF)
  bw <- sampler ( n, BW)
  ed <- sampler ( n, ED)
  at <- sampler (n, AT)
  I = SIR( CS = c, IR= ir, FI= fi, EF = ef, ED = ed, BW =bw, AT= at )
  return (I)
}


AIR <- function (CA =1, IR= 20, ET=24, EF=350, ED=24, BW=70, AT=365*70){
  I = ( CA * IR * ET*  EF * ED ) / ( BW * AT )
  return (I)
}

AIRboot <- function ( n=100, CA=1, IR=25, ET = 24, EF = 350, ED = 24, BW = 85, AT = 365*70){
  c <- sampler ( n, CA)
  ir <- sampler (n, IR)
  et <- sampler (n, ET)
  ef <- sampler ( n, EF)
  bw <- sampler ( n, BW)
  ed <- sampler ( n, ED)
  at <- sampler (n, AT)
  I = AIR( CA = c, IR= ir, ET= et, EF = ef, ED = ed, BW =bw, AT= at )
  return (I)
}


VI <- function (CF=1, IR= 210, FI= 1, EF=350, ED= 24, BW= 80, AT=365*70){
  I = ( CF * IR * (1e-03) *FI* EF * ED ) / (BW * AT )
  return (I)
}

VIboot <- function ( n=100, CF=1, IR= 210, FI= 1, EF=350, ED= 24, BW= 80, AT=365*70){
  c <- sampler ( n, CF)
  ir <- sampler (n, IR)
  fi <- sampler (n, FI)
  ef <- sampler ( n, EF)
  ed <- sampler ( n, ED)
  at <- sampler (n, AT)
  I = VI( CF = c, IR= ir, FI= fi, EF = ef, ED = ed, AT= at )
  return (I)
}



INH<- function (C= 10, EF= 350, ED= 24, PEF= 1.36^9, AT= 365*ED){
  I=(C*EF*ED)/(PEF*AT)
  return(I)
}


HI <- function (I, RFD){
  hi <- I/RFD
  return (hi)
}

HIdermal <- function (AD, RFD, GI){
  hi <- AD/(RFD*GI)
  return (hi)
}

HIinhal<- function (INH, RFC){
  hi<- INH/RFC
  return(hi)
}

RISK <- function (I, SF){
  risk <- I*SF
  return (risk)
}

RISKdermal <- function (AD, SF, GI){
  risk <- AD*(SF/GI)
  return (risk)
}

RISKInhal<- function (URi, I){
  risk<- (I*1e3)*URi
return(risk)
}


Fit_dist_parameter <- function (x){
  
  
  distributions_non_start <- c("normal","log-normal","geometric","exponential","Poisson"
                               , "cauchy" , "logistic", "weibull" )
  
  numn_non_start <- length(distributions_non_start)
  
  results=list() 
  
  for (i in 1:numn_non_start){
    
    if (distributions_non_start[i]=="log-normal"){
      
      ifelse (min(x)>0,
              
              a<- fitdistr(x,distributions_non_start[i])
              ,
              a <- "Not Applicable, vector contain 0")
    }
    
    if (distributions_non_start[i]=="Poisson"){
      
      
      ifelse (min(x)>0,
              
              a<- fitdistr(x,distributions_non_start[i])
              ,
              a <- "Not Applicable, vector contain 0")
    }
    
    if (distributions_non_start[i]=="weibull"){
      
      
      
      ifelse (min(x)>0,
              
              a<- fitdistr(x,distributions_non_start[i])
              ,
              a <- "Not Applicable, vector contain 0")
    }
    
    if ((!distributions_non_start[i]=="log-normal")&(!distributions_non_start[i]=="Poisson")&(!distributions_non_start[i]=="weibull")){
      a <-fitdistr(x,distributions_non_start[i])
    }
    results[[i]] <- a
  }
  
  names(results)=c(distributions_non_start)
  
  return (results)  
  
}

fit_dist_test <- function (x){
  
  
  dist_parameters <- Fit_dist_parameter (x)
  
  distributions <- names(dist_parameters)
  
  norm <-  ks.test(x, "pnorm", mean = dist_parameters$normal$estimate[["mean"]], sd = dist_parameters$normal$estimate[["sd"]])
  
  
  ifelse (class(dist_parameters$`log-normal`)=="character",
          lognorm <- "Not applicable",
          lognorm <- ks.test(x, "plnorm", meanlog = dist_parameters$`log-normal`$estimate[["meanlog"]], 
                             sdlog = dist_parameters$`log-normal`$estimate[["sdlog"]]))
  
  geometric <- ks.test(x, "pgeom", prob = dist_parameters$geometric$estimate[["prob"]])
  
  exponential <- ks.test(x, "pexp", rate = dist_parameters$exponential$estimate[["rate"]])
  
  ifelse (class(dist_parameters$Poisson)=="character",
          poisson <- "Not applicable",
          poisson <- ks.test(x, "ppois", lambda = dist_parameters$Poisson$estimate[["lambda"]]))
  
  cauchy <- ks.test(x, "pcauchy", location = dist_parameters$cauchy$estimate[["location"]], scale = dist_parameters$cauchy$estimate[["scale"]] )
  
  logistic <- ks.test(x, "plogis", location = dist_parameters$logistic$estimate[["location"]], scale = dist_parameters$logistic$estimate[["scale"]] )
  
  
  ifelse (class(dist_parameters$weibull)=="character",
          weibull <- "Not applicable",
          weibull <- ks.test(x, "pweibull", shape = dist_parameters$weibull$estimate[["shape"]], scale = dist_parameters$weibull$estimate[["scale"]] )
  )

  ks<- list(norm,lognorm, geometric, exponential, poisson, cauchy, logistic,
            weibull)
  names(ks)<-distributions
  
  n <- length (x)
  
  ad <- list()
  
  randoms_dists <- list(rnorm(n,dist_parameters$normal$estimate),
                        
                        ifelse (class(dist_parameters$`log-normal`)=="character",
                                "Not applicable",
                                rlnorm(n, dist_parameters$`log-normal`$estimate))
                        
                        ,
                        
                        rgeom(n,dist_parameters$geometric$estimate),
                        rexp(n,dist_parameters$exponential$estimate),
                        
                        ifelse (class(dist_parameters$Poisson)=="character",
                                "Not applicable",
                                rpois(n,dist_parameters$Poisson$estimate))
                        
                        
                        ,
                        
                        rcauchy(n,dist_parameters$cauchy$estimate),
                        rlogis(n,dist_parameters$logistic$estimate),
                        
                        ifelse (class(dist_parameters$weibull)=="character",
                                "Not applicable",
                                rweibull(n, dist_parameters$weibull$estimate))
                        
                        )
                        
   
  names(randoms_dists)<-distributions
  
  for (i in 1:length(randoms_dists)){
    
    if (!class(randoms_dists[[i]])=="character"){
      a<-ad.test(x,randoms_dists[[i]])
      ad[[i]]<-a$ad
    }
    else {
      ad[[i]]<- "Not applicable"
    }
  }
  
  names(ad)<-distributions
  
  Akaike <- list()
  
  for (i in 1:length(dist_parameters) ){
    
    ifelse (class(dist_parameters[[i]])=="fitdistr",
            
            Akaike[[i]]<-AIC(dist_parameters[[i]]),
            
            Akaike[[i]] <- "Not applicable")
  }
  
  names(Akaike)<-distributions
  
  
  BIC <- list ()
  
  for (i in 1:length(dist_parameters) ){
    
    ifelse (class(dist_parameters[[i]])=="fitdistr",
            
            BIC[[i]]<-BIC(dist_parameters[[i]]),
            
            BIC[[i]] <- "Not applicable")
  }
  
  names(BIC)<-distributions
  
  BICvec<-AICvec<-KSvec<-ADvec<-KSvecp<-ADvecp<-numeric(length(distributions))
  
  for (i in 1:length(distributions)){
    
    ifelse (class(BIC[[i]])=="numeric",
            BICvec[i]<-as.vector(BIC[[i]]),
            BICvec[i]<-NA)
    ifelse (class(Akaike[[i]])=="numeric",
            AICvec[i]<-as.vector(Akaike[[i]]),
            AICvec[i]<-NA)
    
    ifelse (!class(ks[[i]])=="character",
            KSvec[i]<-as.vector(ks[[i]]$statistic),
            KSvec[i]<-NA)
    ifelse (!class(ks[[i]])=="character",
            KSvecp[i]<-as.vector(ks[[i]]$p.value),
            KSvecp[i]<-NA)
    
    
    ifelse (!class(ad[[i]])=="character",
            ADvec[i]<-as.vector(ad[[i]][3]),
            ADvec[i]<-NA)
    ifelse (!class(ad[[i]])=="character",
            ADvecp[i]<-as.vector(ad[[i]][5]),
            ADvecp[i]<-NA)
    
  }
  
  sigKs <- sapply(KSvecp, FUN = sig)
  sigAd <- sapply(ADvecp, FUN= sig)
  
  results<-data.frame(distributions,BICvec,AICvec,KSvec,KSvecp,sigKs, ADvec, ADvecp,sigAd)
  names(results)<-c ("Distribution","BayesianIC","AkaikeIC","Kol-SmirD",
                     "Kol-SmirPvalue","Signigicance KS", "And-Darl","And-DarlPvalue", "Signigicance AD")
  results<-results[order(results$BayesianIC),]
  
  return (results)
}

extr_par <- function (x, dist){
  
  size <- shape1 <- shape2 <- NULL
  
  distributions <- c("norm",  "lnorm",  "geom", "exp",  "pois", "gamma",  
                     "cauchy",  "logis",  "weibull", "nbinom", "beta",
                     "chisq",  "t", "f")
  dist_parameters=x
  
  if (dist==distributions[1]) {
    
    para <-  list(mean = dist_parameters$normal$estimate[["mean"]], sd = dist_parameters$normal$estimate[["sd"]])
    
  }
  
  if (dist==distributions[2]) {
    
    para <-list(meanlog = dist_parameters$`log-normal`$estimate[["meanlog"]], sdlog = dist_parameters$`log-normal`$estimate[["sdlog"]])
    
  }
  
  if (dist==distributions[3]) {
    
    para <- list(prob = dist_parameters$geometric$estimate[["prob"]])
    
  }
  
  if (dist==distributions[4]) {
    
    para <- list(rate = dist_parameters$exponential$estimate[["rate"]])
    
  }
  
  if (dist==distributions[5]) {
    
    para <- list(lambda = dist_parameters$Poisson$estimate[["lambda"]])
  }
  
  if (dist==distributions[6]) {
    
    para <- list(shape = dist_parameters$gamma$estimate[["shape"]])
    
  }
  
  if (dist==distributions[7]) {
    
    para <- list(location = dist_parameters$cauchy$estimate[["location"]], scale = dist_parameters$cauchy$estimate[["scale"]] )
  }
  
  if (dist==distributions[8]) {
    
    para <- list(location = dist_parameters$logistic$estimate[["location"]], scale = dist_parameters$logistic$estimate[["scale"]] )
  }
  
  if (dist==distributions[9]) {
    para <- list(shape = dist_parameters$weibull$estimate[["shape"]], scale = dist_parameters$weibull$estimate[["scale"]] )
    
  }
  
  if (dist==distributions[10]) {
    ifelse (class(dist_parameters$`negative binomial`)=="character",
            para <- "Not applicable",
            para <- list(size = dist_parameters$`negative binomial`$estimate[[size]]))
  }
  
  if (dist==distributions[11]) {
    ifelse (class(dist_parameters$beta)=="character",
            para <- "Not applicable",
            para <- list(shape1=dist_parameters$beta$estimate[[shape1]],shape2=dist_parameters$beta$estimate[[shape2]]))
  }
  
  if (dist==distributions[12]) {
    para <- list(df = dist_parameters$`chi-squared`$estimate[["df"]])
  }
  
  if (dist==distributions[13]) {
    para <- list(df = dist_parameters$t$estimate[["df"]])
  }
  
  
  if (dist==distributions[14]) {
    para <- list( df1 = dist_parameters$f$estimate[["df1"]], df2 = dist_parameters$f$estimate[["df2"]])
  }
  return (para)
}

plot_fit_dist<-function (x,dist){
  
  dist_parameters <- Fit_dist_parameter (x)
  
  distributions <- c("norm",  "lnorm",  "geom", "exp",  "pois", "gamma",  
                     "cauchy",  "logis",  "weibull", "nbinom", "beta",
                     "chisq",  "t", "f")
  
  
  parameters <- extr_par (dist_parameters, dist)
  
  
  plotdist(x, dist,para = parameters)
    
  
}


random_number_generator <- function (n, Fited, dist, a, b){
  
  parameters <- extr_par (Fited, dist)
  
  
  app<-append(list(n=n),parameters)
  appp<-append(app,list(spec=dist))
  apppp<-append(appp,list(a=a))
  arg<-append(apppp,list(b=b))
  return (do.call(rtrunc,args=arg))
  
}