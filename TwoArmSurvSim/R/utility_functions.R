
library(dplyr)
library(blockrand)
library(survival)




objfunction<- function(x,lambda,gamma,gammac,theta,ebx,groupfreq,censordist,timeinterval) {
  t2<-ch1<-ch2<-NULL
  ninter<-length(lambda)
  if (ninter==1){
    t<-c(0,Inf)
  }else if (is.null(timeinterval)){
    stop("For piecewise hazard function, timeinterval for each window is needed.")
  }else {
    t<-c(timeinterval,Inf)
    m<-data.frame(lambda=lambda,gamma=gamma,t=timeinterval)
    m$t2<-c(timeinterval[-1],Inf)
    m<- m %>% mutate(ch1=(lambda*t)^gamma) %>% mutate(ch2=(lambda*t2)^gamma) %>% mutate(cm=ch2-ch1) %>% mutate(ch=cumsum(cm)) 
  }
 
  tvalue<-0
  
  if (censordist=='weibull'){
    fint <- function(y,lambda,gamma,lambdac,gammac,ebx,ch1,ch)
    {
      fint <- lambdac^gammac*gammac*y^(gammac-1)*exp(-(lambdac*y)^gammac)*exp(-((lambda*y)^gamma-ch1+ch)*ebx)
    }
  }
  
  if (censordist=='exponential'){
    fint <- function(y,lambda,gamma,lambdac,gammac,ebx,ch1,ch)
    {
      fint <- lambdac*exp(-(lambdac*y))*exp(-((lambda*y)^gamma-ch1+ch)*ebx)
    }
  }
  
  if (censordist=='uniform'){
    fint <- function(y,lambda,gamma,lambdac,gammac,ebx,ch1,ch)
    {
      fint <- (1/lambdac)*exp(-((lambda*y)^gamma-ch1+ch)*ebx)
    }
  }
  
  for (i in 1:length(ebx)){
    rr<-0
    for (j in 1:ninter){
      if (j==1){
        r <- integrate(fint,lambdac=x,lambda=lambda[j],gamma=gamma[j],gammac=gammac,ebx=ebx[i],ch1=0,ch=0,lower = t[j], upper = t[j+1])
      }else{
        r <- integrate(fint,lambdac=x,lambda=lambda[j],gamma=gamma[j],gammac=gammac,ebx=ebx[i],ch1=m$ch1[j],ch=m$ch[j-1],lower = t[j], upper = t[j+1])
      }
      rr<-rr+r$value
    }
    tvalue<-tvalue+rr*groupfreq[i]
  }
  obj <- tvalue - theta 
}



LambdaCensor <- function(lambda=lambda,gamma=gamma,theta=theta,ebx=1,gammac=1,groupfreq=1,censordist='exponential',timeinterval=NULL) {
  #get censor distribution parameter based on fixed censoring rate:  theta
  #censor distribution can be weibull, exponential, or uniform
  #event distribution can be weibull, exponential, or piecewise weibull
  sol <- uniroot(objfunction,lambda=lambda,gamma=gamma,gammac=gammac,theta=theta, ebx=ebx, groupfreq=groupfreq,censordist=censordist,timeinterval=timeinterval, interval= c(0.00001, 1000))
  return(as.numeric(sol$root)) 
}


objfunction_betapw<- function(x,lambda,gamma,gammac,theta,ebx,groupfreq,censordist,timeinterval) {
  t2<-ch1<-ch2<-NULL
  ninter<-ncol(ebx)
  if (ninter==1){
    t<-c(0,Inf)
  }else if (is.null(timeinterval)){
    stop("For piecewise hazard function, timeinterval for each window is needed.")
  }else {
    t<-c(timeinterval,Inf)
    m<-data.frame(lambda=lambda,gamma=gamma,t=timeinterval)
    m$t2<-c(timeinterval[-1],Inf)
    
  }
  
  tvalue<-0
  
  if (censordist=='weibull'){
    fint <- function(y,lambda,gamma,lambdac,gammac,ebx,ch1,ch)
    {
      fint <- lambdac^gammac*gammac*y^(gammac-1)*exp(-(lambdac*y)^gammac)*exp(-((lambda*y)^gamma*ebx-ch1+ch))
    }
  }
  
  if (censordist=='exponential'){
    fint <- function(y,lambda,gamma,lambdac,gammac,ebx,ch1,ch)
    {
      fint <- lambdac*exp(-(lambdac*y))*exp(-((lambda*y)^gamma*ebx-ch1+ch))
    }
  }
  
  if (censordist=='uniform'){
    fint <- function(y,lambda,gamma,lambdac,gammac,ebx,ch1,ch)
    {
      fint <- (1/lambdac)*exp(-((lambda*y)^gamma*ebx-ch1+ch))
    }
  }
  
  for (i in 1:length(groupfreq)){
    
    ebxi<-as.vector(t(ebx[i,]))
    m2<-cbind(m,ebx=ebxi)
    m2$ebx2<-c(ebxi[-1],Inf)
    m2<- m2  %>% mutate(ch1=(lambda*t)^gamma*ebx) %>% mutate(ch2=(lambda*t2)^gamma*ebx) %>% mutate(cm=ch2-ch1) %>% mutate(ch=cumsum(cm)) 
    
    rr<-0
    for (j in 1:ninter){
      if (j==1){
        r <- integrate(fint,lambdac=x,lambda=lambda,gamma=gamma,gammac=gammac,ebx=ebxi[j],ch1=0,ch=0,lower = t[j], upper = t[j+1])
      }else{
        r <- integrate(fint,lambdac=x,lambda=lambda,gamma=gamma,gammac=gammac,ebx=ebxi[j],ch1=m2$ch1[j],ch=m2$ch[j-1],lower = t[j], upper = t[j+1])
      }
      rr<-rr+r$value
    }
    tvalue<-tvalue+rr*groupfreq[i]
  }
  obj <- tvalue - theta 
}



LambdaCensor_betapw <- function(lambda=lambda,gamma=gamma,theta=theta,ebx=1,gammac=1,groupfreq=1,censordist='exponential',timeinterval=NULL) {
  sol <- uniroot(objfunction_betapw,lambda=lambda,gamma=gamma,gammac=gammac,theta=theta, ebx=ebx, groupfreq=groupfreq,censordist=censordist,timeinterval=timeinterval, interval= c(0.00001, 100))
  return(as.numeric(sol$root)) 
}




column_freq<-function(x,namelist,keepID=FALSE){
  Freq<-NULL
  tmpdata<-x[,namelist]
  id<-data.frame(id=do.call(paste,c(x,sep='-')))
  data<-data.frame(x,id)
  gfreq<-as.data.frame(table(data$id)) %>% mutate(count=Freq) %>% mutate(Freq=Freq/nrow(x))
  names(gfreq)[1]="id"
  data<-merge(x=data,y=gfreq,by="id",all.x=TRUE)
  if (keepID) {
    data<-unique(data)
  }else{
    data<-unique(data) %>% select(-id)
  }
  
  rownames(data)<-NULL
  return(data)
  
}



weibullsim<-function(N=NULL,lambda,gamma,x=NULL,betas=NULL){
  if (is.null(x)){
    if (is.null(N)){
      return(data.frame(eventtime=(-log(runif(1)))^(1/gamma)/lambda,status=rep(1,1)))
    }else{
      return(data.frame(eventtime=(-log(runif(N)))^(1/gamma)/lambda,status=rep(1,N)))
    }
    
  }else if (is.null(betas)){
    N<-nrow(x)
    return(data.frame(eventtime=(-log(runif(N)))^(1/gamma)/lambda,status=rep(1,N)))
  }else {
    N<-nrow(x)
    return(data.frame(eventtime=(-log(runif(N))/(exp(as.matrix(x)%*%betas)))^(1/gamma)/lambda,status=rep(1,N)))
  }
}


weibullsim_pw<-function(lambda,gamma,t,x,betas=NULL){
  #weibull simulation, given covariates matrix x (data.frame) and coefficients betas.
  #lambda, gamma: vectors of weibull parameters for each interval. t:  start time of each intervals , always start with 0, should have the same lenght of lambda and gamma.
  t2<-ch1<-ch2<-NULL
  N<-nrow(x)
  if (length(lambda)==1){
    return(data.frame(eventtime=(-log(runif(N))/(exp(as.matrix(x)%*%betas)))^(1/gamma)/lambda,status=rep(1,N)))
  }else if (is.null(t)){
    stop("For piecewise hazard function, timeinterval for each window is needed.")
  } else { 
    m<-data.frame(lambda=lambda,gamma=gamma,t=t)
    m$t2<-c(t[-1],Inf)
    m<- m %>% mutate(ch1=(lambda*t)^gamma) %>% mutate(ch2=(lambda*t2)^gamma) %>% mutate(cm=ch2-ch1) %>% mutate(ch=cumsum(cm)) 
    if (is.null(betas)){
      ebx<-rep(1,N)
    }else{
       ebx<-exp(as.matrix(x)%*%betas)
    }
    sv<-exp(-ebx%*%t(m$ch))
    rsv<-runif(N)
    ind<-apply(sv<=rsv,1,function(x) which(x)[1])
    ch<-c(0,m$ch)
    cp<-data.frame(p1=ch[ifelse(ind==1,1,ind)],p2=ifelse(ind==1,0,m$ch1[ind]),p3=gamma[ind],p4=lambda[ind])
    simdata<-data.frame(eventtime=((-log(rsv)/ebx-cp$p1+cp$p2))^(1/cp$p3)/cp$p4, status=1)
    return(simdata)
  }
}





weibullsim_betapw<-function(lambda,gamma,t,x,betas){
  
  N<-nrow(x)
  bx<-function(y){return(as.matrix(x)%*%y)}
  ebx<-exp(apply(betas,1,bx))
  t2<-c(t[-1],Inf)
  ch1<-ebx%*% diag((lambda*t)^gamma)
  ch2<-ebx%*% diag((lambda*t2)^gamma)
  cm<-ch2-ch1
  ch<-t(apply(cm,1,cumsum))
  sv<-exp(-(ch))
  rsv<-runif(N)
  ind<-apply(sv<=rsv,1,function(x) which(x)[1])
  ch<-cbind(rep(0,N),ch)
  cp<-data.frame(p1=ch[(ind-1)*N+1:N],p2=ch1[(ind-1)*N+1:N],p3=ebx[(ind-1)*N+1:N])
  return(data.frame(eventtime=((-log(rsv)-cp$p1+cp$p2)/cp$p3)^(1/gamma)/lambda, status=1))
  
}




censordata<-function(simdata,lambda,gamma, dropoutrate,ebx=1,gammac=1,groupfreq=1,censordist='exponential',timeinterval=NULL,HRPW=FALSE){
 status<-censortime<-eventtime<-NULL
  if (HRPW){
    lambdac<-LambdaCensor_betapw(lambda=lambda,gamma=gamma,theta=dropoutrate,ebx,gammac=gammac,groupfreq=groupfreq,censordist=censordist,timeinterval=timeinterval)
  }else{
    lambdac<-LambdaCensor(lambda=lambda,gamma=gamma,theta=dropoutrate,ebx,gammac=gammac,groupfreq=groupfreq,censordist=censordist,timeinterval=timeinterval)
  }
  
  np<-nrow(simdata)
  if (censordist=='weibull'){
    cendata<-weibullsim(np,lambdac,gammac)
  }
  if (censordist=='exponential'){
    cendata<-weibullsim(np,lambdac,1)
  }
  if (censordist=='uniform'){
    cendata<-data.frame(eventtime=runif(np)*lambdac,status=rep(1,np))
  }
  simdata$censortime<-cendata$eventtime
  simdata<- simdata %>% mutate( time=ifelse(.data$censortime<.data$eventtime,.data$censortime,.data$eventtime)) %>% mutate(status=ifelse(.data$censortime<.data$eventtime,0,1)) %>% select(-status,status,-eventtime,-censortime)
  return(simdata)
  
}





surv_data_simulation<-function(lambda,gamma,x,betas=NULL, dropoutrate=0,gammac=1,censordist='exponential',timeinterval=NULL,trt_timeinterval=NULL){
  eventtime<-Freq<-NULL
  . = NULL
  
  if (is.null(trt_timeinterval)){
    simdata<-weibullsim_pw(lambda=lambda,gamma=gamma,t=timeinterval,x=x,betas=betas)
  }else{
    simdata<-weibullsim_betapw(lambda=lambda,gamma=gamma,t=trt_timeinterval,x=x,betas=betas)
  }
  simdata<-cbind(x,simdata)
  
  if (length(dropoutrate)==1){
    # droput rate for population
    if (dropoutrate==0){
      return(simdata %>% rename(time=eventtime))
    }else{
      #dropout rate >0
      if (is.null(trt_timeinterval)){
        if (is.null(betas)){
          return(censordata(simdata=simdata,lambda=lambda,gamma=gamma,dropoutrate=dropoutrate,ebx=1,gammac=gammac,censordist=censordist,timeinterval=timeinterval))
        }else{
          
          group<-data.frame(covgroup=do.call(paste,c(x,sep='-')), ebx=exp(as.matrix(x)%*%betas ))
          gfreq<-as.data.frame(table(group$ebx)) %>% mutate(Freq=Freq/nrow(x)) %>% mutate_all(., function(x){as.numeric(as.character(x))})
          names(gfreq)[1]<-'ebx'
          return(censordata(simdata=simdata,lambda=lambda,gamma=gamma,dropoutrate=dropoutrate,ebx=gfreq$ebx,groupfreq=gfreq$Freq,gammac=gammac,censordist=censordist,timeinterval=timeinterval))
          
        }
      }else{
        # piecewise HR
        bx<-function(y){return(as.matrix(x)%*%y)}
        ebx<-exp(apply(betas,1,bx))
        group<-data.frame(x, ebx=ebx)
        gfreq<-column_freq(group,names(x))
        ebx<-gfreq[,grep("ebx",names(gfreq))]
        groupfreq<-gfreq$Freq
        return(censordata(simdata=simdata,lambda=lambda,gamma=gamma,dropoutrate=dropoutrate,ebx=ebx,groupfreq=groupfreq,gammac=gammac,censordist=censordist,timeinterval=trt_timeinterval,HRPW=TRUE))
        
      }
    }
  }else{
    # droprates for each treatment arms
    if (is.null(trt_timeinterval)){
      
      group2<-c(0,1)
      simdata2<-NULL
      for (i in 1:2){
        ind<- x$trt==group2[i]
        tmpdata<-simdata[ind,]
        xx<-as.data.frame(x[ind,])
        group<-data.frame(covgroup=do.call(paste,c(xx,sep='-')), ebx=exp(as.matrix(xx)%*%betas ))
        gfreq<-as.data.frame(table(group$ebx)) %>% mutate(Freq=Freq/nrow(xx)) %>% mutate_all(., function(xx){as.numeric(as.character(xx))})
        names(gfreq)[1]<-'ebx'
        
        simdata2<-rbind(simdata2,censordata(simdata=tmpdata,lambda=lambda,gamma=gamma,dropoutrate=dropoutrate[i],ebx=gfreq$ebx,groupfreq=gfreq$Freq,gammac=gammac,censordist=censordist,timeinterval=timeinterval))
        
      }
      return(simdata2)
      
    }else{
      # piecewise HR
      group2<-c(0,1)
      
      simdata2<-NULL
      for (i in 1:2){
        ind<- x$trt==group2[i]
        tmpdata<-simdata[ind,]
        xx<-x[ind,]
        
        bx<-function(y){return(as.matrix(xx)%*%y)}
        ebx<-exp(apply(betas,1,bx))
        
        group<-data.frame(xx, ebx=ebx)
        gfreq<-column_freq(group,names(xx))
        
        ebx<-gfreq[,grep("ebx",names(gfreq))]
        groupfreq<-gfreq$Freq
        
        
        simdata2<-rbind(simdata2,censordata(simdata=tmpdata,lambda=lambda,gamma=gamma,dropoutrate=dropoutrate[i],ebx=ebx,groupfreq=groupfreq,gammac=gammac,censordist=censordist,timeinterval=trt_timeinterval,HRPW=TRUE))
        
      }
      
      return(simdata2)
    }
    
    
  }
  
}



step_accrual<-function(np,tt,arate)
{
  # np: Total number of patients to be enrolled.
  # tt:  starting time of each time interval (first element should be zero)
  # arate: accrual rate (ie. 5 patients / month) for each time interval.  arate and tt should have the same length.
  
  
  ntable<-data.frame(t1=tt,t2=c(tt[-1],Inf),arate=arate,nt=c(cumsum(diff(tt)*arate[-length(arate)]),Inf))
  ntable<-ntable[1:which(ntable$nt>np)[1],]
  ninter<-nrow(ntable)
  if (ninter>1){
    ntable$t2[ninter]<-ceiling((np-ntable$nt[ninter-1])/ntable$arate[ninter])+ntable$t1[ninter]
    ntable$nt[ninter]<-ntable$nt[ninter-1]+ntable$arate[ninter]*(ntable$t2[ninter]-ntable$t1[ninter])
    ntable$tr<-ntable$nt/ntable$nt[ninter]
    ntable$trl<-c(0,ntable$tr[-ninter])
  }else{
    ntable$t2<-ceiling((np)/ntable$arate)
    ntable$nt<-ntable$arate*(ntable$t2-ntable$t1)
    ntable$tr<-1
    ntable$trl<-0
  }
  
  actime<-NULL
  rn<-runif(np)
  for (i in rn){
    tmp<-ntable[which(ntable$tr>=i)[1],]
    actime<-c(actime,(i-tmp$trl)*(tmp$t2-tmp$t1)/(tmp$tr-tmp$trl)+tmp$t1)
  }
  
  return(actime)
}


linear_accrual<-function(np,rampupt,acceleration){
  #rampup period [0, rampupt]
  #acceleration during ramp up. constant enrollment rate after ramp up
  peakrate<-acceleration*rampupt
  rampupn<-acceleration*rampupt^2/2
  rn<-runif(np)*np
 
  actime<-NULL
  for (i in 1:np){
    if (rn[i]>rampupn){
      actime<-c(actime,rampupt+(rn[i]-rampupn)/peakrate)
    }else{
      actime<-c(actime,sqrt(rn[i]*2/acceleration))
    }
  }
  return(actime)
}


 
trial_data_simulation<-function(simdata,accrual,eventtarget=NULL,maxlpfollowup=NULL){
  trt<-status<-enrolltime<-eventtime<-NULL
  simdata$dropout<-ifelse(simdata$status==1,'N','Y')
  
  ncensor0<- simdata %>% subset(trt==0&status==0) %>% pull(status)
  ncensor1<-simdata %>% subset(trt==1&status==0) %>% pull(status)
  ncensor0<-length(ncensor0)
  ncensor1<-length(ncensor1)
  dr0<-ncensor0/(simdata %>% subset(trt==0) %>% nrow)
  dr1<-ncensor1/(simdata %>% subset(trt==1) %>% nrow)
  dr<- (ncensor0+ncensor1)/nrow(simdata)
  
  
  Info<-data.frame(N_patient=nrow(simdata),N_event=sum(simdata$status==1),Dropout_rate=round(dr,4),Dropout_rate_control=round(dr0,4), Dropout_rate_treatment=round(dr1,4), FirstEnrollTime=round(min(accrual),4),LastEnrollTime=0,DataCutTime=0,DataCutInfo='')
  
  simdata$enrolltime<-accrual
  simdata<- simdata %>% mutate(eventtime=enrolltime+time)
  eventt<- simdata %>% filter(status==1) %>% select(eventtime) %>% arrange(by_group=eventtime)
  eventt<-as.array(eventt$eventtime)
  lastpatient<-max(simdata$enrolltime)

  if (is.null(maxlpfollowup)){
    lptime<-lastpatient+Inf
  } else {
    lptime<-lastpatient+maxlpfollowup
  }
  
  if (is.null(eventtarget)){
    targettime<-Inf
  }else if(length(eventt)<eventtarget){
    targettime<-Inf  
  }else{
    targettime<-eventt[eventtarget]
  }
  
   datacut<-min(targettime,lptime)
    
   if (datacut<max(simdata$eventtime)){
     
      simdata$status[simdata$eventtime>datacut]<-0 
      simdata$time[simdata$eventtime>datacut]<-datacut-simdata$enrolltime[simdata$eventtime>datacut]
      
      
      tmpdata<- simdata %>% filter(time<0)
      if (nrow(tmpdata)>0){
        simdata<- simdata %>% filter(time>=0)
        cm<-paste("warning(","\"","Reached target event early or last patient followup time reached. Trial stopped.","\"",")")
        eval(parse(text=cm))
        Info$N_patient<-nrow(simdata)
        Info$N_event<-table(simdata$status)[2]
        Info$DataCutTime<-round(datacut,4)
        if (targettime<=lptime){
           Info$DataCutInfo<-'target event'
        }else{
           Info$DataCutInfo<-'last patient max followup time'
        }
       
      }else{
        Info$N_patient<-nrow(simdata)
        Info$N_event<-table(simdata$status)[2]
        Info$DataCutTime<-round(datacut,4)
        
        if (targettime<=lptime){
          Info$DataCutInfo<-'target event'
        }else{
          Info$DataCutInfo<-'last patient max followup time'
        }
        
      }
      
      Info$LastEnrollTime<-round(max(simdata$enrolltime),4)
      return(list(data=simdata,Info=Info)) 
    }else{
      Info$DataCutTime<-round(max(simdata$eventtime),4)
      Info$DataCutInfo<-'last censor or event time'
      Info$LastEnrollTime<-round(max(simdata$enrolltime),4)
      return(list(data=simdata,Info=Info)) 
    }
  
}






cov_simu <- function(sample_size = sample_size,factors=factors){
  
  if (is.null(factors)==TRUE) {cov_mat <- NULL
  HR<-NULL
  strata_idx<-NULL
  sample}
  else if (is.null(factors)==FALSE) {
    
    HR <- list()
    strata_idx <- list()
    cov_mat <- as.data.frame(matrix(NA,sample_size,length(factors)))
    
    for(i in 1:length(factors)){
      cov_mat[,i] <- sample(seq(from=0,to=factors[[i]]$N_level-1,by=1), sample_size, replace = TRUE,
                            prob =as.vector(factors[[i]]$prevalence))
      colnames(cov_mat)[i] <- factors[[i]]$name
      HR[[i]] <- factors[[i]]$HR
      strata_idx[[i]] <- factors[[i]]$strata
    }
  }
  return_list <- list(cov_mat=cov_mat,HR=HR,strata_idx=strata_idx,sample_size=sample_size)
  return(return_list)
}


self_blockrand <- function(N=N,trt_levels=trt_levels,blocksize=blocksize,rand_ratio=rand_ratio){
  x_ran <- data.frame()
  for (i in 1:N){
    blocksize_by_rand <- blocksize*sum(rand_ratio)
    block <- rep(1:ceiling(N/blocksize_by_rand), each = blocksize_by_rand)
    
    stra_1 <- data.frame(block, rand=runif(length(block)), id= 1:length(block))
    stra_2 <- stra_1[order(stra_1$block,stra_1$rand),]
    stra_2$treatment <- rep(c(rep(trt_levels,rand_ratio)),times = length(block)/sum(rand_ratio))
    stra_final <- stra_2[order(stra_2$id),][0:N,]
    
    x_ran <- stra_final
  }
  return(x_ran)
}






dummy_convert<-function(dataset,column_names){
  outset<-dataset
  for (cur in column_names){
    curvar<-dataset[,cur]
    tmplevels<-levels(curvar)
    for (curlevel in tmplevels){
      curdummy<-as.data.frame(ifelse(curvar==curlevel,1,0)) 
      names(curdummy)<-paste(cur,'.',curlevel, sep='')
      outset<-cbind(outset,curdummy)
    }
    
  }
  return(outset)
}




randomize_trt <- function(cov_mat=cov_mat,blocksize=blocksize,trtHR=trtHR,rand_ratio=c(1,1)){
  new_strata<-treatment<-stratanum<-trt<-instrata_id<-block<-pat_id<-NULL
  
  lvl.trt <- length(trtHR)
  covmat <- cov_mat$cov_mat
  N <- cov_mat$sample_size
  rand <- unlist(cov_mat$strata_idx)
  HR <- c(trtHR,unlist(cov_mat$HR))
  trt_levels <- c(seq(from=0,to=lvl.trt-1,by=1))
  blocksize <- blocksize
  prob_rand <- rand_ratio/sum(rand_ratio)
  
  if (is.null(blocksize)==TRUE & is.null(covmat)==TRUE){
    x_ran <- as.data.frame(cbind(seq(1,N),sample(trt_levels,N,replace = TRUE,prob = prob_rand)))
    colnames(x_ran) <- c("pat_id","trt")
    final_cov_dat <- x_ran
    final_dummy_dat <- final_cov_dat
  }
  
  else if (is.null(blocksize)==TRUE & is.null(covmat)==FALSE & sum(rand)==0){
    x_ran <- as.data.frame(cbind(seq(1,N),sample(trt_levels,N,replace = TRUE,prob = prob_rand),covmat))
    colnames(x_ran) <- c("pat_id","trt",colnames(covmat))
    final_cov_dat <- x_ran
    final_dummy_dat <- final_cov_dat
  }
  
  else if (is.null(blocksize)==TRUE & is.null(covmat)==FALSE & sum(rand)>0){
    cov_starta <- as.data.frame(covmat[,rand])
    colnames(cov_starta) <- colnames(covmat)[rand]
    
    
    cov_mat_newstrata <- cov_mat$cov_mat %>% 
      mutate(pat_id=row_number()) %>% 
      mutate(new_strata = do.call(paste,c(cov_starta,sep=''))) %>% 
      group_by(new_strata) %>% 
      mutate(instrata_id=row_number())
    
    cov_mat_newstrata_count <- cov_mat$cov_mat %>% 
      mutate(new_strata = do.call(paste,c(cov_starta,sep=''))) %>% 
      group_by(new_strata) %>% 
      count() 
    # cov_mat_newstrata_count$n
    # cov_mat_newstrata_count$new_strata
    
    res_mat <- data.frame()
    for (i in 1:length(cov_mat_newstrata_count$n)){
      strata <-  as.data.frame(cbind(seq(1,cov_mat_newstrata_count$n[i]),
                                     sample(trt_levels,cov_mat_newstrata_count$n[i],
                                            replace = TRUE,prob = prob_rand)))
      strata <- strata[1:cov_mat_newstrata_count$n[i],]
      colnames(strata) <- c("id","treatment")
      strata$stratanum <- i
      strata$stratacod <- cov_mat_newstrata_count$new_strata[i]
      strata$id <- as.integer(strata$id)
      res_mat <- rbind(res_mat,strata)
      
    }
    res_mat <-as_tibble(res_mat)
    
    final_cov_dat <- cov_mat_newstrata %>% left_join(res_mat,by=c("instrata_id"="id","new_strata"="stratacod")) %>% 
      mutate(trt=as.integer(as.character(treatment))) %>% 
      select(-c(treatment,stratanum)) %>% 
      select(trt,everything()) %>% 
      as.data.frame()
    final_dummy_dat <- final_cov_dat %>%
      select(-c(new_strata,instrata_id))
  }
  
  
  else if (is.null(blocksize) == FALSE & is.null(covmat)==TRUE){
    
    x_ran <- self_blockrand(N=N,trt_levels = trt_levels,
                            blocksize = blocksize,rand_ratio = rand_ratio)
    
    final_cov_dat <- x_ran %>% 
      mutate(trt=as.integer(as.character(treatment))) %>% 
      select(id,trt) %>% 
      rename(pat_id=id) 
    final_dummy_dat <- final_cov_dat 
  } 
  else if (is.null(blocksize) == FALSE & is.null(covmat)==FALSE & sum(rand)==0){
    x_ran <- self_blockrand(N=N,trt_levels = trt_levels,
                            blocksize = blocksize,rand_ratio = rand_ratio)
    x_ran_1 <-  x_ran %>% 
      mutate(trt=as.integer(as.character(treatment))) %>% 
      select(id,trt) %>% 
      rename(pat_id=id)
    final_cov_dat <- cbind(x_ran_1,covmat)
    colnames(final_cov_dat) <- c("pat_id","trt",colnames(covmat))
    final_dummy_dat <- final_cov_dat 
  }
  
  else if (is.null(blocksize) == FALSE & is.null(covmat)==FALSE & sum(rand)>0){
    
    cov_starta <- as.data.frame(covmat[,rand])
    colnames(cov_starta) <- colnames(covmat)[rand]
    
    
    cov_mat_newstrata <- cov_mat$cov_mat %>% 
      mutate(pat_id=row_number()) %>% 
      mutate(new_strata = do.call(paste,c(cov_starta,sep=''))) %>% 
      group_by(new_strata) %>% 
      mutate(instrata_id=row_number())
    
    cov_mat_newstrata_count <- cov_mat$cov_mat %>% 
      mutate(new_strata = do.call(paste,c(cov_starta,sep=''))) %>% 
      group_by(new_strata) %>% 
      count() 
    # cov_mat_newstrata_count$n
    # cov_mat_newstrata_count$new_strata
    
    res_mat <- data.frame()
    for (i in 1:length(cov_mat_newstrata_count$n)){
      x_ran <- self_blockrand(N=cov_mat_newstrata_count$n[i],trt_levels = trt_levels,
                              blocksize = blocksize,rand_ratio = rand_ratio)
      strata <-  x_ran
      strata <- strata[1:cov_mat_newstrata_count$n[i],]
      strata$stratanum <- i
      strata$stratacod <- cov_mat_newstrata_count$new_strata[i]
      strata$id <- as.integer(strata$id)
      res_mat <- rbind(res_mat,strata)
      
    }
    res_mat <-as_tibble(res_mat)
    
    final_cov_dat <- cov_mat_newstrata %>% left_join(res_mat,by=c("instrata_id"="id","new_strata"="stratacod")) %>% 
      mutate(trt=as.integer(as.character(treatment))) %>% 
      select(-c(rand,block,treatment,stratanum)) %>% 
      select(trt,everything()) %>% 
      as.data.frame()
    final_dummy_dat <- final_cov_dat %>%
      select(-c(new_strata,instrata_id))  
    
    
  }
  
  
  cat_vars <- final_dummy_dat %>%
    select(-c(pat_id)) %>%
    colnames()
  final_dummy_dat[cat_vars] <- lapply(final_dummy_dat[cat_vars],factor)
  #str(pfs4)
  
  #dummy_dat <- data.frame(predict(dummyVars("~ .",data=final_dummy_dat), newdata=final_dummy_dat))
  #dummy_dat<-dummy_cols(final_dummy_dat,select_columns = cat_vars) %>% select(-all_of(cat_vars))
  dummy_dat<-dummy_convert(final_dummy_dat,cat_vars) %>% select(-all_of(cat_vars))
  dummy_dat <- dummy_dat %>%
    select(-pat_id) %>%
    select(-ends_with(".0"))
 
  
  HR_1 <- HR[!HR==1]
  return_list <- list(final_cov_dat=final_cov_dat
                      ,dummy_dat=dummy_dat,
                      HR=HR_1
  ) 
  
  return(return_list)
}


randomize_trt2 <- function(cov_mat=cov_mat,blocksize=blocksize,rand_ratio=c(1,1)){
  new_strata<-treatment<-stratanum<-trt<-instrata_id<-block<-pat_id<-NULL
  
  lvl.trt <- 2
  covmat <- cov_mat$cov_mat
  N <- cov_mat$sample_size
  rand <- unlist(cov_mat$strata_idx)
  HR <- c(unlist(cov_mat$HR))
  trt_levels <- c(seq(from=0,to=lvl.trt-1,by=1))
  blocksize <- blocksize
  prob_rand <- rand_ratio/sum(rand_ratio)
  
  if (is.null(blocksize)==TRUE & is.null(covmat)==TRUE){
    x_ran <- as.data.frame(cbind(seq(1,N),sample(trt_levels,N,replace = TRUE,prob = prob_rand)))
    colnames(x_ran) <- c("pat_id","trt")
    final_cov_dat <- x_ran
    final_dummy_dat <- final_cov_dat
  }
  
  else if (is.null(blocksize)==TRUE & is.null(covmat)==FALSE & sum(rand)==0){
    x_ran <- as.data.frame(cbind(seq(1,N),sample(trt_levels,N,replace = TRUE,prob = prob_rand),covmat))
    colnames(x_ran) <- c("pat_id","trt",colnames(covmat))
    final_cov_dat <- x_ran
    final_dummy_dat <- final_cov_dat
  }
  
  else if (is.null(blocksize)==TRUE & is.null(covmat)==FALSE & sum(rand)>0){
    cov_starta <- as.data.frame(covmat[,rand])
    colnames(cov_starta) <- colnames(covmat)[rand]
    
    
    cov_mat_newstrata <- cov_mat$cov_mat %>% 
      mutate(pat_id=row_number()) %>% 
      mutate(new_strata = do.call(paste,c(cov_starta,sep=''))) %>% 
      group_by(new_strata) %>% 
      mutate(instrata_id=row_number())
    
    cov_mat_newstrata_count <- cov_mat$cov_mat %>% 
      mutate(new_strata = do.call(paste,c(cov_starta,sep=''))) %>% 
      group_by(new_strata) %>% 
      count() 
    # cov_mat_newstrata_count$n
    # cov_mat_newstrata_count$new_strata
    
    res_mat <- data.frame()
    for (i in 1:length(cov_mat_newstrata_count$n)){
      strata <-  as.data.frame(cbind(seq(1,cov_mat_newstrata_count$n[i]),
                                     sample(trt_levels,cov_mat_newstrata_count$n[i],
                                            replace = TRUE,prob = prob_rand)))
      strata <- strata[1:cov_mat_newstrata_count$n[i],]
      colnames(strata) <- c("id","treatment")
      strata$stratanum <- i
      strata$stratacod <- cov_mat_newstrata_count$new_strata[i]
      strata$id <- as.integer(strata$id)
      res_mat <- rbind(res_mat,strata)
      
    }
    res_mat <-as_tibble(res_mat)
    
    final_cov_dat <- cov_mat_newstrata %>% left_join(res_mat,by=c("instrata_id"="id","new_strata"="stratacod")) %>% 
      mutate(trt=as.integer(as.character(treatment))) %>% 
      select(-c(treatment,stratanum)) %>% 
      select(trt,everything()) %>% 
      as.data.frame()
    final_dummy_dat <- final_cov_dat %>%
      select(-c(new_strata,instrata_id))
  }
  
  
  else if (is.null(blocksize) == FALSE & is.null(covmat)==TRUE){
    
    x_ran <- self_blockrand(N=N,trt_levels = trt_levels,
                            blocksize = blocksize,rand_ratio = rand_ratio)
    
    final_cov_dat <- x_ran %>% 
      mutate(trt=as.integer(as.character(treatment))) %>% 
      select(id,trt) %>% 
      rename(pat_id=id) 
    final_dummy_dat <- final_cov_dat 
  } 
  else if (is.null(blocksize) == FALSE & is.null(covmat)==FALSE & sum(rand)==0){
    x_ran <- self_blockrand(N=N,trt_levels = trt_levels,
                            blocksize = blocksize,rand_ratio = rand_ratio)
    x_ran_1 <-  x_ran %>% 
      mutate(trt=as.integer(as.character(treatment))) %>% 
      select(id,trt) %>% 
      rename(pat_id=id)
    final_cov_dat <- cbind(x_ran_1,covmat)
    colnames(final_cov_dat) <- c("pat_id","trt",colnames(covmat))
    final_dummy_dat <- final_cov_dat 
  }
  
  else if (is.null(blocksize) == FALSE & is.null(covmat)==FALSE & sum(rand)>0){
    
    cov_starta <- as.data.frame(covmat[,rand])
    colnames(cov_starta) <- colnames(covmat)[rand]
    
    
    cov_mat_newstrata <- cov_mat$cov_mat %>% 
      mutate(pat_id=row_number()) %>% 
      mutate(new_strata = do.call(paste,c(cov_starta,sep=''))) %>% 
      group_by(new_strata) %>% 
      mutate(instrata_id=row_number())
    
    cov_mat_newstrata_count <- cov_mat$cov_mat %>% 
      mutate(new_strata = do.call(paste,c(cov_starta,sep=''))) %>% 
      group_by(new_strata) %>% 
      count() 
    # cov_mat_newstrata_count$n
    # cov_mat_newstrata_count$new_strata
    
    res_mat <- data.frame()
    for (i in 1:length(cov_mat_newstrata_count$n)){
      x_ran <- self_blockrand(N=cov_mat_newstrata_count$n[i],trt_levels = trt_levels,
                              blocksize = blocksize,rand_ratio = rand_ratio)
      strata <-  x_ran
      strata <- strata[1:cov_mat_newstrata_count$n[i],]
      strata$stratanum <- i
      strata$stratacod <- cov_mat_newstrata_count$new_strata[i]
      strata$id <- as.integer(strata$id)
      res_mat <- rbind(res_mat,strata)
      
    }
    res_mat <-as_tibble(res_mat)
    
    final_cov_dat <- cov_mat_newstrata %>% left_join(res_mat,by=c("instrata_id"="id","new_strata"="stratacod")) %>% 
      mutate(trt=as.integer(as.character(treatment))) %>% 
      select(-c(rand,block,treatment,stratanum)) %>% 
      select(trt,everything()) %>% 
      as.data.frame()
    final_dummy_dat <- final_cov_dat %>%
      select(-c(new_strata,instrata_id))  
    
    
  }
  
  
  cat_vars <- final_dummy_dat %>%
    select(-c(pat_id)) %>%
    colnames()
  final_dummy_dat[cat_vars] <- lapply(final_dummy_dat[cat_vars],factor)
  #str(pfs4)
  
  # dummy_dat <- data.frame(predict(dummyVars("~ .", data=final_dummy_dat), newdata=final_dummy_dat))
  dummy_dat<-dummy_convert(final_dummy_dat,cat_vars) %>% select(-all_of(cat_vars))
  dummy_dat <- dummy_dat %>%
    select(-pat_id) %>%
    select(-ends_with(".0"))
  
  
  refindex<- c(0,cumsum(unlist(lapply(cov_mat$HR,length))))+1
  refindex<-refindex[-length(refindex)]
  HR_1<-HR[-refindex]
  return_list <- list(final_cov_dat=final_cov_dat
                      ,dummy_dat=dummy_dat,
                      HR=HR_1
  ) 
  
  return(return_list)
}









run_simulation<-function(samplesize, rand_ratio=c(1,1), blocksize, factors=NULL,trtHR=trtHR, trt_timeinterval=NULL, accrual_interval=NULL, accrual_rate=NULL, rampuptime=NULL,acceleration=NULL, lambda, gamma, timeinterval=NULL, dropoutrate=0,gammac=1,censordist='exponential', eventtarget=NULL,maxlpfollowup=NULL, N_simulation=1,alpha=0.05){
  trt<-trt.1<-NULL
  start<-Sys.time()
  
  TrialInfo<-NULL
  EventInfo<-NULL
  ModelResult<-NULL
  StraModelResult<-NULL
  AdjModelResult<-NULL
  
  for (i in 1:N_simulation){
    
    if (is.null(trt_timeinterval)){
      
      
      cov_mat <- cov_simu(sample_size = samplesize,factors = factors)
      generated_covmatrix <- randomize_trt2(cov_mat=cov_mat,blocksize = blocksize,rand_ratio=rand_ratio)
      design <- generated_covmatrix$dummy_dat %>% rename(trt=trt.1) %>% select(trt, everything())
      HR<-generated_covmatrix$HR
      strata_idx<-unlist(cov_mat$strata_idx)
      factornames<-names(cov_mat$cov_mat)
      stratanames<-factornames[strata_idx]
      
      betas<-log(c(trtHR,HR))
      x<-design
      xnames<-names(x)
      sim_data<-surv_data_simulation(lambda=lambda,gamma=gamma,timeinterval=timeinterval,x=x,betas=betas,dropoutrate=dropoutrate,censordist=censordist,gammac=gammac)
    }else{
      cov_mat <- cov_simu(sample_size = samplesize,factors = factors)
      generated_covmatrix <- randomize_trt2(cov_mat=cov_mat,blocksize = blocksize,rand_ratio=rand_ratio)
      design <- generated_covmatrix$dummy_dat %>% rename(trt=trt.1) %>% select(trt, everything())
      HR<-generated_covmatrix$HR
      strata_idx<-unlist(cov_mat$strata_idx)
      factornames<-names(cov_mat$cov_mat)
      stratanames<-factornames[strata_idx]
      
      if (is.null(factors)){
        betas<-data.frame(log(trtHR))
      }else{
        betas<-log(cbind(trtHR,t(matrix(rep(HR,length(trtHR)),ncol=length(trtHR)))))
      }
      
      x<-design
      xnames<-names(x)
      sim_data<-surv_data_simulation(lambda=lambda,gamma=gamma,x,betas=betas, dropoutrate=dropoutrate,gammac=gammac,censordist=censordist,trt_timeinterval=trt_timeinterval)
      
      
    }
    
    if (!is.null(accrual_interval)){
      accrual<-sort(step_accrual(samplesize,accrual_interval,accrual_rate))
      
    }else{
      accrual<-rep(0,samplesize)
    }
    
    trialdata<-trial_data_simulation(sim_data,accrual,eventtarget=eventtarget,maxlpfollowup=maxlpfollowup)
    mcox1<-coxph(Surv(time,status)~trt, data=trialdata$data, ties='breslow')
    m3<-survdiff(Surv(time,status)~trt, data=trialdata$data, rho=0)
    m2<-summary(survfit( Surv(time, status)~trt, data=trialdata$data))
    coxsum<-summary(mcox1)
    coxoutput<-data.frame(HR=exp(mcox1$coefficients),HR_SE=exp(mcox1$coefficients)*sqrt(mcox1$var), LogHR=mcox1$coefficients,LogHR_SE=coxsum$coefficients[3], cox_p=coxsum$coefficients[5], LogRank_p=1-pchisq(m3$chisq,1))
    
    events<-data.frame(N_treatment=m2$table[2,"records"],Event_treatment=m2$table[2,"events"],Median_surv_treatment=m2$table[2,"median"], Median_surv_treatment_CIL=m2$table[2,"0.95LCL"],Median_surv_treatment_CIU=m2$table[2,"0.95UCL"],N_control=m2$table[1,"records"],Event_control=m2$table[1,"events"],Median_surv_control=m2$table[1,"median"], Median_surv_control_CIL=m2$table[1,"0.95LCL"],Median_surv_control_CIU=m2$table[1,"0.95UCL"])
    
    TrialInfo<-rbind(TrialInfo,trialdata$Info)
    ModelResult<-rbind(ModelResult,coxoutput)
    
    
    if (length(factors)>0){
      
      factorvar<-NULL
      for (j in factornames){
        factorvar<-c(factorvar,xnames[grep(j,xnames)])
      }
      factorvar<-paste(factorvar,collapse="+")
      cm<-paste('mcox1<-coxph(Surv(time,status)~trt+', factorvar,', data=trialdata$data, ties=','\'breslow\'',')',collapse='')
      eval(parse(text=cm))
      
      
      
      coxsum<-summary(mcox1)
      adj_coxoutput<-data.frame(HR=exp(mcox1$coefficients)[1],HR_SE=exp(mcox1$coefficients[1])*sqrt(mcox1$var[1.1]), LogHR=mcox1$coefficients[1],LogHR_SE=coxsum$coefficients[1,3], cox_p=coxsum$coefficients[1,5])
      AdjModelResult<-rbind(AdjModelResult,adj_coxoutput)
      
    }
    
    
    
    if (length(stratanames)>0){
      stratavar<-NULL
      flevel<-prod(unlist(lapply(cov_mat$HR[strata_idx],length)))
      for (j in stratanames){
        stratavar<-c(stratavar,xnames[grep(j,xnames)])
      }
      #stratified
      strata<-paste(stratavar,collapse=",")
      
      cm<-paste('mcox1<-coxph(Surv(time,status)~trt+strata(', strata,'), data=trialdata$data, ties=','\'breslow\'',')',collapse='')
      eval(parse(text=cm))
      
      cm<-paste("m3<-survdiff(Surv(time,status)~trt+strata(", strata,"), data=trialdata$data, rho=0)")
      eval(parse(text=cm))
      
      cm<-paste("m2<-summary(survfit( Surv(time, status)~trt+strata(", strata,"), data=trialdata$data))")
      eval(parse(text=cm))
      
      coxsum<-summary(mcox1)
      stra_coxoutput<-data.frame(HR=exp(mcox1$coefficients),HR_SE=exp(mcox1$coefficients)*sqrt(mcox1$var), LogHR=mcox1$coefficients,LogHR_SE=coxsum$coefficients[3], cox_p=coxsum$coefficients[5], LogRank_p=1-pchisq(m3$chisq,1))
      StraModelResult<-rbind(StraModelResult,stra_coxoutput)
      
      events$N_empty_strata<-2*flevel-nrow(m2$table)
      events$zero_event_strata<-length(which(m2$table[,"events"]==0))
    }
    
    EventInfo<-rbind(EventInfo,events)
  }
  rownames(TrialInfo)<-NULL
  rownames(EventInfo)<-NULL
  rownames(ModelResult)<-NULL
  
  stop<-Sys.time()
  print(stop-start)
  
  writeLines("\n")
  writeLines("==================================================Trial Settings=================================================")
  writeLines("\n")
  writeLines(paste("Total Number of Patients: ", samplesize))
  writeLines(paste("Randomization Ratio (control vs treatment): ", rand_ratio[1],":",rand_ratio[2]))
  if ( is.null(blocksize)){
    writeLines("Blocksize: None")
  }else{
     writeLines(paste("Blocksize:",blocksize*2))
  }
 
  writeLines(paste("Event Hazard Function Parameters: lambda =", lambda,", Gamma = ",gamma))
  if (length(dropoutrate)==1)
  {
    writeLines(paste("Dropout Rate: ", dropoutrate))
  }else{
    writeLines(paste("Dropout Rate: Control- ", dropoutrate[1],"  Treatment- ",dropoutrate[2]))
  }
  
  writeLines(paste("Censor Hazard Function:",censordist))
  
  
  if (!is.null(trt_timeinterval)){
    writeLines(paste("Hazar ratio time windows: ", paste(trt_timeinterval,collapse=",")))
  }
  if (!is.null(eventtarget)){
    writeLines(paste("Target Total Event Number: ", eventtarget))
  }
  if (!is.null(maxlpfollowup)){
    writeLines(paste("Maximum Follow up time for last patient: ",maxlpfollowup))
  }
  writeLines(paste("Number of Simulations: ",N_simulation))
  writeLines("\n")
  
  writeLines("==================================================Trial Summary==================================================")
  writeLines("\n")
  
  var_summary<-function(value,rname,power=FALSE,alpha=0.05){
    if (power){
      
      t<-data.frame(N_simulations=length(value), Mean=round(mean(value,na.rm=TRUE),4), SD=round(sd(value,na.rm=TRUE),4), Median=round(median(value,na.rm=TRUE),4), Q1=round(quantile(value,0.25,na.rm=TRUE),4), Q3=round(quantile(value, 0.75,na.rm=TRUE),4), Min=round(min(value,na.rm=TRUE),4),Max=round(max(value,na.rm=TRUE),4), Power=round(length(which(value<alpha))/length(value),4))
      rownames(t)<-rname
    }else{
      t<-data.frame(N_simulations=length(value), Mean=round(mean(value,na.rm=TRUE),4), SD=round(sd(value,na.rm=TRUE),4), Median=round(median(value,na.rm=TRUE),4), Q1=round(quantile(value,0.25,na.rm=TRUE),4), Q3=round(quantile(value, 0.75,na.rm=TRUE),4), Min=round(min(value,na.rm=TRUE),4),Max=round(max(value,na.rm=TRUE),4))
      rownames(t)<-rname
    }
    return(t)
  }
  
  tt<-NULL
  tt<-rbind(tt,var_summary(EventInfo$N_treatment,"Number of patients (treatment)"))
  tt<-rbind(tt,var_summary(EventInfo$N_control,"Number of patients (control)"))
  tt<-rbind(tt,var_summary(EventInfo$Event_treatment,"Number of events (treatment)"))
  tt<-rbind(tt,var_summary(EventInfo$Event_control,"Number of events (control)"))
  tt<-rbind(tt,var_summary(EventInfo$Median_surv_treatment,"Median survival time (treatment)"))
  tt<-rbind(tt,var_summary(EventInfo$Median_surv_control,"Median survival time (control)"))
  tt<-rbind(tt,var_summary(TrialInfo$Dropout_rate_treatment,"Dropout rate (treatment)"))
  tt<-rbind(tt,var_summary(TrialInfo$Dropout_rate_control,"Dropout rate (control)"))
  tt<-rbind(tt,var_summary(TrialInfo$Dropout_rate,"Dropout rate (all)"))
  tt<-rbind(tt,var_summary(TrialInfo$LastEnrollTime,"Enrollment period"))
  tt<-rbind(tt,var_summary(TrialInfo$DataCutTime,"Datacut time"))
 
  
  
  
  print(tt)
  
  writeLines("\n")
  
  writeLines("=================================================== Cox Model ===================================================")
  writeLines("\n")
  
  tt<-NULL
  
  tt<-rbind(tt,cbind(var_summary(ModelResult$HR,"Hazrd Ratio (treatment vs control)"),Power=""))
  
  tt<-rbind(tt,cbind(var_summary(ModelResult$LogHR,"Log Hazrd Ratio (treatment vs control)"),Power=""))
  tt<-rbind(tt,var_summary(ModelResult$cox_p,"P-value (treatment vs control)",power=TRUE,alpha=alpha))
  
  print(tt)
  writeLines("\n")
  
  
  
  
  if (length(stratanames)>0){
    
    writeLines("============================================== Stratified Cox Model =============================================")
    writeLines("\n")
    
    tt<-NULL
    tt<-rbind(tt,cbind(var_summary(StraModelResult$HR,"Hazrd Ratio (treatment vs control)"),Power=""))
    
    tt<-rbind(tt,cbind(var_summary(StraModelResult$LogHR,"Log Hazrd Ratio (treatment vs control)"),Power=""))
    tt<-rbind(tt,var_summary(StraModelResult$cox_p,"P-value (treatment vs control)",power=TRUE,alpha=alpha))
    
    print(tt)
    writeLines("\n")
    
    
  }
  
  if (length(factors)>0){
    
    
    
    writeLines("=============================================== Adjusted Cox Model ==============================================")
    writeLines("\n")
    
    tt<-NULL
    tt<-rbind(tt,cbind(var_summary(AdjModelResult$HR,"Hazrd Ratio (treatment vs control)"),Power=""))
    
    tt<-rbind(tt,cbind(var_summary(AdjModelResult$LogHR,"Log Hazrd Ratio (treatment vs control)"),Power=""))
    tt<-rbind(tt,var_summary(AdjModelResult$cox_p,"P-value (treatment vs control)",power=TRUE,alpha=alpha))
    
    print(tt)
    writeLines("\n")
    writeLines("=================================================================================================================")
    
  }
  
  outdata<-list(TrialInfo=TrialInfo,EventInfo=EventInfo,ModelResult=ModelResult)
  if (length(stratanames)>0){
    outdata<-c(outdata,list(StraModelResult=StraModelResult))
  }
  if (length(factors)>0){
    outdata<-c(outdata,list(AdjModelResult=AdjModelResult))
  }
  
  if (N_simulation==1){
    outdata<-c(outdata,list(Data=trialdata$data))
  }
  
  return(outdata)
}





projection<-function(snapshot_data, enroll_continue=FALSE, samplesize=0, rand_ratio=c(1,1), blocksize=1,accrual_interval=NULL, accrual_rate=NULL, lambda=NULL, trtHR=NULL, dropoutrate=NULL,eventtarget=0,maxlpfollowup=NULL){
  
  
  onstudy<-status<-eventtime<-time_ss<-status_ss<-trt<-Freq<-NULL
  
  . = NULL
  
  lastaccrual<-max(snapshot_data$accrual)
  lastobservation<-max(snapshot_data$accrual+snapshot_data$time)
  npatient<-nrow(snapshot_data)
  

  
  if (!('onstudy' %in% names(snapshot_data))){
    snapshot_data$onstudy<-'Y'
  }
  
  if ('trt' %in% names(snapshot_data)){
    blinded<-FALSE
  }else{
    blinded<-TRUE
  }
  
  if (is.null(dropoutrate)){
    drops<-snapshot_data %>% subset(onstudy=="N")
    if (is.null(drops)){
      dropoutrate<-0
    }else{
      dropoutrate<-nrow(drops %>% subset(status==0))/nrow(drops)
    }
      
  }
  if (is.null(dropoutrate)){
    dropoutrate<-0
  }
  
  
  if (blinded){
    
    if (is.null(lambda)){
      m<-survreg(Surv(time, status) ~ 1,  dist="exponential", data=snapshot_data)
      lambda<-1/exp(m$coefficients)
      
    }
    
    completed<-snapshot_data %>% subset(onstudy=='N' | status==1)
    ongoing<-snapshot_data %>% subset(onstudy=='Y' & status==0)
    ongoing<-ongoing %>% rename(time_ss=time,status_ss=status)
    
    if (enroll_continue & samplesize>npatient){
      nnew<-samplesize-npatient
      newaccrual<-step_accrual(np=nnew,tt=accrual_interval,arate=accrual_rate)+lastaccrual
      newpatient<-data.frame(time=rep(0,nnew),status=rep(0,nnew),accrual=newaccrual) %>% rename(time_ss=time,status_ss=status)
     
      ongoing<-bind_rows(ongoing,newpatient)
    }
    
    
    nongoing<-nrow(ongoing)
    
    if (nongoing>0){
      tmpongoing<-weibullsim(x=ongoing,lambda=lambda,gamma=1)
      if (dropoutrate==0){
        tmpongoing<-tmpongoing %>% rename(time=eventtime)
      }else{
        tmpongoing<-censordata(simdata=tmpongoing,lambda=lambda,gamma=1,dropoutrate=dropoutrate)
      }
      
      ongoing<-cbind(ongoing,tmpongoing) %>% mutate(time=time+time_ss) %>% select(-time_ss,-status_ss)
    }
    
    simdata<-rbind(completed,ongoing)
    
    
    outdata<-trial_data_simulation(simdata=simdata,accrual=simdata$accrual,eventtarget=eventtarget,maxlpfollowup = maxlpfollowup)
    ncensor<-count(ongoing,status) %>% subset(status==0) %>% pull(n)
    if (length(ncensor)==0){
      dr<-0
    }else{
      dr<-ncensor/nrow(ongoing)
    }
    outdata$Info$Dropout_rate_sim<-round(dr,4)
    return(outdata)
    
    
  }else{
    
    control<- snapshot_data %>% subset(trt==0)
    treatment<-snapshot_data %>% subset(trt==1)
    
    if (is.null(lambda)){
      
      m<-survreg(Surv(time, status) ~ 1,  dist="exponential", data=control)
      lambda<-1/exp(m$coefficients)
      
    }
    
    if (is.null(trtHR)){
      #mcox<-coxph(Surv(time,status)~trt, data=snapshot_data, ties='breslow')
      #trtHR<-exp(mcox$coefficients)
      m<-survreg(Surv(time, status) ~ 1,  dist="exponential", data=treatment)
      lambda_t<-1/exp(m$coefficients)
      trtHR<-lambda_t/lambda
    }
    betas<-log(trtHR)
    
      completed<-snapshot_data %>% subset(onstudy=='N' | status==1)
      ongoing<-snapshot_data %>% subset(onstudy=='Y' & status==0)
      ongoing<-ongoing %>% rename(time_ss=time,status_ss=status)
      
      if (enroll_continue & samplesize>npatient){
        nnew<-samplesize-npatient
        cov_mat <- cov_simu(sample_size = nnew,factors = NULL)
        trt_cov <- randomize_trt2(cov_mat=cov_mat,blocksize = blocksize,rand_ratio=rand_ratio)$final_cov_dat$trt
        
        newaccrual<-sort(step_accrual(np=nnew,tt=accrual_interval,arate=accrual_rate))+lastaccrual
        
        newpatient<-data.frame(trt=trt_cov,time=rep(0,nnew),status=rep(0,nnew),accrual=newaccrual) %>% rename(time_ss=time,status_ss=status)
        
        ongoing<-bind_rows(ongoing,newpatient)
      }
      
      
      nongoing<-nrow(ongoing)
      
      if (nongoing>0){
        x<-ongoing %>% select(trt)
        
        tmpsim<-weibullsim(x=x,lambda=lambda,gamma=1,betas=betas)
        
        group<-data.frame(covgroup=do.call(paste,c(x,sep='-')), ebx=exp(as.matrix(x)%*%betas ))
        gfreq<-as.data.frame(table(group$ebx)) %>% mutate(Freq=Freq/nrow(x)) %>% mutate_all(., function(x){as.numeric(as.character(x))})
        names(gfreq)[1]<-'ebx'
        
        if(dropoutrate==0){
          tmpongoing<-tmpsim %>% rename(time=eventtime)
        }else{
          tmpongoing<-censordata(simdata=tmpsim,lambda=lambda,gamma=1,dropoutrate=dropoutrate,ebx=gfreq$ebx,groupfreq=gfreq$Freq)
        }
        
        ongoing<-cbind(ongoing,tmpongoing) %>% mutate(time=time+time_ss) %>% select(-time_ss,-status_ss)

        
      }
      
    
    simdata<-rbind(completed,ongoing)
    
    
    outdata<-trial_data_simulation(simdata=simdata,accrual=simdata$accrual,eventtarget=eventtarget,maxlpfollowup = maxlpfollowup)
    
    ncensor<-count(ongoing,status) %>% subset(status==0) %>% pull(n)
    if (length(ncensor)==0){
      dr<-0
    }else{
      dr<-ncensor/nrow(ongoing)
    }
    outdata$Info$Dropout_rate_sim<-round(dr,4)
    
    return(outdata)
    
  }

}



projection_simulation<-function(snapshot_data,  rand_ratio=c(1,1), enroll_continue=FALSE, samplesize=0, blocksize=1,accrual_interval=NULL, accrual_rate=NULL, lambda=NULL, trtHR=NULL, dropoutrate=NULL,eventtarget=NULL,maxlpfollowup=NULL,N_simulation=1){
 trt<-NULL
  
if ('trt' %in% names(snapshot_data)){
  blind_data<-snapshot_data %>% select(-trt)
  
  Info<-NULL
  
  curinfo<-NULL
  for (i in 1:N_simulation){
    data_projection<-projection(snapshot_data=blind_data, rand_ratio=rand_ratio, enroll_continue=enroll_continue, samplesize=samplesize, blocksize=blocksize,accrual_interval=accrual_interval, accrual_rate=accrual_rate, lambda=NULL, trtHR=NULL,dropoutrate=dropoutrate,eventtarget=eventtarget,maxlpfollowup = maxlpfollowup)
    curinfo<-rbind(curinfo,data_projection$Info) 
  }
  
  writeLines("\n")
  
  writeLines("====================================Trial Summary 1=========================================")
  writeLines("===================Method: Blinded,Hazard Parameter from Snapshot===========================")
  writeLines("\n")
  
  print_info(curinfo)
  curinfo$Method<-"Blinded,Hazard Parameter from Snapshot"
  curinfo$MethodCD='Blinded-Snapshot'
  Info<-rbind(Info,curinfo)
  
  
  curinfo<-NULL
  for (i in 1:N_simulation){
    data_projection<-projection(snapshot_data=snapshot_data, rand_ratio=rand_ratio, enroll_continue=enroll_continue, samplesize=samplesize, blocksize=blocksize,accrual_interval=accrual_interval, accrual_rate=accrual_rate, lambda=NULL, trtHR=NULL,dropoutrate=dropoutrate,eventtarget=eventtarget,maxlpfollowup = maxlpfollowup)
    curinfo<-rbind(curinfo,data_projection$Info) 
  }
  
  writeLines("\n")
  
  writeLines("====================================Trial Summary 2==========================================")
  writeLines("===================Method: Unblinded,Hazard Parameter from Snapshot==========================")
  writeLines("\n")
  
  print_info(curinfo)
  curinfo$Method<-"Unblinded,Hazard Parameter from Snapshot"
  curinfo$MethodCD='Unblinded-Snapshot'
  Info<-rbind(Info,curinfo)
  
  if (!(is.null(lambda)) & !(is.null(trtHR))){
    
    curinfo<-NULL
    for (i in 1:N_simulation){
      data_projection<-projection(snapshot_data=snapshot_data, rand_ratio=rand_ratio, enroll_continue=enroll_continue, samplesize=samplesize, blocksize=blocksize,accrual_interval=accrual_interval, accrual_rate=accrual_rate, lambda=lambda, trtHR=trtHR,dropoutrate=dropoutrate,eventtarget=eventtarget,maxlpfollowup = maxlpfollowup)
      curinfo<-rbind(curinfo,data_projection$Info) 
    }
    
    writeLines("\n")
    
    writeLines("====================================Trial Summary 3=========================================")
    writeLines("===================Method: Unblinded, User Provided Hazard Paramter=========================")
    writeLines("\n")
    
    print_info(curinfo)
    curinfo$Method<-"Unblinded,User Provided Hazard Parameter"
    curinfo$MethodCD='Unblinded-User'
    Info<-rbind(Info,curinfo)
    
  }
  
  
  
  return(Info)
  
}else{
  
  Info<-NULL
  curinfo<-NULL
  for (i in 1:N_simulation){
    data_projection<-projection(snapshot_data=snapshot_data, rand_ratio=rand_ratio, enroll_continue=enroll_continue, samplesize=samplesize, blocksize=blocksize,accrual_interval=accrual_interval, accrual_rate=accrual_rate, lambda=NULL, trtHR=NULL,dropoutrate=dropoutrate,eventtarget=eventtarget,maxlpfollowup = maxlpfollowup)
    curinfo<-rbind(curinfo,data_projection$Info) 
  }
  
  writeLines("\n")
  
  
  writeLines("====================================Trial Summary 1=========================================")
  writeLines("===================Method: Blinded,Hazard Parameter from Snapshot===========================")
  writeLines("\n")
  
  print_info(curinfo)
  curinfo$Method<-"Blinded,Hazard Parameter from Snapshot"
  curinfo$MethodCD='Blinded-Snapshot'
  Info<-rbind(Info,curinfo)
  
  if (!is.null(lambda)){
    curinfo<-NULL
    for (i in 1:N_simulation){
      data_projection<-projection(snapshot_data=snapshot_data, rand_ratio=rand_ratio, enroll_continue=enroll_continue, samplesize=samplesize, blocksize=blocksize,accrual_interval=accrual_interval, accrual_rate=accrual_rate, lambda=lambda, trtHR=trtHR,dropoutrate=dropoutrate,eventtarget=eventtarget,maxlpfollowup = maxlpfollowup)
      curinfo<-rbind(curinfo,data_projection$Info) 
    }
  
    curinfo$Method<-"Blinded, User Provided Hazard Parameter"
    curinfo$MethodCD='Blinded-User'
    
    writeLines("=====================================Trial Summary 2===============================")
    writeLines("===================Method: Blinded, User Provided Hazard Parameter=================")
    writeLines("\n")
    
    print_info(curinfo)
    
    Info<-rbind(Info,curinfo)
  }
  
  return(Info)
  
 }
 
}




print_info<-function(Info){
  tt<-NULL
  value<-Info$N_patient
  t<-data.frame(N=length(value), Mean=round(mean(value,na.rm=TRUE),4), SD=round(sd(value,na.rm=TRUE),4), Median=round(median(value,na.rm=TRUE),4), Q1=round(quantile(value,0.25,na.rm=TRUE),4), Q3=round(quantile(value, 0.75,na.rm=TRUE),4), Min=round(min(value,na.rm=TRUE),4),Max=round(max(value,na.rm=TRUE),4))
  rownames(t)<-"N_patient"
  tt<-rbind(tt,t)
  
  
  value<-Info$N_event
  t<-data.frame(N=length(value), Mean=round(mean(value,na.rm=TRUE),4), SD=round(sd(value,na.rm=TRUE),4), Median=round(median(value,na.rm=TRUE),4), Q1=round(quantile(value,0.25,na.rm=TRUE),4), Q3=round(quantile(value, 0.75,na.rm=TRUE),4), Min=round(min(value,na.rm=TRUE),4),Max=round(max(value,na.rm=TRUE),4))
  rownames(t)<-"N_event"
  tt<-rbind(tt,t)
  
  value<-Info$Dropout_rate_sim
  t<-data.frame(N=length(value), Mean=round(mean(value,na.rm=TRUE),4), SD=round(sd(value,na.rm=TRUE),4), Median=round(median(value,na.rm=TRUE),4), Q1=round(quantile(value,0.25,na.rm=TRUE),4), Q3=round(quantile(value, 0.75,na.rm=TRUE),4), Min=round(min(value,na.rm=TRUE),4),Max=round(max(value,na.rm=TRUE),4))
  rownames(t)<-"Dropout_rate"
  tt<-rbind(tt,t)
  
  value<-Info$LastEnrollTime
  t<-data.frame(N=length(value), Mean=round(mean(value,na.rm=TRUE),4), SD=round(sd(value,na.rm=TRUE),4), Median=round(median(value,na.rm=TRUE),4), Q1=round(quantile(value,0.25,na.rm=TRUE),4), Q3=round(quantile(value, 0.75,na.rm=TRUE),4), Min=round(min(value,na.rm=TRUE),4),Max=round(max(value,na.rm=TRUE),4))
  rownames(t)<-"LastEnrollTime"
  tt<-rbind(tt,t)
  
  
  value<-Info$DataCutTime
  t<-data.frame(N=length(value), Mean=round(mean(value,na.rm=TRUE),4), SD=round(sd(value,na.rm=TRUE),4), Median=round(median(value,na.rm=TRUE),4), Q1=round(quantile(value,0.25,na.rm=TRUE),4), Q3=round(quantile(value, 0.75,na.rm=TRUE),4), Min=round(min(value,na.rm=TRUE),4),Max=round(max(value,na.rm=TRUE),4))
  rownames(t)<-"Datacut_time"
  tt<-rbind(tt,t)
  
  print(tt)
  
  writeLines("\n")
  
}


censor_surv<-function(eventtime,censortime, x){
  simdata<-data.frame(eventtime=eventtime,censortime=censortime)
  simdata<- simdata %>% mutate( time=ifelse(censortime<eventtime,censortime,eventtime)) %>% mutate(status=ifelse(censortime<eventtime,0,1))%>% select(-eventtime,-censortime)
  return(cbind(x,simdata))
}






run_simulation_simsurv<-function(samplesize, rand_ratio=c(1,1), blocksize, factors=NULL, accrual_interval=NULL, accrual_rate=NULL,  eventtarget=NULL,maxlpfollowup=NULL, N_simulation=1,alpha=0.05, simsurv1=NULL, simsurv2=NULL){
  trt<-trt.1<-NULL
  simsurv_e<-NULL
  simsurv_c<-NULL
  trt_timeinterval<-NULL
  timeinterval<-NULL
  trtHR<-NULL
  lambda<-NULL
  gamma<-NULL
  gammac<-NULL
  dropoutrate<-NULL
  censordist<-NULL
  
  start<-Sys.time()
  
  TrialInfo<-NULL
  EventInfo<-NULL
  ModelResult<-NULL
  StraModelResult<-NULL
  AdjModelResult<-NULL
  
  for (i in 1:N_simulation){
    
    if (!(is.null(simsurv1))){
      
      cov_mat <- cov_simu(sample_size = samplesize,factors = factors)
      generated_covmatrix <- randomize_trt2(cov_mat=cov_mat,blocksize = blocksize,rand_ratio=rand_ratio)
      design <- generated_covmatrix$dummy_dat %>% rename(trt=trt.1) %>% select(trt, everything())
      x<-design
      strata_idx<-unlist(cov_mat$strata_idx)
      factornames<-names(cov_mat$cov_mat)
      stratanames<-factornames[strata_idx]
      xnames<-names(x)
      cm1<-paste("simsurv_e=",simsurv1)
      eval(parse(text=cm1))
      
      if (!(is.null(simsurv2))){
        cm2<-paste("simsurv_c=",simsurv2)
        eval(parse(text=cm2))
        sim_data<-censor_surv(simsurv_e$eventtime,simsurv_c$eventtime,x)
      }else{
        simsurv_c<-data.frame(eventtime=rep(Inf,nrow(simsurv_e)),status=rep(1,nrow(simsurv_e)))
        sim_data<-censor_surv(simsurv_e$eventtime,simsurv_c$eventtime,x)
      }
      
    }else{
      
      if (is.null(trt_timeinterval)){
        
        
        cov_mat <- cov_simu(sample_size = samplesize,factors = factors)
        generated_covmatrix <- randomize_trt2(cov_mat=cov_mat,blocksize = blocksize,rand_ratio=rand_ratio)
        design <- generated_covmatrix$dummy_dat %>% rename(trt=trt.1) %>% select(trt, everything())
        HR<-generated_covmatrix$HR
        strata_idx<-unlist(cov_mat$strata_idx)
        factornames<-names(cov_mat$cov_mat)
        stratanames<-factornames[strata_idx]
        
        betas<-log(c(trtHR,HR))
        x<-design
        xnames<-names(x)
        sim_data<-surv_data_simulation(lambda=lambda,gamma=gamma,timeinterval=timeinterval,x=x,betas=betas,dropoutrate=dropoutrate,censordist=censordist,gammac=gammac)
      }else{
        cov_mat <- cov_simu(sample_size = samplesize,factors = factors)
        generated_covmatrix <- randomize_trt2(cov_mat=cov_mat,blocksize = blocksize,rand_ratio=rand_ratio)
        design <- generated_covmatrix$dummy_dat %>% rename(trt=trt.1) %>% select(trt, everything())
        HR<-generated_covmatrix$HR
        strata_idx<-unlist(cov_mat$strata_idx)
        factornames<-names(cov_mat$cov_mat)
        stratanames<-factornames[strata_idx]
        
        if (is.null(factors)){
          betas<-data.frame(log(trtHR))
        }else{
          betas<-log(cbind(trtHR,t(matrix(rep(HR,length(trtHR)),ncol=length(trtHR)))))
        }
        
        x<-design
        xnames<-names(x)
        sim_data<-surv_data_simulation(lambda=lambda,gamma=gamma,x,betas=betas, dropoutrate=dropoutrate,gammac=gammac,censordist=censordist,trt_timeinterval=trt_timeinterval)
        
        
      }
    }
    
    
    
    
    
    if (!is.null(accrual_interval)){
      accrual<-sort(step_accrual(samplesize,accrual_interval,accrual_rate))
      
    }else{
      accrual<-rep(0,samplesize)
    }
    
    trialdata<-trial_data_simulation(sim_data,accrual,eventtarget=eventtarget,maxlpfollowup=maxlpfollowup)
    mcox1<-coxph(Surv(time,status)~trt, data=trialdata$data, ties='breslow')
    m3<-survdiff(Surv(time,status)~trt, data=trialdata$data, rho=0)
    m2<-summary(survfit( Surv(time, status)~trt, data=trialdata$data))
    coxsum<-summary(mcox1)
    coxoutput<-data.frame(HR=exp(mcox1$coefficients),HR_SE=exp(mcox1$coefficients)*sqrt(mcox1$var), LogHR=mcox1$coefficients,LogHR_SE=coxsum$coefficients[3], cox_p=coxsum$coefficients[5], LogRank_p=1-pchisq(m3$chisq,1))
    
    events<-data.frame(N_treatment=m2$table[2,"records"],Event_treatment=m2$table[2,"events"],Median_surv_treatment=m2$table[2,"median"], Median_surv_treatment_CIL=m2$table[2,"0.95LCL"],Median_surv_treatment_CIU=m2$table[2,"0.95UCL"],N_control=m2$table[1,"records"],Event_control=m2$table[1,"events"],Median_surv_control=m2$table[1,"median"], Median_surv_control_CIL=m2$table[1,"0.95LCL"],Median_surv_control_CIU=m2$table[1,"0.95UCL"])
    
    TrialInfo<-rbind(TrialInfo,trialdata$Info)
    ModelResult<-rbind(ModelResult,coxoutput)
    
    
    if (length(factors)>0){
      
      factorvar<-NULL
      for (j in factornames){
        factorvar<-c(factorvar,xnames[grep(j,xnames)])
      }
      factorvar<-paste(factorvar,collapse="+")
      cm<-paste('mcox1<-coxph(Surv(time,status)~trt+', factorvar,', data=trialdata$data, ties=','\'breslow\'',')',collapse='')
      eval(parse(text=cm))
      
      
      
      coxsum<-summary(mcox1)
      adj_coxoutput<-data.frame(HR=exp(mcox1$coefficients)[1],HR_SE=exp(mcox1$coefficients[1])*sqrt(mcox1$var[1.1]), LogHR=mcox1$coefficients[1],LogHR_SE=coxsum$coefficients[1,3], cox_p=coxsum$coefficients[1,5])
      AdjModelResult<-rbind(AdjModelResult,adj_coxoutput)
      
    }
    
    
    
    if (length(stratanames)>0){
      stratavar<-NULL
      flevel<-prod(unlist(lapply(cov_mat$HR[strata_idx],length)))
      for (j in stratanames){
        stratavar<-c(stratavar,xnames[grep(j,xnames)])
      }
      #stratified
      strata<-paste(stratavar,collapse=",")
      
      cm<-paste('mcox1<-coxph(Surv(time,status)~trt+strata(', strata,'), data=trialdata$data, ties=','\'breslow\'',')',collapse='')
      eval(parse(text=cm))
      
      cm<-paste("m3<-survdiff(Surv(time,status)~trt+strata(", strata,"), data=trialdata$data, rho=0)")
      eval(parse(text=cm))
      
      cm<-paste("m2<-summary(survfit( Surv(time, status)~trt+strata(", strata,"), data=trialdata$data))")
      eval(parse(text=cm))
      
      coxsum<-summary(mcox1)
      stra_coxoutput<-data.frame(HR=exp(mcox1$coefficients),HR_SE=exp(mcox1$coefficients)*sqrt(mcox1$var), LogHR=mcox1$coefficients,LogHR_SE=coxsum$coefficients[3], cox_p=coxsum$coefficients[5], LogRank_p=1-pchisq(m3$chisq,1))
      StraModelResult<-rbind(StraModelResult,stra_coxoutput)
      
      events$N_empty_strata<-2*flevel-nrow(m2$table)
      events$zero_event_strata<-length(which(m2$table[,"events"]==0))
    }
    
    EventInfo<-rbind(EventInfo,events)
  }
  rownames(TrialInfo)<-NULL
  rownames(EventInfo)<-NULL
  rownames(ModelResult)<-NULL
  
  stop<-Sys.time()
  print(stop-start)
  
  writeLines("\n")
  writeLines("==================================================Trial Settings=================================================")
  writeLines("\n")
  writeLines(paste("Total Number of Patients: ", samplesize))
  writeLines(paste("Randomization Ratio (control vs treatment): ", rand_ratio[1],":",rand_ratio[2]))
  if ( is.null(blocksize)){
    writeLines("Blocksize: None")
  }else{
    writeLines(paste("Blocksize:",blocksize*2))
  }
  
  if (!is.null(simsurv1)){
    writeLines(paste("Simsurv command for event: ", simsurv1))
  }
  if (!is.null(simsurv2)){
    writeLines(paste("Simsurv command for dropout: ", simsurv2))
  }
  
  if (!is.null(eventtarget)){
    writeLines(paste("Target Total Event Number: ", eventtarget))
  }
  if (!is.null(maxlpfollowup)){
    writeLines(paste("Maximum Follow up time for last patient: ",maxlpfollowup))
  }
  writeLines(paste("Number of Simulations: ",N_simulation))
  writeLines("\n")
  
  writeLines("==================================================Trial Summary==================================================")
  writeLines("\n")
  
  var_summary<-function(value,rname,power=FALSE,alpha=0.05){
    if (power){
      
      t<-data.frame(N_simulations=length(value), Mean=round(mean(value,na.rm=TRUE),4), SD=round(sd(value,na.rm=TRUE),4), Median=round(median(value,na.rm=TRUE),4), Q1=round(quantile(value,0.25,na.rm=TRUE),4), Q3=round(quantile(value, 0.75,na.rm=TRUE),4), Min=round(min(value,na.rm=TRUE),4),Max=round(max(value,na.rm=TRUE),4), Power=round(length(which(value<alpha))/length(value),4))
      rownames(t)<-rname
    }else{
      t<-data.frame(N_simulations=length(value), Mean=round(mean(value,na.rm=TRUE),4), SD=round(sd(value,na.rm=TRUE),4), Median=round(median(value,na.rm=TRUE),4), Q1=round(quantile(value,0.25,na.rm=TRUE),4), Q3=round(quantile(value, 0.75,na.rm=TRUE),4), Min=round(min(value,na.rm=TRUE),4),Max=round(max(value,na.rm=TRUE),4))
      rownames(t)<-rname
    }
    return(t)
  }
  
  tt<-NULL
  tt<-rbind(tt,var_summary(EventInfo$N_treatment,"Number of patients (treatment)"))
  tt<-rbind(tt,var_summary(EventInfo$N_control,"Number of patients (control)"))
  tt<-rbind(tt,var_summary(EventInfo$Event_treatment,"Number of events (treatment)"))
  tt<-rbind(tt,var_summary(EventInfo$Event_control,"Number of events (control)"))
  tt<-rbind(tt,var_summary(EventInfo$Median_surv_treatment,"Median survival time (treatment)"))
  tt<-rbind(tt,var_summary(EventInfo$Median_surv_control,"Median survival time (control)"))
  tt<-rbind(tt,var_summary(TrialInfo$Dropout_rate_treatment,"Dropout rate (treatment)"))
  tt<-rbind(tt,var_summary(TrialInfo$Dropout_rate_control,"Dropout rate (control)"))
  tt<-rbind(tt,var_summary(TrialInfo$Dropout_rate,"Dropout rate (all)"))
  tt<-rbind(tt,var_summary(TrialInfo$LastEnrollTime,"Enrollment period"))
  tt<-rbind(tt,var_summary(TrialInfo$DataCutTime,"Datacut time"))
  
  
  
  
  print(tt)
  
  writeLines("\n")
  
  writeLines("=================================================== Cox Model ===================================================")
  writeLines("\n")
  
  tt<-NULL
  
  tt<-rbind(tt,cbind(var_summary(ModelResult$HR,"Hazrd Ratio (treatment vs control)"),Power=""))
  
  tt<-rbind(tt,cbind(var_summary(ModelResult$LogHR,"Log Hazrd Ratio (treatment vs control)"),Power=""))
  tt<-rbind(tt,var_summary(ModelResult$cox_p,"P-value (treatment vs control)",power=TRUE,alpha=alpha))
  
  print(tt)
  writeLines("\n")
  
  
  
  
  if (length(stratanames)>0){
    
    writeLines("============================================== Stratified Cox Model =============================================")
    writeLines("\n")
    
    tt<-NULL
    tt<-rbind(tt,cbind(var_summary(StraModelResult$HR,"Hazrd Ratio (treatment vs control)"),Power=""))
    
    tt<-rbind(tt,cbind(var_summary(StraModelResult$LogHR,"Log Hazrd Ratio (treatment vs control)"),Power=""))
    tt<-rbind(tt,var_summary(StraModelResult$cox_p,"P-value (treatment vs control)",power=TRUE,alpha=alpha))
    
    print(tt)
    writeLines("\n")
    
    
  }
  
  if (length(factors)>0){
    
    
    
    writeLines("=============================================== Adjusted Cox Model ==============================================")
    writeLines("\n")
    
    tt<-NULL
    tt<-rbind(tt,cbind(var_summary(AdjModelResult$HR,"Hazrd Ratio (treatment vs control)"),Power=""))
    
    tt<-rbind(tt,cbind(var_summary(AdjModelResult$LogHR,"Log Hazrd Ratio (treatment vs control)"),Power=""))
    tt<-rbind(tt,var_summary(AdjModelResult$cox_p,"P-value (treatment vs control)",power=TRUE,alpha=alpha))
    
    print(tt)
    writeLines("\n")
    writeLines("=================================================================================================================")
    
  }
  
  outdata<-list(TrialInfo=TrialInfo,EventInfo=EventInfo,ModelResult=ModelResult)
  if (length(stratanames)>0){
    outdata<-c(outdata,list(StraModelResult=StraModelResult))
  }
  if (length(factors)>0){
    outdata<-c(outdata,list(AdjModelResult=AdjModelResult))
  }
  
  if (N_simulation==1){
    outdata<-c(outdata,list(Data=trialdata$data))
  }
  
  return(outdata)
}

