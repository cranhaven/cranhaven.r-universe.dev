#' @importFrom forecast Arima
#' @importFrom forecast forecast


# Create environment ----
pkg.env <- new.env()


#Supported models ----

pkg.env$models=list(
  
  ## a model
  a = StMoMo::StMoMo(link="log",
                       staticAgeFun = TRUE,
                       periodAgeFun = NULL,
                       cohortAgeFun = NULL),
  ## p model
  
  p = StMoMo::StMoMo(link="log",
                        staticAgeFun = FALSE,
                        periodAgeFun = c("1"),
                        cohortAgeFun = NULL),
  ## c model
  c = StMoMo::StMoMo(link="log",
                        staticAgeFun = FALSE,
                        periodAgeFun = NULL,
                        cohortAgeFun = c("1")),
  ## ac model
  
  ac = StMoMo::StMoMo(link="log",
                     staticAgeFun = TRUE,
                     periodAgeFun = NULL,
                     cohortAgeFun = c("1")),
  
  ## ap model
  
  ap = StMoMo::StMoMo(link="log",
                     staticAgeFun = TRUE,
                     periodAgeFun = c("1"),
                     cohortAgeFun = NULL),
  
  
  ## pc model
  
  pc = StMoMo::StMoMo(link="log",
                     staticAgeFun = FALSE,
                     periodAgeFun = c("1"),
                     cohortAgeFun = c("1")),
  
  ## apc model
  
  apc = StMoMo::apc(),
  
  ## cbd model
  
  cbd = StMoMo::cbd(link = c("log")),
  
  ## m6 model
  
  m6 = StMoMo::m6(link = c("log")),
  
  ## m7 model
  
  m7 = StMoMo::m7(link = c("log"))
  
  
  )



# Utils ----
## Transform the upper run-off triangle to the life-table representation.
pkg.env$t2c <- function(x){
  "
  Transform the upper run-off triangle to the life-table representation.
  "
  I= dim(x)[1]
  J= dim(x)[2]
  
  mx=matrix(NA,nrow=I,ncol=J)
  for(i in 1:(I)){
    for(j in 1:(J)){
      if(i+j<=J+1){
        mx[j,(i+j-1)]=x[i,j]
      }
    }
  }
  return(mx)
}

## Rotate a matrix
pkg.env$rotate <- function(x){ 
  "
  It rotates a matrix by 90 degrees.
  "
  
  
  t(apply(x, 2, rev))}

pkg.env$c2t <- function(x){
  I= dim(x)[1]
  J= dim(x)[2]
  
  mx=matrix(NA,nrow=I,ncol=J)
  for(i in 1:(I)){
    for(j in 1:(J)){
      if(i+j<=J+1){
        mx[i,j]=x[j,(i+j-1)]
      }
    }
  }
  return(mx)
}


pkg.env$t2c.full.square <- function(x){
    "
  Function to transform a full run-off triangle into a square.
  
  This function takes a run-off triangle as input.
  
  It returns a square.
  "
    
    I= dim(x)[1]
    J= dim(x)[2]
    
    mx = matrix(NA, nrow = I, ncol = 2 * J)
    for (i in 1:(I)) {
      for (j in 1:(J)) {
        mx[j,(i+j-1)]=x[i,j]
        
      }
    }
    return(mx)
  }


# lee-carter models
pkg.env$fit.lc.nr <- function(data.T,
                              iter.max=1e+04,
                              tolerance.max=1e-06){
  "
  Fits the LC model via a Netwon-Rhapson from scratch implementation to the data.
  
  "
  
  data.O <- data.T$occurrance
  data.E <- data.T$exposure
  
  wxc <- data.T$fit.w
  
  # wxc <- matrix(1,
  #               nrow = dim(data.O)[1],
  #               ncol=dim(data.O)[2])
  # wxc[is.na(data.E)]<-0
  
  
  ## parameters initial values
  ax = runif(dim(data.O)[1])
  bx = rep(1/dim(data.O)[1],dim(data.O)[1])
  kt = rep(0,dim(data.O)[2])
  
  #create matrices to compute the linear predictor
  ax.mx = matrix(rep(ax,dim(data.O)[2]),
                 byrow = F,
                 ncol=dim(data.O)[2])
  
  bx.mx = matrix(rep(bx,
                     dim(data.O)[2]),
                 byrow = F,
                 ncol=dim(data.O)[2])
  
  kt.mx = matrix(rep(kt,
                     dim(data.O)[1]),
                 byrow = T,
                 nrow=dim(data.O)[1])
  
  
  mu.mx = exp(ax.mx+bx.mx*kt.mx)
  
  dhat = data.E*mu.mx
  
  dxt0=1000
  niter=1000
  
  deltaax=100
  deltabx=100
  deltakt=100
  
  deltadxt=100
  
  kt0=(abs(kt))
  ax0=(abs(ax))
  bx0=(abs(bx))
  
  citer=0
  
  dxt.v=NULL
  
  deltakt.v=NULL
  deltaax.v=NULL
  deltabx.v=NULL
  
  deltadxt.v=NULL
  
  kt1.v=NULL
  
  converged=TRUE
  
  while((abs(deltaax)>tolerance.max|abs(deltabx)>tolerance.max|abs(deltakt)>tolerance.max)&citer<iter.max){
    #update alpha
    ax.num = apply((data.O-dhat)*wxc, 1, sum, na.rm=T)
    ax.den = apply(dhat*wxc, 1, sum, na.rm=T)
    ax = ax - ax.num/(-ax.den)
    
    deltaax=sum(abs(ax)-ax0,na.rm = T)
    ax0=abs(ax)
    deltaax.v=c(deltaax.v,deltaax)
    
    ## compute the new estimates
    ax.mx = matrix(rep(ax,dim(data.O)[2]),
                   byrow = F,
                   ncol=dim(data.O)[2])
    mu.mx = exp(ax.mx+bx.mx*kt.mx)
    
    dhat = data.E*mu.mx
    
    #update gc
    kt.num = apply((data.O-dhat)*wxc*bx.mx, 2, sum, na.rm=T)
    kt.den = apply(dhat*wxc*(bx.mx^2), 2, sum, na.rm=T)
    kt = kt - kt.num/(-kt.den)
    kt[2]=0
    
    deltakt=sum(abs(kt)-kt0,na.rm = T)
    kt0=(abs(kt))
    deltakt.v=c(deltakt.v,deltakt)
    
    kt1.v=c(kt1.v,kt[2])
    
    ## compute the new estimates
    
    kt.mx = matrix(rep(kt,
                       dim(data.O)[1]),
                   byrow = T,
                   nrow=dim(data.O)[1])
    
    mu.mx = exp(ax.mx+bx.mx*kt.mx)
    
    dhat = data.E*mu.mx
    
    #update beta
    bx.num = apply((data.O-dhat)*wxc*kt.mx, 1, sum, na.rm=T)
    bx.den = apply(dhat*wxc*(kt.mx^2), 1, sum, na.rm=T)
    bx = bx - bx.num/(-bx.den)
    bx=bx/sum(bx,na.rm = T)
    
    deltabx=sum(abs(bx)-bx0,na.rm = T)
    bx0=abs(bx)
    deltabx.v=c(deltabx.v,deltabx)
    
    ## compute the new estimates
    bx.mx = matrix(rep(bx,
                       dim(data.O)[2]),
                   byrow = F,
                   ncol=dim(data.O)[2])
    mu.mx = exp(ax.mx+bx.mx*kt.mx)
    
    dhat = data.E*mu.mx
    
    dxt1= sum(2*wxc*(data.O*log(data.O/dhat)-(data.O-dhat)),na.rm = T)
    deltadxt= dxt1-dxt0
    
    deltadxt.v=c(deltadxt.v,deltadxt)
    dxt.v=c(dxt.v,dxt1)
    
    dxt0=dxt1
    citer=citer+1
  }
  
  
  if((abs(deltaax)>tolerance.max|abs(deltabx)>tolerance.max|abs(deltakt)>tolerance.max)){
    warning('The Newton-Rhapson algorithm for the lee-carter may have not converged')
    converged=FALSE
  }
  
  return(list(ax=ax,
              bx=bx,
              kt=kt,
              citer=citer,
              data.T=data.T,
              converged=converged,
              deltaax=deltaax,
              deltabx=deltabx,
              deltakt=deltakt,
              dxt0=dxt0,
              deltadxt=deltadxt,
              converged=converged
              ))
  
}


pkg.env$fit.aac.nr <- function(data.T,
                              iter.max=1e+04,
                              tolerance.max=1e-06,
                              ax.start=NULL,
                              gc.start=NULL){
  "
  Fits the AAC model via a Netwon-Rhapson from scratch implementation to the data.
  
  "
  data.O <- pkg.env$rotate(pkg.env$c2t(data.T$occurrance))
  data.E <- pkg.env$rotate(pkg.env$c2t(data.T$exposure))
  
  wxc <- matrix(1,
                nrow = dim(data.O)[1],
                ncol=dim(data.O)[2])
  wxc[is.na(data.E)]<-0
  
  
  ## parameters initial values
  ax = ax.start
  bx = rep(1/dim(data.O)[1],dim(data.O)[1])
  gc = gc.start
  
  #create matrices to compute the linear predictor
  ax.mx = matrix(rep(ax,dim(data.O)[2]),
                 byrow = F,
                 ncol=dim(data.O)[2])
  
  bx.mx = matrix(rep(bx,
                     dim(data.O)[2]),
                 byrow = F,
                 ncol=dim(data.O)[2])
  
  gc.mx = matrix(rep(gc,
                     dim(data.O)[1]),
                 byrow = T,
                 nrow=dim(data.O)[1])
  
  
  mu.mx = exp(ax.mx+bx.mx*gc.mx)
  
  dhat = data.E*mu.mx
  
  dxt0=1000
  niter=1000
  
  deltaax=100
  deltabx=100
  deltagc=100
  
  deltadxt=100
  
  gc0=(abs(gc))
  ax0=(abs(ax))
  bx0=(abs(bx))
  
  citer=0
  
  dxt.v=NULL
  
  deltagc.v=NULL
  deltaax.v=NULL
  deltabx.v=NULL
  
  deltadxt.v=NULL
  
  gc1.v=NULL
  
  converged=TRUE
  
  while((abs(deltaax)>tolerance.max|abs(deltabx)>tolerance.max|abs(deltagc)>tolerance.max)&citer<iter.max){
    #update alpha
    ax.num = apply((data.O-dhat)*wxc, 1, sum, na.rm=T)
    ax.den = apply(dhat*wxc, 1, sum, na.rm=T)
    ax = ax - ax.num/(-ax.den)
    
    deltaax=sum(abs(ax)-ax0,na.rm = T)
    ax0=abs(ax)
    deltaax.v=c(deltaax.v,deltaax)
    
    ## compute the new estimates
    ax.mx = matrix(rep(ax,dim(data.O)[2]),
                   byrow = F,
                   ncol=dim(data.O)[2])
    mu.mx = exp(ax.mx+bx.mx*gc.mx)
    
    dhat = data.E*mu.mx
    
    #update gc
    gc.num = apply((data.O-dhat)*wxc*bx.mx, 2, sum, na.rm=T)
    gc.den = apply(dhat*wxc*(bx.mx^2), 2, sum, na.rm=T)
    gc = gc - gc.num/(-gc.den)
    gc[1]=0
    
    deltagc=sum(abs(gc)-gc0,na.rm = T)
    gc0=(abs(gc))
    deltagc.v=c(deltagc.v,deltagc)
    
    # gc1.v=c(gc1.v,gc[2])
    
    ## compute the new estimates
    
    gc.mx = matrix(rep(gc,
                       dim(data.O)[1]),
                   byrow = T,
                   nrow=dim(data.O)[1])
    
    mu.mx = exp(ax.mx+bx.mx*gc.mx)
    
    dhat = data.E*mu.mx
    
    #update beta
    bx.num = apply((data.O-dhat)*wxc*gc.mx, 1, sum, na.rm=T)
    bx.den = apply(dhat*wxc*(gc.mx^2), 1, sum, na.rm=T)
    bx = bx - bx.num/(-bx.den)
    bx=bx/sum(bx,na.rm = T)
    
    deltabx=sum(abs(bx)-bx0,na.rm = T)
    bx0=abs(bx)
    deltabx.v=c(deltabx.v,deltabx)
    
    ## compute the new estimates
    bx.mx = matrix(rep(bx,
                       dim(data.O)[2]),
                   byrow = F,
                   ncol=dim(data.O)[2])
    mu.mx = exp(ax.mx+bx.mx*gc.mx)
    
    dhat = data.E*mu.mx
    
    dxt1= sum(2*wxc*(data.O*log(data.O/dhat)-(data.O-dhat)),na.rm = T)
    deltadxt= dxt1-dxt0
    
    deltadxt.v=c(deltadxt.v,deltadxt)
    dxt.v=c(dxt.v,dxt1)
    
    dxt0=dxt1
    citer=citer+1
  }
  
  if((abs(deltaax)>tolerance.max|abs(deltabx)>tolerance.max|abs(deltagc)>tolerance.max)){
    warning('The Newton-Rhapson algorithm for the lee-carter may have not converged')
    converged=FALSE
  }
  
  return(list(ax=ax,
              bx=bx,
              gc=rev(gc), #reverse it as you read the data in the opposite direction
              citer=citer,
              converged=converged,
              deltaax=deltaax,
              deltabx=deltabx,
              deltagc=deltagc,
              dxt0=dxt0,
              deltadxt=deltadxt,
              converged=converged
  ))
  
}

# forecasting with linear trend


pkg.env$fcst <- function(object,
                         hazard.model,
                         gk.fc.model='a',
                         ckj.fc.model='a',
                         gk.order=c(1,1,0),
                         ckj.order=c(0,1,0)){
  
  
  J=dim(object$Dxt)[2]
  
  rates=array(.0,dim=c(J,J))
  
  if(!is.null(hazard.model)){
    
    a.tf <- grepl('a',hazard.model)
    c.tf <- grepl('c',hazard.model)
    p.tf <- grepl('p',hazard.model)
    kt.f=NULL
    gc.f=NULL
    
  }
  
  ## cohorts model
  if(c.tf){
    # c.model <- substr(fc.model,1,1)
    gc.nNA <- max(which(!is.na(object$gc)))
    
    if(gk.fc.model=='a'){
      
      gc.model <- Arima(object$gc[1:gc.nNA], 
                                  order = gk.order, 
                                  include.constant = T)
      gc.f <- forecast(gc.model,h=(length(object$cohorts)-gc.nNA))
      
      
    }else{
      
      gc.data=data.frame(y=object$gc[1:gc.nNA],
                         x=object$cohorts[1:gc.nNA])
      new.gc.data <- data.frame(x=object$cohorts[(gc.nNA+1):length(object$cohorts)])
      
      gc.model <- lm('y~x', 
                     data=gc.data)
      
      gc.f <- forecast::forecast(gc.model,
                                 newdata=new.gc.data)
      
      
    }
    
    #forecasting rates
    cond = !is.na(object$gc)
    gc.2.add = c(object$gc[cond],gc.f$mean)
    gc.mx= matrix(rep(unname(gc.2.add),
                      J),
                  byrow = F,
                  nrow=J)
    rates <- gc.mx+rates
    rates <- pkg.env$t2c.full.square(rates)
    rates[is.na(rates)]=.0
    rates <- rates[,(J+1):(2*J)]
    
    
    
  }
  
  ## period model
  if(p.tf){
    # p.model <- substr(fc.model,2,2)
    kt.nNA <- max(which(!is.na(object$kt[1, ])))
    
    if(ckj.fc.model=='a'){
      
      kt.model=forecast::Arima(as.vector(object$kt[1:kt.nNA]),ckj.order,include.constant = T)
      kt.f <- forecast::forecast(kt.model,h=J)
      
    }else{
      
      kt.data=data.frame(y=object$kt[1:kt.nNA],
                         x=object$years[1:kt.nNA])
      new.kt.data <- data.frame(x=seq(J+1,2*J))
      
      kt.model <- lm('y~x', 
                     data=kt.data)
      
      kt.f <- forecast::forecast(kt.model,newdata=new.kt.data)
      
    }
    #forecasting rates
    kt.mx = matrix(rep(unname(kt.f$mean),
                       J),
                   byrow = T,
                   nrow=J)
    
    rates=kt.mx+rates
    
  }
  
  # projecting age
  if(a.tf){
    
    ax.mx = matrix(rep(unname(object$ax),
                       J),
                   byrow = F,
                   nrow=J)
    ax.mx[is.na(ax.mx)]=.0
    rates=ax.mx+rates
  }
  
  output<- list(rates=exp(rates),
                kt.f=kt.f,
                gc.f=gc.f)
  
  return(output)
  
}


pkg.env$create_lower_triangle <- function(lt){
  
  J <- dim(lt)[1]
  J.stop <- dim(lt)[2]
  out <- array(NA,c(J,J))
  
  for(age in 1:J){
    
    for(calendar in 1:J.stop){
      
      tmp.ix <- J+calendar
      if(tmp.ix-age+1<=J & age<=J){
      out[tmp.ix-age+1,age] <- lt[age,calendar]}
      
      
    }
    
  }
  
  return(out)
  
  
}

pkg.env$create_full_triangle <- function(cumulative.payments.triangle, lt){
  
  J <- dim(lt)[1]
  J.stop <- dim(lt)[2]
  out <- cumulative.payments.triangle
  
  for(age in 1:J){
    
    for(calendar in 1:J.stop){
      
      tmp.ix <- J+calendar
      if(tmp.ix-age+1<=J & age<=J){
        out[tmp.ix-age+1,age] <- lt[age,calendar]}
      
      
    }
    
  }
  
  return(out)
  
  
}


pkg.env$find.development.factors <- function(J,
                                             age.eff,
                                             period.eff,
                                             cohort.eff,
                                             eta){

  # Function that finds the development factors on the upper triangle.
  
  out <- array(NA,dim=c(J,J))
  
  ax <- c(NA,age.eff[!is.na(age.eff)])
  
  if(!is.null(cohort.eff)){
   gc <-  c(cohort.eff[!is.na(cohort.eff)],NA) #last cohort effect will be extrapolated
   
   
  }else{gc<-rep(0,J)}
  
  if(!is.null(period.eff)){
    
    kt <-  c(NA,period.eff[!is.na(period.eff)]) #the first period is disregarded
    
    
  }else{kt<-rep(0,J)}
  
  
  for(i in 1:J){
    for(j in 1:J){
      
      if((i+j-1)<=J){out[i,j] <- ax[j]+gc[i]+kt[i+j-1]}
      
    }
    
  }
  
  out <- (1+(1-eta)*exp(out))/(1-(eta*exp(out)))
  
  return(out)
  
  }


pkg.env$find.fitted.effects <- function(J,
                                       age.eff,
                                       period.eff,
                                       cohort.eff,
                                       effect_log_scale){
  
  # Function that finds the fitted effects.
  
  ax <- c(NA,age.eff[!is.na(age.eff)])
  names(ax) <- c(0:(length(ax)-1))
  
  if(!is.null(cohort.eff)){
    gc <-  c(cohort.eff[!is.na(cohort.eff)],NA) #last cohort effect will be extrapolated
    names(gc) <- c(0:(length(gc)-1))
    
  }else{gc<-NULL}
  
  if(!is.null(period.eff)){
    
    kt <-  c(NA,period.eff[!is.na(period.eff)]) #the first period is disregarded
    names(kt) <- c(0:(length(kt)-1))
    
  }else{kt<-NULL}
  
  out <- list(
    fitted_development_effect= ax,
    fitted_calendar_effect=kt,
    fitted_accident_effect= gc
    )
  
  if(effect_log_scale==FALSE){out<-lapply(out,exp)}
  
  return(out)
  
  
  }

