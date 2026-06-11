utils::globalVariables(c("x"))
d_hat<-function(data,m=4,g, range=NULL,lattice=NULL, selection=TRUE,criterion="BIC"){
  n<-length(data)
  if(is.null(lattice)&is.null(range)){ stop('one between range and lattice must be specified')
  }else if((!is.null(lattice))&(!is.null(range))){ stop('only one between range and lattice must be specified')
  }else if(!is.null(lattice)){
    N<-length(lattice)
    L=lattice[1]
    U=lattice[N]
    pG<-g(lattice)
    Gx<-cumsum(pG)
    Gf<-function(x){Gx[which(lattice==x)]}
    Gf<-Vectorize(Gf)
    ptilde<-c()
    for(i in 1:N){
      ptilde[i]<-sum(data==lattice[i])/n}
    TjG<-as.matrix(LP.basis(pG,m+1))
    TjG<-as.matrix(TjG[,-(m+1)])
    LPj<-c()
    for(j in 1:m){
      LPj[j]<-sum(TjG[,j]*ptilde)}
    if(selection==TRUE){
      LPj<-denoise(LPj,n,criterion)}

    z.L2<- approxExtrap(Gx,1+TjG%*%LPj,xout=c(0,Gx,1),f=1,method="constant")
    dhat<-approxfun(z.L2$x,z.L2$y, rule=2,f=1,method="constant",ties=mean)
    dhat<-Vectorize(dhat)
    z.L2<- approxExtrap(lattice,1+TjG%*%LPj,xout=c(L,lattice,U),f=1,method="constant")
    dhat.x<-approxfun(z.L2$x,z.L2$y, rule=2,f=1,method="constant",ties=mean)
    dhat.x<-Vectorize(dhat.x)
    if(sum(pG*max(dhat(Gx),0))!=1){
      fcm<-function(K3)sum(pG*ifelse(dhat(Gx)-K3>0,dhat(Gx)-K3,0))-1
      K3<-uniroot(fcm,interval = c(-10,10),tol = .Machine$double.eps^3)$root[1]
      d_Gajek.u<-function(u)ifelse(dhat(u)-K3>0,dhat(u)-K3,0)
      d_Gajek.u<-Vectorize(d_Gajek.u)
      d_Gajek.x<-function(x)ifelse(dhat.x(x)-K3>0,dhat.x(x)-K3,0)
      d_Gajek.x<-Vectorize(d_Gajek.u)
    }else{ d_Gajek.u<-function(u)max(dhat(u),0)
    d_Gajek.x<-function(x)max(dhat.x(x),0)
    d_Gajek.x<-Vectorize(d_Gajek.x)}
  }else{
    L<-range[1]
    U<-range[2]
    G<-function(x)integrate(g,L,x)$value
    G<-Vectorize(G)
    u<-G(data)
    S<- as.matrix(Legj(u, m))
    LPj <- apply(S,FUN="mean",2)
    if(selection==TRUE){
      LPj<-denoise(LPj,n,criterion)}
    z.L2<- approxExtrap(u,1 + S%*%LPj,xout=c(0,u,1))
    dhat<- approxfun(z.L2$x,z.L2$y, rule=2,ties=mean)
    z.L2<- approxExtrap(data,1 + S%*%LPj,xout=c(L,data,U))
    dhat.x<- approxfun(z.L2$x,z.L2$y, rule=2,ties=mean)
    f<-function(x)g(x)*max(dhat(G(x)),0)
    if(integrate(f,L,U,stop.on.error = FALSE)$value!=1){
      fgaj<-function(x,K)g(x)*ifelse(dhat(G(x))>=0,dhat(G(x))-K,0)
      fminus1<-function(K3)integrate(fgaj,K=K3,L,U,stop.on.error = FALSE)$value-1
      K<-uniroot(fminus1,interval = c(-10,10),tol = .Machine$double.eps^3)$root[1]
      d_Gajek.u<-function(u)ifelse(dhat(u)>=0,dhat(u)-K,0)
      d_Gajek.u<-Vectorize(d_Gajek.u)
      d_Gajek.x<-function(x)ifelse(dhat.x(x)>=0,dhat.x(x)-K,0)
      d_Gajek.x<-Vectorize(d_Gajek.x)
    }else{ d_Gajek.u<-function(u)max(dhat(u),0)
    d_Gajek.x<-function(x)max(dhat.x(x),0)
    d_Gajek.x<-Vectorize(d_Gajek.x)}
  }
  fx<-function(x)g(x)*d_Gajek.x(x)
  return(list(LPj=LPj,du=d_Gajek.u,dx=d_Gajek.x,f=fx))
}

dmixtruncnorm<-function(x,pis,means,sds,range){
  if(sum(pis)-1>0.0001)stop("The sum of the pis must be 1.")
  p<-length(means)
  init<-0
  for(j in 1:p){
    init<-paste(init,"+",pis[j],"*dtruncnorm(x,a=", range[1], ",b=",range[2],",mean=",means[j],",sd=",sds[j],")",sep="")}
  h<-function(x)eval(parse(text=init))
  h<-Vectorize(h)
  return(h(x))}

rmixtruncnorm<-function(n,pis,means,sds,range){
  if(sum(pis)-1>0.0001)stop("The sum of the pis must be 1.")
  p<-length(means)
  Mat<-matrix(rep(0,n*p),ncol=p,nrow=n)
  for(j in 1:p){
    Mat[,j]<-rtruncnorm(n,a=range[1],b=range[2],mean=means[j],sd=sds[j])
  }
  flags <- t(rmultinom(n, size=rep(1,p), prob=pis))
  sample <-as.numeric(apply(Mat*flags,1,sum))
  return(sample)}

dmixnegbinom<-function(x,pis,size,probs){
  if(sum(pis)-1>0.0001)stop("The sum of the pis must be 1.")
  p<-length(probs)
  init<-0
  for(j in 1:p){
    init<-paste(init,"+",pis[j],"*dnbinom(x,size=",size,",prob=",probs[j],")",sep="")}
  h<-function(x)eval(parse(text=init))
  h<-Vectorize(h)
  return(h(x))}

rmixnegbinom<-function(n,pis,size,probs){
  if(sum(pis)-1>0.0001)stop("The sum of the pis must be 1.")
  p<-length(probs)
  Mat<-matrix(rep(0,n*p),ncol=p,nrow=n)
  for(j in 1:p){
    Mat[,j]<-rnbinom(n,size=size,prob=probs[j])
  }
  flags <- t(rmultinom(n, size=rep(1,p), prob=pis))
  sample <-as.numeric(apply(Mat*flags,1,sum))
  return(sample)}

find_h_cont<-function(data,g,dhat,range=NULL,M_0=NULL,par0=NULL,lbs,ubs,check.plot=TRUE,ylim.f=c(0,2),ylim.d=c(0,2),global=FALSE){
  message("The optimization may take several minutes...")
  p<-(length(par0)+1)/3
  L<-range[1]
  U<-range[2]
  G<-function(x)integrate(g,L,x)$value
  G<-Vectorize(G)
  f<-function(x)g(x)*dhat(x)
  h<-function(x,par){
    init<-0
    pis<-par[1:(p-1)]
    means<-par[p:(2*p-1)]
    sds<-par[(2*p):length(par)]
    for(j in 1:(p-1)){
      init<-paste(init,"+",pis[j],"*dtruncnorm(x,a=", L, ",b=",U,",mean=",means[j],",sd=",sds[j],")",sep="")}
    init<-paste(init,"+",1-sum(pis),"*dtruncnorm(x,a=", L, ",b=",U,",mean=",means[p],",sd=",sds[p],")",sep="")
    return((eval(parse(text=init))))}
  dHF<-function(x,par) f(x)/h(x,par)
  dHG=function(x,par) g(x)/h(x,par)
  d_M<-function(x,par){(ifelse(dhat(x)>=1,dHF(x,par),0)+
                          ifelse(dhat(x)<1,dHG(x,par),0))}
  minusd_M<-function(x,par){-(ifelse(dhat(x)>=1,dHF(x,par),0)+
                                ifelse(dhat(x)<1,dHG(x,par),0))}

  opts1<-list("algorithm"="NLOPT_GN_DIRECT","xtol_rel"=1.e-5,maxeval=400)
  maxH<-function(par){
    xMstar<-nloptr(x0=M_0,par=par,eval_f=minusd_M,lb=L,ub=U,
                   opts=opts1)$solution
    return(d_M(xMstar,par))
  }
  if (global == TRUE){
    opts<-list("algorithm"="NLOPT_GN_DIRECT","xtol_rel"=1.e-5,maxeval=400)}
  else{
    opts<-list("algorithm"="NLOPT_LN_BOBYQA","xtol_rel"=1.e-8,maxeval=2000)}
  parMstar<-nloptr(x0=par0,eval_f=maxH,lb=lbs,ub=ubs,opts=opts)$solution

  if(check.plot==TRUE){
    oldpar <- par(mfrow=c(1,2),mar=c(5,5,1.5,1))
    on.exit(par(oldpar))
    par(mfrow=c(1,2),mar=c(5,5,1.5,1))
    curve(h(x,parMstar),ylim=ylim.f,xlim=range,lwd=3,lty=3,col="mediumpurple1",ylab="Density",cex.axis=2,cex.lab=2)
    curve(f(x),col="darkgreen",lty=4,lwd=2, add=TRUE)
    curve(g(x),add=TRUE,lty=2,lwd=2,cex=1.5,col="tomato3")
    curve(maxH(parMstar)*h(x,parMstar),lwd=2,lty=1,col="black",add=TRUE)
    legend("topright", legend=c(expression(hat(f)[m](x)),expression(g(x)),expression(h(x)),expression(paste("M"^"*",h(x)))),
           lty=c(4,2,3,1),lwd=c(2,2,3,2) ,col=c("darkgreen","tomato3","mediumpurple1","black"), cex=1,
           box.col=rgb(0, 0, 0,0))
    curve(d_M(x,parMstar),xlim=range,ylim=ylim.d,lwd=3,lty=3,col="mediumpurple1",ylab="Density",cex.axis=2,cex.lab=2)
    curve(dHF(x,parMstar),col="darkgreen",lty=4,lwd=2,add=TRUE)
    curve(dHG(x,parMstar),add=TRUE,lty=2,lwd=2,cex=1.5,col="tomato3")
    curve(maxH(parMstar)*d_M(x,parMstar),lwd=2,lty=1,col="black",add=TRUE)
    legend("bottomleft", legend=c(expression(paste(d,"(",H(x),";",H,",",hat(F)[m],")")),expression(paste(d,"(",H(x),";",H,",",G,")")),
                                  expression(paste(d,"(",H(x),";",H,",",hat(F)[m],")I(", x%in%D^+" ",")+",
                                                   d,"(",H(x),";",H,",",G,,")I(",x %in% D^-" ",")")),
                                  expression(paste("M"^"*","[",d,"(",H(x),";",H,",",hat(F)[m],")I(",x %in% D^+" ",")+",
                                                   d,"(",H(x),";",H,",",G,,")I(",x %in% D^-" ",")]"))),
           lty=c(4,2,3,1),lwd=c(2,2,3,2) ,col=c("darkgreen","tomato3","mediumpurple1","black"), cex=1,
           box.col=rgb(0, 0, 0,0),bg="transparent")
    message("...if  not satisfactory, try changing the initial parameters or make arugment global TRUE.")
  }
  Mstar=maxH(parMstar)
  pis<-c(parMstar[1:(p-1)],1-sum(parMstar[1:(p-1)]))
  means<-parMstar[p:(2*p-1)]
  sds<-parMstar[(2*p):length(parMstar)]
  hopt=function(x)dmixtruncnorm(x,pis,means,sds,range)
  hopt=Vectorize(hopt)
  return(list(Mstar=Mstar,pis=pis,means=means,sds=sds,h=hopt))
}

find_h_disc<-function(data,g,dhat,lattice=NULL,M_0=NULL,size,par0=NULL,check.plot=TRUE,ylim.f=c(0,2),ylim.d=c(0,2),global=FALSE){
  message("The optimization may take several minutes...")
  L<-lattice[1]
  U<-lattice[length(lattice)]
  p<-(length(par0))/2
  pp<-length(par0)
  N<-length(lattice)
  pG<-g(lattice)
  Gx<-cumsum(pG)
  Gf<-function(x){Gx[which(lattice==x)]}
  Gf<-Vectorize(Gf)
  f<-function(x)g(x)*dhat(x)
  h<-function(x,par){
    init<-0
    pis<-par[1:(p-1)]
    probs<-par[(p+1):length(par)]
    for(j in 1:(p-1)){
      init<-paste(init,"+",pis[j],"*dnbinom(x,size=",size,",prob=",probs[j],")",sep="")}
    init<-paste(init,"+",1-sum(pis),"*dnbinom(x,size=",size,",prob=",probs[p],")",sep="")
    return((eval(parse(text=init))))}
  dHF_init=function(x,par) f(x)/h(x,par)
  dHF<-function(x,par){
    z.L2=approxExtrap(lattice, dHF_init(lattice, par), xout=c(L,lattice,U), f=1,method="constant")
    dhf=approxfun(z.L2$x,z.L2$y, rule=2,f=1,method="constant",ties=mean)
    return (dhf(x))
  }
  dHF<-Vectorize(dHF,"x")
  dHG_init=function(x,par) g(x)/h(x,par)
  dHG<-function(x,par){
    z.L2=approxExtrap(lattice, dHG_init(lattice, par), xout=c(L,lattice,U), f=1,method="constant")
    dhg=approxfun(z.L2$x,z.L2$y, rule=2,f=1,method="constant",ties=mean)
    return (dhg(x))
  }
  dHG<-Vectorize(dHG,"x")
  d_M<-function(x,par){(ifelse(dhat(x)>=1,dHF(x,par),0)+
                          ifelse(dhat(x)<1,dHG(x,par),0))}
  minusd_M<-function(x,par){-(ifelse(dhat(x)>=1,dHF(x,par),0)+
                                ifelse(dhat(x)<1,dHG(x,par),0))}
  minusd_M(lattice,par0)
  if (global == TRUE){
    opts1<-list("algorithm"="NLOPT_GN_DIRECT","xtol_rel"=1.e-5,maxeval=500)
  } else{
    opts1<-list("algorithm"="NLOPT_LN_BOBYQA","xtol_rel"=1.e-4,maxeval=200)}
  maxH<-function(par){#par=par0
    xMstar<-ceiling(nloptr(x0=M_0,eval_f=minusd_M,par=par,lb=L,ub=U,
                           opts=opts1)$solution)
    return(d_M(xMstar,par))  }
  if (global == TRUE){
    opts<-list("algorithm"="NLOPT_GN_DIRECT","xtol_rel"=1.e-5,maxeval=500)
  } else{
    opts<-list("algorithm"="NLOPT_LN_BOBYQA","xtol_rel"=1.e-4,maxeval=200)}
  parMstar<-nloptr(x0=par0,eval_f=maxH,lb=rep(0,pp),ub=rep(1,pp),opts=opts)$solution
  if(check.plot==TRUE){
    oldpar <- par(mfrow=c(1,2),mar=c(5,5,1.5,1))
    on.exit(par(oldpar))
    par(mfrow=c(1,2),mar=c(5,5,1.5,1))
    plot(lattice,h(lattice,parMstar),ylim=ylim.f,xlim=c(L,U),lwd=3,lty=3,type="o",col="mediumpurple1",ylab="Probability",xlab=expression(x),cex.axis=2,cex.lab=2,cex=2,pch=1)
    lines(lattice,f(lattice),col="darkgreen",lty=4,lwd=2,type="o",cex=2,pch=2)
    lines(lattice,g(lattice),lty=2,lwd=2,cex=2,col="tomato3",type="o",pch=3)
    lines(lattice,maxH(parMstar)*h(lattice,parMstar),lwd=2,lty=1,col="black",type="o",cex=2,pch=4)
    legend("topright", legend=c(expression(hat(f)[m](x)),expression(g(x)),expression(h(x)),expression(paste("M"^"*",h(x)))),
           lty=c(4,2,3,1),lwd=c(2,2,3,2),pch=c(2,3,1,4),col=c("darkgreen","tomato3","mediumpurple1","black"), cex=1.3,
           box.col=rgb(0, 0, 0,0))
    steps=stepfun(lattice, c(d_M(lattice,parMstar),0))
    plot.stepfun(steps, xlim=c(L,U),ylim=ylim.d,pch=1,ylab="Density",main="",xlab=expression(x),
                 lwd=3,lty=3,cex.lab=2, cex.axis=2,cex.points=2,col="mediumpurple1")
    steps_2=stepfun(lattice, c(dHF(lattice,parMstar),0))
    plot.stepfun(steps_2, xlim=c(L,U),pch=2,lwd=2,lty=4,cex.points=2,col="darkgreen",add=T)
    steps_3=stepfun(lattice, c(dHG(lattice,parMstar),0))
    plot(steps_3, xlim=c(L,U),ylim=c(-0.4,3.7),lty=2,lwd=2,pch=3,col="tomato3",cex=2,add=T)
    steps_4=stepfun(lattice, c(maxH(parMstar)*d_M(lattice,parMstar),0))
    plot(steps_4, xlim=c(L,U),pch=4,lwd=2,lty=1,cex=2,col="black",add=T)
    legend("bottomleft",legend=c(expression(paste(d,"(",H(x),";",H,",",hat(F)[m],")")),expression(paste(d,"(",H(x),";",H,",",G,")")),
                                 expression(paste(d,"(",H(x),";",H,",",hat(F)[m],")I(", x%in%D^+" ",")+",
                                                  d,"(",H(x),";",H,",",G,,")I(",x %in% D^-" ",")")),
                                 expression(paste("M"^"*","[",d,"(",H(x),";",H,",",hat(F)[m],")I(",x %in% D^+" ",")+",
                                                  d,"(",H(x),";",H,",",G,,")I(",x %in% D^-" ",")]"))),
           lty=c(4,2,3,1),lwd=c(2,2,3,2),pch=c(2,3,1,4),col=c("darkgreen","tomato3","mediumpurple1","black"), cex=1.3,
           box.col=rgb(0, 0, 0,0),bg="transparent")
    message("...if  not satisfactory, try changing the initial parameters.")
  }
  Mstar=maxH(parMstar)
  pis<-c(parMstar[1:(p-1)],1-sum(parMstar[1:(p-1)]))
  probs<-parMstar[(p):length(parMstar)]
  hopt=function(x)dmixnegbinom(x,pis,size,probs)
  hopt=Vectorize(hopt)
  return(list(Mstar=Mstar,pis=pis,size=size,probs=probs,h=hopt))
}

CDplot<-function(data,m=4,g,par0=NULL, range=NULL,lattice=NULL, selection=TRUE,criterion="BIC",B=1000,samplerG=NULL, h=NULL,
                 samplerH=NULL, R=500,ylim=c(0,2),CD.plot=TRUE){
  L<-ifelse(is.null(lattice),range[1],min(lattice) )
  U<-ifelse(is.null(lattice),range[2],max(lattice) )
  n<-length(data)
  uu<-seq(0,1,length=R)
  D<-c()
  Mat_ddG<-Mat_ddF<-matrix(rep(0,R*B),nrow=B,ncol=R)
  if(!is.null(par0)){loglik<-function(par,x){-sum(log(g(x,par)))}
  par_obs<-optim(par0, loglik,x=data)$par
  ghat<-function(x)g(x,par_obs)
  ghat<-Vectorize(ghat)
  dd<-d_hat(data,m,ghat,range,lattice,selection,criterion)
  D_obs<-sum(dd$LPj^2)
  if(!is.null(lattice)){pGobs<-ghat(lattice)
  Gxobs<-cumsum(pGobs)
  G<-function(x){Gxobs[which(lattice==x)]}
  G<-Vectorize(G)
  }else{G<-function(x)integrate(ghat,L,x)$value
  G<-Vectorize(G)}
  ddx<-function(x)dd$dx(x)
  ddx<-Vectorize(ddx)
  ddu<-function(u)dd$du(u)
  ddu<-Vectorize(ddu)
  fhat<-function(x)ghat(x)*dd$dx(x)
  fhat<-Vectorize(fhat)
  if(!is.null(samplerG)){#B=10
    M<-max(dd$du(uu))
    for(b in 1:B){#Sampling from G
      message(paste("Performing smoothed bootstrap...iteration n.",b ))
      xG<-samplerG(round(n*2*M))
      dataG<-sample(xG,n)
      par_G<-optim(par_obs, loglik,x=dataG)$par
      ghatG<-function(x)g(x,par_G)
      ghatG<-Vectorize(ghatG)
      ddG<-d_hat(dataG,m,ghatG,range,lattice,selection,criterion)
      D[b]<-sum(ddG$LPj^2)
      dduG<-function(u)ddG$du(u)
      dduG<-Vectorize(dduG)
      Mat_ddG[b,]<-dduG(uu)
      #Sampling from Fhat
      v<-runif(round(n*2*M))
      dataF<-sample(xG[v*M<=ddx(xG)],n)
      par_F<-optim(par_obs, loglik,x=dataF)$par
      ghatF<-function(x)g(x,par_F)
      ghatF<-Vectorize(ghatF)
      ddF<-d_hat(dataF,m,ghatF,range,lattice,selection,criterion)
      dduF<-function(u)ddF$du(u)
      dduF<-Vectorize(dduF)
      Mat_ddF[b,]<-dduF(uu)}
  }else{dHF<-function(x) fhat(x)/h(x)
  dHG=function(x) ghat(x)/h(x)
  d_M<-function(x){-(ifelse(dd$dx(x)>=1,dHF(x),0)+ifelse(dd$dx(x)<1,dHG(x),0))}
  dHF<-Vectorize(dHF)
  dHG<-Vectorize(dHG)
  d_M<-Vectorize(d_M)
  opts1<-list("algorithm"="NLOPT_GN_DIRECT","xtol_rel"=1.e-8,maxeval=1000)
  M<-nloptr(x0=(U-L)/2,eval_f=d_M,lb=L,ub=U,opts=opts1)$solution
  for(b in 1:B){#Sampling from H #b=1
    message(paste("Performing smoothed bootstrap...iteration n.",b ))
    xH<-samplerH(n=round(n*2*M))
    indexesplus<-which(round(ddx(xH),5)>=1)
    xHDplus= xH[indexesplus]
    xHDminus=xH[-indexesplus]
    Nplus<-length(xHDplus)
    Nminus<-length(xHDminus)
    v<-runif(round(n*2*M))
    unif1<-v[1:Nminus]
    index_Dminus_acceptFG=which(unif1*M<=dHF(xHDminus))
    xF1<-xG1<-xHDminus[index_Dminus_acceptFG]
    xHDminus2<-xHDminus[-index_Dminus_acceptFG]
    unif2 = unif1[-index_Dminus_acceptFG]
    index_Dminus_acceptG=which(unif2*M<=dHG(xHDminus2))
    xG2<-xHDminus2[index_Dminus_acceptG]
    unif3 =  v[(Nminus+1):round(n*2*M)]
    index_Dplus_acceptFG=which(unif3*M<=dHG(xHDplus))
    xF2<-xG3<-xHDplus[index_Dplus_acceptFG]
    xHDplus2<-xHDplus[-index_Dplus_acceptFG]
    unif4 = unif3[-index_Dplus_acceptFG]
    index_Dplus_acceptF=which(unif4*M<=dHF(xHDplus2))
    xF3<-xHDplus2[index_Dplus_acceptF]
    dataF=sample(c(xF1,xF2,xF3),n)
    dataG=sample(c(xG1,xG2,xG3),n)
    #analysis on G
    par_G<-optim(par_obs, loglik,x=dataG)$par
    ghatG<-function(x)g(x,par_G)
    ghatG<-Vectorize(ghatG)
    ddG<-d_hat(dataG,m,ghatG,range,lattice,selection,criterion)
    D[b]<-sum(ddG$LPj^2)
    dduG<-function(u)ddG$du(u)
    dduG<-Vectorize(dduG)
    Mat_ddG[b,]<-dduG(uu)
    #analysis on F
    par_F<-optim(par_obs, loglik,x=dataF)$par
    ghatF<-function(x)g(x,par_F)
    ghatF<-Vectorize(ghatF)
    ddF<-d_hat(dataF,m,ghatF,range,lattice,selection,criterion)
    dduF<-function(u)ddF$du(u)
    dduF<-Vectorize(dduF)
    Mat_ddF[b,]<-dduF(uu)}
  }
  }else{dd<-d_hat(data,m,g,range,lattice,selection,criterion)
  ddx<-function(x)dd$dx(x)
  ddx<-Vectorize(ddx)
  ddu<-function(u)dd$du(u)
  ddu<-Vectorize(ddu)
  D_obs<-sum(dd$LPj^2)
  if(!is.null(lattice)){pGobs<-g(lattice)
  Gxobs<-cumsum(pGobs)
  G<-function(x){Gxobs[which(lattice==x)]}
  G<-Vectorize(G)
  }else{G<-function(x)integrate(g ,L,x)$value
  G<-Vectorize(G)}
  fhat<-function(x)g(x)*ddx(x)
  fhat<-Vectorize(fhat)
  if(!is.null(samplerG)){#B=10
    M<-max(dd$du(uu))
    for(b in 1:B){#Sampling from G b=1
      message(paste("Performing smoothed bootstrap...iteration n.",b ))
      xG<-samplerG(round(n*2*M))
      dataG<-sample(xG,n)
      ddG<-d_hat(dataG,m,g,range,lattice,selection,criterion)
      D[b]<-sum(ddG$LPj^2)
      dduG<-function(u)ddG$du(u)
      dduG<-Vectorize(dduG)
      Mat_ddG[b,]<-dduG(uu)
      #Sampling from Fhat
      v<-runif(round(n*2*M))
      dataF<-sample(xG[v*M<=dd$du(G(xG))],n)
      ddF<-d_hat(dataF,m,g,range,lattice,selection,criterion)
      dduF<-function(u)ddF$du(u)
      dduF<-Vectorize(dduF)
      Mat_ddF[b,]<-dduF(uu)}
  }else{dHF<-function(x) fhat(x)/h(x)
  dHG=function(x) g(x)/h(x)
  d_M<-function(x){-(ifelse(ddx(x)>=1,dHF(x),0)+ifelse(ddx(x)<1,dHG(x),0))}
  dHF<-Vectorize(dHF)
  dHG<-Vectorize(dHG)
  d_M<-Vectorize(d_M)
  opts1<-list("algorithm"="NLOPT_GN_DIRECT","xtol_rel"=1.e-8,maxeval=2000)
  M<-nloptr(x0=(U+L)/2,eval_f=d_M,lb=L,ub=U,opts=opts1)$solution
  for(b in 1:B){#Sampling from H
    message(paste("Performing smoothed bootstrap...iteration n.",b ))
    xH<-samplerH(n=round(n*2*M))
    indexesplus<-which(ddx(xH)>=1)
    xHDplus= xH[indexesplus]
    xHDminus=xH[-indexesplus]
    Nplus<-length(xHDplus)
    Nminus<-length(xHDminus)
    v<-runif(round(n*2*M))
    unif1<-v[1:Nminus]
    index_Dminus_acceptFG=which(unif1*M<=dHF(xHDminus))
    xF1<-xG1<-xHDminus[index_Dminus_acceptFG]
    xHDminus2<-xHDminus[-index_Dminus_acceptFG]
    unif2 = unif1[-index_Dminus_acceptFG]
    index_Dminus_acceptG=which(unif2*M<=dHG(xHDminus2))
    xG2<-xHDminus2[index_Dminus_acceptG]
    unif3 =  v[(Nminus+1):round(n*2*M)]
    index_Dplus_acceptFG=which(unif3*M<=dHG(xHDplus))
    xF2<-xG3<-xHDplus[index_Dplus_acceptFG]
    xHDplus2<-xHDplus[-index_Dplus_acceptFG]
    unif4 = unif3[-index_Dplus_acceptFG]
    index_Dplus_acceptF=which(unif4*M<=dHF(xHDplus2))
    xF3<-xHDplus2[index_Dplus_acceptF]
    dataF=sample(c(xF1,xF2,xF3),n)
    dataG=sample(c(xG1,xG2,xG3),n)
    #analysis on G
    ddG<-d_hat(dataG,m,g,range,lattice,selection,criterion)
    D[b]<-sum(ddG$LPj^2)
    dduG<-function(u)ddG$du(u)
    dduG<-Vectorize(dduG)
    Mat_ddG[b,]<-dduG(uu)
    #analysis on F
    ddF<-d_hat(dataF,m,g,range,lattice,selection,criterion)
    dduF<-function(u)ddF$du(u)
    dduF<-Vectorize(dduF)
    Mat_ddF[b,]<-dduF(uu)}
  }
  }
  ##Now compute the bands and do the actual plot
  SD<-SDf<-SDftrue<-SDfemp<-c()
  processG<-matrix(rep(0,R*B),nrow=B,ncol=R)
  for(r in 1:R){
    SD[r]<-sd(Mat_ddG[,r])
    SDf[r]<-sd(Mat_ddF[,r])
    processG[,r]<-(Mat_ddG[,r]-1)/sd(Mat_ddG[,r])
  }
  MaxDistr<-apply(abs(processG),1,max)
  calpha<-quantile(MaxDistr,0.95)
  p.value<-mean(n*D>=n*D_obs)
  #Graphical functions
  if(CD.plot==TRUE){Transparency<-function(color, shade=80){color2<-col2rgb(color)
  apply(color2, 2, function(color3){rgb(red=color3[1], green=color3[2],
                                        blue=color3[3],alpha=shade, maxColorValue=250)})}
  transgreen <- Transparency("chartreuse2")
  transgrey <- Transparency("grey67")
  oldpar <- par(mfrow=c(1,1),mar=c(5,6,1,1))
  on.exit(par(oldpar))
  par(mfrow=c(1,1),mar=c(5,6,1,1))
  plot(uu,ddu(uu),type="l",main=" ",ylim=ylim,ylab=expression(hat(d)(u,G,F)),xlab="u",cex.axis=2,cex.lab=2,col="darkgreen",lwd=2,xaxt='n')
  abline(h=1,lwd=2,lty=2,col="tomato3")
  if(is.null(lattice)){polygon(c(uu,rev(uu)),c(rep(1,length(uu))-calpha*SD,rev(rep(1,length(uu))+calpha*SD)),col=transgrey, border = FALSE)
    polygon(c(uu,rev(uu)),c(ddu(uu)-SDf,rev(ddu(uu)+SDf)),col=transgreen, border = FALSE)}else{
      polygon.step <- function(x, y1, y2, border=FALSE, ...) {
        nx <- length(x)
        ny <- length(y1)
        if (length(y2)!=ny) stop("y1 and y2 must be the same length")
        if (nx != (ny+1)) stop("x must be one longer than y")
        xx <- c(x[1], rep(x[-c(1,nx)], rep(2,nx-2)), x[nx])
        xxx <- c(xx, rev(xx))
        yy1 <- rep(y1, rep(2,ny))
        yy2 <- rep(y2, rep(2,ny))
        yyy <- c(yy1, rev(yy2))
        polygon(xxx, yyy, border=border, ...)
      }
      polygon.step(c(0,uu),1-calpha*SD,1+calpha*SD,col=transgrey)
      polygon.step(c(0,uu),ddu(uu)-SDf,ddu(uu)+SDf,col=transgreen)}
  legend("topleft",legend=c(expression(hat(d)(u,G,F)), expression("95% confidence bands"),"Standard error"),lwd=1,
         col=c("darkgreen",transgrey, transgreen),pch=c(NA,22,22),bty="n",lty = c(1, NA,NA),
         pt.bg=c(transgreen,transgrey),cex =1)
  Axis(side = 1, at = seq(0, 1, by = 0.1), cex.axis=2,cex=2, col = "black", col.lab = "black")}
  return(list(Deviance=D_obs,p_value=p.value))   }


