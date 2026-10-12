#'Dynamic threshold panel function
#'
#'
#'@param f the formula
#'@param data the dataframe
#'@param index c=("id","time")
#'@param maxlags the instruments maximum lags number
#'@param initnum column number of the initial variable
#'@param conf the confidential interval
#'@param conf1 the confidential interval
#'@param conf2 the confidential interval
#'@param graph TRUE to show the threshold plot
#'@importFrom stats pchisq as.formula model.frame model.matrix model.response update nobs pf pnorm formula na.omit ave
#'@importFrom graphics abline legend lines par plot title
#'@importFrom plyr count
#'@importFrom gtools na.replace
#'@import Formula
#'
#'@examples
#
#'############################################
#'# Fit the dynamic threshold panel data
#'############################################
#'# Load data
#'data(Mena)
#'############################################
#'# Estimate the model
#'############################################
#'reg<-dtp(GDPPC ~ FDI+OPEN|INF|INF,Mena,index=c("pays","ann"),4,2,0.95,0.8,1,graph = TRUE)
#'summary(reg)
#'
#'
#'@export

dtp<- function(f,data,index=c("state","year"),maxlags,initnum,conf,conf1,conf2,graph=TRUE){
  data<-newframe(data,index=index)
  f1<-as.formula(f)
  fm<-f1
  mf <- match.call(expand.dots = FALSE)
  m <- match(c("formula", "data", "subset", "na.action"), names(mf), 0)
  mf <- mf[c(1, m)]
  ffm <- Formula::Formula(fm)
  mf[[1]] <- as.name("model.frame")
  mf$formula <- ffm
  fffm<-update(ffm, ~ . -1| . -1|. -1)
  mf <- model.frame(formula=fffm, data=data)
  z2 <- model.matrix(fffm, data = mf, rhs = 1)
  q <- model.matrix(fffm, data = mf, rhs = 2)
  c <- model.matrix(fffm, data = mf, rhs = 3)
  y<-model.response(mf)
  a<-unlist(strsplit(as.character(f1),"[|]"))
  instrument<-makeinstru(data,a[[2]],maxlags,initnum)
  x<-as.matrix(instrument$inst)
  initial<-instrument$initial
  z1<-as.matrix(initial)
  largeT<-data$TT
  t<-data$t
  yt = tr(y,largeT,t)
  ct = tr(c,largeT,t)
  zt1= tr(z1,largeT,t)


  k=length(z2[1,]);
 zt2= matrix(c(0),length(yt[,1]),k)
 # zt2=matlab::zeros(length(yt[,1]),k)
  #i=1
  #while (i<=k){
  for(i in 1:k){
    zt2[,i]=tr(z2[,i],largeT,t)
    #i=i+1
  }

  ii=1
  qt<-0
  for (i in 1:length(q)){
    if (t[i]<largeT[i]){
      qt[ii]<-q[i]
      ii=ii+1
    }
  }
  qt=t(qt)

  #**********2sls****************
  xx=cbind(x,zt2)
  z1hat<-xx%*%regress(zt1,xx)

  zhat=cbind(z1hat,zt2)

  #********
  n=length(yt[,1])
  xx=cbind(zhat,tr(c,largeT,t))
  k=length(xx[1,])
  e=yt-xx%*%regress(yt,xx) #SSR0=sum(e^2)
  ss0<-sum(e^2)
  s0 <- det(t(e)%*%e)
  n1 <- round(.05*n)+k
  n2 <- round(.95*n)-k
  qs <- sort(q)
  qs <- qs[n1:n2]
  qs <- as.matrix(unique(qs))
  qn <- nrow(qs)
  sn <- matrix(0,qn,1)
  r=1
 # while (r <=qn){
  for(r in 1:qn){
    d <- (q<=qs[r])
    xxx=cbind(xx,tr(c*d,largeT,t),tr(d,largeT,t))
    xxx <- xxx-xx%*%regress(xxx,xx)
    ex <- e-xxx%*%regress(e,xxx)
    sn[r] <- det(t(ex)%*%ex)
   # r=r+1
  }
  r <- which.min(sn)
  smin <- sn[r]
  qhat <- qs[r]
  d <- (q<=qhat)
  xxx=cbind(zhat,tr(c*d,largeT,t),tr(d,largeT,t))
  beta=regress(yt,xxx)
  yhat=xxx%*%beta
  e=yt-yhat
  lr=n*(sn/smin-1)
  sig2=smin/n
  i=length(zhat[1,])
  beta1=beta[i+1]
  beta2=beta[i+3]

  if (ncol(yt)> 1){
    eta1 <- 1
    eta2 <- 1
  }else{
    r1 <- (ct%*%(beta1-beta2))^2
    r2 <- r1*(e^2)
    qx <- cbind(t(qt^0),t(qt^1),t(qt^2))
    qh <- cbind(qhat^0,qhat^1,qhat^2)
    m1 <- qr.solve(qx,r1)
    m2 <- qr.solve(qx,r2)
    g1 <- qh%*%m1
    g2 <- qh%*%m2
    eta1 <- as.numeric((g2/g1)/sig2)
    sigq <- sqrt(mean((qt-mean(q))^2))
    hband <- 2.344*sigq/(n^(.2))
    u <- (qhat-qt)/hband
    u2 <- u^2
    f <- mean((1-u2)*(u2<=1))*(.75/hband)
    df <- -mean(-u*(u2<=1))*(1.5/(hband^2))
    eps <- r1 - qx%*%m1
    sige <- (t(eps)%*%eps)/(n-3)
    hband <- as.vector(sige/(4*f*((m1[3]+(m1[2]+2*m1[3]*qhat)*df/f)^2)))
    u2 <- ((qhat-qt)/hband)^2
    kh <- ((1-u2)*.75/hband)*(u2<=1)
    g1 <- mean(kh%*%r1)
    g2 <- mean(kh%*%r2)
    eta2 <- as.numeric((g2/g1)/sig2)
  }
  c1 <- -2*log(1-sqrt(conf))
  lr0 <- as.numeric(lr >= c1)
  lr1 <- as.numeric(lr >= (c1*eta1))
  lr2 <- as.numeric(lr >= (c1*eta2))
  if (!is.na(max(lr0)==1)){
    qcf_0 <- cbind(qs[which.min(lr0)],qs[qn+1-which.min(rev(lr0))])
  }else{
    qcf_0 <- cbind(qs[1],qs[qn])
  }
  if (!is.na(max(lr1)==1)){
    qcf_h1 <- cbind(qs[which.min(lr1)],qs[qn+1-which.min(rev(lr1))])
  }else{
    qcf_h1 <- cbind(qs[1],qs[qn])
  }
  if (!is.na(max(lr2)==1)){
    qcf_h2 <- cbind(qs[which.min(lr2)],qs[qn+1-which.min(rev(lr2))])
  }else{
    qcf_h2 <- cbind(qs[1],qs[qn])
  }

  if (graph==TRUE){
    par(mar = c(5,4,4,8))
    mtit <- "Confidence Interval Construction for Threshold"
    ytit <- "Likelihood Ratio Sequence in gamma"
    xtit <- "Threshold Variable"
    xxlim <- range(qs)
    clr <- matrix(1,qn,1)*c1
    yylim <- range(rbind(lr,clr))
    if (ncol(as.matrix(y)) == 1) {clr <- cbind(clr,(clr*eta1),(clr*eta2))}
    plot(qs,lr,lty=1
         ,xlim=xxlim,ylim=yylim,col=1,type="l",ann=0)
    lines(qs,clr[,1],lty=2,col=2)
    if (ncol(as.matrix(y)) == 1){
      lines(qs,clr[,2],lty=1,col=3)
      lines(qs,clr[,3],lty=1,col=4)
    }
    title(main=mtit,ylab=ytit,xlab=xtit,cex.main=0.8,cex.lab=0.8)
    tit1 <- "LRn(gamma)"
    tit2 <- "90% Critical"
    tit3 <- "Hetero Corrected - 1"
    tit4 <- "Hetero Corrected - 2"
    legend(par("usr")[2],par("usr")[4],
           xpd = TRUE ,
           bty = "n",
           c(tit1,tit2,tit3,tit4),lty=c(1,1,1,1),cex=0.6,col=c(1,2,3,4) )

  }

  z <- cbind(zt1,zt2)
  da <- (q<=qhat)
  db <- 1-da
  zi=cbind(zt1,zt2,tr(c*da,largeT,t),tr(da,largeT,t),tr(c*db,largeT,t)) #regime-specific constant inserted here%
  xi=cbind(x,zt2,tr(c*da,largeT,t),tr(da,largeT,t),tr(c*db,largeT,t)) #regime-specific constant inserted here%
  yi=yt

  out1<- gmm_linear(yi,zi,xi)
  beta<-out1$beta
  se <- out1$se
  betal=beta-se*1.96
  betau=beta+se*1.96
  #######################################
  n=length(yt[,1])
  xx=cbind(zhat,tr(c,largeT,t))
  k=length(xx[1,])
  e=yt-xx%*%regress(yt,xx)
  s0 <- det(t(e)%*%e)
  n1 <- round(.05*n)+k
  n2 <- round(.95*n)-k
  qs <- sort(q)
  qs <- qs[n1:n2]
  qs <- as.matrix(unique(qs))
  qn <- nrow(qs)
  sn <- matrix(0,qn,1)
  for (r in 1:qn){
    d <- (q<=qs[r])
    xxx=cbind(xx,tr(c*d,largeT,t),tr(d,largeT,t))
    xxx <- xxx-xx%*%regress(xxx,xx)
    ex <- e-xxx%*%regress(e,xxx)
    sn[r] <- det(t(ex)%*%ex)
  }
  r <- which.min(sn)
  smin <- sn[r]
  qhat <- qs[r]
  d <- (q<=qhat)
  xxx=cbind(zhat,tr(c*d,largeT,t),tr(d,largeT,t))
  dd=1-d
  xxx=cbind(xxx,tr(c*dd,largeT,t))
  betaf=regress(yt,xxx)
  yhat=xxx%*%betaf
  e=yt-yhat
  lr=n*(sn/smin-1)
  sig2=smin/n
  i=length(x[1,])
  betaf1=betaf[i+1]
  betaf2=beta[i+3]


  if (ncol(yt)> 1){
    eta1 <- 1
    eta2 <- 1
  }else{
    r1 <- (ct%*%(betaf1-betaf2))^2
    r2 <- r1*(e^2)
    qx <- cbind(t(qt^0),t(qt^1),t(qt^2))
    qh <- cbind(qhat^0,qhat^1,qhat^2)
    m1 <- qr.solve(qx,r1)
    m2 <- qr.solve(qx,r2)
    g1 <- qh%*%m1
    g2 <- qh%*%m2
    eta1 <- as.vector((g2/g1)/sig2)
    sigq <- sqrt(mean((qt-mean(q))^2))
    hband <- 2.344*sigq/(n^(.2))
    u <- (qhat-qt)/hband
    u2 <- u^2
    f <- mean((1-u2)*(u2<=1))*(.75/hband)
    df <- -mean(-u*(u2<=1))*(1.5/(hband^2))
    eps <- r1 - qx%*%m1
    sige <- (t(eps)%*%eps)/(n-3)
    hband <- as.vector(sige/(4*f*((m1[3]+(m1[2]+2*m1[3]*qhat)*df/f)^2)))
    u2 <- ((qhat-qt)/hband)^2
    kh <- ((1-u2)*.75/hband)*(u2<=1)
    g1 <- mean(kh%*%r1)
    g2 <- mean(kh%*%r2)
    eta2 <- as.vector((g2/g1)/sig2)
  }
  c1 <- -2*log(1-sqrt(conf))
  lr0 <- (lr >= c1)
  lr1 <- (lr >= (c1*eta1))
  lr2 <- (lr >= (c1*eta2))
  if (max(lr0)==1){
    qcfi_0 <- cbind(qs[which.min(lr0)],qs[qn+1-which.min(rev(lr0))])
  }else{
    qcfi_0 <- cbind(qs[1],qs[qn])
  }
  if (!is.na(max(lr1)==1)){
    qcfi_1 <- cbind(qs[which.min(lr1)],qs[qn+1-which.min(rev(lr1))])
  }else{
    qcfi_1 <- cbind(qs[1],qs[qn])
  }
  if (!is.na(max(lr2)==1)){
    qcfi_2 <- cbind(qs[which.min(lr2)],qs[qn+1-which.min(rev(lr2))])
  }else{
    qcfi_2 <- cbind(qs[1],qs[qn])
  }
  ########################


  if (conf2==0) qcf <- qcfi_0
  if (conf2==1) qcf <- qcfi_1
  if (conf2==2) qcf <- qcfi_2
  qq <- unique(q)
  qq <- as.matrix(sort(qq))
  qq <- qq[qq<=qcf[2]]
  qq <- as.matrix(qq[qq>=qcf[1]])
  for (i in 1:nrow(qq)){
    qi <- qq[i]
    dai <- (q<=qi)
    dbi <- 1-dai
    yi=yt
    zi=cbind(zt1,zt2,tr(c*dai,largeT,t),tr(dai,largeT,t),tr(c*dbi,largeT,t))# %regime-specific constant inserted here%
    xi=cbind(x,zt2,tr(c*dai,largeT,t),tr(dai,largeT,t),tr(c*dbi,largeT,t)) #%regime-specific constant inserted here%

    out2= gmm_linear(yi,zi,xi)
    sei=out2$se
    ss1<-sum(out2$e^2)
    betafi<-out2$beta
    betafil <- apply(t(cbind((betafi-sei*1.96),betal)),2,min)
    betafiu <- apply(t(cbind((betafi+sei*1.96),betau)),2,max)
  }
  nz<-length(zhat[1,])
  #############################
  # wald linearity test
  #############################
  lm<- n*(ss0-ss1)/ss0
  df<-ncol(xx)
  lmpv<-pchisq(lm,df,lower.tail = FALSE)
  #############################
  # Fisher linearity test
  #############################
  N<-max(data$id)
  fm<-(n*(ss0-ss1)/df)/(ss0/(n-N-k))
  fmpv<-pchisq(fm,df,lower.tail = FALSE)
  #############################
  # LRT linearity test
  #############################
  lgrt<-n*(log(ss0)-log(ss1))
  lgrtpv<-pchisq(lgrt,df,lower.tail = FALSE)
  ##############################
  df<-nrow(z2)-ncol(x)
  alpha<-betaf[(1:nz)]
  std<-se[(1:nz)]
  tval<-alpha/std
  pval<-2*pnorm(-abs(tval))
  lw<-betafil[(1:nz)]
  up<-betafiu[(1:nz)]

  alpha1<-betaf[(nz+1):(nz+2)]
  std1<-se[(nz+1):(nz+2)]
  tval1<-alpha1/std1
  pval1<-2*pnorm(-abs(tval1))
  lw1<-betafil[(nz+1):(nz+2)]
  up1<-betafiu[(nz+1):(nz+2)]

  alpha2<-betaf[(nz+3):(nz+3)]
  std2<-se[(nz+3):(nz+3)]
  tval2<-alpha2/std2
  pval2<-2*pnorm(-abs(tval2))
  lw2<-betafil[(nz+3):(nz+3)]
  up2<-betafiu[(nz+3):(nz+3)]
  nmes<-colnames(z2)
  out<-list(alpha=alpha,std=std,lw=lw,up=up,tval=tval,pval=pval,alpha1=alpha1,std1=std1,lw1=lw1,
            up1=up1,tval1=tval1,pval1=pval1,alpha2=alpha2,std2=std2,lw2=lw2,up2=up2,tval2=tval2,pval2=pval2,
            lm=lm,lmpv=lmpv,fm=fm,fmpv=fmpv,lgrt=lgrt,lgrtpv=lgrtpv,nmes=nmes,qhat=qhat,qcfi_0=qcfi_0,qcfi_1=qcfi_1,
            qcfi_2=qcfi_2,da=da,db=db)
  class(out) <- "dtp"
  return(out)


}
