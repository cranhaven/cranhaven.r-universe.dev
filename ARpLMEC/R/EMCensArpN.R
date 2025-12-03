


EMCensArpN<-function(cc,y,x,z,tt,nj,Arp,initial,cens.type,LI,LS,MaxIter,ee,Prev,step,isubj,xpre,zpre)
{
  start.time <- Sys.time()
  pb = tkProgressBar(title = "AR(p)-N-LMEC by EM", min = 0,max = MaxIter, width = 300)
  setTkProgressBar(pb, 0, label=paste("Iter ",0,"/",MaxIter,"     -     ",0,"% done",sep = ""))
  
  
   if(cens.type=="left"){
    LI=rep(-Inf,length(cc))
    LS=rep(Inf,length(cc))
    LS[cc==1]=y[cc==1]
    LI=as.vector(LI)
    LS=as.vector(LS)
  }
  
  if(cens.type=="right"){
    LI=rep(-Inf,length(cc))
    LI[cc==1]=y[cc==1]
    LS=rep(Inf,length(cc))
    LI=as.vector(LI)
    LS=as.vector(LS)
  }
  
  if(cens.type=="interval"){
    LI=LI
    LS=LS
    LI=as.vector(LI)
    LS=as.vector(LS)
  }
  
  
  m<-length(nj)[1]
  N<-sum(nj)
  p<-dim(x)[2]
  q1<-dim(z)[2]
  m1<-m*p
  m2<-m*q1
  
  
  if(!is.null(initial)){
    beta1<-initial$betas
    sigmae<- initial$sigma2
    D1<-initial$alphas
    iD1<- solve(D1)
    iD1 <- (iD1 + t(iD1))/2
    
    if(!is.null(initial$phi)){
      if(Arp>=1){
        piis = as.numeric(pacf((y - x%*%beta1),lag.max=Arp,plot=F)$acf)
        phi = initial$phi
        teta <- c(beta1,sigmae,D1[upper.tri(D1, diag = T)],phi)
        teta1<- c(beta1,sigmae,D1[upper.tri(D1, diag = T)],phi)
      }
      if(Arp=="UNC"){
        Arp=0
        piis = 0
        phi = 0
        teta <- c(beta1,sigmae,D1[upper.tri(D1, diag = T)])
        teta1<- c(beta1,sigmae,D1[upper.tri(D1, diag = T)])}
    }
    
    if(is.null(initial$phi)){
      if(Arp>=1){
        piis = as.numeric(pacf((y - x%*%beta1),lag.max=Arp,plot=F)$acf)
        phi = estphit(piis)
        teta <- c(beta1,sigmae,D1[upper.tri(D1, diag = T)],phi)
        teta1<- c(beta1,sigmae,D1[upper.tri(D1, diag = T)],phi)}
      if(Arp=="UNC"){
        Arp=0
        piis = 0
        phi = 0
        teta <- c(beta1,sigmae,D1[upper.tri(D1, diag = T)])
        teta1<- c(beta1,sigmae,D1[upper.tri(D1, diag = T)])}
    }
  }
  
  
  
  
  if(is.null(initial)){ 
    beta1=solve(t(x)%*%x)%*%t(x)%*%y  
    sigmae= 0.5 
    D1=0.1*diag(dim(z)[2])
    iD1<- solve(D1)
    iD1 <- (iD1 + t(iD1))/2
    if(Arp>=1){
      piis = as.numeric(pacf((y - x%*%beta1),lag.max=Arp,plot=F)$acf)
      phi = estphit(piis)
      teta <- c(beta1,sigmae,D1[upper.tri(D1, diag = T)],phi)
      teta1<- c(beta1,sigmae,D1[upper.tri(D1, diag = T)],phi)}
    if(Arp=="UNC"){
      Arp=0
      piis = 0
      phi = 0
      teta <- c(beta1,sigmae,D1[upper.tri(D1, diag = T)])
      teta1<- c(beta1,sigmae,D1[upper.tri(D1, diag = T)])}
  }
  
  
  criterio<-1
  count<-0
  
  loglik <- logliknArplmec(y=y,x=x,z=z,cc=cc,ttc=tt,nj=nj,LL=LI,LU=LS,betas=beta1,sigmae=sigmae,D1=D1,pii=piis)
  
  loglikp <- loglik 
  
  while(criterio > ee){
    
    count <- count + 1
    
    soma1<- matrix(0,q1,q1)
    soma2<-0
    soma3<- matrix(0,p,p)
    soma4<- matrix(0,p,1)
    soma5<- matrix(0,p,p)
    MI <- matrix(0,p+1+length(D1[upper.tri(D1, diag = T)])+Arp,
                 p+1+length(D1[upper.tri(D1, diag = T)])+Arp) 
    
    
    ubi=matrix(0,m2,m)
    ubbi=matrix(0,m2,m2)
    uybi=matrix(0,N,m2)
    uyyi=matrix(0,N,N)
    uyi=matrix(0,N,m)
    yhi=matrix(0,N,1)
    xi=matrix(0,N,m1)
    zi=matrix(0,N,m2) 
    
    ver<-matrix(0,m,1)
    
    for (j in 1:m ){
      
      cc1=cc[(sum(nj[1:j-1])+1) : (sum(nj[1:j]))]
      tt1=tt[(sum(nj[1:j-1])+1) : (sum(nj[1:j]))]
      y1=y[(sum(nj[1:j-1])+1) : (sum(nj[1:j]))]
      x1=matrix(x[(sum(nj[1:j-1])+1) : (sum(nj[1:j])),  ],ncol=p)
      z1=matrix(z[(sum(nj[1:j-1])+1) : (sum(nj[1:j])) ,  ],ncol=q1)
      
      
      
      LI1<- LI[(sum(nj[1:j-1])+1) : (sum(nj[1:j]))]
      LS1<- LS[(sum(nj[1:j-1])+1) : (sum(nj[1:j]))]
      gammai=x1%*%beta1
      
   
      
      if(Arp==0){eGama=diag(1,nj[j])
      Gama=eGama*sigmae}
      if(Arp!=0){
        Gama<- MatArp(piis,tt1,sigmae) 
        eGama<-Gama/sigmae
      }
      
      Psi<-(Gama+(z1)%*%D1%*%t(z1))
      Psi<-(Psi+t(Psi))/2
      delta<- solve(iD1+(t(z1)%*%solve(eGama)%*%(z1*(1/sigmae))))
      
      if(sum(cc1)==0){
        
        uy<- matrix(y1,nj[j],1)
        uyy<- y1%*%t(y1)
        ub<- delta%*%(t(z1)*(1/sigmae))%*%solve(eGama)%*%(uy-gammai)
        ubb<- delta+(delta%*%(t(z1)*((1/sigmae)^2))%*%solve(eGama)%*%(uyy-uy%*%t(gammai)-gammai%*%t(uy)+gammai%*%t(gammai))%*%solve(eGama)%*%z1%*%delta)
        uyb<- (uyy-uy%*%t(gammai))%*%solve(eGama)%*%(z1*(1/sigmae))%*%delta
        ver[j,]<- LaplacesDemon::dmvn(x=as.vector(y1),mu=as.vector(gammai),Sigma=Psi)
        
      }
      
      if(sum(cc1)>=1){
        
   
        if(sum(cc1)==nj[j]){
          muc=x1%*%beta1
          Sc<-Psi
          aux<- relliptical::mvtelliptical(lower = as.vector(LI1),upper=as.vector(LS1),mu = as.vector(muc), Sigma =Sc,dist = "Normal")
          uy<- aux$EY
          uyy<- aux$EYY
          ub<- delta%*%(t(z1)*(1/sigmae))%*%solve(eGama)%*%(uy-gammai)
          ubb<- delta+(delta%*%(t(z1)*((1/sigmae)^2))%*%solve(eGama)%*%(uyy-uy%*%t(gammai)-gammai%*%t(uy)+gammai%*%t(gammai))%*%solve(eGama)%*%z1%*%delta)
          uyb<- (uyy-uy%*%t(gammai))%*%solve(eGama)%*%(z1*(1/sigmae))%*%delta
          ver[j,]<-  TruncatedNormal::pmvnorm(lb=as.vector(LI1), ub=as.vector(LS1), mu=as.vector(muc),sigma=Sc)
         
          
        }
        
        else {
          muc=x1[cc1==1,]%*%beta1+Psi[cc1==1,cc1==0]%*%solve(Psi[cc1==0,cc1==0])%*%(y1[cc1==0]-x1[cc1==0,]%*%beta1)
          Sc <-Psi[cc1==1,cc1==1]-Psi[cc1==1,cc1==0]%*%solve(Psi[cc1==0,cc1==0])%*%Psi[cc1==0,cc1==1]
          Sc=(Sc+t(Sc))/2
          aux<- relliptical::mvtelliptical(lower = as.vector(LI1[cc1==1]),upper=as.vector(LS1[cc1==1]),mu = as.vector(muc), Sigma =Sc)
          uy <-matrix(y1,nj[j],1)
          uy[cc1==1]<- aux$EY
          uyy<- uy%*%t(uy)
          uyy[cc1==1,cc1==1]<- aux$EYY
          ub<- delta%*%(t(z1)*(1/sigmae))%*%solve(eGama)%*%(uy-gammai)
          ubb<- delta+(delta%*%(t(z1)*((1/sigmae)^2))%*%solve(eGama)%*%(uyy-uy%*%t(gammai)-gammai%*%t(uy)+gammai%*%t(gammai))%*%solve(eGama)%*%z1%*%delta)
          uyb<- (uyy-uy%*%t(gammai))%*%solve(eGama)%*%(z1*(1/sigmae))%*%delta
          
          ver[j,]<- LaplacesDemon::dmvn(x=as.vector(y1[cc1==0]),mu=as.vector(gammai[cc1==0]),Sigma=as.matrix(Psi[cc1==0,cc1==0]))*( TruncatedNormal::pmvnorm(lb=as.vector(LI1[cc1==1]),ub=as.vector(LS1[cc1==1]),mu=as.vector(muc),sigma=Sc))[1]
          
          
        }
        
      }
      
      soma1<- soma1 + ubb
      soma2<- soma2 + (sum(diag(uyy%*%solve(eGama)))-t(uy)%*%solve(eGama)%*%gammai-t(gammai)%*%solve(eGama)%*%uy-sum(diag(solve(eGama)%*%((uyb)%*%t(z1))))-sum(diag(solve(eGama)%*%((uyb)%*%t(z1))))
                       +t(gammai)%*%solve(eGama)%*%z1%*%ub+t(ub)%*%t(z1)%*%solve(eGama)%*%gammai+t(gammai)%*%solve(eGama)%*%gammai+sum(diag(ubb%*%t(z1)%*%solve(eGama)%*%z1)))
      soma3<- soma3 + (t(x1)%*%solve(eGama)%*%x1)
      soma4<- soma4 + (t(x1)%*%solve(eGama)%*%(uy-z1%*%ub))
      soma5<- soma5 + (t(x1)%*%solve(Psi)%*%x1-t(x1)%*%solve(Psi)%*%(uyy-uy%*%t(uy))%*%solve(Psi)%*%x1)
      
      
      ubi[(((j-1)*q1)+1) : (j*q1), j]<-ub
      ubbi[(((j-1)*q1)+1) : (j*q1), (((j-1)*q1)+1) : (j*q1)]<-ubb
      uybi[(sum(nj[1:j-1])+1) : (sum(nj[1:j])),(((j-1)*q1)+1) : (j*q1)]<-uyb
      uyyi[(sum(nj[1:j-1])+1) : (sum(nj[1:j])),(sum(nj[1:j-1])+1) : (sum(nj[1:j]))]<-uyy
      uyi[(sum(nj[1:j-1])+1) : (sum(nj[1:j])),j]<-uy
      yhi[(sum(nj[1:j-1])+1) : (sum(nj[1:j])),] <- uy
      zi[(sum(nj[1:j-1])+1) : (sum(nj[1:j])), (((j-1)*q1)+1) : (j*q1)]<-z1
      xi[(sum(nj[1:j-1])+1) : (sum(nj[1:j])), (((j-1)*p)+1) : (j*p)]<-x1
      
      
      tetaMI=c(beta1,sigmae,phi)
      si<-Jt(tetaMI,uy,x1,z1,tt1,ub,ubb,p,Arp,D1)
      MI <- MI + t(si)%*%si
      
    }
    yorg<-apply(yhi,1,sum)
    beta1<- solve(soma3)%*%soma4
    sigmae<- (1/N)*(soma2)
    sigmae<-as.numeric(sigmae)
    D1<- (1/m)*(soma1)
    iD1<- solve(D1) 
    teta1 <- c(beta1,sigmae,D1[upper.tri(D1, diag = T)])
    
    if(Arp!=0){
      piis <- optim(piis, method = "L-BFGS-B", FCiArp, lower =rep(-.999,Arp), upper =rep(.999,Arp), beta1=beta1,sigmae=sigmae, ubi=ubi,ubbi=ubbi,uybi=uybi,uyyi=uyyi,uyi=uyi,x=x,z=z,tt=tt,nj=nj,hessian=TRUE)$par
      phi=estphit(piis) 
      teta1 <- c(beta1,sigmae,D1[upper.tri(D1, diag = T)], phi)  }
    if(Arp==0){phi=0}
    
    varbeta<-solve(soma5)
    logver <- sum(log(ver))
    
    
    loglik <- logliknArplmec(y=y,x=x,z=z,cc=cc,ttc=tt,nj=nj,LL=LI,LU=LS,betas=beta1,sigmae=sigmae,D1=D1,pii=piis)
    loglikp1 <- loglik 
    
    
    
    if(count > 1){
      criterio <- sqrt(((loglikp1/loglikp)-1)%*%((loglikp1/loglikp)-1))
      setTkProgressBar(pb, count, label=paste("Iter ",count,"/",MaxIter,"     -     ",floor((count)/(MaxIter)*100),"% done",sep = ""))
    }    
    if(count==MaxIter){criterio <- 0.0000000000001}
    
    
    
    teta <- teta1
    loglikp <- loglikp1
    
 
    
  }
  
  dd<-D1[upper.tri(D1, diag = T)]
  
  npar<-length(c(teta1))
  
  ni<-sum(nj)
  
  loglik<-loglikp
  
  AICc<- -2*loglik +2*npar
  AICcorr<- AICc + ((2*npar*(npar+1))/(ni-npar-1))
  BICc <- -2*loglik +log(ni)*npar
  
  if(Prev){ contt=0
  Predicao<- matrix(0,length(isubj),1+step)
  for (j in isubj ){
    
    contt=contt+1
    IndPred=c(rep(0,nj[j]),rep(1,step))
    xobs=x[(sum(nj[1:j-1])+1) : (sum(nj[1:j])),  ]
    xprei=xpre[(step*contt-(step-1)) : (step*contt),  ]
    xobspre=rbind(xobs,xprei)
    
    zobs=z[(sum(nj[1:j-1])+1) : (sum(nj[1:j])) ,  ]
    zprei=zpre[(step*contt-(step-1)) : (step*contt),  ]
    zobspre=rbind(zobs,zprei)
    
    gammai = xobs%*%beta1
    yobs=uyi[(sum(nj[1:j-1])+1) : (sum(nj[1:j])),j]
    tt1=tt[(sum(nj[1:j-1])+1) : (sum(nj[1:j]))]
    
    if(Arp==0){ Gama=diag(1,nj[j]+step)*sigmae}
    if(Arp!=0){ tt1=c(tt1, tt1[nj[j]]+seq(1:step))
    Gama<- MatArp(piis,tt1,sigmae)}
    PsiPred<-(Gama+(zobspre)%*%t(D1)%*%t(zobspre))
    Aux1Pred <- xprei%*%beta1
    Aux2Pred <- PsiPred[IndPred==1,IndPred==0]%*%solve(PsiPred[IndPred==0,IndPred==0])
    Aux3Pred <- (yobs-gammai)
    
    Predicao[contt,1] <- j               
    Predicao[contt,2:(step+1)] <- Aux1Pred + Aux2Pred%*%Aux3Pred               
    
  }
  Predicao=as.data.frame(Predicao)
  colnames(Predicao) = c("subj",paste("step",1:step))
  }
  if(!Prev){Predicao=NULL}
  
  SE=round(sqrt(diag(solve(MI))),3)
  intPar=round(1.96*SE,3)
  
  tableB  = data.frame(round(beta1,3),SE[1:p],paste("<",round(beta1,3)-round(intPar[1:p],3),",",round(beta1,3)+round(intPar[1:p],3),">"))
  rownames(tableB) = paste("beta",1:p)
  colnames(tableB) = c("Est","SE","IConf(95%)")
  
  
  if((round(sigmae,3)-round(intPar[p+1],3))<0) tableS  = data.frame(round(sigmae,3),SE[p+1],paste("<",0,",",round(sigmae,3)+round(intPar[p+1],3),">"))
  if((round(sigmae,3)-round(intPar[p+1],3))>=0) tableS  = data.frame(round(sigmae,3),SE[p+1],paste("<",round(sigmae,3)-round(intPar[p+1],3),",",round(sigmae,3)+round(intPar[p+1],3),">"))
  rownames(tableS) = "Sigma^2"
  colnames(tableS) = c("Est","SE","IConf(95%)")
  
  if(Arp!=0){
    tableP  = data.frame(round(phi,3),SE[(p+2):(p+1+Arp)],paste("<",round(phi,3)-round(intPar[(p+2):(p+1+Arp)],3),",",round(phi,3)+round(intPar[(p+2):(p+1+Arp)],3),">"))
    rownames(tableP) = paste("Phi",1:Arp)
    colnames(tableP) = c("Est","SE","IConf(95%)")
  }
  if(Arp==0){ phi=NULL; tableP =NULL }
  
  nnp=0
  for(al in 1:dim(D1)[1]) 
  {noa=paste(1:al,al,sep = "")
  nnp=c(nnp,noa)
  }
  nnp=nnp[-1]
  ici=round(dd,3)-round(intPar[(p+2+Arp):(p+1+length(D1[upper.tri(D1, diag = T)])+Arp)],3)
  ics=round(dd,3)+round(intPar[(p+2+Arp):(p+1+length(D1[upper.tri(D1, diag = T)])+Arp)],3)
  ici[as.numeric(nnp)%%11==0&ici<0]=0
  tableA  = data.frame(round(dd,3),SE[(p+2+Arp):(p+1+length(D1[upper.tri(D1, diag = T)])+Arp)],paste("<",ici,",",ics,">"))
  rownames(tableA) = paste("Alpha",nnp)
  colnames(tableA) = c("Est","SE","IConf(95%)")
  
  
  
  res <-fitY<- vector(mode = "numeric", length = sum(nj))
  Di <- matrix(0,dim(z)[2],dim(z)[2]) 
  Di[upper.tri(Di, diag = T)] <- dd
  Di[lower.tri(Di, diag = T)] <- dd
  Di      <- round(Di,6)
  for (k in 1:length(nj)) 
  {tc<-tt[(sum(nj[1:k-1])+1) : (sum(nj[1:k]))]
  if(Arp=="UNC"){Mq<-diag(1,length(tc))}
  if(Arp!="UNC"){Mq<- MatArpJ(phi,tc,sigmae)}
  Sii<-solve(round(z[(sum(nj[1:k-1])+1):(sum(nj[1:k])),]%*%Di%*%t(z[(sum(nj[1:k-1])+1):(sum(nj[1:k])),])+sigmae*Mq,6))
  yyii<-yorg[(sum(nj[1:k-1])+1) :(sum(nj[1:k]))]-x[(sum(nj[1:k-1])+1) : (sum(nj[1:k])),]%*%beta1
  res[(sum(nj[1:k-1])+1) : (sum(nj[1:k]))]=sqrtm(Sii)%*%(yyii)
  fitY[(sum(nj[1:k-1])+1) : (sum(nj[1:k]))]= x[(sum(nj[1:k-1])+1) : (sum(nj[1:k])),]%*%beta1+
  z[(sum(nj[1:k-1])+1):(sum(nj[1:k])),]%*%as.matrix(ubi[(((k-1)*q1)+1) : (k*q1),k])
  }   
  

  end.time <- Sys.time()
  time.taken <- end.time - start.time
  
  obj.out <- list(beta1 = beta1, sigmae= sigmae, phi=phi, dd = dd, loglik=loglik,
                  AIC=AICc, BIC=BICc, AICcorr=AICcorr, iter = count, varbeta=varbeta,
                  ubi = ubi, ubbi = ubbi, uybi = uybi, uyi = uyi, uyyi = uyyi , MI=MI, yorg=yorg,residuals=res,yfit=fitY,
                  Prev= Predicao, time=time.taken, SE=SE,tableB=tableB,tableS=tableS,tableP=tableP,
                  tableA=tableA)
  
  
  if  (count == MaxIter)
  {
    setTkProgressBar(pb, MaxIter, label=paste("MaxIter reached ",count,"/",MaxIter,"    -    100 % done",sep = ""))
    close(pb)
  }
  else
  {
    setTkProgressBar(pb, MaxIter, label=paste("Convergence at Iter ",count,"/",MaxIter,"    -    100 % done",sep = ""))
    close(pb)
  }
  
  
  class(obj.out) <- "ARpNLMEC"
  
  return(obj.out)
  
}


EMCensDECN<-function(cc,y,x,z,tt,nj,struc,initial,cens.type,LI,LS,MaxIter,ee,Prev,step,isubj,xpre,zpre)
{
  start.time <- Sys.time()
  pb = tkProgressBar(title = "DEC-N-LMEC by EM", min = 0,max = MaxIter, width = 300)
  setTkProgressBar(pb, 0, label=paste("Iter ",0,"/",MaxIter,"     -     ",0,"% done",sep = ""))
  
  if(cens.type=="left"){
    LI=rep(-Inf,length(cc))
    LS=rep(Inf,length(cc))
    LS[cc==1]=y[cc==1]
    LI=as.vector(LI)
    LS=as.vector(LS)
  }
  if(cens.type=="right"){
    LI=rep(-Inf,length(cc))
    LI[cc==1]=y[cc==1]
    LS=rep(Inf,length(cc))
    LI=as.vector(LI)
    LS=as.vector(LS)
  }
  if(cens.type=="interval"){
    LI=LI
    LS=LS
    LI=as.vector(LI)
    LS=as.vector(LS)
  }
  
  m<-length(nj)
  N<-sum(nj)
  
  criterio<-1
  count<-0
  
  
  
  if(struc=="DEC"){
    
    p<-dim(x)[2]
    q1<-dim(z)[2]
    m1<-m*p
    m2<-m*q1
    
    if(!is.null(initial)){
      beta1<-initial$betas
      sigmae<- initial$sigma2
      D1<-initial$alphas
      iD1<- solve(D1)
      iD1 <- (iD1 + t(iD1))/2
      if(!is.null(initial$phi1)){
        phi1 <- initial$phi1
        phi2 <- initial$phi2}
      if(is.null(initial$phi1)){
        phi1 <- 0.1
        phi2 <- 1}
    }
    
    if(is.null(initial)){ 
      beta1=solve(t(x)%*%x)%*%t(x)%*%y  
      sigmae=  0.25 
      D1=0.1*diag(dim(z)[2])
      iD1<- solve(D1)
      iD1 <- (iD1 + t(iD1))/2
      phi1 <- 0.1
      phi2 <- 1
    }
    
    rho= phi1
    gamma<-phi2 
    
    teta <- c(beta1,sigmae,D1[upper.tri(D1, diag = T)],rho,gamma)
    tetacrit <- c(beta1,sigmae)
    
    q1<-dim(D1)[2]
    p<-length(beta1)  
    m1<-m*p
    m2<-m*q1
    
    ubi=matrix(0,m2,m)
    
    loglik <- logliknslmec(y=y,x=x,z=z,cc=cc,ttc=tt,nj=nj,LL=LI,LU=LS,betas=beta1,sigmae=sigmae,D1=D1,phi1=phi1,phi2=phi2,struc=struc)
    
    loglikp <- loglik 
    
    criterio<-1
    count<-0
    
    while(criterio > ee){
      
      count <- count + 1
      soma1<-matrix(0,q1,q1)
      soma2<-0
      soma3<-matrix(0,p,p)
      soma4<-matrix(0,p,1)
      soma5<-matrix(0,p,p) 
      MI <- matrix(0,p+1+length(D1[upper.tri(D1, diag = T)])+2,
                   p+1+length(D1[upper.tri(D1, diag = T)])+2) 
      res <- vector(mode = "numeric", length = N)
      ub1<-ubi
      
      ubi=matrix(0,m2,m)     
      ubbi=matrix(0,m2,m2)
      uybi=matrix(0,N,m2)
      uyyi=matrix(0,N,N)
      uyi=matrix(0,N,m)
      yhi=matrix(0,N,1)
      xi=matrix(0,N,m1)
      zi=matrix(0,N,m2) 
      ver<-matrix(0,m,1)
      
      for (j in 1:m ){
        
        
        cc1=cc[(sum(nj[1:j-1])+1) : (sum(nj[1:j]))]
        tt1=tt[(sum(nj[1:j-1])+1) : (sum(nj[1:j]))]
        y1=y[(sum(nj[1:j-1])+1) : (sum(nj[1:j]))]
        x1=matrix(x[(sum(nj[1:j-1])+1) : (sum(nj[1:j])),  ],ncol=p)
        z1=matrix(z[(sum(nj[1:j-1])+1) : (sum(nj[1:j])) ,  ],ncol=q1)
        LI1<- LI[(sum(nj[1:j-1])+1) : (sum(nj[1:j]))]
        LS1<- LS[(sum(nj[1:j-1])+1) : (sum(nj[1:j]))]
        gammai=x1%*%beta1
        Gama<- MatAr1(tt1,rho,gamma,sigmae)
        eGama<-Gama/sigmae
        Psi<-(Gama+(z1)%*%D1%*%t(z1))
        Psi<-(Psi+t(Psi))/2
        delta<- solve(iD1+(t(z1)%*%solve(eGama)%*%(z1*(1/sigmae))))
        
        if(sum(cc1)==0){
          uy<- matrix(y1,nj[j],1)
          uyy<- y1%*%t(y1)
          ub<- delta%*%(t(z1)*(1/sigmae))%*%solve(eGama)%*%(uy-gammai)
          ubb<- delta+(delta%*%(t(z1)*((1/sigmae)^2))%*%solve(eGama)%*%(uyy-uy%*%t(gammai)-gammai%*%t(uy)+gammai%*%t(gammai))%*%solve(eGama)%*%z1%*%delta)
          uyb<- (uyy-uy%*%t(gammai))%*%solve(eGama)%*%(z1*(1/sigmae))%*%delta
          ver[j,]<- LaplacesDemon::dmvn(x=as.vector(y1),mu=as.vector(gammai),Sigma=Psi)
          
        }
        
        if(sum(cc1)>=1){
          
          if(sum(cc1)==nj[j]){
            
            muc=x1%*%beta1
            Sc<-Psi
            aux<- relliptical::mvtelliptical(lower = as.vector(LI1),upper=as.vector(LS1),mu = as.vector(muc), Sigma =Sc)
            uy<- aux$EY
            uyy<- aux$EYY
            ub<- delta%*%(t(z1)*(1/sigmae))%*%solve(eGama)%*%(uy-gammai)
            ubb<- delta+(delta%*%(t(z1)*((1/sigmae)^2))%*%solve(eGama)%*%(uyy-uy%*%t(gammai)-gammai%*%t(uy)+gammai%*%t(gammai))%*%solve(eGama)%*%z1%*%delta)
            uyb<- (uyy-uy%*%t(gammai))%*%solve(eGama)%*%(z1*(1/sigmae))%*%delta
           
            ver[j,]<-  TruncatedNormal::pmvnorm(lb=as.vector(LI1), ub=as.vector(LS1), mu=c(muc),sigma=Sc)
            
            
          }
          
          else {
            muc=x1[cc1==1,]%*%beta1+Psi[cc1==1,cc1==0]%*%solve(Psi[cc1==0,cc1==0])%*%(y1[cc1==0]-x1[cc1==0,]%*%beta1)
            Sc <-Psi[cc1==1,cc1==1]-Psi[cc1==1,cc1==0]%*%solve(Psi[cc1==0,cc1==0])%*%Psi[cc1==0,cc1==1]
            Sc=(Sc+t(Sc))/2
            aux<- relliptical::mvtelliptical(lower = as.vector(LI1[cc1==1]),upper=as.vector(LS1[cc1==1]),mu = as.vector(muc), Sigma =Sc)
            uy <-matrix(y1,nj[j],1)
            uy[cc1==1]<- aux$EY
            uyy<- uy%*%t(uy)
            uyy[cc1==1,cc1==1]<- aux$EYY
            ub<- delta%*%(t(z1)*(1/sigmae))%*%solve(eGama)%*%(uy-gammai)
            ubb<- delta+(delta%*%(t(z1)*((1/sigmae)^2))%*%solve(eGama)%*%(uyy-uy%*%t(gammai)-gammai%*%t(uy)+gammai%*%t(gammai))%*%solve(eGama)%*%z1%*%delta)
            uyb<- (uyy-uy%*%t(gammai))%*%solve(eGama)%*%(z1*(1/sigmae))%*%delta
           
            ver[j,]<- LaplacesDemon::dmvn(x=as.vector(y1[cc1==0]),mu=as.vector(gammai[cc1==0]),Sigma=as.matrix(Psi[cc1==0,cc1==0]))*( TruncatedNormal::pmvnorm(lb=as.vector(LI1[cc1==1]),ub=as.vector(LS1[cc1==1]),mu=as.vector(muc),sigma=Sc))[1]
          }
          
        }
        
        soma1<- soma1 + ubb
        soma2<- soma2 + (sum(diag(uyy%*%solve(eGama)))-t(uy)%*%solve(eGama)%*%gammai-t(gammai)%*%solve(eGama)%*%uy-sum(diag(solve(eGama)%*%((uyb)%*%t(z1))))-sum(diag(solve(eGama)%*%((uyb)%*%t(z1))))
                         +t(gammai)%*%solve(eGama)%*%z1%*%ub+t(ub)%*%t(z1)%*%solve(eGama)%*%gammai+t(gammai)%*%solve(eGama)%*%gammai+sum(diag(ubb%*%t(z1)%*%solve(eGama)%*%z1)))
        soma3<- soma3 + (t(x1)%*%solve(eGama)%*%x1)
        soma4<- soma4 + (t(x1)%*%solve(eGama)%*%(uy-z1%*%ub))
        soma5<- soma5 + (t(x1)%*%solve(Psi)%*%x1-t(x1)%*%solve(Psi)%*%(uyy-uy%*%t(uy))%*%solve(Psi)%*%x1)     
        
        uyyi[(sum(nj[1:j-1])+1) : (sum(nj[1:j])), (sum(nj[1:j-1])+1) : (sum(nj[1:j]))]<-uyy
        uyi[(sum(nj[1:j-1])+1) : (sum(nj[1:j])), j]<-uy
        uybi[(sum(nj[1:j-1])+1) : (sum(nj[1:j])), (((j-1)*q1)+1) : (j*q1)]<-uyb
        ubi[(((j-1)*q1)+1) : (j*q1), j]<-ub
        ubbi[(((j-1)*q1)+1) : (j*q1), (((j-1)*q1)+1) : (j*q1)]<-ubb
        zi[(sum(nj[1:j-1])+1) : (sum(nj[1:j])), (((j-1)*q1)+1) : (j*q1)]<-z1
        xi[(sum(nj[1:j-1])+1) : (sum(nj[1:j])), (((j-1)*p)+1) : (j*p)]<-x1
        yhi[(sum(nj[1:j-1])+1) : (sum(nj[1:j])),] <- uy
        dbeta <- (1/sigmae)*((t(x1)%*%solve(eGama)%*%(uy-z1%*%ub)) - (t(x1)%*%solve(eGama)%*%x1)%*%beta1)
        dsigma <- -(1/2)*((nj[j]/sigmae)-(1/sigmae^2)*((sum(diag(uyy%*%solve(eGama)))-t(uy)%*%solve(eGama)%*%gammai-t(gammai)%*%solve(eGama)%*%uy-sum(diag(solve(eGama)%*%((uyb)%*%t(z1))))-sum(diag(solve(eGama)%*%((uyb)%*%t(z1))))
                                                        +t(gammai)%*%solve(eGama)%*%z1%*%ub+t(ub)%*%t(z1)%*%solve(eGama)%*%gammai+t(gammai)%*%solve(eGama)%*%gammai+sum(diag(ubb%*%t(z1)%*%solve(eGama)%*%z1)))))
        
        
        Dp <- DevEiAr1(tt1,rho,gamma,sigmae) 
        Dpr <- Dp$devR_r
        Dpg <- Dp$devR_g
        dE1_r <- sum(diag(solve(eGama)%*%Dpr))
        dE2_r <- -(solve(eGama)%*%Dpr%*%solve(eGama)) 
        dE1_g <- sum(diag(solve(eGama)%*%Dpg))
        dE2_g <- -(solve(eGama)%*%Dpg%*%solve(eGama))   
        drho <- - 0.5*dE1_r-(0.5/sigmae)*((sum(diag(uyy%*%dE2_r))-t(uy)%*%dE2_r%*%gammai-t(gammai)%*%dE2_r%*%uy-sum(diag(dE2_r%*%((uyb)%*%t(z1))))-sum(diag(dE2_r%*%((uyb)%*%t(z1))))
                                           +t(gammai)%*%dE2_r%*%z1%*%ub+t(ub)%*%t(z1)%*%dE2_r%*%gammai+t(gammai)%*%dE2_r%*%gammai+sum(diag(ubb%*%t(z1)%*%dE2_r%*%z1))))      
        dgama <- - 0.5*dE1_g-(0.5/sigmae)*((sum(diag(uyy%*%dE2_g))-t(uy)%*%dE2_g%*%gammai-t(gammai)%*%dE2_g%*%uy-sum(diag(dE2_g%*%((uyb)%*%t(z1))))-sum(diag(dE2_g%*%((uyb)%*%t(z1))))
                                            +t(gammai)%*%dE2_g%*%z1%*%ub+t(ub)%*%t(z1)%*%dE2_g%*%gammai+t(gammai)%*%dE2_g%*%gammai+sum(diag(ubb%*%t(z1)%*%dE2_g%*%z1))))
        
        
        D_der <- Derivadas(D1)
        deralpha<-rep(0,length(D1[upper.tri(D1, diag = T)]))
        md2<-dim(D1)[1]  
        kont <- 0
        for(i1 in 1:md2){
          for(i2 in 1:(md2+1-i1)){
            kont <- kont+1
            di <- D_der[[i1]][[i2]]
            deralpha[kont] <- (-0.5)*sum(diag(iD1%*%di-iD1%*%di%*%iD1*ubb))     
          }
        }
        si <- matrix(c(t(dbeta),t(dsigma),t(drho),t(dgama),t(deralpha)),p+1+2+length(D1[upper.tri(D1, diag = T)]),1)
        MI <- MI + si%*%t(si)
        
      }
      yorg<-apply(yhi,1,sum)
      beta1<- solve(soma3)%*%soma4
      sigmae<- (1/N)*(soma2)
      sigmae<-as.numeric(sigmae)
      D1<- (1/m)*(soma1)                      
      iD1<- solve(D1)                                                                                                         
      rhos <- optim(c(rho,gamma), method = "L-BFGS-B", FCi, lower =c(0.01,0.01), upper =c(0.9,30), beta1=beta1,sigmae=sigmae,tt=tt,ubi=ubi,ubbi=ubbi,uybi=uybi,uyyi=uyyi,uyi=uyi,xi=xi,zi=zi,nj=nj,hessian=TRUE)$par
      rho<-rhos[1]
      gamma<-rhos[2]
      teta1 <- c(beta1,sigmae,D1[upper.tri(D1, diag = T)],rho,gamma)
      teta1crit <- c(beta1,sigmae)
      varbeta<-solve(soma5)
      logver <- sum(log(ver))
      
      loglik <- logliknslmec(y=y,x=x,z=z,cc=cc,ttc=tt,nj=nj,LL=LI,LU=LS,betas=beta1,sigmae=sigmae,D1=D1,phi1=rho,phi2=gamma,struc=struc)
      
      loglikp1 <- loglik     
      
      if(count > 1){
        criterio <- sqrt(((loglikp1/loglikp)-1)%*%((loglikp1/loglikp)-1))
        setTkProgressBar(pb, count, label=paste("Iter ",count,"/",MaxIter,"     -     ",floor((count)/(MaxIter)*100),"% done",sep = ""))
      }    
      if(count==MaxIter){criterio <- 0.0000000000001}
      
      teta <- teta1
      loglikp <- loglikp1
      
      tetacrit <- teta1crit
      logver1<-logver
      
    }
    
  }  
  if(struc=="DEC(AR)"){
    p<-dim(x)[2]
    q1<-dim(z)[2]
    m1<-m*p
    m2<-m*q1
    if(!is.null(initial)){
      beta1<-initial$betas
      sigmae<- initial$sigma2
      D1<-initial$alphas
      iD1<- solve(D1)
      iD1 <- (iD1 + t(iD1))/2
      if(!is.null(initial$phi1)){
        phi1 <- initial$phi1
        phi2 <- 1}
      if(is.null(initial$phi1)){
        phi1 <- 0.1
        phi2 <- 1}
      
    }
    if(is.null(initial)){ 
      beta1=solve(t(x)%*%x)%*%t(x)%*%y  
      sigmae= 0.25 
      D1=0.1*diag(dim(z)[2])
      iD1<- solve(D1)
      iD1 <- (iD1 + t(iD1))/2
      phi1 <- 0.1
      phi2 <- 1
    }
    rho= phi1
    gamma<-phi2 
    teta <- c(beta1,sigmae,D1[upper.tri(D1, diag = T)],rho)
    tetacrit <- c(beta1,sigmae)

    
    ubi=matrix(0,m2,m)
    
    loglik <- logliknslmec(y=y,x=x,z=z,cc=cc,ttc=tt,nj=nj,LL=LI,LU=LS,betas=beta1,sigmae=sigmae,D1=D1,phi1=phi1,phi2=phi2,struc=struc)
    
    loglikp <- loglik 
    
    criterio<-1
    count<-0
    
    while(criterio > ee){
      
      count <- count + 1
      soma1<-matrix(0,q1,q1)
      soma2<-0
      soma3<-matrix(0,p,p)
      soma4<-matrix(0,p,1)
      soma5<-matrix(0,p,p) 
      MI <- matrix(0,p+1+length(D1[upper.tri(D1, diag = T)])+1,
                   p+1+length(D1[upper.tri(D1, diag = T)])+1) 
      ub1<-ubi
      res <- vector(mode = "numeric", length = N)
      ubi=matrix(0,m2,m)    
      ubbi=matrix(0,m2,m2)
      uybi=matrix(0,N,m2)
      uyyi=matrix(0,N,N)
      uyi=matrix(0,N,m)
      yhi=matrix(0,N,1)
      xi=matrix(0,N,m1)
      zi=matrix(0,N,m2)
      ver<-matrix(0,m,1)
      
      for (j in 1:m ){
        
        cc1=cc[(sum(nj[1:j-1])+1) : (sum(nj[1:j]))]
        tt1=tt[(sum(nj[1:j-1])+1) : (sum(nj[1:j]))]
        y1=y[(sum(nj[1:j-1])+1) : (sum(nj[1:j]))]
        x1=matrix(x[(sum(nj[1:j-1])+1) : (sum(nj[1:j])),  ],ncol=p)
        z1=matrix(z[(sum(nj[1:j-1])+1) : (sum(nj[1:j])) ,  ],ncol=q1)
        LI1<- LI[(sum(nj[1:j-1])+1) : (sum(nj[1:j]))]
        LS1<- LS[(sum(nj[1:j-1])+1) : (sum(nj[1:j]))]
        gammai=x1%*%beta1
        Gama<- MatAr1(tt1,rho,gamma,sigmae)
        eGama<-Gama/sigmae
        Psi<-(Gama+(z1)%*%D1%*%t(z1))
        delta<- solve(iD1+(t(z1)%*%solve(eGama)%*%(z1*(1/sigmae))))
        
        
        if(sum(cc1)==0){
          
          uy<- matrix(y1,nj[j],1)
          uyy<- y1%*%t(y1)
          ub<- delta%*%(t(z1)*(1/sigmae))%*%solve(eGama)%*%(uy-gammai)
          ubb<- delta+(delta%*%(t(z1)*((1/sigmae)^2))%*%solve(eGama)%*%(uyy-uy%*%t(gammai)-gammai%*%t(uy)+gammai%*%t(gammai))%*%solve(eGama)%*%z1%*%delta)
          uyb<- (uyy-uy%*%t(gammai))%*%solve(eGama)%*%(z1*(1/sigmae))%*%delta
          ver[j,]<- LaplacesDemon::dmvn(x=as.vector(y1),mu=as.vector(gammai),Sigma=Psi)
          
        }
        
        if(sum(cc1)>=1){
          
          if(sum(cc1)==nj[j]){
            muc=x1%*%beta1
            Sc<-Psi
            Sc=(Sc+t(Sc))/2
            aux<- relliptical::mvtelliptical(lower = as.vector(LI1),upper=as.vector(LS1),mu = as.vector(muc), Sigma =Sc)
            uy<- aux$EY
            uyy<- aux$EYY
            ub<- delta%*%(t(z1)*(1/sigmae))%*%solve(eGama)%*%(uy-gammai)
            ubb<- delta+(delta%*%(t(z1)*((1/sigmae)^2))%*%solve(eGama)%*%(uyy-uy%*%t(gammai)-gammai%*%t(uy)+gammai%*%t(gammai))%*%solve(eGama)%*%z1%*%delta)
            uyb<- (uyy-uy%*%t(gammai))%*%solve(eGama)%*%(z1*(1/sigmae))%*%delta
            ver[j,]<-  TruncatedNormal::pmvnorm(lb=as.vector(LI1), ub=as.vector(LS1), mu=as.vector(muc),sigma=Sc)
            
            
          }
          
          else {
            
            muc=x1[cc1==1,]%*%beta1+Psi[cc1==1,cc1==0]%*%solve(Psi[cc1==0,cc1==0])%*%(y1[cc1==0]-x1[cc1==0,]%*%beta1)
            Sc <-Psi[cc1==1,cc1==1]-Psi[cc1==1,cc1==0]%*%solve(Psi[cc1==0,cc1==0])%*%Psi[cc1==0,cc1==1]
            Sc=(Sc+t(Sc))/2
            aux<- relliptical::mvtelliptical(lower = as.vector(LI1[cc1==1]),upper=as.vector(LS1[cc1==1]),mu = as.vector(muc), Sigma =Sc)
            uy <-matrix(y1,nj[j],1)
            uy[cc1==1]<- aux$EY
            uyy<- uy%*%t(uy)
            uyy[cc1==1,cc1==1]<- aux$EYY
            ub<- delta%*%(t(z1)*(1/sigmae))%*%solve(eGama)%*%(uy-gammai)
            ubb<- delta+(delta%*%(t(z1)*((1/sigmae)^2))%*%solve(eGama)%*%(uyy-uy%*%t(gammai)-gammai%*%t(uy)+gammai%*%t(gammai))%*%solve(eGama)%*%z1%*%delta)
            uyb<- (uyy-uy%*%t(gammai))%*%solve(eGama)%*%(z1*(1/sigmae))%*%delta
            
            ver[j,]<- LaplacesDemon::dmvn(x=as.vector(y1[cc1==0]),mu=as.vector(gammai[cc1==0]),Sigma=as.matrix(Psi[cc1==0,cc1==0]))*( TruncatedNormal::pmvnorm(lb=as.vector(LI1[cc1==1]),ub=as.vector(LS1[cc1==1]),mu=as.vector(muc),sigma=Sc))[1]
            
            
          }
          
        }
        
        soma1<- soma1 + ubb
        soma2<- soma2 + (sum(diag(uyy%*%solve(eGama)))-t(uy)%*%solve(eGama)%*%gammai-t(gammai)%*%solve(eGama)%*%uy-sum(diag(solve(eGama)%*%((uyb)%*%t(z1))))-sum(diag(solve(eGama)%*%((uyb)%*%t(z1))))
                         +t(gammai)%*%solve(eGama)%*%z1%*%ub+t(ub)%*%t(z1)%*%solve(eGama)%*%gammai+t(gammai)%*%solve(eGama)%*%gammai+sum(diag(ubb%*%t(z1)%*%solve(eGama)%*%z1)))
        soma3<- soma3 + (t(x1)%*%solve(eGama)%*%x1)
        soma4<- soma4 + (t(x1)%*%solve(eGama)%*%(uy-z1%*%ub))
        soma5<- soma5 + (t(x1)%*%solve(Psi)%*%x1-t(x1)%*%solve(Psi)%*%(uyy-uy%*%t(uy))%*%solve(Psi)%*%x1)     
        
        uyyi[(sum(nj[1:j-1])+1) : (sum(nj[1:j])), (sum(nj[1:j-1])+1) : (sum(nj[1:j]))]<-uyy
        uyi[(sum(nj[1:j-1])+1) : (sum(nj[1:j])), j]<-uy
        uybi[(sum(nj[1:j-1])+1) : (sum(nj[1:j])), (((j-1)*q1)+1) : (j*q1)]<-uyb
        ubi[(((j-1)*q1)+1) : (j*q1), j]<-ub
        ubbi[(((j-1)*q1)+1) : (j*q1), (((j-1)*q1)+1) : (j*q1)]<-ubb
        yhi[(sum(nj[1:j-1])+1) : (sum(nj[1:j])),] <- uy
        zi[(sum(nj[1:j-1])+1) : (sum(nj[1:j])), (((j-1)*q1)+1) : (j*q1)]<-z1
        xi[(sum(nj[1:j-1])+1) : (sum(nj[1:j])), (((j-1)*p)+1) : (j*p)]<-x1  
        
        dbeta <- (1/sigmae)*((t(x1)%*%solve(eGama)%*%(uy-z1%*%ub)) - (t(x1)%*%solve(eGama)%*%x1)%*%beta1)
        dsigma <- -(1/2)*((nj[j]/sigmae)-(1/sigmae^2)*((sum(diag(uyy%*%solve(eGama)))-t(uy)%*%solve(eGama)%*%gammai-t(gammai)%*%solve(eGama)%*%uy-sum(diag(solve(eGama)%*%((uyb)%*%t(z1))))-sum(diag(solve(eGama)%*%((uyb)%*%t(z1))))
                                                        +t(gammai)%*%solve(eGama)%*%z1%*%ub+t(ub)%*%t(z1)%*%solve(eGama)%*%gammai+t(gammai)%*%solve(eGama)%*%gammai+sum(diag(ubb%*%t(z1)%*%solve(eGama)%*%z1)))))
        Dp <- DevEiAr1(tt1,rho,gamma,sigmae) 
        Dpr <- Dp$devR_r
        dE1_r <- sum(diag(solve(eGama)%*%Dpr))
        dE2_r <- -(solve(eGama)%*%Dpr%*%solve(eGama)) 
        drho <- - 0.5*dE1_r-(0.5/sigmae)*((sum(diag(uyy%*%dE2_r))-t(uy)%*%dE2_r%*%gammai-t(gammai)%*%dE2_r%*%uy-sum(diag(dE2_r%*%((uyb)%*%t(z1))))-sum(diag(dE2_r%*%((uyb)%*%t(z1))))
                                           +t(gammai)%*%dE2_r%*%z1%*%ub+t(ub)%*%t(z1)%*%dE2_r%*%gammai+t(gammai)%*%dE2_r%*%gammai+sum(diag(ubb%*%t(z1)%*%dE2_r%*%z1))))      
        D_der <- Derivadas(D1)
        deralpha<-rep(0,length(D1[upper.tri(D1, diag = T)]))
        md2<-dim(D1)[1]  
        kont <- 0
        for(i1 in 1:md2){
          for(i2 in 1:(md2+1-i1)){
            kont <- kont+1
            di <- D_der[[i1]][[i2]]
            deralpha[kont] <- (-0.5)*sum(diag(iD1%*%di-iD1%*%di%*%iD1*ubb))     
          }
        }
        
        si <- matrix(c(t(dbeta),t(dsigma),t(drho),t(deralpha)),p+1+length(D1[upper.tri(D1, diag = T)])+1,1)
        MI <- MI + si%*%t(si)
        
      }
      yorg<-apply(yhi,1,sum)
      beta1<- solve(soma3)%*%soma4
      sigmae<- (1/N)*(soma2)
      sigmae<-as.numeric(sigmae)
      D1<- (1/m)*(soma1)
      iD1<- solve(D1)                                                                                                         
      rhos <- optim(rho, method = "L-BFGS-B", FCi_gamma, lower = 0.0001, upper =0.9, gamma=gamma,beta1=beta1,sigmae=sigmae,tt=tt,ubi=ubi,ubbi=ubbi,uybi=uybi,uyyi=uyyi,uyi=uyi,xi=xi,zi=zi,nj=nj,hessian=TRUE)$par
      rho<-rhos[1]
      gamma<-1
      teta1 <- c(beta1,sigmae,D1[upper.tri(D1, diag = T)],rho)
      varbeta<-solve(soma5)
      logver <- sum(log(ver))
      
      teta1crit <- c(beta1,sigmae)
      
      loglik <- logliknslmec(y=y,x=x,z=z,cc=cc,ttc=tt,nj=nj,LL=LI,LU=LS,betas=beta1,sigmae=sigmae,D1=D1,phi1=rho,phi2=gamma,struc=struc)
      
      loglikp1 <- loglik     
      
      if(count > 1){
        criterio <- sqrt(((loglikp1/loglikp)-1)%*%((loglikp1/loglikp)-1))
        setTkProgressBar(pb, count, label=paste("Iter ",count,"/",MaxIter,"     -     ",floor((count)/(MaxIter)*100),"% done",sep = ""))
      }    
      if(count==MaxIter){criterio <- 0.0000000000001}
      
      teta <- teta1
      loglikp <- loglikp1
      
      tetacrit <- teta1crit
      logver1<-logver
      
    }
  } 
  if(struc=="SYM"){
    
    p<-dim(x)[2]
    q1<-dim(z)[2]
    m1<-m*p
    m2<-m*q1
    
    if(!is.null(initial)){
      beta1<-initial$betas
      sigmae<- initial$sigma2
      D1<-initial$alphas
      iD1<- solve(D1)
      iD1 <- (iD1 + t(iD1))/2
      if(!is.null(initial$phi1)){
        phi1 <- initial$phi1
        phi2 <- 0}
      if(is.null(initial$phi1)){
        phi1 <- 0.1
        phi2 <- 0}
    }
    
    if(is.null(initial)){ 
      beta1=solve(t(x)%*%x)%*%t(x)%*%y  
      sigmae= 0.25 
      D1=0.1*diag(dim(z)[2])
      iD1<- solve(D1)
      iD1 <- (iD1 + t(iD1))/2
      phi1 <- 0.1
      phi2 <- 0
    }
    
    
    
    rho=phi1
    gamma<-phi2
    
    teta <- c(beta1,sigmae,D1[upper.tri(D1, diag = T)],rho)
    tetacrit <- c(beta1,sigmae)
    
    q1<-dim(D1)[2]
    p<-length(beta1)  
    m1<-m*p
    m2<-m*q1
    
    ubi=matrix(0,m2,m)  
    loglik <- logliknslmec(y=y,x=x,z=z,cc=cc,ttc=tt,nj=nj,LL=LI,LU=LS,betas=beta1,sigmae=sigmae,D1=D1,phi1=phi1,phi2=phi2,struc=struc)
    
    loglikp <- loglik 
    criterio<-1
    count<-0
    
    while(criterio >ee){
      
      count <- count + 1
      
      soma1<-matrix(0,q1,q1)
      soma2<-0
      soma3<-matrix(0,p,p)
      soma4<-matrix(0,p,1)
      soma5<-matrix(0,p,p) 
      MI <- matrix(0,p+1+length(D1[upper.tri(D1, diag = T)])+1,
                   p+1+length(D1[upper.tri(D1, diag = T)])+1) 
      
      ub1<-ubi
      res <- vector(mode = "numeric", length = N)
      ubi=matrix(0,m2,m)    
      ubbi=matrix(0,m2,m2)
      uybi=matrix(0,N,m2)
      uyyi=matrix(0,N,N)
      uyi=matrix(0,N,m)
      yhi=matrix(0,N,1)
      xi=matrix(0,N,m1)
      zi=matrix(0,N,m2)
      ver<-matrix(0,m,1)
      
      
      for (j in 1:m ){
        
        
        cc1=cc[(sum(nj[1:j-1])+1) : (sum(nj[1:j]))]
        tt1=tt[(sum(nj[1:j-1])+1) : (sum(nj[1:j]))]
        y1=y[(sum(nj[1:j-1])+1) : (sum(nj[1:j]))]
        x1=matrix(x[(sum(nj[1:j-1])+1) : (sum(nj[1:j])),  ],ncol=p)
        z1=matrix(z[(sum(nj[1:j-1])+1) : (sum(nj[1:j])) ,  ],ncol=q1)
        LI1<- LI[(sum(nj[1:j-1])+1) : (sum(nj[1:j]))]
        LS1<- LS[(sum(nj[1:j-1])+1) : (sum(nj[1:j]))]
        gammai=x1%*%beta1
        Gama<- MatAr1(tt1,rho,gamma,sigmae)
        eGama<-Gama/sigmae
        Psi<-(Gama+(z1)%*%D1%*%t(z1))
        Psi=(Psi+t(Psi))/2
        delta<- solve(iD1+(t(z1)%*%solve(eGama)%*%(z1*(1/sigmae))))
        
        
        if(sum(cc1)==0){
          
          uy<- matrix(y1,nj[j],1)
          uyy<- y1%*%t(y1)
          ub<- delta%*%(t(z1)*(1/sigmae))%*%solve(eGama)%*%(uy-gammai)
          ubb<- delta+(delta%*%(t(z1)*((1/sigmae)^2))%*%solve(eGama)%*%(uyy-uy%*%t(gammai)-gammai%*%t(uy)+gammai%*%t(gammai))%*%solve(eGama)%*%z1%*%delta)
          uyb<- (uyy-uy%*%t(gammai))%*%solve(eGama)%*%(z1*(1/sigmae))%*%delta
          ver[j,]<- LaplacesDemon::dmvn(x=as.vector(y1),mu=as.vector(gammai),Sigma=Psi)
          
        }
        
        if(sum(cc1)>=1){
          
          if(sum(cc1)==nj[j]){
            
            muc=x1%*%beta1
            Sc<-Psi
            Sc=(Sc+t(Sc))/2
            aux<- relliptical::mvtelliptical(lower = as.vector(LI1),upper=as.vector(LS1),mu = as.vector(muc), Sigma =Sc)
            uy<- aux$EY
            uyy<- aux$EYY
            ub<- delta%*%(t(z1)*(1/sigmae))%*%solve(eGama)%*%(uy-gammai)
            ubb<- delta+(delta%*%(t(z1)*((1/sigmae)^2))%*%solve(eGama)%*%(uyy-uy%*%t(gammai)-gammai%*%t(uy)+gammai%*%t(gammai))%*%solve(eGama)%*%z1%*%delta)
            uyb<- (uyy-uy%*%t(gammai))%*%solve(eGama)%*%(z1*(1/sigmae))%*%delta
            ver[j,]<-TruncatedNormal::pmvnorm(lb=as.vector(LI1), ub=as.vector(LS1), mu=as.vector(muc),sigma=Sc)
            
          }
          
          else {
            muc=x1[cc1==1,]%*%beta1+Psi[cc1==1,cc1==0]%*%solve(Psi[cc1==0,cc1==0])%*%(y1[cc1==0]-x1[cc1==0,]%*%beta1)
            Sc <-Psi[cc1==1,cc1==1]-Psi[cc1==1,cc1==0]%*%solve(Psi[cc1==0,cc1==0])%*%Psi[cc1==0,cc1==1]
            Sc=(Sc+t(Sc))/2
            aux<- relliptical::mvtelliptical(lower = as.vector(LI1[cc1==1]),upper=as.vector(LS1[cc1==1]),mu = as.vector(muc), Sigma =Sc)
            uy <-matrix(y1,nj[j],1)
            uy[cc1==1]<- aux$EY
            uyy<- uy%*%t(uy)
            uyy[cc1==1,cc1==1]<- aux$EYY
            ub<- delta%*%(t(z1)*(1/sigmae))%*%solve(eGama)%*%(uy-gammai)
            ubb<- delta+(delta%*%(t(z1)*((1/sigmae)^2))%*%solve(eGama)%*%(uyy-uy%*%t(gammai)-gammai%*%t(uy)+gammai%*%t(gammai))%*%solve(eGama)%*%z1%*%delta)
            uyb<- (uyy-uy%*%t(gammai))%*%solve(eGama)%*%(z1*(1/sigmae))%*%delta
            
             ver[j,]<- LaplacesDemon::dmvn(x=as.vector(y1[cc1==0]),mu=as.vector(gammai[cc1==0]),Sigma=as.matrix(Psi[cc1==0,cc1==0]))*( TruncatedNormal::pmvnorm(lb=as.vector(LI1[cc1==1]),ub=as.vector(LS1[cc1==1]),mu=as.vector(muc),sigma=Sc))[1]
            
            
          }
          
        }
        
        soma1<- soma1 + ubb
        soma2<- soma2 + (sum(diag(uyy%*%solve(eGama)))-t(uy)%*%solve(eGama)%*%gammai-t(gammai)%*%solve(eGama)%*%uy-sum(diag(solve(eGama)%*%((uyb)%*%t(z1))))-sum(diag(solve(eGama)%*%((uyb)%*%t(z1))))
                         +t(gammai)%*%solve(eGama)%*%z1%*%ub+t(ub)%*%t(z1)%*%solve(eGama)%*%gammai+t(gammai)%*%solve(eGama)%*%gammai+sum(diag(ubb%*%t(z1)%*%solve(eGama)%*%z1)))
        soma3<- soma3 + (t(x1)%*%solve(eGama)%*%x1)
        soma4<- soma4 + (t(x1)%*%solve(eGama)%*%(uy-z1%*%ub))
        soma5<- soma5 + (t(x1)%*%solve(Psi)%*%x1-t(x1)%*%solve(Psi)%*%(uyy-uy%*%t(uy))%*%solve(Psi)%*%x1)     
        
        uyyi[(sum(nj[1:j-1])+1) : (sum(nj[1:j])), (sum(nj[1:j-1])+1) : (sum(nj[1:j]))]<-uyy
        uyi[(sum(nj[1:j-1])+1) : (sum(nj[1:j])), j]<-uy
        uybi[(sum(nj[1:j-1])+1) : (sum(nj[1:j])), (((j-1)*q1)+1) : (j*q1)]<-uyb
        ubi[(((j-1)*q1)+1) : (j*q1), j]<-ub
        ubbi[(((j-1)*q1)+1) : (j*q1), (((j-1)*q1)+1) : (j*q1)]<-ubb
        yhi[(sum(nj[1:j-1])+1) : (sum(nj[1:j])),] <- uy
        zi[(sum(nj[1:j-1])+1) : (sum(nj[1:j])), (((j-1)*q1)+1) : (j*q1)]<-z1
        xi[(sum(nj[1:j-1])+1) : (sum(nj[1:j])), (((j-1)*p)+1) : (j*p)]<-x1
        
        dbeta <- (1/sigmae)*((t(x1)%*%solve(eGama)%*%(uy-z1%*%ub)) - (t(x1)%*%solve(eGama)%*%x1)%*%beta1)
        
        dsigma <- -(1/2)*((nj[j]/sigmae)-(1/sigmae^2)*((sum(diag(uyy%*%solve(eGama)))-t(uy)%*%solve(eGama)%*%gammai-t(gammai)%*%solve(eGama)%*%uy-sum(diag(solve(eGama)%*%((uyb)%*%t(z1))))-sum(diag(solve(eGama)%*%((uyb)%*%t(z1))))
                                                        +t(gammai)%*%solve(eGama)%*%z1%*%ub+t(ub)%*%t(z1)%*%solve(eGama)%*%gammai+t(gammai)%*%solve(eGama)%*%gammai+sum(diag(ubb%*%t(z1)%*%solve(eGama)%*%z1)))))
        
        
        Dp <- DevEiAr1(tt1,rho,gamma,sigmae) 
        Dpr <- Dp$devR_r
        dE1_r <- sum(diag(solve(eGama)%*%Dpr))
        dE2_r <- -(solve(eGama)%*%Dpr%*%solve(eGama)) 
        drho <- - 0.5*dE1_r-(0.5/sigmae)*((sum(diag(uyy%*%dE2_r))-t(uy)%*%dE2_r%*%gammai-t(gammai)%*%dE2_r%*%uy-sum(diag(dE2_r%*%((uyb)%*%t(z1))))-sum(diag(dE2_r%*%((uyb)%*%t(z1))))
                                           +t(gammai)%*%dE2_r%*%z1%*%ub+t(ub)%*%t(z1)%*%dE2_r%*%gammai+t(gammai)%*%dE2_r%*%gammai+sum(diag(ubb%*%t(z1)%*%dE2_r%*%z1))))      
        D_der <- Derivadas(D1)
        deralpha<-rep(0,length(D1[upper.tri(D1, diag = T)]))
        md2<-dim(D1)[1]  
        kont <- 0
        for(i1 in 1:md2){
          for(i2 in 1:(md2+1-i1)){
            kont <- kont+1
            di <- D_der[[i1]][[i2]]
            deralpha[kont] <- (-0.5)*sum(diag(iD1%*%di-iD1%*%di%*%iD1*ubb))     
          }
        }
        
        si <- matrix(c(t(dbeta),t(dsigma),t(drho),t(deralpha)),p+1+length(D1[upper.tri(D1, diag = T)])+1,1)
        MI <- MI + si%*%t(si)
        
      }
      yorg<-apply(yhi,1,sum)
      beta1<- solve(soma3)%*%soma4
      sigmae<- (1/N)*(soma2)
      sigmae<-as.numeric(sigmae)
      D1<- (1/m)*(soma1)
      iD1<- solve(D1)                                                                                                         
      rhos <- optim(rho, method = "L-BFGS-B", FCi_gamma, lower = 0.0001, upper =0.9, gamma=gamma,beta1=beta1,sigmae=sigmae,tt=tt,ubi=ubi,ubbi=ubbi,uybi=uybi,uyyi=uyyi,uyi=uyi,xi=xi,zi=zi,nj=nj,hessian=TRUE)$par
      rho<-rhos[1]
      gamma<-0
      teta1 <- c(beta1,sigmae,D1[upper.tri(D1, diag = T)],rho)
      teta1crit <- c(beta1,sigmae)
      varbeta<-solve(soma5)
      logver <- sum(log(ver))
      loglik <- logliknslmec(y=y,x=x,z=z,cc=cc,ttc=tt,nj=nj,LL=LI,LU=LS,betas=beta1,sigmae=sigmae,D1=D1,phi1=rho,phi2=gamma,struc=struc)
      
      loglikp1 <- loglik     
      
      if(count > 1){
        criterio <- sqrt(((loglikp1/loglikp)-1)%*%((loglikp1/loglikp)-1))
        setTkProgressBar(pb, count, label=paste("Iter ",count,"/",MaxIter,"     -     ",floor((count)/(MaxIter)*100),"% done",sep = ""))
      }    
      if(count==MaxIter){criterio <- 0.0000000000001}
      
      teta <- teta1
      loglikp <- loglikp1
      
      tetacrit <- teta1crit
      logver1<-logver
      
    }
  }
  if(struc=="UNC"){
    p<-dim(x)[2]
    q1<-dim(z)[2]
    m1<-m*p
    m2<-m*q1
    
    if(!is.null(initial)){
      beta1<-initial$betas
      sigmae<- initial$sigma2
      D1<-initial$alphas
      iD1<- solve(D1)
      iD1 <- (iD1 + t(iD1))/2
      phi1 <- NULL
      phi2 <- NULL
    }
    
    if(is.null(initial)){ 
      beta1=solve(t(x)%*%x)%*%t(x)%*%y  
      sigmae= 0.25 
      D1=0.1*diag(dim(z)[2])
      iD1<- solve(D1)
      iD1 <- (iD1 + t(iD1))/2
      phi1 <- NULL
      phi2 <- NULL
    }
    
    teta <- c(beta1,sigmae,D1[upper.tri(D1, diag = T)])
    tetacrit <- c(beta1,sigmae)
    
    q1<-dim(D1)[2]
    p<-length(beta1)  
    m1<-m*p
    m2<-m*q1
    
    ubi=matrix(0,m2,m)  
    loglik <- logliknslmec(y=y,x=x,z=z,cc=cc,ttc=tt,nj=nj,LL=LI,LU=LS,betas=beta1,sigmae=sigmae,D1=D1,phi1=phi1,phi2=phi2,struc=struc)
    
    loglikp <- loglik 
    criterio<-1
    count<-0
    
    while(criterio > ee){
      
      count <- count + 1
      
      soma1<-matrix(0,q1,q1)
      soma2<-0
      soma3<-matrix(0,p,p)
      soma4<-matrix(0,p,1)
      soma5<-matrix(0,p,p) 
      MI <- matrix(0,p+1+length(D1[upper.tri(D1, diag = T)]),
                   p+1+length(D1[upper.tri(D1, diag = T)])) 
      ub1<-ubi
      res <- vector(mode = "numeric", length = N)
      ubi=matrix(0,m2,m)     
      ubbi=matrix(0,m2,m2)
      uybi=matrix(0,N,m2)
      uyyi=matrix(0,N,N)
      uyi=matrix(0,N,m)
      yhi=matrix(0,N,1)
      xi=matrix(0,N,m1)
      zi=matrix(0,N,m2)
      
      ver<-matrix(0,m,1)
      
      
      for (j in 1:m ){
        
        
        cc1=cc[(sum(nj[1:j-1])+1) : (sum(nj[1:j]))]
        tt1=tt[(sum(nj[1:j-1])+1) : (sum(nj[1:j]))]
        y1=y[(sum(nj[1:j-1])+1) : (sum(nj[1:j]))]
        x1=matrix(x[(sum(nj[1:j-1])+1) : (sum(nj[1:j])),  ],ncol=p)
        z1=matrix(z[(sum(nj[1:j-1])+1) : (sum(nj[1:j])) ,  ],ncol=q1)
        LI1<- LI[(sum(nj[1:j-1])+1) : (sum(nj[1:j]))]
        LS1<- LS[(sum(nj[1:j-1])+1) : (sum(nj[1:j]))]
        gammai=x1%*%beta1
        
        if(sum(cc1)==0){
          
          Psi<-(sigmae*diag(nj[j])+(z1)%*%D1%*%t(z1))
          Psi<-(Psi+t(Psi))/2
          
          delta<- solve(iD1+(t(z1)%*%((z1*(1/sigmae)))))
          uy<- matrix(y1,nj[j],1)
          uyy<- y1%*%t(y1)
          ub<- delta%*%(t(z1)*(1/sigmae))%*%(uy-gammai)
          ubb<- delta+(delta%*%(t(z1)*((1/sigmae)^2))%*%(uyy-uy%*%t(gammai)-gammai%*%t(uy)+gammai%*%t(gammai))%*%z1%*%delta)
          uyb<- (uyy-uy%*%t(gammai))%*%(z1*(1/sigmae))%*%delta
          ver[j,]<- LaplacesDemon::dmvn(x=as.vector(y1),mu=as.vector(gammai),Sigma=Psi)
          
        }
        
        if(sum(cc1)>=1){
          
          Psi<-(sigmae*diag(nj[j])+(z1)%*%D1%*%t(z1))
          Psi<-(Psi+t(Psi))/2
          
          if(sum(cc1)==nj[j]){
            muc=x1%*%beta1
            Sc<-Psi
            Sc=(Sc+t(Sc))/2
            aux<- relliptical::mvtelliptical(lower = as.vector(LI1),upper=as.vector(LS1),mu = as.vector(muc), Sigma =Sc)
            uy<- aux$EY
            uyy<- aux$EYY
            ub<- delta%*%(t(z1)*(1/sigmae))%*%solve(eGama)%*%(uy-gammai)
            ubb<- delta+(delta%*%(t(z1)*((1/sigmae)^2))%*%solve(eGama)%*%(uyy-uy%*%t(gammai)-gammai%*%t(uy)+gammai%*%t(gammai))%*%solve(eGama)%*%z1%*%delta)
            uyb<- (uyy-uy%*%t(gammai))%*%solve(eGama)%*%(z1*(1/sigmae))%*%delta
            
            ver[j,]<-  TruncatedNormal::pmvnorm(lb=as.vector(LI1),ub= as.vector(LS1), mu=c(muc),sigma=Sc)
            
          }
          
          else {
            muc=x1[cc1==1,]%*%beta1+Psi[cc1==1,cc1==0]%*%solve(Psi[cc1==0,cc1==0])%*%(y1[cc1==0]-x1[cc1==0,]%*%beta1)
            Sc <-Psi[cc1==1,cc1==1]-Psi[cc1==1,cc1==0]%*%solve(Psi[cc1==0,cc1==0])%*%Psi[cc1==0,cc1==1]
            Sc=(Sc+t(Sc))/2
            aux<- relliptical::mvtelliptical(lower = as.vector(LI1[cc1==1]),upper=as.vector(LS1[cc1==1]),mu = as.vector(muc), Sigma =Sc)
            uy <-matrix(y1,nj[j],1)
            uy[cc1==1]<- aux$EY
            uyy<- uy%*%t(uy)
            uyy[cc1==1,cc1==1]<- aux$EYY
            ub<- delta%*%(t(z1)*(1/sigmae))%*%solve(eGama)%*%(uy-gammai)
            ubb<- delta+(delta%*%(t(z1)*((1/sigmae)^2))%*%solve(eGama)%*%(uyy-uy%*%t(gammai)-gammai%*%t(uy)+gammai%*%t(gammai))%*%solve(eGama)%*%z1%*%delta)
            uyb<- (uyy-uy%*%t(gammai))%*%solve(eGama)%*%(z1*(1/sigmae))%*%delta
            ver[j,]<- LaplacesDemon::dmvn(x=as.vector(y1[cc1==0]),mu=as.vector(gammai[cc1==0]),Sigma=as.matrix(Psi[cc1==0,cc1==0]))*( TruncatedNormal::pmvnorm(lb=as.vector(LI1[cc1==1]),up=as.vector(LS1[cc1==1]),mu=as.vector(muc),sigma=Sc))[1]
            uyy<- matrix(0,nj[j],nj[j])
          }
          
        }
        
        soma1<- soma1 + ubb
        soma2<- soma2 + (sum(diag(uyy))-t(uy)%*%gammai-t(gammai)%*%uy-sum(diag(t(uyb)%*%z1))-sum(diag(uyb%*%t(z1)))
                         +t(gammai)%*%z1%*%ub+t(ub)%*%t(z1)%*%gammai+t(gammai)%*%gammai+sum(diag(ubb%*%t(z1)%*%z1)))
        soma3<- soma3 + (t(x1)%*%x1)
        soma4<- soma4 + (t(x1)%*%(uy-z1%*%ub))
        soma5<- soma5 + (t(x1)%*%solve(Psi)%*%x1-t(x1)%*%solve(Psi)%*%(uyy-uy%*%t(uy))%*%solve(Psi)%*%x1)
        
        
        uyyi[(sum(nj[1:j-1])+1) : (sum(nj[1:j])), (sum(nj[1:j-1])+1) : (sum(nj[1:j]))]<-uyy
        uyi[(sum(nj[1:j-1])+1) : (sum(nj[1:j])), j]<-uy
        uybi[(sum(nj[1:j-1])+1) : (sum(nj[1:j])), (((j-1)*q1)+1) : (j*q1)]<-uyb
        ubi[(((j-1)*q1)+1) : (j*q1), j]<-ub
        ubbi[(((j-1)*q1)+1) : (j*q1), (((j-1)*q1)+1) : (j*q1)]<-ubb
        yhi[(sum(nj[1:j-1])+1) : (sum(nj[1:j])),] <- uy
        zi[(sum(nj[1:j-1])+1) : (sum(nj[1:j])), (((j-1)*q1)+1) : (j*q1)]<-z1
        xi[(sum(nj[1:j-1])+1) : (sum(nj[1:j])), (((j-1)*p)+1) : (j*p)]<-x1
        
        dbeta <- (1/sigmae)*((t(x1)%*%solve(eGama)%*%(uy-z1%*%ub)) - (t(x1)%*%solve(eGama)%*%x1)%*%beta1)
        dsigma <- -(1/2)*((nj[j]/sigmae)-(1/sigmae^2)*((sum(diag(uyy%*%solve(eGama)))-t(uy)%*%solve(eGama)%*%gammai-t(gammai)%*%solve(eGama)%*%uy-sum(diag(solve(eGama)%*%((uyb)%*%t(z1))))-sum(diag(solve(eGama)%*%((uyb)%*%t(z1))))
                                                        +t(gammai)%*%solve(eGama)%*%z1%*%ub+t(ub)%*%t(z1)%*%solve(eGama)%*%gammai+t(gammai)%*%solve(eGama)%*%gammai+sum(diag(ubb%*%t(z1)%*%solve(eGama)%*%z1)))))
        
        D_der <- Derivadas(D1)
        deralpha<-rep(0,length(D1[upper.tri(D1, diag = T)]))
        md2<-dim(D1)[1]  
        kont <- 0
        for(i1 in 1:md2){
          for(i2 in 1:(md2+1-i1)){
            kont <- kont+1
            di <- D_der[[i1]][[i2]]
            deralpha[kont] <- (-0.5)*sum(diag(iD1%*%di-iD1%*%di%*%iD1*ubb))     
          }
        }
        si <- matrix(c(t(dbeta),t(dsigma),t(deralpha),p+1+length(D1[upper.tri(D1, diag = T)]),1))
        MI <- MI + si%*%t(si)
      }
      yorg<-apply(yhi,1,sum)
      beta1<- solve(soma3)%*%soma4
      sigmae<- (1/(N))*as.numeric(soma2)
      D1<- (1/(m))*(soma1)
      iD1<-solve(D1)
      
      teta1 <- c(beta1,sigmae,D1[upper.tri(D1, diag = T)])
      teta1crit <- c(beta1,sigmae)
      varbeta<-solve(soma5)
      logver <- sum(log(ver))
      
      loglik <- logliknslmec(y=y,x=x,z=z,cc=cc,ttc=tt,nj=nj,LL=LI,LU=LS,betas=beta1,sigmae=sigmae,D1=D1,phi1=rho,phi2=gamma,struc=struc)
      
      loglikp1 <- loglik     
      
      if(count > 1){
        criterio <- sqrt(((loglikp1/loglikp)-1)%*%((loglikp1/loglikp)-1))
        setTkProgressBar(pb, count, label=paste("Iter ",count,"/",MaxIter,"     -     ",floor((count)/(MaxIter)*100),"% done",sep = ""))
      }    
      if(count==MaxIter){criterio <- 0.0000000000001}
      
      teta <- teta1
      loglikp <- loglikp1
      
      tetacrit <- teta1crit
      logver1<-logver
      
      gamma <- 0
      rho <- 0    
      
    }		
  } 
  
  
  dd<-D1[upper.tri(D1, diag = T)]
  
  npar<-length(c(teta1))
  
  ni<-sum(nj)
  
  loglik<-logver1
  
  AICc<- -2*loglik +2*npar
  AICcorr<- AICc + ((2*npar*(npar+1))/(ni-npar-1))
  BICc <- -2*loglik +log(ni)*npar
  MI<-round((MI+t(MI))/2,6)
  SE=round(sqrt(diag(ginv(MI))),3)
  intPar=round(1.96*SE,3)
  
  tableB  = data.frame(round(beta1,3),SE[1:p],paste("<",round(beta1,3)-round(intPar[1:p],3),",",round(beta1,3)+round(intPar[1:p],3),">"))
  rownames(tableB) = paste("beta",1:p)
  colnames(tableB) = c("Est","SE","IConf(95%)")
  
  
  if((round(sigmae,3)-round(intPar[p+1],3))<0) tableS  = data.frame(round(sigmae,3),SE[p+1],paste("<",0,",",round(sigmae,3)+round(intPar[p+1],3),">"))
  if((round(sigmae,3)-round(intPar[p+1],3))>=0) tableS  = data.frame(round(sigmae,3),SE[p+1],paste("<",round(sigmae,3)-round(intPar[p+1],3),",",round(sigmae,3)+round(intPar[p+1],3),">"))
  rownames(tableS) = "Sigma^2"
  colnames(tableS) = c("Est","SE","IConf(95%)")
  
  if(struc=="DEC"){
    phi=c(gamma,rho)  
    tableP  = data.frame(round(phi,3),SE[(p+2):(p+1+2)],paste("<",round(phi,3)-round(intPar[(p+2):(p+1+2)],3),",",round(phi,3)+round(intPar[(p+2):(p+1+2)],3),">"))
    rownames(tableP) = paste("Phi",1:2)
    colnames(tableP) = c("Est","SE","IConf(95%)")
  }
  if(struc=="DEC(AR)"){
    phi=rho  
    tableP  = data.frame(round(phi,3),SE[(p+2):(p+1+1)],paste("<",round(phi,3)-round(intPar[(p+2):(p+1+1)],3),",",round(phi,3)+round(intPar[(p+2):(p+1+1)],3),">"))
    rownames(tableP) = paste("Phi",1)
    colnames(tableP) = c("Est","SE","IConf(95%)")
  }
  if(struc=="SYM"){
    phi=rho  
    tableP  = data.frame(round(phi,3),SE[(p+2):(p+1+1)],paste("<",round(phi,3)-round(intPar[(p+2):(p+1+1)],3),",",round(phi,3)+round(intPar[(p+2):(p+1+1)],3),">"))
    rownames(tableP) = paste("Phi",1)
    colnames(tableP) = c("Est","SE","IConf(95%)")
  }
  if(struc=="UNC"){ phi=NULL; tableP =NULL }
  
  nnp=0
  for(al in 1:dim(D1)[1]) 
  {noa=paste(1:al,al,sep = "")
  nnp=c(nnp,noa)
  }
  nnp=nnp[-1]
  ici=round(dd,3)-round(intPar[(p+2+length(phi)):(p+1+length(D1[upper.tri(D1, diag = T)])+length(phi))],3)
  ics=round(dd,3)+round(intPar[(p+2+length(phi)):(p+1+length(D1[upper.tri(D1, diag = T)])+length(phi))],3)
  ici[as.numeric(nnp)%%11==0&ici<0]=0
  tableA  = data.frame(round(dd,3),SE[(p+2+length(phi)):(p+1+length(D1[upper.tri(D1, diag = T)])+length(phi))],paste("<",ici,",",ics,">"))
  rownames(tableA) = paste("Alpha",nnp)
  colnames(tableA) = c("Est","SE","IConf(95%)")
  
  
  
  
  res <-fitY<- vector(mode = "numeric", length = sum(nj))
  Di <- matrix(0,dim(z)[2],dim(z)[2]) 
  Di[upper.tri(Di, diag = T)] <- dd
  Di[lower.tri(Di, diag = T)] <- dd
  Di      <- round(Di,6)
  for (k in 1:length(nj)) 
  {
    tc<-tt[(sum(nj[1:k-1])+1) : (sum(nj[1:k]))]
    if(struc=="DEC") Mq<- MatDec(tc,phi[1],phi[2],"DEC")
    if(struc=="DEC(AR)") Mq<- MatDec(tc,phi[1],1,"DEC(AR)")
    if(struc=="SYM") Mq<- MatDec(tc,phi[1],0,"SYM")
    if(struc=="UNC") Mq<- MatDec(tc,phi1=NULL,phi2=NULL,"UNC")
    Sii<-ginv(round(z[(sum(nj[1:k-1])+1):(sum(nj[1:k])),]%*%Di%*%t(z[(sum(nj[1:k-1])+1):(sum(nj[1:k])),])+sigmae*Mq,6))
    yyii<-yorg[(sum(nj[1:k-1])+1) :(sum(nj[1:k]))]-x[(sum(nj[1:k-1])+1) : (sum(nj[1:k])),]%*%beta1
    res[(sum(nj[1:k-1])+1) : (sum(nj[1:k]))]=sqrtm(Sii)%*%(yyii)
    fitY[(sum(nj[1:k-1])+1) : (sum(nj[1:k]))]= x[(sum(nj[1:k-1])+1) : (sum(nj[1:k])),]%*%beta1+
      z[(sum(nj[1:k-1])+1):(sum(nj[1:k])),]%*%as.matrix(ubi[(((k-1)*q1)+1) : (k*q1),k])
  }                                                                                                                               
  

  end.time <- Sys.time()
  time.taken <- end.time - start.time
  
  obj.out <- list(beta1 = beta1, sigmae= sigmae, phi=phi, dd = dd, loglik=loglik,
                  AIC=AICc, BIC=BICc, AICcorr=AICcorr, iter = count, varbeta=varbeta,
                  ubi = ubi, ubbi = ubbi, uybi = uybi, uyi = uyi, uyyi = uyyi , MI=MI, 
                  yorg =yorg,residuals=res,yfit=fitY,
                  time=time.taken, SE=SE,tableB=tableB,tableS=tableS,tableP=tableP,
                  tableA=tableA)
  
  
  if  (count == MaxIter)
  {
    setTkProgressBar(pb, MaxIter, label=paste("MaxIter reached ",count,"/",MaxIter,"    -    100 % done",sep = ""))
    close(pb)
  }
  else
  {
    setTkProgressBar(pb, MaxIter, label=paste("Convergence at Iter ",count,"/",MaxIter,"    -    100 % done",sep = ""))
    close(pb)
  }
  
  
  class(obj.out) <- "DECNLMEC"
  
  return(obj.out)
  
}


