
estphit = function(pit){
  p = length(pit)
  Phi = matrix(0,ncol=p,nrow=p)
  if (p>1) {
    diag(Phi) = pit
    for (j in 2:p) {
      for (k in 1:(j-1)) {
        Phi[j,k] = Phi[j-1,k] - pit[j]*Phi[j-1,j-k]
      }
    }
    return(Phi[p,])
  }
  else return(pit)
}
MatArp<- function(pii,tt,sigma2){
  if(min(tt)==0){tt=tt+1}else{tt=tt}
  A <- .Mat(pii =pii,n=max(tt),sigma2=sigma2)
  B <- matrix(NA,nrow=length(tt),ncol=length(tt))
  ii <- 0
  for(i in tt)
  {
    ii <- ii+1
    jj <- 0
    for(j in tt)
    {
      jj <- jj+1  
      B[ii,jj]<-A[i,j]   
    }
  }
  return(B)
}
.Mat<-function(pii,n,sigma2){
  p = length(pii)
  phi=estphit(pii) 
  if (n==1) Rn = 1
  else Rn = toeplitz(ARMAacf(ar=phi, ma=0, lag.max = n-1))
  rhos = ARMAacf(ar=phi, ma=0, lag.max = p)[(1:p)+1]
  Rnx<-sigma2*Rn/(1-sum(rhos*phi))
  Rnx<-(Rnx+t(Rnx))/2
  return(Rnx)
}
MatArpJ<- function(phi,tt,sigma2){
  if(min(tt)==0){tt=tt+1}else{tt=tt}
  A <-.MatJ(phi =phi,n=max(tt),sigma2=sigma2)
  B <- matrix(NA,nrow=length(tt),ncol=length(tt))
  ii <- 0
  for(i in tt)
  {
    ii <- ii+1
    jj <- 0
    for(j in tt)
    {
      jj <- jj+1  
      B[ii,jj]<-A[i,j]   
    }
  }
  return(B)
}
.MatJ<-function(phi,n,sigma2){
  p = length(phi)
  if (n==1) Rn = 1
  else Rn = toeplitz(ARMAacf(ar=phi, ma=0, lag.max = n-1))
  rhos = ARMAacf(ar=phi, ma=0, lag.max = p)[(1:p)+1]
  Rnx<-sigma2*Rn/(1-sum(rhos*phi))
  Rnx<-(Rnx+t(Rnx))/2
  return(Rnx)
}
logliktArplmec <- function(nu,y,x,z,cc,ttc,nj,LL,LU,betas,sigmae,D1,pii){
  p <- dim(x)[2]
  
  m <- length(nj)[1]
  q1 <- dim(z)[2]
  gamma1 <- as.vector(c(betas))
  iD1 <- solve(D1)
  iD1 <- (iD1 + t(iD1))/2
  
  ver <- matrix(0,m,1)
  
  for(j in 1:m)
  {
    cc1 <- cc[(sum(nj[1:j-1])+1) : (sum(nj[1:j]))]
    y1 <- y[(sum(nj[1:j-1])+1) : (sum(nj[1:j]))]
    x1 <- matrix(x[(sum(nj[1:j-1])+1) : (sum(nj[1:j])),  ],ncol=p)
    z1 <- matrix(z[(sum(nj[1:j-1])+1) : (sum(nj[1:j])) ,  ],ncol=q1)
    tt1 <- ttc[(sum(nj[1:j-1])+1) : (sum(nj[1:j]))]
    W1 <- x1
    
    LL1 <- LL[(sum(nj[1:j-1])+1) : (sum(nj[1:j]))]
    LU1 <- LU[(sum(nj[1:j-1])+1) : (sum(nj[1:j]))]
    
    muii <- W1%*%gamma1
    if(length(pii)>1 )
    { eGamma<-MatArp(pii,tt1,sigmae)
    Gama <- eGamma/sigmae}
    
    if(length(pii)==1 )
    { if(pii!=0 ){
      eGamma<-MatArp(pii,tt1,sigmae)
      Gama <- eGamma/sigmae}
      if(pii==0)
      { Gama=diag(1,nj[j])
      eGamma=Gama*sigmae}
    }
    invGama <- solve(Gama)
    SIGMA <- (sigmae*Gama + (z1)%*%D1%*%t(z1)) 
    SIGMA <-(SIGMA+t(SIGMA))/2
    SIGMAinv <- solve(SIGMA)
    Lambda1 <- solve(iD1 + (t(z1)%*%invGama%*%z1)*(1/sigmae))
    Lambda1 <- (Lambda1 + t(Lambda1))/2
    
    if(sum(cc1)==0)
    {  
  
      
      ver[j,] <- suppressWarnings(LaplacesDemon::dmvt(x = as.vector(y1), mu = as.vector(muii), S = as.matrix(SIGMA), df = nu ))
      
    }
    if(sum(cc1)>=1)
    {
      
      if(sum(cc1)==nj[j])
      {
         ver[j,] <- suppressWarnings(TruncatedNormal::pmvt(lb = as.vector(LL1),ub=as.vector(LU1), mu = as.vector(muii),df= nu, sigma = as.matrix(SIGMA)))
        
     }
      else{
        
        muiic <-  W1[cc1==1,]%*%gamma1 + SIGMA[cc1==1,cc1==0]%*%solve(SIGMA[cc1==0,cc1==0])%*%(y1[cc1==0]-W1[cc1==0,]%*%gamma1)
        Si <- SIGMA[cc1==1,cc1==1]-SIGMA[cc1==1,cc1==0]%*%solve(SIGMA[cc1==0,cc1==0])%*%SIGMA[cc1==0,cc1==1]
        Si <- (Si+t(Si))/2
        
        Qy0 <- as.numeric(t(y1[cc1==0]-W1[cc1==0,]%*%gamma1)%*%solve(SIGMA[cc1==0,cc1==0])%*%(y1[cc1==0]-W1[cc1==0,]%*%gamma1))
        
        auxQy0 <- as.numeric((nu + Qy0)/(nu + length(cc1[cc1==0])))
        
        Sc0 <- auxQy0*Si
        
        LL1c <- LL1[cc1==1]
        LU1c <- LU1[cc1==1]
        
            
        ver[j,] <- suppressWarnings(LaplacesDemon::dmvt(x = as.vector(y1[cc1==0]),mu =as.vector(muii[cc1==0]),S =as.matrix(SIGMA[cc1==0,cc1==0]),df= nu)*as.numeric(TruncatedNormal::pmvt(lb = as.vector(LL1c),ub=as.vector(LU1c), mu = as.vector(muiic),df=nu, sigma = as.matrix(Sc0))))
       
      }
      
    } 
  } 
  
  logvero <- sum(log(ver))
  
  return(logvero)
}
FCiArpt <- function(pii,beta1,sigmae,ttc,ubi,ubbi,uybi,uyyi,uyi,ui,x,z,nj){
  
  m <- length(nj)[1]
  p <- dim(x)[2]
  q1 <- dim(z)[2]
  beta1 <- as.vector(c(beta1))
  soma <- 0
  
  for (j in 1:m ){
    
    x1 <- matrix(x[(sum(nj[1:j-1])+1) : (sum(nj[1:j])),  ],ncol=p)
    z1 <- matrix(z[(sum(nj[1:j-1])+1) : (sum(nj[1:j])) ,  ],ncol=q1)
    muii <- x1%*%beta1
    tt1 <- ttc[(sum(nj[1:j-1])+1) : (sum(nj[1:j]))]
    
    ub <- matrix(ubi[(((j-1)*q1)+1) : (j*q1), j], nrow=q1, ncol=1)
    ubb <- as.matrix(ubbi[(((j-1)*q1)+1) : (j*q1), (((j-1)*q1)+1) : (j*q1)])
    uyb <- matrix(uybi[(sum(nj[1:j-1])+1) : (sum(nj[1:j])),(((j-1)*q1)+1) : (j*q1)], ncol=q1)
    uyy <- as.matrix(uyyi[(sum(nj[1:j-1])+1) : (sum(nj[1:j])),(sum(nj[1:j-1])+1) : (sum(nj[1:j]))])
    uy <- matrix(uyi[(sum(nj[1:j-1])+1) : (sum(nj[1:j])),j],ncol=1)
    u <- ui[j]
    
    Cii <- MatArp(pii,tt1,sigmae)/sigmae 
    Cii <- (Cii + t(Cii))/2
    if(det(Cii)<=0){A <- 1}else{A <- det(Cii)}
    invCii <- solve(Cii)
    
    Ai <-  as.vector(sum(diag(uyy%*%invCii)) - sum(diag(invCii%*%((uyb)%*%t(z1)))) - sum(diag(invCii%*%(z1%*%t(uyb)))) + sum(diag(ubb%*%t(z1)%*%invCii%*%z1))
                     - t(uy)%*%invCii%*%muii - t(muii)%*%invCii%*%uy  + t(muii)%*%invCii%*%z1%*%ub + t(ub)%*%t(z1)%*%invCii%*%muii + u*t(muii)%*%invCii%*%muii)
    
    soma <- soma - 0.5*log(A) - (0.5/sigmae)*Ai                
  }
  
  return(-soma)
}
FCiArp<-function(piis,beta1,sigmae, ubi,ubbi,uybi,uyyi,uyi,x,z,tt,nj){
  m<-length(nj)[1]
  N<-sum(nj)
  p<-dim(x)[2]
  q1<-dim(z)[2]
  m1<-m*p
  m2<-m*q1
  
  soma=0
  for (j in 1:m ){
    
    ub<-ubi[(((j-1)*q1)+1) : (j*q1), j]
    ubb<-ubbi[(((j-1)*q1)+1) : (j*q1), (((j-1)*q1)+1) : (j*q1)]
    uyb<-uybi[(sum(nj[1:j-1])+1) : (sum(nj[1:j])),(((j-1)*q1)+1) : (j*q1)]
    uyy<-uyyi[(sum(nj[1:j-1])+1) : (sum(nj[1:j])),(sum(nj[1:j-1])+1) : (sum(nj[1:j]))]
    uy<-uyi[(sum(nj[1:j-1])+1) : (sum(nj[1:j])),j]
    x1=matrix(x[(sum(nj[1:j-1])+1) : (sum(nj[1:j])),  ],ncol=p)
    z1=matrix(z[(sum(nj[1:j-1])+1) : (sum(nj[1:j])) ,  ],ncol=q1)
    tt1=tt[(sum(nj[1:j-1])+1) : (sum(nj[1:j]))]
    gammai=x1%*%beta1                                                
    Cii<- MatArp(piis,tt1,sigmae)/sigmae
    Cii <- (Cii + t(Cii))/2
    if(det(Cii)<=0){A <- 1}else{A <- det(Cii)}
    invCii <- solve(Cii)
    

    
      Ai= (sum(diag(uyy%*%solve(Cii)))-
             t(uy)%*%solve(Cii)%*%gammai-
             t(gammai)%*%solve(Cii)%*%uy-
             sum(diag(solve(Cii)%*%((uyb)%*%t(z1))))-
             sum(diag(solve(Cii)%*%((uyb)%*%t(z1))))+
             t(gammai)%*%solve(Cii)%*%z1%*%ub+
             t(ub)%*%t(z1)%*%solve(Cii)%*%gammai+
             t(gammai)%*%solve(Cii)%*%gammai+
             sum(diag(ubb%*%t(z1)%*%solve(Cii)%*%z1)))
 
      soma <- soma - 0.5*log(A) - (0.5/sigmae)*Ai  
           
    
    
     }
  
  return(-soma)
}
Dbeta = function(beta,y,x,z,b,p) {
  n = length(y)
  D = matrix(0,p+1,p+1)
  for (ii in 1:(p+1)) {
    for (jj in 1:(p+1)) {
      D[ii,jj] = sum((y-x%*%beta-z%*%b)[ii:(n+1-jj)]*(y-x%*%beta-z%*%b)[jj:(n+1-ii)])
      D[is.na(D)]<-0
    }
  }
  return(D)
}
D11   = function(beta,y,x,z,b,p) matrix(Dbeta(beta,y,x,z,b,p)[1,1])
Dphi = function(beta,y,x,z,b,p) matrix(Dbeta(beta,y,x,z,b,p)[2:(p+1),1])
Dphiphi = function(beta,y,x,z,b,p) Dbeta(beta,y,x,z,b,p)[2:(p+1),2:(p+1)]
dD<-function(M){
  
  m1<-dim(M)[1]
  m2<-dim(M)[2]  
  d<-list()
  for(i in 1:m1){
    d[[i]]<-list()
    for(j in 1:(m2+1-i)){
      d[[i]][[j]]<-matrix(0,m1,m2)
      if(j==1){d[[i]][[j]][i,i]<-1}   
      else{
        d[[i]][[j]][i,i+(j-1)]<-d[[i]][[j]][i+(j-1),i]<-1}
    }
  }
  
  return(d=d)
  
}
Jt = function(theta,y,x,z,tt,b,bb,p,Arp,D1) {
  l      = p
  n      = length(y)
  beta   = matrix(theta[1:l])
  sig2   = theta[l+1]
  
  if(Arp==0){
    Mn     = diag(1,n)*sig2
    invMn  = solve(Mn/sig2)
    spi= (t(y-x%*%beta-z%*%b)%*%invMn%*%(y-x%*%beta-z%*%b))
  }
  if(Arp!=0){  phi    = theta[(l+2):length(theta)]
  p      = length(phi)
  Mn     = MatArpJ(phi,tt,sig2)
  invMn  = solve(Mn/sig2)
  lambda = matrix(c(-1,phi))
  spi    = t(lambda)%*%Dbeta(beta,y,x,z,b,p)%*%lambda}
  dbeta  = 1/sig2*(t(x)%*%invMn%*%(y-z%*%b)- t(x)%*%invMn%*%x%*%beta)
  
  dsig2  = -n/(2*sig2) +spi/(2*sig2^2)
  if(length(D1)==1){
    dD_alp = 1
    dalpha<-rep(0,1)
    md2<-1
  }
  if(length(D1)>1){
    dD_alp = dD(D1) 
    dalpha<-rep(0,length(D1[upper.tri(D1, diag = T)]))
    md2<-dim(D1)[1]  }
  kont <- 0
  for(i1 in 1:md2){
    for(i2 in 1:(md2+1-i1)){
      kont <- kont+1
      di <- dD_alp[[i1]][[i2]]
      dalpha[kont] <- (-0.5)*sum(diag(solve(D1)%*%di-solve(D1)%*%di%*%solve(D1)*bb))     
    }
  }
  derivadas=cbind(t(dbeta),t(dsig2),t(dalpha))
  if(Arp!=0){
    gp = function(phi,sigma=sig2)ifelse(length(phi)==1,log(MatArpJ(phi,tt,sigma))
                                        ,log(det(MatArpJ(phi,tt,sigma))))
    dgp    = matrix(jacobian(gp,phi))
    dphi   = -1/sig2*(-Dphi(beta,y,x,z,b,p) + Dphiphi(beta,y,x,z,b,p)%*%phi)-1/2*dgp
    derivadas=cbind(t(dbeta),t(dsig2),t(dphi),t(dalpha))
  }
  return(derivadas)
}
MatDec <- function(tt,phi1,phi2,struc){
  
  r <- length(tt)
  
  if(struc=="DEC" || struc=="DEC(AR)"){
    if(phi2<=0.0000001){
      W <- matrix(phi1,nrow=r,ncol=r)
      for (i in 1:r){W[i,i]<- 1}
      V <- W 
    }
    else{
      H <- (abs(outer(tt, tt, "-")))^phi2
      V <- (phi1^H)
    }
  } 
  if(struc=="SYM"){
    W <- matrix(phi1,nrow=r,ncol=r)
    diag(W)<-1
    V <- W
    
  }
  if(struc=="MA"){
    W <- matrix(0,nrow=r,ncol=r)
    for (i in 1:r){
      W[i,i]<- 1
      for(j in 1:r){ 
        dif <- abs(tt[i]-tt[j])
        if(dif==1){W[i,j]= phi1}}}
    V <- W
    
  }
  if(struc=="UNC"){
    W <- diag(1,nrow=r,ncol=r)
    V <- W
    
  }
  return(V)
}
FCit <- function(phiG,beta1,sigmae,ttc,ubi,ubbi,uybi,uyyi,uyi,ui,x,z,nj,struc){
  
  phi1 <- phiG[1]
  phi2 <- phiG[2]
  m <- length(nj)[1]
  p <- dim(x)[2]
  q1 <- dim(z)[2]
  beta1 <- as.vector(c(beta1))
  soma <- 0
  
  for (j in 1:m ){
    
    x1 <- matrix(x[(sum(nj[1:j-1])+1) : (sum(nj[1:j])),  ],ncol=p)
    z1 <- matrix(z[(sum(nj[1:j-1])+1) : (sum(nj[1:j])) ,  ],ncol=q1)
    muii <- x1%*%beta1
    tt1 <- ttc[(sum(nj[1:j-1])+1) : (sum(nj[1:j]))]
    
    ub <- matrix(ubi[(((j-1)*q1)+1) : (j*q1), j], nrow=q1, ncol=1)
    ubb <- as.matrix(ubbi[(((j-1)*q1)+1) : (j*q1), (((j-1)*q1)+1) : (j*q1)])
    uyb <- matrix(uybi[(sum(nj[1:j-1])+1) : (sum(nj[1:j])),(((j-1)*q1)+1) : (j*q1)], ncol=q1)
    uyy <- as.matrix(uyyi[(sum(nj[1:j-1])+1) : (sum(nj[1:j])),(sum(nj[1:j-1])+1) : (sum(nj[1:j]))])
    uy <- matrix(uyi[(sum(nj[1:j-1])+1) : (sum(nj[1:j])),j],ncol=1)
    u <- ui[j]
    
    Cii <- MatDec(tt1,phi1,phi2,struc) 
    Cii <- (Cii + t(Cii))/2
    if(det(Cii)<=0){A <- 1}else{A <- det(Cii)}
    invCii <- solve(Cii)
    
    Ai <-  as.vector(sum(diag(uyy%*%invCii)) - sum(diag(invCii%*%((uyb)%*%t(z1)))) - sum(diag(invCii%*%(z1%*%t(uyb)))) + sum(diag(ubb%*%t(z1)%*%invCii%*%z1))
                     - t(uy)%*%invCii%*%muii - t(muii)%*%invCii%*%uy  + t(muii)%*%invCii%*%z1%*%ub + t(ub)%*%t(z1)%*%invCii%*%muii + u*t(muii)%*%invCii%*%muii)
    
    soma <- soma - 0.5*log(A) - (0.5/sigmae)*Ai                
  }
  
  return(-soma)
}
FCiphi1t <- function(phi1,phi2,beta1,sigmae,ttc,ubi,ubbi,uybi,uyyi,uyi,ui,x,z,nj,struc){
  
  m <- length(nj)[1]
  p <- dim(x)[2]
  q1 <- dim(z)[2]
  
  beta1 <- as.vector(c(beta1))
  soma <- 0
  
  for (j in 1:m ){
    
    x1 <- matrix(x[(sum(nj[1:j-1])+1) : (sum(nj[1:j])),  ],ncol=p)
    z1 <- matrix(z[(sum(nj[1:j-1])+1) : (sum(nj[1:j])) ,  ],ncol=q1)
    muii <- x1%*%beta1
    tt1 <- ttc[(sum(nj[1:j-1])+1) : (sum(nj[1:j]))]
    
    ub <- matrix(ubi[(((j-1)*q1)+1) : (j*q1), j], nrow=q1, ncol=1)
    ubb <- as.matrix(ubbi[(((j-1)*q1)+1) : (j*q1), (((j-1)*q1)+1) : (j*q1)])
    uyb <- matrix(uybi[(sum(nj[1:j-1])+1) : (sum(nj[1:j])),(((j-1)*q1)+1) : (j*q1)], ncol=q1)
    uyy <- as.matrix(uyyi[(sum(nj[1:j-1])+1) : (sum(nj[1:j])),(sum(nj[1:j-1])+1) : (sum(nj[1:j]))])
    uy <- matrix(uyi[(sum(nj[1:j-1])+1) : (sum(nj[1:j])),j],ncol=1)
    u <- ui[j]
    
    Cii <- MatDec(tt1,phi1,phi2,struc) 
    Cii <- (Cii + t(Cii))/2
    if(det(Cii)<=0){A <- 1}else{A <- det(Cii)}
    invCii <- solve(Cii)
    
    Ai <-  as.vector(sum(diag(uyy%*%invCii)) - sum(diag(invCii%*%((uyb)%*%t(z1)))) - sum(diag(invCii%*%(z1%*%t(uyb)))) + sum(diag(ubb%*%t(z1)%*%invCii%*%z1))
                     - t(uy)%*%invCii%*%muii - t(muii)%*%invCii%*%uy  + t(muii)%*%invCii%*%z1%*%ub + t(ub)%*%t(z1)%*%invCii%*%muii + u*t(muii)%*%invCii%*%muii)
    
    soma <- soma - 0.5*log(A) - (0.5/sigmae)*Ai               
    
  }
  
  return(-soma)
}
logliktslmec <-  function(nu,y,x,z,cc,ttc,nj,LL,LU,betas,sigmae,D1,phi1,phi2,struc){
  p <- dim(x)[2]
  
  m <- length(nj)[1]
  q1 <- dim(z)[2]
  gamma1 <- as.vector(c(betas))
  iD1 <- solve(D1)
  iD1 <- (iD1 + t(iD1))/2
  
  ver <- matrix(0,m,1)
  
  for(j in 1:m)
  { 
    cc1 <- cc[(sum(nj[1:j-1])+1) : (sum(nj[1:j]))]
    y1 <- y[(sum(nj[1:j-1])+1) : (sum(nj[1:j]))]
    x1 <- matrix(x[(sum(nj[1:j-1])+1) : (sum(nj[1:j])),  ],ncol=p)
    z1 <- matrix(z[(sum(nj[1:j-1])+1) : (sum(nj[1:j])) ,  ],ncol=q1)
    tt1 <- ttc[(sum(nj[1:j-1])+1) : (sum(nj[1:j]))]
    W1 <- x1
    
    LL1 <- LL[(sum(nj[1:j-1])+1) : (sum(nj[1:j]))]
    LU1 <- LU[(sum(nj[1:j-1])+1) : (sum(nj[1:j]))]
    
    muii <- W1%*%gamma1
    Gama <- MatDec(tt1,phi1,phi2,struc)
    invGama <- solve(Gama)
    SIGMA <- (sigmae*Gama + (z1)%*%D1%*%t(z1)) 
    SIGMA <-(SIGMA+t(SIGMA))/2
    SIGMAinv <- solve(SIGMA)
    Lambda1 <- solve(iD1 + (t(z1)%*%invGama%*%z1)*(1/sigmae))
    Lambda1 <- (Lambda1 + t(Lambda1))/2
    
    if(sum(cc1)==0)
    {
    
     
      ver[j,] <- suppressWarnings(LaplacesDemon::dmvt(x = as.vector(y1),mu = as.vector(muii), S = as.matrix(SIGMA), df = nu ))
      
          }
    if(sum(cc1)>=1)
    {
      
      if(sum(cc1)==nj[j])
      {
        
        ver[j,] <- suppressWarnings(TruncatedNormal::pmvt(lb = as.vector(LL1),ub=as.vector(LU1), mu= as.vector(muii),df=nu,sigma = as.matrix(SIGMA) ))
   }
      else{
        
        muiic <-  W1[cc1==1,]%*%gamma1 + SIGMA[cc1==1,cc1==0]%*%solve(SIGMA[cc1==0,cc1==0])%*%(y1[cc1==0]-W1[cc1==0,]%*%gamma1)
        Si <- SIGMA[cc1==1,cc1==1]-SIGMA[cc1==1,cc1==0]%*%solve(SIGMA[cc1==0,cc1==0])%*%SIGMA[cc1==0,cc1==1]
        Si <- (Si+t(Si))/2
        
        Qy0 <- as.numeric(t(y1[cc1==0]-W1[cc1==0,]%*%gamma1)%*%solve(SIGMA[cc1==0,cc1==0])%*%(y1[cc1==0]-W1[cc1==0,]%*%gamma1))
        
        auxQy0 <- as.numeric((nu + Qy0)/(nu + length(cc1[cc1==0])))
        
        Sc0 <- auxQy0*Si
        
        LL1c <- LL1[cc1==1]
        LU1c <- LU1[cc1==1]

         
        ver[j,] <- suppressWarnings(LaplacesDemon::dmvt(x = as.vector(y1[cc1==0]),mu =as.vector(muii[cc1==0]),S =as.matrix(SIGMA[cc1==0,cc1==0]),df = nu)*as.numeric(TruncatedNormal::pmvt(lb = as.vector(LL1c),ub=as.vector(LU1c), mu = as.vector(muiic),df=nu,sigma = as.matrix(Sc0))))
        
     
      }
      
    } 
  } 
  
  logvero <- sum(log(ver))
  
  return(logvero)
}
MatAr1<-function(tt,rho,gamma,sigma2){
  H <- (abs(outer(tt, tt, "-")))^gamma
  diag(H)<-0
  V <- sigma2*(rho^H)
  return(V)
}
DevEiAr1<-function(tt,rho,gamma,sigma2){
  if(gamma<=0.0000001)
  {
    r <- length(tt)
    devR_r <- matrix(1,nrow=r,ncol=r)
    diag(devR_r) <- 0
    devR_g <- matrix(0,nrow=r,ncol=r)
  }
  else
  {
    func1 <- function(x,y){(abs(x-y))^gamma*rho^((abs(x-y))^gamma-1)}
    H1 <- (outer(tt, tt, func1))
    diag(H1) <- 0
    func2 <- function(x,y){(abs(x-y))^gamma*log(abs(x-y))*log(rho)*rho^((abs(x-y))^gamma)}
    H2 <- (outer(tt, tt, func2))
    diag(H2) <- 0
    devR_r <- H1
    devR_g <- H2
  }
  
  obj.out <- list(devR_r = devR_r, devR_g = devR_g)
  return(obj.out)
}
Derivadas<-function(M){
  
  m1<-dim(M)[1]
  m2<-dim(M)[2]  
  d<-list()
  for(i in 1:m1){
    d[[i]]<-list()
    for(j in 1:(m2+1-i)){
      d[[i]][[j]]<-matrix(0,m1,m2)
      if(j==1){d[[i]][[j]][i,i]<-1}   
      else{
        d[[i]][[j]][i,i+(j-1)]<-d[[i]][[j]][i+(j-1),i]<-1}
    }
  }
  
  return(d=d)
  
}
FCi<-function(rhoG,beta1,sigmae,tt,ubi,ubbi,uybi,uyyi,uyi,xi,zi,nj){
  
  rho<-rhoG[1]
  gamma<-rhoG[2]
  
  m<-length(nj)
  N<-sum(nj)
  p<-length(beta1) 
  q1<-dim(ubi)[1]/dim(ubi)[2]
  m1<-m*p
  m2<-m*q1
  
  soma=0
  
  for (j in 1:m ){
    
    ub<-ubi[(((j-1)*q1)+1) : (j*q1), j]
    ubb<-ubbi[(((j-1)*q1)+1) : (j*q1), (((j-1)*q1)+1) : (j*q1)]
    uyb<-uybi[(sum(nj[1:j-1])+1) : (sum(nj[1:j])),(((j-1)*q1)+1) : (j*q1)]
    uyy<-uyyi[(sum(nj[1:j-1])+1) : (sum(nj[1:j])),(sum(nj[1:j-1])+1) : (sum(nj[1:j]))]
    uy<-uyi[(sum(nj[1:j-1])+1) : (sum(nj[1:j])),j]
    
    z1=zi[(sum(nj[1:j-1])+1) : (sum(nj[1:j])), (((j-1)*q1)+1) : (j*q1)]
    x1=xi[(sum(nj[1:j-1])+1) : (sum(nj[1:j])), (((j-1)*p)+1) : (j*p)]
    
    tt1=tt[(sum(nj[1:j-1])+1) : (sum(nj[1:j]))]
    gammai=x1%*%beta1
    
    Cii<-MatAr1(tt1,rho,gamma,sigmae)
    
    if(nj[j]>1){
      soma<- soma - 0.5*log(det(Cii))-0.5*(sum(diag(uyy%*%solve(Cii)))-t(uy)%*%solve(Cii)%*%gammai-t(gammai)%*%solve(Cii)%*%uy-sum(diag(solve(Cii)%*%((uyb)%*%t(z1))))-sum(diag(solve(Cii)%*%(z1%*%t(uyb))))
                                           +t(gammai)%*%solve(Cii)%*%z1%*%ub+t(ub)%*%t(z1)%*%solve(Cii)%*%gammai+t(gammai)%*%solve(Cii)%*%gammai+sum(diag(ubb%*%t(z1)%*%solve(Cii)%*%z1)))
    }
    if(nj[j]==1){
      soma<- soma - 0.5*log(det(Cii))-0.5*(sum(diag(uyy%*%solve(Cii)))-t(uy)%*%solve(Cii)%*%gammai-t(gammai)%*%solve(Cii)%*%uy-sum(diag(solve(Cii)%*%((uyb)%*%z1)))-sum(diag(solve(Cii)%*%(t(z1)%*%(uyb))))
                                           +t(gammai)%*%solve(Cii)%*%z1%*%ub+t(ub)%*%z1%*%solve(Cii)%*%gammai+t(gammai)%*%solve(Cii)%*%gammai+sum(diag(ubb%*%z1%*%solve(Cii)%*%z1)))
    }
    
  }
  
  return(-soma)
}
FCi_gamma<-function(rhoG,gamma,beta1,sigmae,tt,ubi,ubbi,uybi,uyyi,uyi,xi,zi,nj){
  
  rho<-rhoG
  
  m<-length(nj)
  N<-sum(nj)
  p<-length(beta1) 
  q1<-dim(ubi)[1]/dim(ubi)[2]
  m1<-m*p
  m2<-m*q1
  
  soma=0
  
  for (j in 1:m ){
    
    ub<-ubi[(((j-1)*q1)+1) : (j*q1), j]
    ubb<-ubbi[(((j-1)*q1)+1) : (j*q1), (((j-1)*q1)+1) : (j*q1)]
    uyb<-uybi[(sum(nj[1:j-1])+1) : (sum(nj[1:j])),(((j-1)*q1)+1) : (j*q1)]
    uyy<-uyyi[(sum(nj[1:j-1])+1) : (sum(nj[1:j])),(sum(nj[1:j-1])+1) : (sum(nj[1:j]))]
    uy<-uyi[(sum(nj[1:j-1])+1) : (sum(nj[1:j])),j]
    
    z1=zi[(sum(nj[1:j-1])+1) : (sum(nj[1:j])), (((j-1)*q1)+1) : (j*q1)]
    x1=xi[(sum(nj[1:j-1])+1) : (sum(nj[1:j])), (((j-1)*p)+1) : (j*p)]
    
    tt1=tt[(sum(nj[1:j-1])+1) : (sum(nj[1:j]))]
    gammai=x1%*%beta1                                                
    Cii<-MatAr1(tt1,rho,gamma,sigmae)
    
    soma<- soma - 0.5*log(det(Cii))-0.5*(sum(diag(uyy%*%solve(Cii)))-t(uy)%*%solve(Cii)%*%gammai-t(gammai)%*%solve(Cii)%*%uy-sum(diag(solve(Cii)%*%((uyb)%*%t(z1))))-sum(diag(solve(Cii)%*%(z1%*%t(uyb))))
                                         +t(gammai)%*%solve(Cii)%*%z1%*%ub+t(ub)%*%t(z1)%*%solve(Cii)%*%gammai+t(gammai)%*%solve(Cii)%*%gammai+sum(diag(ubb%*%t(z1)%*%solve(Cii)%*%z1)))
    
  }
  
  return(-soma)
}
logliknArplmec <- function(y,x,z,cc,ttc,nj,LL,LU,betas,sigmae,D1,pii){
  
  p <- dim(x)[2]
  
  m <- length(nj)[1]
  q1 <- dim(z)[2]
  gamma1 <- as.vector(c(betas))
  iD1 <- solve(D1)
  iD1 <- (iD1 + t(iD1))/2
  
  ver <- matrix(0,m,1)
  
  for(j in 1:m)
  {
    cc1 <- cc[(sum(nj[1:j-1])+1) : (sum(nj[1:j]))]
    y1 <- y[(sum(nj[1:j-1])+1) : (sum(nj[1:j]))]
    x1 <- matrix(x[(sum(nj[1:j-1])+1) : (sum(nj[1:j])),  ],ncol=p)
    z1 <- matrix(z[(sum(nj[1:j-1])+1) : (sum(nj[1:j])) ,  ],ncol=q1)
    tt1 <- ttc[(sum(nj[1:j-1])+1) : (sum(nj[1:j]))]
    W1 <- x1
    
    LL1 <- LL[(sum(nj[1:j-1])+1) : (sum(nj[1:j]))]
    LU1 <- LU[(sum(nj[1:j-1])+1) : (sum(nj[1:j]))]
    
    muii <- W1%*%gamma1
    if(length(pii)>1 )
    { eGamma<-MatArp(pii,tt1,sigmae)
    Gama <- eGamma/sigmae}
    
    if(length(pii)==1 )
    { if(pii!=0 ){
      eGamma<-MatArp(pii,tt1,sigmae)
      Gama <- eGamma/sigmae}
      if(pii==0)
      { Gama=diag(1,nj[j])
      eGamma=Gama*sigmae}
    }
    invGama <- solve(Gama)
    SIGMA <- (sigmae*Gama + (z1)%*%D1%*%t(z1)) 
    SIGMA <-(SIGMA+t(SIGMA))/2
    SIGMAinv <- solve(SIGMA)
    Lambda1 <- solve(iD1 + (t(z1)%*%invGama%*%z1)*(1/sigmae))
    Lambda1 <- (Lambda1 + t(Lambda1))/2
    
    if(sum(cc1)==0)
    {
      ver[j,] <- suppressWarnings(LaplacesDemon::dmvn(x = as.vector(y1),mu = as.vector(muii),Sigma = as.matrix(SIGMA)))
      
      
      }
    if(sum(cc1)>=1)
    {
      
      if(sum(cc1)==nj[j])
      {
    
        
        
       ver[j,] <- suppressWarnings(TruncatedNormal::pmvnorm(lb = as.vector(LL1),ub=as.vector(LU1), 
                                                    mu = as.vector(muii),sigma = as.matrix(SIGMA)))
      }
      else{
        muiic <-  W1[cc1==1,]%*%gamma1 + SIGMA[cc1==1,cc1==0]%*%solve(SIGMA[cc1==0,cc1==0])%*%(y1[cc1==0]-W1[cc1==0,]%*%gamma1)
        Si <- SIGMA[cc1==1,cc1==1]-SIGMA[cc1==1,cc1==0]%*%solve(SIGMA[cc1==0,cc1==0])%*%SIGMA[cc1==0,cc1==1]
        Si <- (Si+t(Si))/2
        
        
        Sc0 <- Si
        
        LL1c <- LL1[cc1==1]
        LU1c <- LU1[cc1==1]
     
        ver[j,] <- suppressWarnings(LaplacesDemon::dmvn(x = as.vector(y1[cc1==0]),mu =as.vector(muii[cc1==0]),
                                                    Sigma =as.matrix(SIGMA[cc1==0,cc1==0]))*
                                      as.numeric(TruncatedNormal::pmvnorm(lb= as.vector(LL1c),ub=as.vector(LU1c), mu = as.vector(muiic),
                                                                  sigma = as.matrix(Sc0))))
        
                                                                                                                     
  
      }
      
    } 
  } 
  
  logvero <- sum(log(ver))
  
  return(logvero)
}
logliknslmec <-  function(y,x,z,cc,ttc,nj,LL,LU,betas,sigmae,D1,phi1,phi2,struc){
  
  p <- dim(x)[2]
  
  m <- length(nj)[1]
  q1 <- dim(z)[2]
  gamma1 <- as.vector(c(betas))
  iD1 <- solve(D1)
  iD1 <- (iD1 + t(iD1))/2
  
  ver <- matrix(0,m,1)
  
  for(j in 1:m)
  { 
    cc1 <- cc[(sum(nj[1:j-1])+1) : (sum(nj[1:j]))]
    y1 <- y[(sum(nj[1:j-1])+1) : (sum(nj[1:j]))]
    x1 <- matrix(x[(sum(nj[1:j-1])+1) : (sum(nj[1:j])),  ],ncol=p)
    z1 <- matrix(z[(sum(nj[1:j-1])+1) : (sum(nj[1:j])) ,  ],ncol=q1)
    tt1 <- ttc[(sum(nj[1:j-1])+1) : (sum(nj[1:j]))]
    W1 <- x1
    
    LL1 <- LL[(sum(nj[1:j-1])+1) : (sum(nj[1:j]))]
    LU1 <- LU[(sum(nj[1:j-1])+1) : (sum(nj[1:j]))]
    
    muii <- W1%*%gamma1
    Gama <- MatDec(tt1,phi1,phi2,struc)
    invGama <- solve(Gama)
    SIGMA <- (sigmae*Gama + (z1)%*%D1%*%t(z1)) 
    SIGMA <-(SIGMA+t(SIGMA))/2
    SIGMAinv <- solve(SIGMA)
    Lambda1 <- solve(iD1 + (t(z1)%*%invGama%*%z1)*(1/sigmae))
    Lambda1 <- (Lambda1 + t(Lambda1))/2
    
    if(sum(cc1)==0)
    {
     ver[j,] <- suppressWarnings(LaplacesDemon::dmvn(x = as.vector(y1),mu = as.vector(muii),Sigma = as.matrix(SIGMA)))
      
    }
    if(sum(cc1)>=1)
    {
      
      if(sum(cc1)==nj[j])
      {
      
        ver[j,] <- suppressWarnings(TruncatedNormal::pmvnorm(lb = as.vector(LL1),ub=as.vector(LU1), mu = as.vector(muii),sigma = as.matrix(SIGMA)))
        
       }
      else{
        
        muiic <-  W1[cc1==1,]%*%gamma1 + SIGMA[cc1==1,cc1==0]%*%solve(SIGMA[cc1==0,cc1==0])%*%(y1[cc1==0]-W1[cc1==0,]%*%gamma1)
        Si <- SIGMA[cc1==1,cc1==1]-SIGMA[cc1==1,cc1==0]%*%solve(SIGMA[cc1==0,cc1==0])%*%SIGMA[cc1==0,cc1==1]
        Si <- (Si+t(Si))/2
        
   
        
        Sc0 <-Si
        
        LL1c <- LL1[cc1==1]
        LU1c <- LU1[cc1==1]
        
        
        ver[j,] <- suppressWarnings(LaplacesDemon::dmvn(x = as.vector(y1[cc1==0]),mu =as.vector(muii[cc1==0]),Sigma =as.matrix(SIGMA[cc1==0,cc1==0]))*
                                      as.numeric(TruncatedNormal::pmvnorm(lb = as.vector(LL1c),ub=as.vector(LU1c), mu = as.vector(muiic),sigma = as.matrix(Sc0))))
        
        
      }
      
    } 
  } 
  
  logvero <- sum(log(ver))
  
  return(logvero)
}
logliktArplmec_o <- function(nu,y,x,z,cc,ttc,nj,LL,LU,betas,sigmae,D1,pii){
  
  p <- dim(x)[2]
  
  m <- length(nj)[1]
  q1 <- dim(z)[2]
  gamma1 <- as.vector(c(betas))
  iD1 <- solve(D1)
  iD1 <- (iD1 + t(iD1))/2
  
  ver <- matrix(0,m,1)
  
  for(j in 1:m)
  {
    cc1 <- cc[(sum(nj[1:j-1])+1) : (sum(nj[1:j]))]
    y1 <- y[(sum(nj[1:j-1])+1) : (sum(nj[1:j]))]
    x1 <- matrix(x[(sum(nj[1:j-1])+1) : (sum(nj[1:j])),  ],ncol=p)
    z1 <- matrix(z[(sum(nj[1:j-1])+1) : (sum(nj[1:j])) ,  ],ncol=q1)
    tt1 <- ttc[(sum(nj[1:j-1])+1) : (sum(nj[1:j]))]
    W1 <- x1
    
    LL1 <- LL[(sum(nj[1:j-1])+1) : (sum(nj[1:j]))]
    LU1 <- LU[(sum(nj[1:j-1])+1) : (sum(nj[1:j]))]
    
    muii <- W1%*%gamma1
    if(length(pii)>1 )
    { eGamma<-MatArp(pii,tt1,sigmae)
    Gama <- eGamma/sigmae}
    
    if(length(pii)==1 )
    { if(pii!=0 ){
      eGamma<-MatArp(pii,tt1,sigmae)
      Gama <- eGamma/sigmae}
      if(pii==0)
      { Gama=diag(1,nj[j])
      eGamma=Gama*sigmae}
    }
    invGama <- solve(Gama)
    SIGMA <- (sigmae*Gama + (z1)%*%D1%*%t(z1)) 
    SIGMA <-(SIGMA+t(SIGMA))/2
    SIGMAinv <- solve(SIGMA)
    Lambda1 <- solve(iD1 + (t(z1)%*%invGama%*%z1)*(1/sigmae))
    Lambda1 <- (Lambda1 + t(Lambda1))/2
    
    if(sum(cc1)==0)
    {  
      
      
      ver[j,] <- suppressWarnings(LaplacesDemon::dmvt(x = as.vector(y1), mu = as.vector(muii), S = as.matrix(SIGMA), df = nu ))
      
    }
    if(sum(cc1)>=1)
    {
      
      if(sum(cc1)==nj[j])
      {
        ver[j,] <- suppressWarnings(TruncatedNormal::pmvt(lb = as.vector(LL1),ub=as.vector(LU1), mu = as.vector(muii),df= nu, sigma = as.matrix(SIGMA)))
        
      }
      else{
        
        muiic <-  W1[cc1==1,]%*%gamma1 + SIGMA[cc1==1,cc1==0]%*%solve(SIGMA[cc1==0,cc1==0])%*%(y1[cc1==0]-W1[cc1==0,]%*%gamma1)
        Si <- SIGMA[cc1==1,cc1==1]-SIGMA[cc1==1,cc1==0]%*%solve(SIGMA[cc1==0,cc1==0])%*%SIGMA[cc1==0,cc1==1]
        Si <- (Si+t(Si))/2
        
        Qy0 <- as.numeric(t(y1[cc1==0]-W1[cc1==0,]%*%gamma1)%*%solve(SIGMA[cc1==0,cc1==0])%*%(y1[cc1==0]-W1[cc1==0,]%*%gamma1))
        
        auxQy0 <- as.numeric((nu + Qy0)/(nu + length(cc1[cc1==0])))
        
        Sc0 <- auxQy0*Si
        
        LL1c <- LL1[cc1==1]
        LU1c <- LU1[cc1==1]
        
        
        ver[j,] <- suppressWarnings(LaplacesDemon::dmvt(x = as.vector(y1[cc1==0]),mu =as.vector(muii[cc1==0]),S =as.matrix(SIGMA[cc1==0,cc1==0]),df= nu)*as.numeric(TruncatedNormal::pmvt(lb = as.vector(LL1c),ub=as.vector(LU1c), mu = as.vector(muiic),df=nu, sigma = as.matrix(Sc0))))
        
      }
      
    } 
  } 
  
  logvero <- sum(-log(ver))
  
  return(logvero)
}
logliktslmec_o <-  function(nu,y,x,z,cc,ttc,nj,LL,LU,betas,sigmae,D1,phi1,phi2,struc){
  p <- dim(x)[2]
  
  m <- length(nj)[1]
  q1 <- dim(z)[2]
  gamma1 <- as.vector(c(betas))
  iD1 <- solve(D1)
  iD1 <- (iD1 + t(iD1))/2
  
  ver <- matrix(0,m,1)
  
  for(j in 1:m)
  { 
    cc1 <- cc[(sum(nj[1:j-1])+1) : (sum(nj[1:j]))]
    y1 <- y[(sum(nj[1:j-1])+1) : (sum(nj[1:j]))]
    x1 <- matrix(x[(sum(nj[1:j-1])+1) : (sum(nj[1:j])),  ],ncol=p)
    z1 <- matrix(z[(sum(nj[1:j-1])+1) : (sum(nj[1:j])) ,  ],ncol=q1)
    tt1 <- ttc[(sum(nj[1:j-1])+1) : (sum(nj[1:j]))]
    W1 <- x1
    
    LL1 <- LL[(sum(nj[1:j-1])+1) : (sum(nj[1:j]))]
    LU1 <- LU[(sum(nj[1:j-1])+1) : (sum(nj[1:j]))]
    
    muii <- W1%*%gamma1
    Gama <- MatDec(tt1,phi1,phi2,struc)
    invGama <- solve(Gama)
    SIGMA <- (sigmae*Gama + (z1)%*%D1%*%t(z1)) 
    SIGMA <-(SIGMA+t(SIGMA))/2
    SIGMAinv <- solve(SIGMA)
    Lambda1 <- solve(iD1 + (t(z1)%*%invGama%*%z1)*(1/sigmae))
    Lambda1 <- (Lambda1 + t(Lambda1))/2
    
    if(sum(cc1)==0)
    {
      
      
      ver[j,] <- suppressWarnings(LaplacesDemon::dmvt(x = as.vector(y1),mu = as.vector(muii), S = as.matrix(SIGMA), df = nu ))
      
    }
    if(sum(cc1)>=1)
    {
      
      if(sum(cc1)==nj[j])
      {
        
        ver[j,] <- suppressWarnings(TruncatedNormal::pmvt(lb = as.vector(LL1),ub=as.vector(LU1), mu= as.vector(muii),df=nu,sigma = as.matrix(SIGMA) ))
      }
      else{
        
        muiic <-  W1[cc1==1,]%*%gamma1 + SIGMA[cc1==1,cc1==0]%*%solve(SIGMA[cc1==0,cc1==0])%*%(y1[cc1==0]-W1[cc1==0,]%*%gamma1)
        Si <- SIGMA[cc1==1,cc1==1]-SIGMA[cc1==1,cc1==0]%*%solve(SIGMA[cc1==0,cc1==0])%*%SIGMA[cc1==0,cc1==1]
        Si <- (Si+t(Si))/2
        
        Qy0 <- as.numeric(t(y1[cc1==0]-W1[cc1==0,]%*%gamma1)%*%solve(SIGMA[cc1==0,cc1==0])%*%(y1[cc1==0]-W1[cc1==0,]%*%gamma1))
        
        auxQy0 <- as.numeric((nu + Qy0)/(nu + length(cc1[cc1==0])))
        
        Sc0 <- auxQy0*Si
        
        LL1c <- LL1[cc1==1]
        LU1c <- LU1[cc1==1]
        
        
        ver[j,] <- suppressWarnings(LaplacesDemon::dmvt(x = as.vector(y1[cc1==0]),mu =as.vector(muii[cc1==0]),S =as.matrix(SIGMA[cc1==0,cc1==0]),df = nu)*as.numeric(TruncatedNormal::pmvt(lb = as.vector(LL1c),ub=as.vector(LU1c), mu = as.vector(muiic),df=nu,sigma = as.matrix(Sc0))))
        
        
      }
      
    } 
  } 
  
  logvero <- sum(-log(ver))
  
  return(logvero)
}
