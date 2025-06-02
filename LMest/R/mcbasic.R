mcbasic <- function(S,yv,modBasic=0,tol=10^-8,maxit=1000,out_se=FALSE){

  # Preliminaries
  check_der = FALSE  # to check derivatives
  n = sum(yv)
  sS = dim(S)
  ns = sS[1]
  TT = sS[2]
  mod = modBasic
  b = max(S)
  U = Pi  = array(0,c(b+1,b+1,TT))

  # piv = table(factor(rep(S[,1],yv),levels = min(S):max(S)))/n ## Modifica Alessio
  piv = rep(0,b+1)
  for(y in 0:b) piv[y+1] = sum(yv[S[,1]==y])/n

  if(out_se) sepiv = sqrt(piv*(1-piv)/n)

  for(t in 2:TT) for(j1 in 0:b) for(j2 in 0:b) U[j1+1,j2+1,t] = sum(yv[S[,t-1]==j1 & S[,t]==j2])
  U = pmax(U,10^-300)

  if(out_se) sePi = array(0,c(b+1,b+1,TT))
  if(mod==0){
    for(t in 2:TT) Pi[,,t] = diag(1/rowSums(U[,,t]))%*%U[,,t]
    if(out_se) for(t in 2:TT) sePi[,,t] = sqrt((Pi[,,t]*(1-Pi[,,t]))/rowSums(U[,,t]))

  }
  if(mod==1){
    Ut = apply(U[,,2:TT],c(1,2),sum)
    Pi[,,2:TT] = array(diag(1/rowSums(Ut))%*%Ut,c(b+1,b+1,TT-1))
    if(out_se) sePi[,,2:TT] = sqrt((Pi[,,2]*(1-Pi[,,2]))/rowSums(Ut))

  }

  if(mod>1){
    Ut1 = U[,,2:mod]
    if(length(dim(Ut1))>2) Ut1 = apply(Ut1,c(1,2),sum)
    Ut2 = U[,,(mod+1):TT]
    if(length(dim(Ut2))>2) Ut2 = apply(Ut2,c(1,2),sum)
    Pi[,,2:mod] = array(diag(1/rowSums(Ut1,2))%*%Ut1,c(b+1,b+1,mod-1))
    Pi[,,(mod+1):TT] = array(diag(1/rowSums(Ut2,2))%*%Ut2,c(b+1,b+1,TT-mod))
    if(out_se){
      sePi[,,2:mod] =  sqrt((Pi[,,2]*(1-Pi[,,2]))/rowSums(Ut1))
      sePi[,,(mod+1):TT] = sqrt((Pi[,,(mod+1)]*(1-Pi[,,(mod+1)]))/rowSums(Ut2))
    }
  }

  # Compute log-likelihood
  lk = sum(yv*log(piv[S[,1]+1]))+sum(U[,,2:TT]*log(Pi[,,2:TT]))
  Phi = array(1,c(ns,b+1,TT))
  Phi[,,1] = Phi[,,1]%*%diag(piv)
  for(t in 2:TT) Phi[,,t] = Phi[,,t]*(Phi[,,t-1]%*%Pi[,,t])

  Fy = t(apply(yv*Phi,c(2,3),sum)/n)


  # Compute number of parameters
  np = b
  if(mod==0) np = np+(TT-1)*(b+1)*b
  if(mod==1) np = np+(b+1)*b
  if(mod>1) np = np+2*(b+1)*b

  aic = -2*lk+np*2
  bic = -2*lk+np*log(n)
  dimnames(Pi)=list(category=0:b,category=0:b,time=1:TT)
  dimnames(Fy) = list(time=1:TT,category=0:b)
  out = list(lk=lk,piv=piv,Pi=Pi,np=np,aic=aic,bic=bic,Fy=Fy,n=n,TT=TT,modBasic=mod,
             ns=ns,yv=yv)
  if(out_se){
    dimnames(sePi) = list(category=0:b,category=0:b,time=1:TT)
    out$sepiv = sepiv
    out$sePi = sePi
  }
  class(out)="MCbasic"
  return(out)
}
