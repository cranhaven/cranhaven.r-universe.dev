complk_covmanifest.cont <- function(Y,R,yv,piv,Pi,Mu,Si,k,fort=TRUE){

# ---- Preliminaries ----
  if(!fort){
    dnorm1 <-function(y,mu,si2) f = exp(-(y-mu)^2/si2/2)/sqrt(2*pi*si2)
    dmvnorm1 <-function(y,mu,Si) f = exp(-c((y-mu)%*%solve(Si)%*%(y-mu))/2)/sqrt(det(2*pi*Si))
  }
  sY = dim(Y)
  ns = as.integer(sY[1])
  TT = as.integer(sY[2])
  n = sum(yv)
  if(length(sY)==2) r = 1 else r = sY[3]
  r = as.integer(r)
  if(r==1){
    if(is.matrix(Y)) Y = array(Y,c(dim(Y),1))
    if(is.matrix(R)) R = array(R,c(dim(R),1))
  }
  if(is.null(R)) miss = FALSE else miss = any(!R)

# ---- Compute conditional probabilities ----
  # t0 = proc.time()
  Phi = array(1,c(ns,k,TT)); L = array(0,c(ns,k,TT))
  if(miss){
    if(fort){
      RR = array(as.integer(1*R),c(ns,TT,r))
      out = .Fortran("normmiss2",Y,RR,ns,TT,r,k,Mu,Si,Phi=Phi)
      Phi = out$Phi
    }else{
      j = 0
      for(i in 1:ns) for(t in 1:TT){
        j = j+1
        if(all(!R[i,t,])){
          Phi[i,,t] = 1
        }else{
          indo = R[i,t,]
          if(sum(R[i,t,])==1) for(u in 1:k) Phi[i,u,t] = pmax(dnorm1(Y[i,t,indo],Mu[j,indo,u],Si[indo,indo]),0.1^300)
          else for(u in 1:k) Phi[i,u,t] = pmax(dmvnorm1(Y[i,t,indo],Mu[j,indo,u],Si[indo,indo]),0.1^300)
        }
      }
    }
  }else{
    j = 0
    for(i in 1:ns) for(t in 1:TT){
      j = j+1
      if(r==1){
        for(u in 1:k) Phi[i,u,t] = pmax(dnorm(Y[i,t,],Mu[j,,u],sqrt(Si)),0.1^300)
      }else{
        for(u in 1:k) Phi[i,u,t] =  pmax(dmvnorm(Y[i,t,],Mu[j,,u],Si),0.1^300)
      }
    }
  }
  # print(c(2,1,proc.time()-t0))

# ---- forward recursion ----
  L[,,1] = Phi[,,1]%*%diag(piv)
  if(n==1) Lt = sum(L[,,1])
  else Lt = rowSums(L[,,1])
  lk = sum(yv*log(Lt))
  L[,,1] = L[,,1]/Lt
  # print(c(2,2,proc.time()-t0))
  for(t in 2:TT){
    L[,,t] = Phi[,,t]*(L[,,t-1]%*%Pi[,,t])
    if(n==1) Lt = sum(L[1,,t])
    else Lt = rowSums(L[,,t])
    lk = lk+sum(yv*log(Lt))
    L[,,t] = L[,,t]/Lt
  }
  # print(c(2,3,proc.time()-t0))
  if(n==1) pv = sum(L[1,,TT])
  else pv = rowSums(L[,,TT])

  #lk = sum(log(pv))
  out = list(lk=lk,Phi=Phi,L=L,pv=pv)

}
