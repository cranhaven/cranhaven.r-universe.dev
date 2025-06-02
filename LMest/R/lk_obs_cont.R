lk_obs_cont <- function(th,yv,Bm,Cm,k,Y,TT,r,mod){

# compute corresponding parameters
  ns = dim(Y)[1]
  n = sum(yv)
  th1 = th[1:(k*r)]; th = th[-(1:(k*r))]
  Mu = matrix(th1,r,k)

  th1 = th[1:(r*(r+1)/2)]; th = th[-(1:(r*(r+1)/2))]
  Si = matrix(0,r,r)
  Si[upper.tri(Si,TRUE)]=th1
  if(r>1) Si = Si+t(Si-diag(diag(Si)))

  if(k==1){
    piv = 1; Pi = array(1,c(1,1,TT))
  }else{
    th1 = th[1:(k-1)]; th = th[-(1:(k-1))]
    piv = exp(Bm%*%th1); piv = as.vector(piv/sum(piv))
    if(mod==0){
      Pi = array(0,c(k,k,TT))
      for(t in 2:TT) for(u in 1:k){
        th1 = th[1:(k-1)]; th = th[-(1:(k-1))]
        if(k==2) Pi[u,,t] = exp(Cm[,,u]*th1)
        else Pi[u,,t] = exp(Cm[,,u]%*%th1)
        Pi[u,,t] = Pi[u,,t]/sum(Pi[u,,t])
      }
    }
    if(mod==1){
      Pi = matrix(0,k,k)
      for(u in 1:k){
        th1 = th[1:(k-1)]; th = th[-(1:(k-1))]
        if(k==2) Pi[u,] = exp(Cm[,,u]*th1)
        else Pi[u,] = exp(Cm[,,u]%*%th1)
        Pi[u,] = Pi[u,]/sum(Pi[u,])
      }
      Pi = array(Pi,c(k,k,TT))
    }
    if(mod>1){
      Pi = array(0,c(k,k,TT))
      for(u in 1:k){
        th1 = th[1:(k-1)]; th = th[-(1:(k-1))]
        if(k==2) Pi[u,,2:mod] = exp(Cm[,,u]*th1)
        else Pi[u,,2:mod] = exp(Cm[,,u]%*%th1)
        Pi[u,,2:mod] = Pi[u,,2]/sum(Pi[u,,2])
      }
      for(u in 1:k){
        th1 = th[1:(k-1)]; th = th[-(1:(k-1))]
        if(k==2) Pi[u,,(mod+1):TT] = exp(Cm[,,u]*th1)
        else Pi[u,,(mod+1):TT] = exp(Cm[,,u]%*%th1)
        Pi[u,,(mod+1):TT] = Pi[u,,mod+1]/sum(Pi[u,,mod+1])
      }
    }
    Pi[,,1] = 0
  }

# compute log-likelihood
  out = complk_cont(Y,yv,piv,Pi,Mu,Si,k)
  lk = out$lk; Phi = out$Phi; L = out$L; pv = out$pv
  sc = NULL

# ---- E-step ----
# Compute V and U
  if(k==1){
    V = array(yv,c(ns,1,TT))
  }else{
    V = array(0,c(ns,k,TT)); U = array(0,c(k,k,TT))
    Yvp = matrix(yv/pv,ns,k)
    M = matrix(1,ns,k);
    V[,,TT] = Yvp*L[,,TT]
    U[,,TT] = (t(L[,,TT-1])%*%(Yvp*Phi[,,TT]))*Pi[,,TT]
    for(t in seq(TT-1,2,-1)){
      M = (Phi[,,t+1]*M)%*%t(Pi[,,t+1]);
      V[,,t] = Yvp*L[,,t]*M
      U[,,t] = (t(L[,,t-1])%*%(Yvp*Phi[,,t]*M))*Pi[,,t]
    }
    M = (Phi[,,2]*M)%*%t(Pi[,,2])
    V[,,1] = Yvp*L[,,1]*M
  }

# ---- M-step ----
# Update Mu
   # print(Mu)
  iSi = solve(Si)
  Yv = matrix(Y,ns*TT,r)
  Vv = matrix(aperm(V,c(1,3,2)),ns*TT,k)
  for(u in 1:k) sc = c(sc,iSi%*%(t(Yv)%*%Vv[,u]-sum(Vv[,u])*Mu[,u]))

# Update Si
  tmp=0
  for(u in 1:k){
    Tmp = Yv-rep(1,ns*TT)%*%t(Mu[,u])
    tmp = tmp+t(Tmp)%*%(Vv[,u]*Tmp)
  }
  tmp = iSi%*%tmp%*%iSi

  tmp = tmp-(n*TT)*iSi
  diag(tmp) = diag(tmp)/2
  sc = c(sc,tmp[upper.tri(tmp,TRUE)])

# Update piv and Pi
  if(k>1){
    sc = c(sc,t(Bm)%*%(colSums(V[,,1])-n*piv))
    if(mod==0) {
      for(t in 2:TT) for(u in 1:k) sc = c(sc,t(Cm[,,u])%*%(U[u,,t]-sum(U[u,,t])*Pi[u,,t]))
    }
    if(mod==1){
      Ut = apply(U[,,2:TT],c(1,2),sum)
      for(u in 1:k) sc = c(sc,t(Cm[,,u])%*%(Ut[u,]-sum(Ut[u,])*Pi[u,,2]))
    }
    if(mod>1){
      Ut1 = U[,,2:mod]
      if(length(dim(Ut1))>2) Ut1 = apply(Ut1,c(1,2),sum)
      Ut2 = U[,,(mod+1):TT]
      if(length(dim(Ut2))>2) Ut2 = apply(Ut2,c(1,2),sum)
      for(u in 1:k) sc = c(sc,t(Cm[,,u])%*%(Ut1[u,]-sum(Ut1[u,])*Pi[u,,2]))
      for(u in 1:k) sc = c(sc,t(Cm[,,u])%*%(Ut2[u,]-sum(Ut2[u,])*Pi[u,,mod+1]))
    }
  }

# return
  out = list(lk=lk,sc=sc)

}
