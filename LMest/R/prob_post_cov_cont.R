prob_post_cov_cont <- function(Y, yv, Mu, Si, Piv, PI, Phi, L, pv, der=FALSE,
                               dlPhi=NULL, dlPiv=NULL, dlPI=NULL, dlL=NULL, dlL2=NULL, dlpv=NULL){

# preliminaries
  sY = dim(Y)
  ns = sY[1]
  TT = sY[2]
  n = sum(yv)
  if(length(sY)==2) r = 1	else r = sY[3]
  k = ncol(Piv)

# use backward recursion for posterior probabilities	
  V = V1 = array(0,c(ns,k,TT)); U = array(0,c(k,k,ns,TT))
# new
  # Pv = matrix(1/pv,ns,k)
  # V1[,,TT] = L[,,TT]*Pv
  V1[,,TT] = L[,,TT]
# end new
  
  V[,,TT] = yv*V1[,,TT]

# new
  # AA = Pv*Phi[,,TT]
  # for(i in 1:ns) U[,,i,TT] = outer(L[i,,TT-1],AA[i,])*yv[i]
  AA = Phi[,,TT]
  for(i in 1:ns){
    Tmp = PI[,,i,TT]*outer(L[i,,TT-1],AA[i,])
    U[,,i,TT] = yv[i]*Tmp/sum(Tmp)
  }
  # M = matrix(1,ns,k)
  M = matrix(1/k,ns,k)
# end new

# backward
  if(TT>2) for(t in seq(TT-1,2,-1)){
    MP = Phi[,,t+1]*M
    for(i in 1:ns) M[i,] = PI[,,i,t+1]%*%MP[i,]
    M = M/rowSums(M)

# new
    # AA = Pv*Phi[,,t]*M
    AA = Phi[,,t]*M
    # for(i in 1:ns) U[,,i,t] = outer(L[i,,t-1],AA[i,])*yv[i]
    for(i in 1:ns){
      Tmp = PI[,,i,t]*outer(L[i,,t-1],AA[i,])
      U[,,i,t] = yv[i]*Tmp/sum(Tmp)
    }
    # V1[,,t] = L[,,t]*M*Pv
    V1[,,t] = L[,,t]*M
    V1[,,t] = V1[,,t]/rowSums(V1[,,t])
# end new
    V[,,t] = yv*V1[,,t]
  }
# new
  # U = U*PI
# end new
  MP = Phi[,,2]*M
  for(i in 1:ns) M[i,] = PI[,,i,2]%*%MP[i,]
# new
  # V1[,,1] = L[,,1]*M*Pv
  V1[,,1] = L[,,1]*M
  V1[,,1] = V1[,,1]/rowSums(V1[,,1])
# end new

  V[,,1] = yv*V1[,,1]
  
# compute derivatives
# if the derivative is required
  if(der){
    ns = n #to check
    nal = dim(dlPhi)[4]; nbe = dim(dlPiv)[3]; nga = dim(dlPI)[5]
    npar = nal+nbe+nga
    indal = 1:nal; indbe = nal+(1:nbe); indga = nal+nbe+(1:nga)
    dlV = array(0,c(ns,k,TT,npar)); dlU = array(0,c(k,k,ns,TT,npar))
# last time
    M = matrix(1,ns,k); dlM = array(0,c(ns,k,npar))
    for(u in 1:k) dlV[,u,TT,] = dlL[,u,TT,] - dlpv
    for(u in 1:k) for(ub in 1:k) dlU[ub,u,,TT,] = dlL2[,ub,u,TT,] - dlpv
# previous occasions
    for(t in seq(TT-1,1,-1)){
      M2 = array(0,c(ns,k,k)); dlM2 = array(0,c(ns,k,k,npar))
      for(ub in 1:k) for(u in 1:k){
        M2[,ub,u] = M[,u]*PI[ub,u,,t+1]*Phi[,u,t+1]
        dlM2[,ub,u,] = dlM[,u,]
        dlM2[,ub,u,indal] = dlM2[,ub,u,indal]+dlPhi[,u,t+1,]
        dlM2[,ub,u,indga] = dlM2[,ub,u,indga]+dlPI[ub,u,,t+1,]
      }
      M = apply(M2,c(1,2),sum)
      dlM = array(0,c(ns,k,npar))
      for(ub in 1:k) for(u in 1:k) dlM[,ub,] = dlM[,ub,]+M2[,ub,u]/M[,ub]*dlM2[,ub,u,] 
      for(u in 1:k) dlV[,u,t,] = dlL[,u,t,] + dlM[,u,] - dlpv 			
      if(t>1) for(u in 1:k) for(ub in 1:k) dlU[ub,u,,t,] = dlL2[,ub,u,t,]+dlM[,u,] - dlpv
    }
  }else{
    dlU = NULL; dlV=NULL
  }
# final output
  out = list(U=U,V=V,dlU=dlU,dlV=dlV)

}
