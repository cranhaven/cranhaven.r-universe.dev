lk_obs_latent_cont <- function(th,Y,R,yv,XXdis,Xlab,ZZdis,Zlab,param,fort=TRUE){

# ---- Preliminaries ----
  if(is.null(R)) miss = FALSE else miss = any(!R)
  sY = dim(Y)
  ns = sY[1]
  TT = sY[2]
  n = sum(yv)
  if(length(sY)==2) r = 1 else r = sY[3]
  k = dim(XXdis)[1]
#  nc1 = dim(XXdis)[2]/(k-1)-1
  nc1 = dim(XXdis)[2]/(k-1)
  if(param=="multilogit"){
#    nc2 = dim(ZZdis)[2]/(k-1)-1
    nc2 = dim(ZZdis)[2]/(k-1)
  }else if(param=="difflogit"){
    nc2 = (dim(ZZdis)[2]-(k*(k-1)))/(k-1)
  }
  Xndis = max(Xlab)
  Zndis = max(Zlab)
#  separate parameters
# Mu e Si
  th1 = th[1:(k*r)]; th = th[-(1:(k*r))]
  Mu = matrix(th1,r,k)

  th1 = th[1:(r*(r+1)/2)]; th = th[-(1:(r*(r+1)/2))]
  Si = matrix(0,r,r)
  Si[upper.tri(Si,TRUE)]=th1
  if(r>1) Si = Si+t(Si-diag(diag(Si)))

# parameters on initial probabilities
#  ind = (1:((1+nc1)*(k-1)))
  ind = 1:(nc1*(k-1))
  be = th[ind];
  out = prob_multilogit(XXdis,be,Xlab,fort)
  Piv = out$P; Pivdis = out$Pdis
  # count = ((1+nc1)*(k-1))
  count = nc1*(k-1)

# parameters on transition probabilities
  if(param=="multilogit"){
#    ind = count+(1:((nc2+1)*(k-1)*k))
    ind = count+(1:(nc2*(k-1)*k))
    # Ga = matrix(th[ind],(nc2+1)*(k-1),k)
    Ga = matrix(th[ind],nc2*(k-1),k)
    PIdis = array(0,c(Zndis,k,k)); PI = array(0,c(k,k,ns,TT))
    for(h in 1:k){
      tmp = ZZdis[,,,h]
      if(nc2<=1) tmp = array(tmp,c(k,(k-1),Zndis))
      out = prob_multilogit(tmp,Ga[,h],Zlab,fort)
      PIdis[,,h] = out$Pdis; PI[h,,,2:TT] = array(as.vector(t(out$P)),c(1,k,ns,TT-1))
      if(is.na(sum(PI))) browser() ########## ERRRORE con fort=TRUE a volte si ha NaN
    }
    # count = count+(nc2+1)*(k-1)*k
    count = count+nc2*(k-1)*k
  }else if(param=="difflogit"){
    ind = count+(1:(k*(k-1)+(k-1)*nc2))
    Ga = matrix(th[ind],k*(k-1)+(k-1)*nc2)
    PI = array(0,c(k,k,ns,TT))
    out = prob_multilogit(ZZdis,Ga,Zlab,fort)
    PIdis = out$Pdis; PI[,,,2:TT] = array(as.vector(t(out$P)),c(k,k,ns,TT-1))
    PI = aperm(PI,c(2,1,3,4))
    count = count+(k*(k-1)+(k-1)*nc2)
  }
# compute log-likelihood
  out = lk_comp_latent_cont(Y,R,yv,Piv,PI,Mu,Si,k,fort=fort)
  lk = out$lk; Phi = out$Phi; L = out$L; pv = out$pv
# backward recursion

# ---- E-step ----
  out = prob_post_cov_cont(Y,yv,Mu,Si,Piv,PI,Phi,L,pv)
  U = out$U; V = out$V
  sc = NULL
# Compute V and U

# ---- M-step ----
  iSi = solve(Si)
  Vv = matrix(aperm(V,c(1,3,2)),ns*TT,k)
  if(miss){
    Sitmp = matrix(0,r,r)
    Var = array(0,c(n,TT,r,r))
    tmp=0
    for(u in 1:k){
      Y1 = Y
      for(i in 1:ns) for(t in 1:TT){
        nr = sum(R[i,t,])
        if(nr==0){
          Y1[i,t,] = Mu[,u]
          Var[i,t,,] = Si
        }else if(nr<r){
          indo = R[i,t,]; indm = !R[i,t,]
          Tmp = Si[indm,indo]%*%solve(Si[indo,indo])
          Y1[i,t,][indm] = Mu[indm,u]+Tmp%*%(Y[i,t,][indo]-Mu[indo,u])
          Var[i,t,,][indm,indm] = Si[indm, indm]-Tmp%*%Si[indo,indm]
        }
      }
      Yv1 = matrix(Y1,ns*TT)
      Var1 = array(Var,c(ns*TT,r,r))
      # score for Mu and Si	
      sc = c(sc,iSi%*%(t(Yv1)%*%Vv[,u]-sum(Vv[,u])*Mu[,u]))
      Tmp = Yv1-rep(1,ns*TT)%*%t(Mu[,u])
      tmp = tmp+t(Tmp)%*%(Vv[,u]*Tmp)+apply(Vv[,u]*Var1,c(2,3),sum)
    }
    tmp = iSi%*%tmp%*%iSi
    tmp = tmp-(n*TT)*iSi
    diag(tmp) = diag(tmp)/2
    sc = c(sc,tmp[upper.tri(tmp,TRUE)])
  }else{
    # score for Mu
    Yv = matrix(Y,ns*TT,r)
    Vv = matrix(aperm(V,c(1,3,2)),ns*TT,k)
    for(u in 1:k) sc = c(sc,iSi%*%(t(Yv)%*%Vv[,u]-sum(Vv[,u])*Mu[,u]))
    
    # score for Si
    tmp=0
    for(u in 1:k){
      Tmp = Yv-rep(1,ns*TT)%*%t(Mu[,u])
      tmp = tmp+t(Tmp)%*%(Vv[,u]*Tmp)
    }
    tmp = iSi%*%tmp%*%iSi
    tmp = tmp-(n*TT)*iSi
    diag(tmp) = diag(tmp)/2
    sc = c(sc,tmp[upper.tri(tmp,TRUE)])
  }
# Update piv and Pi
  out = est_multilogit(V[,,1],XXdis,Xlab,be,Pivdis,fort=fort,ex=TRUE)
  sc = c(sc,out$sc)

# score and info Pi
  if(param=="multilogit"){
    for(h in 1:k){
      UU = NULL
      for(t in 2:TT) UU = rbind(UU,t(U[h,,,t]))
      tmp = ZZdis[,,,h]
      if(nc2==1) tmp = array(tmp,c(k,(k-1),Zndis))
      tmp2 = PIdis[,,h]
      if(Zndis==1) tmp2 = matrix(tmp2,1,k)
      out = est_multilogit(UU,tmp,Zlab,Ga[,h],tmp2,fort=fort,ex=TRUE)
      sc = c(sc,out$sc)
    }
  }else if(param=="difflogit"){
    Tmp = aperm(U[,,,2:TT,drop=FALSE],c(1,3,4,2))
    Tmp = matrix(Tmp,ns*k*(TT-1),k)
    out = est_multilogit(Tmp,ZZdis,Zlab,Ga,PIdis,fort=fort,ex=TRUE)
    sc = c(sc,out$sc)
  }

# ---- Output ---
    out = list(lk=lk,sc=sc,Mu=Mu,Si=Si,be=be,Ga=Ga,Piv=Piv,PI=PI)
}
