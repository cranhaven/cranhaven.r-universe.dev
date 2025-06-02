lk_obs_cont_miss <- function(th,yv,Bm,Cm,k,Y,R,TT,r,mod,fort){

# compute corresponding parameters
  if(is.null(R)) miss = FALSE else miss = any(!R)
  ns = as.integer(dim(Y)[1])
  n = sum(yv)
  th1 = th[1:(k*r)]; th = th[-(1:(k*r))]
  Mu = matrix(th1,r,k)
  th1 = th[1:(r*(r+1)/2)]; th = th[-(1:(r*(r+1)/2))]
  Si = matrix(0,r,r)
  if(r==1){
    Si = th1
  }else{
    Si[upper.tri(Si,TRUE)] = th1
    Si = Si+t(Si-diag(diag(Si)))
  }
  if(k>1){
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
  if(k==1){
    Yv = matrix(Y,ns*TT,r)
    if(miss){
      lk = 0
      for (i in 1:ns) for(t in 1:TT){
        indo = R[i,t,]
        if(sum(R[i,t,])==1){
          lk = lk + dnorm(Y[i,t,][indo],Mu[indo],sqrt(Si[indo,indo]),log=TRUE)
        }else if(sum(R[i,t,])>1){
          lk = lk + dmvnorm(Y[i,t,][indo],Mu[indo],Si[indo,indo],log=TRUE)
        }
      }
    }else{
      lk = sum(dmvnorm(Yv,Mu,as.matrix(Si),log=TRUE))
    }
  }else{
    out = complk_cont_miss(Y,R,yv,piv,Pi,Mu,Si,k)
    lk = out$lk; Phi = out$Phi; L = out$L; pv = out$pv
  }
  sc = NULL

# ---- E-step ----
  if(k==1){
    if(miss){
      Yimp = Y; Vc = matrix(0,r,r)
      for(i in 1:ns) for(t in 1:TT){
        indo = R[i,t,]
        if(sum(indo)==0){
          Yimp[i,t,] = c(Mu)
          Vc = Vc+Si
        }else if(sum(indo)<r){
          iSi = ginv(Si[indo,indo])
          Yimp[i,t,!indo] = Mu[!indo]+Si[!indo,indo]%*%iSi%*%(Y[i,t,indo]-Mu[indo])
          Vc[!indo,!indo] = Vc[!indo,!indo]+Si[!indo,!indo]-Si[!indo,indo]%*%iSi%*%Si[indo,!indo]
        }
      }
    }
  }else{
# Compute V and U
    V = array(0,c(ns,k,TT)); U = array(0,c(k,k,TT))
    M = matrix(1,ns,k)
    if(n==1) V[,,TT] = L[,,TT]/sum(L[1,,TT])
    else V[,,TT] = yv*L[,,TT]/rowSums(L[,,TT])
    if(fort){
      U[,,TT] = .Fortran("prodnormw",L[,,TT-1],Phi[,,TT],Pi[,,TT],ns,k,D=matrix(0,k,k),yv)$D
    }else{
      for(i in 1:ns){
        Tmp = (L[i,,TT-1]%o%Phi[i,,TT])*Pi[,,TT]
        U[,,TT] = U[,,TT]+Tmp/sum(Tmp)*yv[i]
      }
    }
    if(TT>2){
      for(t in seq(TT-1,2,-1)){
        #browser()
        M = (Phi[,,t+1]*M)%*%t(Pi[,,t+1])
        M = M/rowSums(M)
        V[,,t] = L[,,t]*M
        if(n==1) V[,,t] = V[,,t]/sum(V[1,,t])
        else V[,,t] = V[,,t]/rowSums(V[,,t])*yv
        if(fort){
          U[,,t] = .Fortran("prodnormw",L[,,t-1],Phi[,,t]*M,Pi[,,t],ns,k,D=matrix(0,k,k),yv)$D
        }else{
          for(i in 1:ns){
            Tmp = (L[i,,t-1]%o%(Phi[i,,t]*M[i,]))*Pi[,,t]
            U[,,t] = U[,,t]+Tmp/sum(Tmp)*yv[i] ##SP: yv[i]
          }
        }
      }
    }
    # print(c(1,proc.time()-t0))
    M = (Phi[,,2]*M)%*%t(Pi[,,2])
    M = M/rowSums(M)
    V[,,1] = L[,,1]*M
    if(n==1) V[,,1] = V[,,1]/sum(V[1,,1])
    else V[,,1] = V[,,1]/rowSums(V[,,1])*yv
    # print(c(3,proc.time()-t0))
    # If required store parameters
  }

# ---- M-step ----
  if(k==1){
    if(!miss){
      Yimp = Y
      Vc = 0
    }
    Yvimp = matrix(Yimp,ns*TT,r)
    yvv = rep(yv,TT)
    nt = ns*TT
    iSi = solve(Si)
    sc = iSi%*%(colSums(yvv*Yvimp)-c(n*TT*Mu))
    Mu = as.matrix(colSums(yvv*Yvimp,na.rm=TRUE))/(n*TT)
    Tmp = Yvimp-rep(1,nt)%o%c(Mu)
    tmp = t(Tmp)%*%(yvv*Tmp)+Vc
    tmp = iSi%*%tmp%*%iSi
    tmp = tmp-(n*TT)*iSi
    diag(tmp) = diag(tmp)/2
    sc = c(sc,tmp[upper.tri(tmp,TRUE)])
  }else{
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
    sc = c(sc,t(Bm)%*%(colSums(V[,,1])-n*piv))
    if(mod==0){
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
# ---- Output ----
  out = list(lk=lk,sc=sc)	
}
