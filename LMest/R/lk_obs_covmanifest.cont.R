lk_obs_covmanifest.cont <- function(th,yv,Bm,Cm,k,Y,R,X,TT,r,ncov,mod,fort){

# compute corresponding parameters
  if(is.null(R)) miss = FALSE else miss = any(!R)
  ns = as.integer(dim(Y)[1])
  n = sum(yv)
  nt = ns*TT
  th1 = th[1:((ncov+k)*r)]; th = th[-(1:((ncov+k)*r))]
  Be = matrix(th1,ncov+k,r)
  if(k==1){
    Mu = X%*%Be
  }else{
    Mu = array(0,c(nt,r,k))
    for(u in 1:k){
      uv = rep(0,k); uv[u] = 1
      Xu = cbind(rep(1,nt)%o%uv,X[,-1])
      Mu[,,u] = Xu%*%Be
    }
  }
  th1 = th[1:(r*(r+1)/2)]; th = th[-(1:(r*(r+1)/2))]
  Si = matrix(0,r,r)
  if(r==1){
    Si[1,1] = th1
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
    Yv = matrix(aperm(Y,c(2,1,3)),ns*TT,r)
    if(miss) Rv = matrix(aperm(R,c(2,1,3)),ns*TT,r)
    nt = nrow(Yv)
    if(miss){
      lk = 0
      for (i in 1:nt){
        indo = Rv[i,]
        if(sum(indo)==1){
          lk = lk + dnorm(Yv[i,indo],Mu[i,indo],sqrt(Si[indo,indo]),log=TRUE)
        }else if(sum(indo)>1){
          lk = lk + dmvnorm(Yv[i,indo],Mu[i,indo],Si[indo,indo],log=TRUE)
        }
      }
    }else{
      yvv = rep(yv,each=TT)
      lk = 0
      if(r==1) for(i in 1:nt) lk = lk+dnorm(Yv[i,],Mu[i,],sqrt(Si),log=TRUE)*yvv[i]
      else for(i in 1:nt) lk = lk+dmvnorm(Yv[i,],Mu[i,],Si,log=TRUE)*yvv[i]
    }
  }else{
    out = complk_covmanifest.cont(Y,R,yv,piv,Pi,Mu,Si,k)
    lk = out$lk; Phi = out$Phi; L = out$L; pv = out$pv
  }
  sc = NULL

# ---- E-step ----
  if(k==1){
    if(miss){
      Yvimp = Yv; Vc = matrix(0,r,r)
      for(i in 1:nt){
        indo = Rv[i,]
        if(sum(indo)==0){
          Yvimp[i,] = Mu[i,]
          Vc = Vc+Si
        }else if(sum(indo)<r){
          iSi = ginv(Si[indo,indo])
          Yvimp[i,!indo] = Mu[i,!indo]+Si[!indo,indo]%*%iSi%*%(Yv[i,indo]-Mu[i,indo])
          Vc[!indo,!indo] = Vc[!indo,!indo]+Si[!indo,!indo]-Si[!indo,indo]%*%iSi%*%Si[indo,!indo]
        }
      }
      Yimp = aperm(array(Yvimp,c(TT,ns,r)),c(2,1,3))
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
        else V[,,t] = yv*V[,,t]/rowSums(V[,,t])
        if(fort){
          U[,,t] = .Fortran("prodnormw",L[,,t-1],Phi[,,t]*M,Pi[,,t],ns,k,D=matrix(0,k,k),yv)$D
        }else{
          for(i in 1:ns){
            Tmp = (L[i,,t-1]%o%(Phi[i,,t]*M[i,]))*Pi[,,t]
            U[,,t] = U[,,t]+Tmp/sum(Tmp)*yv[i]
          }
        }
      }
    }
    # print(c(1,proc.time()-t0))
    M = (Phi[,,2]*M)%*%t(Pi[,,2])
    M = M/rowSums(M)
    V[,,1] = L[,,1]*M
    if(n==1) V[,,1] = V[,,1]/sum(V[1,,1])
    else V[,,1] = yv*V[,,1]/rowSums(V[,,1])
    # print(c(3,proc.time()-t0))
    # If required store parameters
  }

# ---- M-step ----
  if(k==1){
    if(!miss){
      Yimp = Y
      Vc = 0
    }
    Yvimp = matrix(aperm(Yimp,c(2,1,3)),ns*TT,r)
    nt = n*TT
    iSi = solve(Si)
    sc = iSi%*%t(Yvimp-Mu)%*%X
    yvv = rep(yv,each=TT)
    Tmp = Yvimp-Mu
    tmp = t(Tmp)%*%(yvv*Tmp)+Vc
    tmp = iSi%*%tmp%*%iSi
    tmp = tmp-(n*TT)*iSi
    diag(tmp) = diag(tmp)/2
    sc = c(sc,tmp[upper.tri(tmp,TRUE)])
  }else{
    iSi = solve(Si)
    Vv = matrix(aperm(V,c(3,1,2)),ns*TT,k)
    if(miss){
      Sitmp = matrix(0,r,r)
      Y1 = array(Y,c(n,TT,r,k))
      Var = array(0,c(n,TT,r,r))
      j = 0
      for(i in 1:ns) for(t in 1:TT){
        j = j+1
        nr = sum(R[i,t,])
        if(nr==0){
          Y1[i,t,,] = Mu[j,,]
          Var[i,t,,] = Si
        }else if(nr<r){
          indo = R[i,t,]; indm = !R[i,t,]
          Tmp = Si[indm,indo]%*%solve(Si[indo,indo])
          Var[i,t,indm,indm] = Si[indm, indm]-Tmp%*%Si[indo,indm]
          for(u in 1:k) Y1[i,t,indm,u] = Mu[j,indm,u]+Tmp%*%(Y[i,t,indo]-Mu[j,indo,u])
        }
      }
# score for Mu and Si
      sc = rep(0,r*(ncov+k))
      tmp = matrix(0,r,r)
      Y1v = array(aperm(Y1,c(2,1,3,4)),c(ns*TT,r,k))
      Var1 = array(Var,c(ns*TT,r,r))
      for(u in 1:k){
        uv = rep(0,k); uv[u] = 1
        Xu = cbind(rep(1,nt)%o%uv,X[,-1])
        Tmp = Y1v[,,u]-Mu[,,u]
        tmp = tmp+t(Tmp)%*%(Vv[,u]*Tmp)+apply(Vv[,u]*Var1,c(2,3),sum)
        sc = sc+c(t(Xu)%*%(Vv[,u]*Tmp)%*%iSi)
      }
      tmp = iSi%*%tmp%*%iSi
      tmp = tmp-nt*iSi
      diag(tmp) = diag(tmp)/2
      sc = c(sc,tmp[upper.tri(tmp,TRUE)])
    }else{
# score for Be
      Yv = matrix(aperm(Y,c(2,1,3)),ns*TT,r)
      Vv = matrix(aperm(V,c(3,1,2)),ns*TT,k)
      sc = rep(0,r*(ncov+k))
      tmp = matrix(0,r,r)
      for(u in 1:k){
        uv = rep(0,k); uv[u] = 1
        Xu = cbind(rep(1,nt)%o%uv,X[,-1])
        Tmp = Yv-Mu[,,u]
        tmp = tmp+t(Tmp)%*%(Vv[,u]*Tmp)
        sc = sc+c(t(Xu)%*%(Vv[,u]*Tmp)%*%iSi)
      }
# score for Si
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
