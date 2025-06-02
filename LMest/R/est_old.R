est_lm_basic <-
  function(S,yv,k,start=0,mod=0,tol=10^-8,maxit=1000,out_se=FALSE,piv=NULL,Pi=NULL,Psi=NULL){

    warning("est_lm_basic function is no longer maintained. Please look at lmest function",call. = FALSE)
    # Preliminaries
    check_der = FALSE  # to check derivatives
    n = sum(yv)
    sS = dim(S)
    ns = sS[1]
    TT = sS[2]
    if(min(S,na.rm=T)>0){
      cat("|------------------- WARNING -------------------|\n")
      cat("|The first response category must be coded as 0 |\n")
      cat("|-----------------------------------------------|\n")
    }

    if(is.data.frame(S)){
      warning("Data frame not allowed for S")
    }

    if(ns!=length(yv)) stop("dimensions mismatch between S and yv")

    if(length(sS)==2){
      r = 1
      if(is.matrix(S)) S = array(S,c(dim(S),1))
    }else r = sS[3]

    miss = any(is.na(S))
    if(miss){
      cat("Missing data in the dataset, treated as missing at random\n")
      R = 1 * (!is.na(S))
      S[is.na(S)] = 0
    }else{
      R = NULL
    }
    Sv = matrix(S,ns*TT,r)
    if(miss) Rv = matrix(R,ns*TT,r)
    bv = apply(Sv,2,max)
    b = max(bv)
    m = vector("list",r)
    for(j in 1:r){
      m[[j]]$Co = cbind(-diag(bv[j]),diag(bv[j]))
      Maj = cbind(lower.tri(matrix(1,bv[j],bv[j]), diag = TRUE),rep(0,bv[j]))
      m[[j]]$Ma = rbind(Maj,1-Maj)
    }
    th = NULL; sc = NULL; J = NULL
    if(out_se){
      for(j in 1:r){
        m[[j]]$A = cbind(-rep(1,bv[j]),diag(bv[j]))
        m[[j]]$Am = rbind(rep(0,bv[j]),diag(bv[j]))
      }
      B = cbind(-rep(1,k-1),diag(k-1))
      Bm = rbind(rep(0,k-1),diag(k-1))
      C = array(0,c(k-1,k,k))
      Cm = array(0,c(k,k-1,k))
      for(u in 1:k){
        C[,,u] = rbind(cbind(diag(u-1),-rep(1,u-1),matrix(0,u-1,k-u)),
                       cbind(matrix(0,k-u,u-1),-rep(1,k-u),diag(k-u)))
        Cm[,,u] = rbind(cbind(diag(u-1),matrix(0,u-1,k-u)),
                        rep(0,k-1),
                        cbind(matrix(0,k-u,u-1),diag(k-u)))
      }
    }
    # When there is just 1 latent class
    if(k == 1){
      piv = 1; Pi = 1
      P = matrix(NA,b+1,r)
      for(j in 1:r) P[1:(bv[j]+1),j] = 0
      for(t in 1:TT){
        for(j in 1:r){
          for(y in 0:b){
            ind = which(S[,t,j]==y)
            P[y+1,j] = P[y+1,j]+sum(yv[ind])
          }
        }
      }
      Psi = P/(n*TT)
      dimnames(Psi)=list(category=0:b,item=1:r)
      pm = rep(1,ns)
      for(t in 1:TT) for(j in 1:r) pm = pm*Psi[S[,t,j]+1,j]
      lk = sum(yv*log(pm))
      np = r*b
      aic = -2*lk+np*2
      bic = -2*lk+np*log(n)
      out = list(lk=lk,piv=piv,Pi=Pi,Psi=Psi,np=np,aic=aic,bic=bic,lkv=NULL,J=NULL,V=NULL,th=NULL,sc=NULL,call=match.call())
      class(out)="LMbasic"
      return(out)
    }
    # Starting values
    if(start == 0){
      P = matrix(NA,b+1,r); E = matrix(NA,b,r)
      for(j in 1:r) P[1:(bv[j]+1),j] = 0
      for(t in 1:TT) for(j in 1:r) for(y in 0:b){
        ind = which(S[,t,j]==y)
        P[y+1,j] = P[y+1,j]+sum(yv[ind])
        E[1:bv[j],j] = m[[j]]$Co%*%log(m[[j]]$Ma%*%P[1:(bv[j]+1),j])
      }
      Psi = array(NA,c(b+1,k,r)); Eta = array(NA,c(b,k,r))
      grid = seq(-k,k,2*k/(k-1))
      for(c in 1:k) for(j in 1:r){
        etac = E[1:bv[j],j]+grid[c]
        Eta[1:bv[j],c,j] = etac
        Psi[1:(bv[j]+1),c,j] = invglob(etac)
      }
      piv = rep(1,k)/k
      Pi = matrix(1,k,k)+9*diag(k); Pi = diag(1/rowSums(Pi))%*%Pi;
      Pi = array(Pi,c(k,k,TT)); Pi[,,1] = 0
    }
    if(start==1){
      Psi = array(NA,c(b+1,k,r))
      for(j in 1:r){
        Psi[1:(bv[j]+1),,j] = matrix(runif((bv[j]+1)*k),bv[j]+1,k)
        for(c in 1:k) Psi[1:(bv[j]+1),c,j] = Psi[1:(bv[j]+1),c,j]/sum(Psi[1:(bv[j]+1),c,j])
      }
      Pi = array(runif(k^2*TT),c(k,k,TT))
      for(t in 2:TT) Pi[,,t] = diag(1/rowSums(Pi[,,t]))%*%Pi[,,t]
      Pi[,,1] = 0
      piv = runif(k); piv = piv/sum(piv)
    }
    if(start==2){
      if(is.null(piv)) stop("initial value of the initial probabilities (piv) must be given in input")
      if(is.null(Pi)) stop("initial value of the transition probabilities (Pi) must be given in input")
      if(is.null(Psi)) stop("initial value of the conditional response probabilities (Psi) must be given in input")
      piv = piv
      Pi = Pi
      Psi = Psi
    }
    # Compute log-likelihood
    out = complk(S,R,yv,piv,Pi,Psi,k)
    lk = out$lk; Phi = out$Phi; L = out$L; pv = out$pv
    lk0 = sum(yv*log(yv/n)); dev = 2*(lk0-lk)
    cat("------------|-------------|-------------|-------------|-------------|-------------|-------------|\n");
    cat("     mod    |      k      |    start    |     step    |     lk      |    lk-lko   | discrepancy |\n");
    cat("------------|-------------|-------------|-------------|-------------|-------------|-------------|\n");
    cat(sprintf("%11g",c(mod,k,start,0,lk)),"\n",sep=" | ")
    it = 0; lko = lk-10^10; lkv = NULL
    par = c(piv,as.vector(Pi),as.vector(Psi))
    if(any(is.na(par))) par = par[-which(is.na(par))]
    paro = par
    # Iterate until convergence
    while((lk-lko)/abs(lk)>tol & it<maxit){
      Psi0 = Psi; piv0 = piv; Pi0 = Pi
      it = it+1;
      # ---- E-step ----
      # Compute V and U
      #time = proc.time()
      V = array(0,c(ns,k,TT)); U = array(0,c(k,k,TT))
      Yvp = matrix(yv/pv,ns,k)
      M = matrix(1,ns,k)
      V[,,TT] = Yvp*L[,,TT]
      U[,,TT] = (t(L[,,TT-1])%*%(Yvp*Phi[,,TT]))*Pi[,,TT]
      if(TT>2){
        for(t in seq(TT-1,2,-1)){
          M = (Phi[,,t+1]*M)%*%t(Pi[,,t+1]);
          V[,,t] = Yvp*L[,,t]*M
          U[,,t] = (t(L[,,t-1])%*%(Yvp*Phi[,,t]*M))*Pi[,,t]
        }
      }
      M = (Phi[,,2]*M)%*%t(Pi[,,2])
      V[,,1] = Yvp*L[,,1]*M
      #print(proc.time()-time)

      # If required store parameters
      # ---- M-step ----
      # Update Psi
      Y1 = array(NA,c(b+1,k,r))
      for(j in 1:r) Y1[1:(bv[j]+1)] = 0
      Vv = matrix(aperm(V,c(1,3,2)),ns*TT,k)
      for(j in 1:r) for(jb in 0:bv[j]) {
        ind = which(Sv[,j]==jb)
        if(length(ind)==1){
          if(miss) Y1[jb+1,,j] = Vv[ind,]*Rv[ind,j]
          else Y1[jb+1,,j] = Vv[ind,]
        }
        if(length(ind)>1){
          if(miss) Y1[jb+1,,j] = colSums(Vv[ind,]*Rv[ind,j])
          else Y1[jb+1,,j] = colSums(Vv[ind,])
        }

      }
      for(j in 1:r) for(c in 1:k){
        tmp = Y1[1:(bv[j]+1),c,j]
        if(any(is.na(tmp))) tmp[is.na(tmp)] = 0
        tmp = pmax(tmp/sum(tmp),10^-10)
        Psi[1:(bv[j]+1),c,j] = tmp/sum(tmp)
      }

      #print(proc.time()-time)
      # Update piv and Pi
      piv = colSums(V[,,1])/n
      U = pmax(U,10^-300)
      if(mod==0) for(t in 2:TT) Pi[,,t] = diag(1/rowSums(U[,,t]))%*%U[,,t]
      if(mod==1){
        Ut = apply(U[,,2:TT],c(1,2),sum)
        Pi[,,2:TT] = array(diag(1/rowSums(Ut))%*%Ut,c(k,k,TT-1))
      }
      if(mod>1){
        Ut1 = U[,,2:mod]
        if(length(dim(Ut1))>2) Ut1 = apply(Ut1,c(1,2),sum)
        Ut2 = U[,,(mod+1):TT]
        if(length(dim(Ut2))>2) Ut2 = apply(Ut2,c(1,2),sum)
        Pi[,,2:mod] = array(diag(1/rowSums(Ut1,2))%*%Ut1,c(k,k,mod-1))
        Pi[,,(mod+1):TT] = array(diag(1/rowSums(Ut2,2))%*%Ut2,c(k,k,TT-mod))
      }
      #print(proc.time()-time)
      # Compute log-likelihood
      paro = par; par = c(piv,as.vector(Pi),as.vector(Psi))
      if(any(is.na(par))) par = par[-which(is.na(par))]
      lko = lk

      out = complk(S,R,yv,piv,Pi,Psi,k)
      lk = out$lk; Phi = out$Phi; L = out$L; pv = out$pv
      if(it/10 == round(it/10)) cat(sprintf("%11g",c(mod,k,start,it,lk,lk-lko,max(abs(par-paro)))),"\n",sep=" | ")
      lkv = c(lkv,lk)
      #print(proc.time()-time)
    }
    # Compute information matrix if required
    if(out_se){
      th = NULL
      for(u in 1:k) for(j in 1:r) th = c(th,m[[j]]$A%*%log(Psi[1:(bv[j]+1),u,j]))
      th = c(th,B%*%log(piv))
      if(mod==0) for(t in 2:TT) for(u in 1:k) th = c(th,C[,,u]%*%log(Pi[u,,t]))
      if(mod==1) for(u in 1:k) th = c(th,C[,,u]%*%log(Pi[u,,2]))
      if(mod>1) {
        for(u in 1:k) th = c(th,C[,,u]%*%log(Pi[u,,2]))
        for(u in 1:k) th = c(th,C[,,u]%*%log(Pi[u,,mod+1]))
      }
      lth = length(th)
      out = recursions(S,R,yv,Psi,piv,Pi,k,lth,m,Bm,Cm,bv,mod)
      F1 = out$F1; F2 = out$F2; F1d = out$F1d; F2d = out$F2d

      sc = NULL
      Y = array(NA,c(b+1,TT,k,r))
      for(j in 1:r) Y[1:(bv[j]+1),,,j] = 0
      for(j in 1:r) for(t in 1:TT) for(jb in 0:bv[j]){
        ind = which(S[,t,j]==jb)
        if(length(ind)==1){
          if(miss) Y[jb+1,t,,j] = F1[,t,ind]*R[ind,t,j]*yv[ind]
          else Y[jb+1,t,,j] = F1[,t,ind]*yv[ind]
        }
        if(length(ind)>1){
          if(miss) Y[jb+1,t,,j] = F1[,t,ind]%*%(R[ind,t,j]*yv[ind])
          else Y[jb+1,t,,j] = F1[,t,ind]%*%yv[ind]
        }
      }
      Y1 = apply(Y,c(1,3,4),sum)
      for(u in 1:k) for(j in 1:r){
        indj = 1:(bv[j]+1)
        sc = c(sc,t(m[[j]]$Am)%*%(Y1[indj,u,j]-sum(Y1[indj,u,j])*Psi[indj,u,j]))
      }
      Y2 = Y1

      for(u in 1:k) for(j in 1:r){
        indj = 1:(bv[j]+1)
        Juj = sum(Y1[indj,u,j])*t(m[[j]]$Am)%*%(diag(Psi[indj,u,j])-Psi[indj,u,j]%o%Psi[indj,u,j])%*%m[[j]]$Am
        if(u==1 && j==1) J = Juj
        else J = blkdiag(J,Juj)
      }
      bv1 = F1[,1,]%*%yv
      sc = c(sc,t(Bm)%*%(bv1-sum(bv1)*piv))
      J = blkdiag(J,n*t(Bm)%*%(diag(piv)-piv%o%piv)%*%Bm)
      if(mod==0){
        for(t in 2:TT){
          Ut = 0
          for(i in 1:ns) Ut = Ut+yv[i]*F2[,,t,i]
          for(u in 1:k){
            sc = c(sc,t(Cm[,,u])%*%(Ut[u,]-sum(Ut[u,])*Pi[u,,t]))
            J = blkdiag(J,sum(Ut[u,])*t(Cm[,,u])%*%(diag(Pi[u,,t])-Pi[u,,t]%o%Pi[u,,t])%*%Cm[,,u])
          }
        }
      }
      if(mod==1){
        Ut = 0
        for(i in 1:ns) for(t in 2:TT) Ut = Ut+yv[i]*F2[,,t,i]
        for(u in 1:k){
          sc = c(sc,t(Cm[,,u])%*%(Ut[u,]-sum(Ut[u,])*Pi[u,,2]))
          J = blkdiag(J,sum(Ut[u,])*t(Cm[,,u])%*%(diag(Pi[u,,2])-Pi[u,,2]%o%Pi[u,,2])%*%Cm[,,u])
        }
      }
      if(mod>1){
        Ut=0
        for(i in 1:ns) for(t in 2:mod) Ut = Ut+yv[i]*F2[,,t,i]
        for(u in 1:k){
          sc = c(sc,t(Cm[,,u])%*%(Ut[u,]-sum(Ut[u,])*Pi[u,,2]))
          J = blkdiag(J,sum(Ut[u,])*t(Cm[,,u])%*%(diag(Pi[u,,2])-Pi[u,,2]%o%Pi[u,,2])%*%Cm[,,u])
        }
        Ut=0
        for(i in 1:ns) for(t in (mod+1):TT) Ut = Ut+yv[i]*F2[,,t,i]
        for(u in 1:k){
          sc = c(sc,t(Cm[,,u])%*%(Ut[u,]-sum(Ut[u,])*Pi[u,,mod+1]))
          J = blkdiag(J,sum(Ut[u,])*t(Cm[,,u])%*%(diag(Pi[u,,mod+1])-Pi[u,,mod+1]%o%Pi[u,,mod+1])%*%Cm[,,u])
        }

      }
      J = as.matrix(J)
      Jd = NULL
      for(pa in 1:lth){
        scj = NULL
        Y = array(NA,c(b+1,TT,k,r))
        for(j in 1:r) Y[1:(bv[j]+1),,,j] = 0
        for(j in 1:r) for(t in 1:TT) for(jb in 0:b){
          ind = which(S[,t,j]==jb)
          if(length(ind)==1){
            if(miss) Y[jb+1,t,,j] = F1d[,t,ind,pa]*R[ind,t,j]*yv[ind]
            else Y[jb+1,t,,j] = F1d[,t,ind,pa]*yv[ind]
          }
          if(length(ind)>1){
            if(miss) Y[jb+1,t,,j] = F1d[,t,ind,pa]%*%(R[ind,t,j]*yv[ind])
            else Y[jb+1,t,,j] = F1d[,t,ind,pa]%*%yv[ind]
          }
        }
        Y1 = apply(Y,c(1,3,4),sum)
        for(u in 1:k) for(j in 1:r){
          indj = 1:(bv[j]+1)
          scj = c(scj,t(m[[j]]$Am)%*%(Y1[indj,u,j]-sum(Y1[indj,u,j])*Psi[indj,u,j]))
        }
        bv1 = F1d[,1,,pa]%*%yv
        scj = c(scj,t(Bm)%*%(bv1-sum(bv1)*piv))
        if(mod==0) {
          for(t in 2:TT){
            Ut = 0
            for(i in 1:ns) Ut = Ut+yv[i]*F2d[,,t,i,pa]
            for(u in 1:k) scj = c(scj,t(Cm[,,u])%*%(Ut[u,]-sum(Ut[u,])*Pi[u,,t]))
          }
        }
        if(mod==1){
          Ut = 0
          for(i in 1:ns) for(t in 2:TT) Ut = Ut+yv[i]*F2d[,,t,i,pa]
          for(u in 1:k) scj = c(scj,t(Cm[,,u])%*%(Ut[u,]-sum(Ut[u,])*Pi[u,,2]))
        }
        if(mod>1){
          Ut = 0
          for(i in 1:ns) for(t in 2:mod) Ut = Ut+yv[i]*F2d[,,t,i,pa]
          for(u in 1:k) scj = c(scj,t(Cm[,,u])%*%(Ut[u,]-sum(Ut[u,])*Pi[u,,2]))
          Ut = 0
          for(i in 1:ns) for(t in (mod+1):TT) Ut = Ut+yv[i]*F2d[,,t,i,pa]
          for(u in 1:k) scj = c(scj,t(Cm[,,u])%*%(Ut[u,]-sum(Ut[u,])*Pi[u,,mod+1]))
        }

        Jd = cbind(Jd,scj)
      }
      J = J-Jd
      Va = ginv(J)
      for(u in 1:k) for(j in 1:r){
        indj = 1:(bv[j]+1)
        Om = diag(Psi[indj,u,j])-Psi[indj,u,j]%o%Psi[indj,u,j]
        if(u==1) {
          if(j==1) M = Om%*%m[[j]]$Am
          else M = blkdiag(M,Om%*%m[[j]]$Am)
        } else M = blkdiag(M,Om%*%m[[j]]$Am)
      }
      Om = diag(piv)-tcrossprod(piv,piv)
      M = blkdiag(M,Om%*%Bm)
      if(mod==0){
        for(t in 2:TT) for(u in 1:k){
          Om = diag(Pi[u,,t])-Pi[u,,t]%o%Pi[u,,t]
          M = blkdiag(M,Om%*%Cm[,,u])
        }
      }
      if(mod==1){
        for(u in 1:k){
          Om = diag(Pi[u,,2])-Pi[u,,2]%o%Pi[u,,2]
          M = blkdiag(M,Om%*%Cm[,,u])
        }
      }
      if(mod>1){
        for(u in 1:k){
          Om = diag(Pi[u,,2])-Pi[u,,2]%o%Pi[u,,2]
          M = blkdiag(M,Om%*%Cm[,,u])
        }
        for(u in 1:k){
          Om = diag(Pi[u,,mod+1])-Pi[u,,mod+1]%o%Pi[u,,mod+1]
          M = blkdiag(M,Om%*%Cm[,,u])
        }
      }
      M = as.matrix(M)
      Va = M%*%Va%*%t(M)
      dVa = diag(Va)
      if(any(dVa<0)) warning("Negative elements in the estimated variance-covariance matrix for the parameters estimates")
      se = sqrt(abs(dVa))
      # Divide parameters
      nPsi = sum(bv+1)*k
      sePsi = se[1:nPsi]
      sepiv = se[nPsi+(1:k)]
      if(mod==0) sePi = se[nPsi+k+(1:(k*k*(TT-1)))]
      if(mod==1) sePi = se[nPsi+k+(1:(k*k))]
      if(mod>1) sePi = se[nPsi+k+(1:(k*k*2))]
      #to check derivatives
      if(check_der){
        J0 = J
        th0 = th-10^-5/2
        out = lk_obs(th0,m,Bm,Cm,bv,k,S,R=R,yv,TT,r,mod)
        lk0 = out$lk; sc0 = out$sc
        lth = length(th)
        scn = rep(0,lth)
        J = matrix(0,lth,lth)
        for(j in 1:lth){
          thj = th0; thj[j] = thj[j]+10^-5
          out = lk_obs(thj,m,Bm,Cm,bv,k,S,R=R,yv,TT,r,mod)
          scn[j] = (out$lk-lk0)/10^-5
          J[,j] = (out$sc-sc0)/10^-5
        }
        J = -(J+t(J))/2
        print(c(lk,lk0))
        print(round(cbind(sc,scn,sc0),5))
        print(round(cbind(diag(J),diag(J0),diag(J)-diag(J0)),4))
        browser()
      }
    }
    # Compute number of parameters
    np = (k-1)+k*sum(bv)
    if(mod==0) np = np+(TT-1)*k*(k-1)
    if(mod==1) np = np+k*(k-1)
    if(mod>1) np = np+2*k*(k-1)
    aic = -2*lk+np*2
    bic = -2*lk+np*log(n)
    cat(sprintf("%11g",c(mod,k,start,it,lk,lk-lko,max(abs(par-paro)))),"\n",sep=" | ")
    # adjust output
    if(any(yv!=1)) V = V/yv

    lk = as.vector(lk)
    dimnames(Pi)=list(state=1:k,state=1:k,time=1:TT)
    dimnames(Psi)=list(category=0:b,state=1:k,item=1:r)

    out = list(lk=lk,piv=piv,Pi=Pi,Psi=Psi,np=np,aic=aic,bic=bic,lkv=lkv,V=V,call=match.call())
    if(out_se){
      sePsi0 = sePsi
      sePsi = array(NA,c(b+1,k,r))
      ind = 0
      for(u in 1:k) for(j in 1:r){
        indj = 1:(bv[j]+1)
        ind = max(ind)+indj
        sePsi[indj,u,j] = sePsi0[ind]
      }
      sePsi = array(sePsi,c(b+1,k,r))
      sePi0 = sePi
      sePi = array(0,c(k,k,TT))
      if(mod>1){
        sePi0 = array(sePi0,c(k,k,2))
        sePi0 = aperm(sePi0,c(2,1,3))
        sePi[,,2:mod] = sePi0[,,1]
        sePi[,,(mod+1):TT] = sePi0[,,2]
      } else {
        sePi[,,2:TT] = sePi0
        sePi = aperm(sePi,c(2,1,3))
      }
      dimnames(sePsi) = list(category=0:b,state=1:k,item=1:r)
      dimnames(sePi) = list(state=1:k,state=1:k,time=1:TT)

      out$sepiv = sepiv
      out$sePi = sePi
      out$sePsi = sePsi
    }

    cat("------------|-------------|-------------|-------------|-------------|-------------|-------------|\n");
    class(out)="LMbasic"
    (out)
  }


est_lm_basic_cont <-
  function(Y,k,start=0,mod=0,tol=10^-8,maxit=1000,out_se=FALSE,piv=NULL,Pi=NULL,Mu=NULL,Si=NULL){

    warning("est_lm_basic_cont function is no longer maintained. Please look at lmestCont function",call. = FALSE)
    # Preliminaries
    check_der = FALSE  # to check derivatives
    sY = dim(Y)
    n = sY[1]
    TT = sY[2]

    if(is.data.frame(Y)){
      warning("Data frame not allowed for Y")
    }

    if(length(sY)==2){
      r = 1
      if(is.matrix(Y)) Y = array(Y,c(dim(Y),1))
    }else r = sY[3]

    Yv = matrix(Y,n*TT,r)

    ## Check and inpute for missing data
    miss = any(is.na(Yv))
    if(miss){
      Yv = cbind(1,Yv)
      pYv = prelim.mix(Yv,1)
      thhat = em.mix(prelim.mix(Yv,1))
      rngseed(1)
      Yv = imp.mix(pYv, da.mix(pYv,thhat,steps=100), Yv)[,-1]
      Y = array(Yv,c(n,TT,r))
      cat("Missing data in the dataset. imp.mix function (mix package) used for imputation.\n")
    }


    th = NULL; sc = NULL; J = NULL
    if(out_se){
      B = cbind(-rep(1,k-1),diag(k-1))
      Bm = rbind(rep(0,k-1),diag(k-1))
      C = array(0,c(k-1,k,k))
      Cm = array(0,c(k,k-1,k))
      for(u in 1:k){
        C[,,u] = rbind(cbind(diag(u-1),-rep(1,u-1),matrix(0,u-1,k-u)),
                       cbind(matrix(0,k-u,u-1),-rep(1,k-u),diag(k-u)))
        Cm[,,u] = rbind(cbind(diag(u-1),matrix(0,u-1,k-u)),
                        rep(0,k-1),
                        cbind(matrix(0,k-u,u-1),diag(k-u)))
      }
    }

    # When there is just 1 latent class
    if(k == 1){
      piv = 1; Pi = 1
      Mu = colMeans(Yv)
      Si = cov(Yv)
      lk = sum(dmvnorm(Yv,Mu,Si,log=TRUE))
      np = k*r+r*(r+1)/2
      aic = -2*lk+np*2
      bic = -2*lk+np*log(n)
      out =     		list(lk=lk,piv=piv,Pi=Pi,Mu=Mu,Si=Si,np=np,aic=aic,bic=bic,lkv=NULL,V=NULL,call=match.call())
      class(out)="LMbasiccont"
      (out)
    }

    # Starting values
    if(start == 0){
      mu = colMeans(Yv)
      Si = cov(Yv); std = sqrt(diag(Si))
      qt = qnorm((1:k)/(k+1))
      Mu = matrix(0,r,k)
      for(u in 1:k) Mu[,u] = qt[u]*std+mu

      piv = rep(1,k)/k
      Pi = matrix(1,k,k)+9*diag(k); Pi = diag(1/rowSums(Pi))%*%Pi;
      Pi = array(Pi,c(k,k,TT)); Pi[,,1] = 0
    }
    if(start==1){
      Mu = matrix(0,r,k)
      mu = colMeans(Yv)
      Si = cov(Yv)
      for(u in 1:k) Mu[,u] = rmvnorm(1,mu,Si)
      Pi = array(runif(k^2*TT),c(k,k,TT))
      for(t in 2:TT) Pi[,,t] = diag(1/rowSums(Pi[,,t]))%*%Pi[,,t]
      Pi[,,1] = 0
      piv = runif(k); piv = piv/sum(piv)
    }
    if(start==2){
      if(is.null(piv)) stop("initial value of the initial probabilities (piv) must be given in input")
      if(is.null(Pi)) stop("initial value of the transition probabilities (Pi) must be given in input")
      if(is.null(Mu)) stop("initial value of the conditional means of the response variables (Mu) must be given in input")
      if(is.null(Si)) stop("initial value of the var-cov matrix common to all states (Si) must be given in input")
      piv = piv
      Pi = Pi
      Mu = Mu
      Si = Si
    }

    # Compute log-likelihood
    out = complk_cont(Y,piv,Pi,Mu,Si,k)
    lk = out$lk; Phi = out$Phi; L = out$L; pv = out$pv
    cat("------------|-------------|-------------|-------------|-------------|-------------|-------------|\n");
    cat("     mod    |      k      |    start    |     step    |     lk      |    lk-lko   | discrepancy |\n");
    cat("------------|-------------|-------------|-------------|-------------|-------------|-------------|\n");
    cat(sprintf("%11g",c(mod,k,start,0,lk)),"\n",sep=" | ")
    it = 0; lko = lk-10^10; lkv = NULL
    par = c(piv,as.vector(Pi),as.vector(Mu),as.vector(Si))
    if(any(is.na(par))) par = par[-which(is.na(par))]
    paro = par
    # Iterate until convergence
    while((lk-lko)/abs(lk)>tol & it<maxit){
      Mu0 = Mu; Si0 = Si; piv0 = piv; Pi0 = Pi
      it = it+1;
      # ---- E-step ----
      # Compute V and U
      #time = proc.time()
      V = array(0,c(n,k,TT)); U = array(0,c(k,k,TT))
      Yvp = matrix(1/pv,n,k)
      M = matrix(1,n,k)
      V[,,TT] = Yvp*L[,,TT]
      U[,,TT] = (t(L[,,TT-1])%*%(Yvp*Phi[,,TT]))*Pi[,,TT]
      if(TT>2){
        for(t in seq(TT-1,2,-1)){
          M = (Phi[,,t+1]*M)%*%t(Pi[,,t+1]);
          V[,,t] = Yvp*L[,,t]*M
          U[,,t] = (t(L[,,t-1])%*%(Yvp*Phi[,,t]*M))*Pi[,,t]
        }
      }
      M = (Phi[,,2]*M)%*%t(Pi[,,2])
      V[,,1] = Yvp*L[,,1]*M

      # If required store parameters
      # ---- M-step ----
      # Update Mu
      Vv = matrix(aperm(V,c(1,3,2)),n*TT,k)
      for(u in 1:k) Mu[,u] = (t(Yv)%*%Vv[,u])/sum(Vv[,u])
      # Update Si
      Si = matrix(0,r,r)
      for(u in 1:k){
        Tmp = Yv-rep(1,n*TT)%o%Mu[,u]
        Si= Si+ t(Tmp)%*%(Vv[,u]*Tmp)
      }
      Si = Si/(n*TT)

      #print(proc.time()-time)
      # Update piv and Pi
      piv = colSums(V[,,1])/n
      U = pmax(U,10^-300)
      if(mod==0) for(t in 2:TT) Pi[,,t] = diag(1/rowSums(U[,,t]))%*%U[,,t]
      if(mod==1){
        Ut = apply(U[,,2:TT],c(1,2),sum)
        Pi[,,2:TT] = array(diag(1/rowSums(Ut))%*%Ut,c(k,k,TT-1))
      }
      if(mod>1){
        Ut1 = U[,,2:mod]
        if(length(dim(Ut1))>2) Ut1 = apply(Ut1,c(1,2),sum)
        Ut2 = U[,,(mod+1):TT]
        if(length(dim(Ut2))>2) Ut2 = apply(Ut2,c(1,2),sum)
        Pi[,,2:mod] = array(diag(1/rowSums(Ut1,2))%*%Ut1,c(k,k,mod-1))
        Pi[,,(mod+1):TT] = array(diag(1/rowSums(Ut2,2))%*%Ut2,c(k,k,TT-mod))
      }
      #print(proc.time()-time)
      # Compute log-likelihood
      paro = par; par = c(piv,as.vector(Pi),as.vector(Mu),as.vector(Si))
      if(any(is.na(par))) par = par[-which(is.na(par))]
      lko = lk
      out = complk_cont(Y,piv,Pi,Mu,Si,k)

      lk = out$lk; Phi = out$Phi; L = out$L; pv = out$pv
      if(it/10 == round(it/10)) cat(sprintf("%11g",c(mod,k,start,it,lk,lk-lko,max(abs(par-paro)))),"\n",sep=" | ")
      lkv = c(lkv,lk)
      #print(proc.time()-time)
    }
    # Compute information matrix if required
    if(out_se){
      th = NULL
      th = c(th,as.vector(Mu))
      th = c(th,Si[upper.tri(Si,TRUE)])
      th = c(th,B%*%log(piv))
      if(mod==0) for(t in 2:TT) for(u in 1:k) th = c(th,C[,,u]%*%log(Pi[u,,t]))
      if(mod==1) for(u in 1:k) th = c(th,C[,,u]%*%log(Pi[u,,2]))

      th0 = th-10^-5/2
      #   browser()
      out = lk_obs_cont(th0,Bm,Cm,k,Y,TT,r,mod)
      lk0 = out$lk; sc0 = out$sc
      lth = length(th)
      scn = rep(0,lth)
      J = matrix(0,lth,lth)
      for(j in 1:lth){
        thj = th0; thj[j] = thj[j]+10^-5
        out = lk_obs_cont(thj,Bm,Cm,k,Y,TT,r,mod)
        scn[j] = (out$lk-lk0)/10^-5
        J[,j] = (out$sc-sc0)/10^-5
      }
      J = -(J+t(J))/2
      #    print(c(lk,lk0))
      #    print(round(cbind(scn,sc0,round(scn-sc0,4)),5))
      #  se = sqrt(diag(ginv(J)))
      Va = ginv(J)
      nMu = r*k
      nSi = r*(r+1)/2
      Va2 = Va[1:(nMu+nSi),1:(nMu+nSi)]
      se2 = sqrt(diag(Va2))

      Va = Va[-(1:(nMu+nSi)),-(1:(nMu+nSi))]
      Om = diag(piv)-tcrossprod(piv,piv)
      M = Om%*%Bm
      if(mod==0){
        for(t in 2:TT) for(u in 1:k){
          Om = diag(Pi[u,,t])-Pi[u,,t]%o%Pi[u,,t]
          M = blkdiag(M,Om%*%Cm[,,u])
        }
      }
      if(mod==1){
        for(u in 1:k){
          Om = diag(Pi[u,,2])-Pi[u,,2]%o%Pi[u,,2]
          M = blkdiag(M,Om%*%Cm[,,u])
        }
      }
      if(mod>1){
        for(u in 1:k){
          Om = diag(Pi[u,,2])-Pi[u,,2]%o%Pi[u,,2]
          M = blkdiag(M,Om%*%Cm[,,u])
        }
        for(u in 1:k){
          Om = diag(Pi[u,,mod+1])-Pi[u,,mod+1]%o%Pi[u,,mod+1]
          M = blkdiag(M,Om%*%Cm[,,u])
        }
      }
      M = as.matrix(M)
      Va = M%*%Va%*%t(M)
      dVa = diag(Va)
      if(any(dVa<0)) warning("Negative elements in the estimated variance-covariance matrix for the parameters estimates")
      se = sqrt(abs(dVa))
      # Divide parameters
      se = c(se2,se)
      seMu = se[1:nMu]
      seSi = se[nMu+(1:nSi)]
      sepiv = se[nMu+nSi+(1:k)]

      if(mod==0) sePi = se[nMu+nSi+k+(1:(k*k*(TT-1)))]
      if(mod==1) sePi = se[nMu+nSi+k+(1:(k*k))]
      if(mod>1) sePi = se[nMu+nSi+k+(1:(k*k*2))]
    }
    # Compute number of parameters
    np = (k-1)+k*r+r*(r+1)/2
    if(mod==0) np = np+(TT-1)*k*(k-1)
    if(mod==1) np = np+k*(k-1)
    if(mod>1) np = np+2*k*(k-1)
    aic = -2*lk+np*2
    bic = -2*lk+np*log(n)
    cat(sprintf("%11g",c(mod,k,start,it,lk,lk-lko,max(abs(par-paro)))),"\n",sep=" | ")
    # adjust output
    #	if(any(yv!=1)) V = V/yv

    lk = as.vector(lk)
    dimnames(Pi)=list(state=1:k,state=1:k,time=1:TT)
    #	dimnames(Mu) = list(dimnames(Y)[[3]],state=1:k)
    #	dimnames(Si) = list(dimnames(Y)[[3]],dimnames(Y)[[3]])
    if(r==1) dimnames(Mu) = list(item=1,state=1:k) else dimnames(Mu)=list(item=1:r,state=1:k)
    dimnames(Si)=list(item=1:r,item=1:r)

    out = list(lk=lk,piv=piv,Pi=Pi,Mu=Mu,Si=Si,np=np,aic=aic,bic=bic,lkv=lkv,V=V,call=match.call())
    if(miss) out$Y = Y
    if(out_se){
      seMu = matrix(seMu,r,k)
      seSi2 = matrix(0,r,r)
      seSi2[upper.tri(seSi2,TRUE)]=seSi
      seSi2[lower.tri(seSi2)]=seSi2[upper.tri(seSi2)]
      seSi = seSi2
      sePi0 = sePi
      sePi = array(0,c(k,k,TT))
      if(mod>1){
        sePi0 = array(sePi0,c(k,k,2))
        sePi0 = aperm(sePi0,c(2,1,3))
        sePi[,,2:mod] = sePi0[,,1]
        sePi[,,(mod+1):TT] = sePi0[,,2]
      } else {
        sePi[,,2:TT] = sePi0
        sePi = aperm(sePi,c(2,1,3))
      }
      dimnames(sePi) = list(state=1:k,state=1:k,time=1:TT)
      if(r==1) dimnames(seMu) = list(item=1,state=1:k) else dimnames(seMu)=list(item=1:r,state=1:k)

      out$sepiv = sepiv
      out$sePi = sePi
      out$seMu = seMu
      out$seSi = seSi
    }
    cat("------------|-------------|-------------|-------------|-------------|-------------|-------------|\n");
    class(out)="LMbasiccont"
    (out)
  }

est_lm_cov_latent_cont <-
  function(Y,X1=NULL,X2=NULL,yv = rep(1,nrow(Y)),k,start=0,tol=10^-8,maxit=1000,
           param="multilogit",Mu=NULL,Si=NULL,Be=NULL,Ga=NULL,output=FALSE,out_se=FALSE){

    warning("est_lm_cov_latent_cont function is no longer maintained. Please look at lmestCont function",call. = FALSE)
    # Fit the LM model for continuous outcomes with individual covariates in the distribution of the latent process
    #
    # INPUT:
    # Y = array of available continuous outcome (n x TT x r)
    # X1 = matrix of covariates affecting the initial probabilities
    # X2 = array of covariates affecting the transition probabilities
    # yv = vector of frequencies
    # k = number of latent states
    # start = initialization (0 = deterministic, 1 = random, 2 = initial values in input)
    # maxit = maximum number of iterations
    # param = type of parametrization for the transition probabilities:
    #         multilogit = standard multinomial logit for every row of the transition matrix
    #         difflogit  = multinomial logit based on the difference between two sets of parameters
    # Mu = conditional means of the response variables (if start=2)
    # Si = var-cov matrix common to all states (if start=2)
    # Be = parameters on the initial probabilities (if start=2)
    # Ga = parameters on the transition probabilities (if start=2)
    # output = to  additional output

    # Preliminaries
    check_der = FALSE # to check score and info
    sY = dim(Y)
    n = sY[1]
    TT = sY[2]
    if(length(sY)==2) r = 1
    else r = sY[3]
    if(is.data.frame(Y)) warning("Data frame not allowed for Y")
    if(!is.null(X1)) if(any(is.na(X1))) stop("missing data not allowed in X1")
    if(!is.null(X2)) if(any(is.na(X2))) stop("missing data not allowed in X2")

    Yv = matrix(Y,n*TT,r)

    ## Check and inpute for missing data

    miss = any(is.na(Yv))
    if(miss){
      Yv = cbind(1,Yv)
      pYv = prelim.mix(Yv,1)
      thhat = em.mix(prelim.mix(Yv,1))
      rngseed(1)
      Yv = imp.mix(pYv, da.mix(pYv,thhat,steps=100), Yv)[,-1]
      Y = array(Yv,c(n,TT,r))
      cat("Missing data in the dataset. imp.mix function (mix package) used for imputation.\n")
    }

    # Covariate structure and related matrices: initial probabilities
    if(k == 2){
      GBe = as.matrix(c(0,1))
    }else{
      GBe = diag(k); GBe = GBe[,-1]
    }
    if(is.null(X1)){
      nc1=0
      Xlab = rep(1,n)
      nameBe = NULL
    }else{
      if(is.vector(X1)) X1 = matrix(X1,n,1)
      nc1 = dim(X1)[2] # number of covariates on the initial probabilities
      if(n!= dim(X1)[1]) stop("dimension mismatch between S and X1")
      nameBe = colnames(X1)
      out = aggr_data(X1)
      Xdis = out$data_dis
      if(nc1==1) Xdis = matrix(Xdis,length(Xdis),1)
      Xlab = out$label
    }
    Xndis = max(Xlab)
    XXdis = array(0,c(k,(k-1)*(nc1+1),Xndis))
    for(i in 1:Xndis){
      if(nc1==0) xdis = 1 else xdis = c(1,Xdis[i,])
      XXdis[,,i] = GBe%*%(diag(k-1)%x%t(xdis))
    }

#---- only one state ----
    if(k==1){
      Y1 = matrix(Y,n*TT,r)
      Mu = colMeans(Y1)
      Tmp = Y1-rep(1,n*TT)%o%Mu
      Si = t(Tmp)%*%Tmp/(n*TT)
      lk = sum(dmvnorm(Y1,Mu,Si,log=TRUE))
      np = r+r*(r+1)/2
      aic = -2*lk+2*np
      bic = -2*lk+log(n)*np
      lkv = lk
      Be = Ga = NULL
      out = list(lk=lk,Be=Be,Ga=Ga,Mu=Mu,Si=Si,np=np,aic=aic,bic=bic,lkv=lkv,
                 call=match.call(),param=param)
      return(out)
    }
    
    # for the transition probabilities
    if(is.null(X2)){
      if(param=="difflogit"){
        warning("with X2=NULL parametrization difflogit not considered")
        param="multilogit"
      }
      nc2 = 0
      Zlab = rep(1,n*(TT-1))
      nameGa = NULL
      Zndis = max(Zlab)
    }else{
      dimX2 <- dim(X2)[2]
      if(is.null(dimX2))
      {
        dimX2 <- 1
      }
      if(TT==2)
      {
        X2 = array(X2,c(n,1,dimX2))
      }
      if(is.matrix(X2)) X2 = array(X2,c(n,TT-1,1))
      nc2 = dim(X2)[3] # number of covariates on the transition probabilities
      if(n!= dim(X2)[1]) stop("dimension mismatch between S and X2")
      nameGa = colnames(aperm(X2,c(1,3,2)))
      Z = NULL
      for(t in 1:(TT-1)) Z = rbind(Z,X2[,t,])
      if(nc2==1) Z = as.vector(X2)
      out = aggr_data(Z); Zdis = out$data_dis; Zlab = out$label; Zndis = max(Zlab)
      if(nc2==1) Zdis=matrix(Zdis,length(Zdis),1)
    }
    if(param=="multilogit"){
      ZZdis = array(0,c(k,(k-1)*(nc2+1),Zndis,k))
      for(h in 1:k){
        if(k==2){
          if(h == 1) GGa = as.matrix(c(0,1)) else GGa = as.matrix(c(1,0))
        }else{
          GGa = diag(k); GGa = GGa[,-h]
        }
        for(i in 1:Zndis){
          if(nc2==0) zdis = 1 else zdis = c(1,Zdis[i,])
          ZZdis[,,i,h] = GGa%*%(diag(k-1)%x%t(zdis))
        }
      }
    }else if(param=="difflogit"){
      Zlab = (((Zlab-1)*k)%x%rep(1,k))+rep(1,n*(TT-1))%x%(1:k)
      ZZdis = array(0,c(k,k*(k-1)+(k-1)*nc2,Zndis*k))
      j = 0
      for(i in 1:Zndis){
        for(h in 1:k){
          j = j+1
          if(k==2){
            if(h == 1) GGa = as.matrix(c(0,1)) else GGa = as.matrix(c(1,0))
          }else{
            GGa = diag(k); GGa = GGa[,-h]
          }
          u = matrix(0,1,k); u[1,h] = 1
          U = diag(k); U[,h] = U[,h]-1
          U = U[,-1]
          ZZdis[,,j] = cbind(u%x%GGa,U%x%t(Zdis[i,]))
        }
      }
    }

    # When there is just 1 latent class
    if(k == 1){
      Piv = rep(1,n); Pi = 1
      yvv = rep(yv,TT)
      Mu = colSums(yvv*Yv)/sum(yvv)
      Di = Yv-rep(1,n*TT)%o%Mu
      Si = t(Di)%*%(yvv*Di)/sum(yvv)
      lk = sum(yvv*dmvnorm(Yv,Mu,Si,log=TRUE))
      np = k*r+r*(r+1)/2
      aic = -2*lk+np*2
      bic = -2*lk+np*log(n)
      out = list(lk=lk,Piv=Piv,Pi=Pi,Mu=Mu,Si=Si,np=np,aic=aic,bic=bic,lkv=NULL,V=NULL,call=match.call())
      class(out)="LMlatentcont"
      (out)
    }
    # Starting values: deterministic initialization
    if(start == 0){
      yvv = rep(yv,TT)
      mu = colSums(yvv*Yv)/sum(yvv)
      Di = Yv-rep(1,n*TT)%o%mu
      Si = t(Di)%*%(yvv*Di)/sum(yvv)
      std = sqrt(diag(Si))
      qt = qnorm((1:k)/(k+1))
      Mu = matrix(0,r,k)
      for(u in 1:k) Mu[,u] = qt[u]*std+mu
      # parameters on initial probabilities
      be = array(0,(nc1+1)*(k-1))
      out = prob_multilogit(XXdis,be,Xlab)
      Piv = out$P; Pivdis = out$Pdis
      # parameters on transition probabilities
      if(param=="multilogit"){
        Ga = matrix(0,(nc2+1)*(k-1),k)
        Ga[1+(0:(k-2))*(nc2+1),] = -log(10)
        PIdis = array(0,c(Zndis,k,k)); PI = array(0,c(k,k,n,TT))
        for(h in 1:k){
          tmp = ZZdis[,,,h]
          if(nc2==0) tmp = array(tmp,c(k,(k-1),Zndis))
          out = prob_multilogit(tmp,Ga[,h],Zlab)
          PIdis[,,h] = out$Pdis; PI[h,,,2:TT] = array(as.vector(t(out$P)),c(1,k,n,TT-1))
        }
      }else if(param=="difflogit"){
        Ga = matrix(0,k*(k-1)+(k-1)*nc2)
        Ga[1:((h-1)*k)] = -log(10)
        PI = array(0,c(k,k,n,TT))
        out = prob_multilogit(ZZdis,Ga,Zlab)
        PIdis = out$Pdis; PI[,,,2:TT] = array(as.vector(t(out$P)),c(k,k,n,TT-1))
        PI = aperm(PI,c(2,1,3,4))
      }
    }

    # random initialization
    if(start==1){
      Mu = matrix(0,r,k)
      mu = colMeans(Yv)
      Si = cov(Yv)
      for(u in 1:k) Mu[,u] = rmvnorm(1,mu,Si)
      # parameters on initial probabilities
      be = c(rnorm(1),rep(0,nc1))
      if(k>2) for(h in 2:(k-1)) be = c(be,rnorm(1),rep(0,nc1))
      out = prob_multilogit(XXdis,be,Xlab)
      Piv = out$P; Pivdis = out$Pdis
      # parameters on transition probabilities
      if(param=="multilogit"){
        #	Ga = matrix(-abs(rnorm((nc2+1)*(k-1),k)),(nc2+1)*(k-1),k)/2
        Ga = matrix(0,(nc2+1)*(k-1),k)
        Ga[1+(0:(k-2))*(nc2+1),] = -abs(rnorm((k-1)))
        PIdis = array(0,c(Zndis,k,k)); PI = array(0,c(k,k,n,TT))
        for(h in 1:k){
          tmp = ZZdis[,,,h]
          if(nc2==0) tmp = array(tmp,c(k,(k-1),Zndis))
          out = prob_multilogit(tmp,Ga[,h],Zlab)
          PIdis[,,h] = out$Pdis; PI[h,,,2:TT] = array(as.vector(t(out$P)),c(1,k,n,TT-1))
        }
      }else if(param=="difflogit"){
        Ga = c(-abs(rnorm(k*(k-1))),rep(0,(k-1)*nc2))
        PI = array(0,c(k,k,n,TT))
        out = prob_multilogit(ZZdis,Ga,Zlab)
        PIdis = out$Pdis; PI[,,,2:TT] = array(as.vector(t(out$P)),c(k,k,n,TT-1))
        PI = aperm(PI,c(2,1,3,4))
      }
    }
    # initialization as input
    if(start==2){
      # parameters on initial probabilities
      be = as.vector(Be)
      out = prob_multilogit(XXdis,be,Xlab)
      Piv = out$P; Pivdis = out$Pdis
      # parameters on transition probabilities
      if(param=="multilogit"){
        if(is.list(Ga)) stop("invalid mode (list) for Ga")
        Ga = matrix(Ga,(nc2+1)*(k-1),k)
        PIdis = array(0,c(Zndis,k,k)); PI = array(0,c(k,k,n,TT))
        for(h in 1:k){
          out = prob_multilogit(ZZdis[,,,h],Ga[,h],Zlab)
          PIdis[,,h] = out$Pdis; PI[h,,,2:TT] = array(as.vector(t(out$P)),c(1,k,n,TT-1))
        }
      }else if(param=="difflogit"){
        if(is.list(Ga)) Ga = c(as.vector(t(Ga[[1]])),as.vector(Ga[[2]]))
        if(length(Ga)!=k*(k-1)+(k-1)*nc2) stop("invalid dimensions for Ga")
        PI = array(0,c(k,k,n,TT))
        out = prob_multilogit(ZZdis,Ga,Zlab)
        PIdis = out$Pdis; PI[,,,2:TT] = array(as.vector(t(out$P)),c(k,k,n,TT-1))
        PI = aperm(PI,c(2,1,3,4))
      }
    }

    ###### standard EM #####
    out = lk_comp_latent_cont(Y,yv,Piv,PI,Mu,Si,k)
    lk = out$lk; Phi = out$Phi; L = out$L; pv = out$pv
    # 	if(is.nan(lk)) browser()
    it = 0; lko = lk-10^10; lkv = NULL
    par = c(as.vector(Piv),as.vector(PI),as.vector(Mu),as.vector(Si))
    if(any(is.na(par))) par = par[-which(is.na(par))]
    paro = par
    # Iterate until convergence
    # display output
    cat("------------|-------------|-------------|-------------|-------------|-------------|\n");
    cat("      k     |    start    |     step    |     lk      |    lk-lko   | discrepancy |\n");
    cat("------------|-------------|-------------|-------------|-------------|-------------|\n");
    cat(sprintf("%11g",c(k,start,0,lk)),"\n",sep=" | ")
    #cat("",sprintf("%11g",c(0,lk)),"\n",sep=" | ")
    while((lk-lko)/abs(lk)>tol & it<maxit){
      Mu0 = Mu; Si0 = Si; Piv0 = Piv; PI0 = PI
      it = it+1
      # ---- E-step ----
      # Compute V and U
      out = prob_post_cov_cont(Y,yv,Mu,Si,Piv,PI,Phi,L,pv)
      U = out$U; V = out$V
      # If required store parameters
      # ---- M-step ----
      # Update Mu
      Vv = matrix(aperm(V,c(1,3,2)),n*TT,k)
      for(u in 1:k) Mu[,u] = (t(Yv)%*%Vv[,u])/sum(Vv[,u])
      # Update Si
      Si = matrix(0,r,r)
      for(u in 1:k){
        Tmp = Yv-rep(1,n*TT)%o%Mu[,u]
        Si = Si+ t(Tmp)%*%(Vv[,u]*Tmp)
      }
      Si = Si/(sum(yv)*TT)
      # Update piv
      out = est_multilogit(V[,,1],XXdis,Xlab,be,Pivdis)
      be = out$be; Pivdis = out$Pdi; Piv = out$P
      # Update Pi
      if(param=="multilogit"){
        for(h in 1:k){
          UU = NULL
          for(t in 2:TT) UU = rbind(UU,t(U[h,,,t]))
          tmp = ZZdis[,,,h]
          if(nc2==0) tmp = array(tmp,c(k,(k-1),Zndis))
          tmp2 = PIdis[,,h]
          if(Zndis==1) tmp2 = matrix(tmp2,1,k)
          out = est_multilogit(UU,tmp,Zlab,Ga[,h],tmp2)
          PIdis[,,h] = out$Pdis; PI[h,,,2:TT] = array(as.vector(t(out$P)),c(1,k,n,TT-1)); Ga[,h] = out$be
        }
      }else if(param=="difflogit"){
        Tmp = aperm(U[,,,2:TT,drop=FALSE],c(1,3,4,2))
        Tmp = matrix(Tmp,n*k*(TT-1),k)
        out = est_multilogit(Tmp,ZZdis,Zlab,Ga,PIdis)
        PIdis = out$Pdis; Ga = out$be
        Tmp = array(out$P,c(k,n,TT-1,k))
        PI[,,,2:TT] = aperm(Tmp,c(1,4,2,3))
      }
      # Compute log-likelihood
      paro = par; par = c(as.vector(Piv),as.vector(PI),as.vector(Mu),as.vector(Si))
      if(any(is.na(par))) par = par[-which(is.na(par))]
      lko = lk
      out = lk_comp_latent_cont(Y,yv,Piv,PI,Mu,Si,k)
      lk = out$lk; Phi = out$Phi; L = out$L; pv = out$pv
      # Display output
      if(it/10 == floor(it/10)){
        #cat("",sprintf("%11g",c(it,lk,lk-lko,max(abs(par-paro)))),"\n",sep=" | ")
        cat(sprintf("%11g",c(k,start,it,lk,lk-lko,max(abs(par-paro)))),"\n",sep=" | ")
      }
      lkv = c(lkv,lk)
    }
    if(it/10 > floor(it/10))  cat(sprintf("%11g",c(k,start,it,lk,lk-lko,max(abs(par-paro)))),"\n",sep=" | ")

    if(out_se){
      th = NULL
      th = c(th,as.vector(Mu))
      th = c(th,Si[upper.tri(Si,TRUE)])
      th = c(th, be)
      if(param=="multilogit"){
        for(h in 1:k) th = c(th, Ga[,h])
      }else if(param=="difflogit") th = c(th,Ga)
      #	  th0 = th-10^-5/2
      out = lk_obs_latent_cont(th,Y,yv,XXdis,Xlab,ZZdis,Zlab,param)
      lk0 = out$lk; sc0 = out$sc
      lth = length(th)
      scn = rep(0,lth); Fn = matrix(0,lth,lth)
      for(h in 1:lth){
        thh = th; thh[h] = thh[h]+10^-5
        outh = lk_obs_latent_cont(thh,Y,yv,XXdis,Xlab,ZZdis,Zlab,param)
        scn[h] = (outh$lk-lk0)/10^-5
        Fn[,h] = (outh$sc-sc0)/10^-5
      }
      # print(round(cbind(sc0,scn,sc0-scn),4))

      J = -(Fn+t(Fn))/2
      iJ = ginv(J)
      se = sqrt(diag(iJ))

      nMu = r*k
      nSi = r*(r+1)/2
      nbe = (1+nc1)*(k-1)
      if(param=="multilogit") nga=(1+nc2)*(k-1)*k else if(param=="difflogit") nga=(k+nc2)*(k-1)
      seMu = se[1:nMu]
      seSi = se[nMu+(1:nSi)]
      sebe = se[nMu+nSi+(1:nbe)]
      sega = se[nMu+nSi+nbe+(1:nga)]
    }

    # Compute number of parameters
    np = k*r+r*(r+1)/2
    np = np+(k-1)*(nc1+1)
    if(param=="multilogit") np = np+(k-1)*(nc2+1)*k else if(param=="difflogit")  np = np+(k-1)*(nc2+k)
    aic = -2*lk+np*2
    bic = -2*lk+np*log(n)
    #	out = list(lk=lk,piv=piv,Pi=Pi,Psi=Psi,np=np,aic=aic,bic=bic,lkv=lkv,J=J,V=V1,th=th,sc=sc)
    # local decoding
    Ul = matrix(0,n,TT)
    for(i in 1:n) for(t in 1:TT){
      Ul[i,t] = which.max(V[i,,t])
    }

    Be = matrix(be,nc1+1,k-1)
    if (is.null(nameBe)){
      if(nc1==0) nameBe = c("Intercept") else nameBe = c("intercept",paste("X1",1:nc1,sep=""))
    }else{
      nameBe = c("intercept",nameBe)
    }

    dimnames(Be) = list(nameBe,logit=2:k)
    if(out_se) {seBe = matrix(sebe,nc1+1,k-1); dimnames(seBe) = list(nameBe,logit=2:k)}
    if(param=="multilogit"){
      if(is.null(nameGa)){
        if(nc2==0) nameGa = c("Intercept") else nameGa = c("intercept", paste("X2",1:nc2,sep=""))
      }else{
        nameGa = c("intercept",nameGa)
      }
      if(k>2) {
        Ga = array(as.vector(Ga),c(nc2+1,k-1,k))
        dimnames(Ga) = list(nameGa,logit=2:k,logit=1:k)
      }else if(k==2){

        dimnames(Ga) = 	list(nameGa,logit=1:k)
      }
      if(out_se){
        if(k==2){
          seGa = matrix(sega,nc2+1,2)
          dimnames(seGa) = list(nameGa,logit=1:k)
        }else if(k>2){
          seGa = array(as.vector(sega),c(nc2+1,k-1,k))
          dimnames(seGa) = list(nameGa,logit=2:k,logit=1:k)
        }
      }
    }else if(param=="difflogit"){
      Ga0 = Ga
      Ga = vector("list",2)
      seGa = vector("list",2)
      Ga[[1]] = t(matrix(Ga0[1:(k*(k-1))],k-1,k))
      Ga[[2]] = matrix(Ga0[(k*(k-1))+(1:((k-1)*nc2))],nc2,k-1)
      if(is.null(nameGa)){
        nameGa2 = paste("X2",1:nc2,sep="")
      }else{
        nameGa2 = nameGa
      }
      if (k==2) {
        dimnames(Ga[[1]]) = list(intercept=1:k,logit=k)
        dimnames(Ga[[2]])=list(nameGa2,logit=k)
      } else if (k>2){
        dimnames(Ga[[1]]) = list(intercept=1:k,logit=2:k)
        dimnames(Ga[[2]])=list(nameGa2,logit=2:k)
      }
      if(out_se){
        seGa[[1]] = t(matrix(sega[1:(k*(k-1))],k-1,k))
        seGa[[2]] = matrix(sega[(k*(k-1))+(1:((k-1)*nc2))],nc2,k-1)
        if(k==2){
          dimnames(seGa[[1]]) = list(intercept=1:k,logit=k)
          dimnames(seGa[[2]])=list(nameGa2,logit=k)
        }else if (k>2){
          dimnames(seGa[[1]]) = list(intercept=1:k,logit=2:k)
          dimnames(seGa[[2]])=list(nameGa2,logit=2:k)
        }
      }
    }
  # adjust output
    lk = as.vector(lk)
    if(output){
      dimnames(Piv)=list(subject=1:n,state=1:k)
      dimnames(PI)=list(state=1:k,state=1:k,subject=1:n,time=1:TT)
    }
    if(r==1) dimnames(Mu) = list(item=1,state=1:k) else dimnames(Mu)=list(item=1:r,state=1:k)
    dimnames(Si)=list(item=1:r,item=1:r)
    out = list(lk=lk,Be=Be,Ga=Ga,Mu=Mu,Si=Si,np=np,aic=aic,bic=bic,lkv=lkv,
               call=match.call(),param=param)
    if(out_se){
      seMu = matrix(seMu,r,k)
      if(r==1) dimnames(seMu) = list(item=1,state=1:k) else dimnames(seMu)=list(item=1:r,state=1:k)
      seSi2 = matrix(0,r,r)
      seSi2[upper.tri(seSi2,TRUE)]=seSi
      seSi2[lower.tri(seSi2)]=seSi2[upper.tri(seSi2)]
      seSi = seSi2
      dimnames(seSi)=list(item=1:r,item=1:r)
      out$seMu = seMu
      out$seSi = seSi
      out$seBe = seBe
      out$seGa = seGa
    }
    # final output
    if(miss) out$Y = Y
    if(output){
      out$PI = PI
      out$Piv = Piv
      out$Ul = Ul
    }
    #cat(" |-------------|-------------|-------------|-------------|\n");
    cat("------------|-------------|-------------|-------------|-------------|-------------|\n");
    class(out)="LMlatentcont"
    (out)
  }

est_lm_cov_latent <-
  function(S,X1=NULL,X2=NULL,yv=rep(1,nrow(S)),k,start=0,tol=10^-8,maxit=1000,param="multilogit",
           Psi,Be,Ga,fort=TRUE,output=FALSE,out_se=FALSE,fixPsi=FALSE){

    warning("est_lm_cov_latent function is no longer maintained. Please look at lmest function",call. = FALSE)
    # Fit the LM model with individual covariates in the distribution of the latent process
    #
    # INPUT:
    # S = array of available configurations (n x TT x r)
    # X1 = matrix of covariates affecting the initial probabilities
    # X2 = array of covariates affecting the transition probabilities
    # Psi = conditional response probabilities
    # Be = parameters on the initial probabilities (if start=2)
    # Ga = parameters on the transition probabilities (if start=2)
    # start = initialization (0 = deterministic, 1 = random, 2 = initial values in input)
    # param = type of parametrization for the transition probabilities:
    #         multilogit = standard multinomial logit for every row of the transition matrix
    #         difflogit  = multinomial logit based on the difference between two sets of parameters
    # fort   = fortran use (FALSE for not use fortran)
    # output = to  additional output
    # out_se  = TRUE for computing the information and standard errors
    # fixPsi = TRUE if Psi is given in input and is not updated anymore

    # Preliminaries
    check_der = FALSE # to check score and info
    if(fort!=TRUE) fort = FALSE
    sS = dim(S)
    ns = sS[1]
    TT = sS[2]
    n = sum(yv)
    if(length(sS)==2) r = 1
    else r = sS[3]
    if(min(S,na.rm=TRUE)>0){
      cat("|------------------- WARNING -------------------|\n")
      cat("|The first response category must be coded as 0 |\n")
      cat("|-----------------------------------------------|\n")
    }
    if(is.data.frame(S)){
      warning("Data frame not allowed for S")
    }
    if(ns!=length(yv)) stop("dimensions mismatch between S and yv")
    if(!is.null(X1)) if(any(is.na(X1))) stop("missing data not allowed in X1")
    if(!is.null(X2)) if(any(is.na(X2))) stop("missing data not allowed in X2")
    miss = any(is.na(S))
    if(miss){
      cat("Missing data in the dataset, treated as missing at random\n")
      R = 1 * (!is.na(S))
      S[is.na(S)] = 0
    }else{
      R = NULL
    }

    Sv = matrix(S,ns*TT,r)
    if(miss) Rv = matrix(R,ns*TT,r)

    if(r==1){
      if(is.matrix(S)) S = array(S,c(dim(S),1))
      b = max(S); mb = b; sb = b
      Co = cbind(-diag(b),diag(b))
      Ma = cbind(lower.tri(matrix(1,b,b), diag = TRUE),rep(0,b))
      Ma = rbind(Ma,1-Ma)
    }else{
      b = rep(0,r)
      for(j in 1:r) b[j] = max(S[,,j])
      mb = max(b); sb = sum(b)
      Matr = vector("list",r)
      for(j in 1:r){
        Matr[[j]]$Co = cbind(-diag(b[j]),diag(b[j]))
        Ma = cbind(lower.tri(matrix(1,b[j],b[j]), diag = TRUE),rep(0,b[j]))
        Matr[[j]]$Ma = rbind(Ma,1-Ma)
      }
    }
    th = NULL; sc = NULL
    J = NULL

    # Covariate structure and related matrices: initial probabilities
    if(k == 2) GBe = as.matrix(c(0,1)) else{
      GBe = diag(k); GBe = GBe[,-1]
    }
    if(is.null(X1)){
      nc1=0
      Xlab = rep(1,ns)
      nameBe = NULL
    }else{
      if(is.vector(X1)) X1 = matrix(X1,ns,1)
      nc1 = dim(X1)[2] # number of covariates on the initial probabilities
      if(ns!= dim(X1)[1]) stop("dimension mismatch between S and X1")

      nameBe = colnames(X1)

      out = aggr_data(X1,fort=fort)
      Xdis = out$data_dis
      if(nc1==1) Xdis = matrix(Xdis,length(Xdis),1)
      Xlab = out$label
    }
    Xndis = max(Xlab)
    XXdis = array(0,c(k,(k-1)*(nc1+1),Xndis))
    for(i in 1:Xndis){
      if(nc1==0) xdis = 1 else xdis = c(1,Xdis[i,])
      XXdis[,,i] = GBe%*%(diag(k-1)%x%t(xdis))
    }


    # for the transition probabilities
    if(is.null(X2)){
      if(param=="difflogit"){
        warning("with X2=NULL parametrization difflogit not considered")
        param="multilogit"
      }
      nc2 = 0
      Zlab = rep(1,ns*(TT-1))
      nameGa = NULL
      Zndis = max(Zlab)
    }else{
      if(TT==2) X2 = array(X2,c(ns,1,dim(X2)[2]))
      if(is.matrix(X2)) X2 = array(X2,c(ns,TT-1,1))
      nc2 = dim(X2)[3] # number of covariates on the transition probabilities
      if(ns!= dim(X2)[1]) stop("dimension mismatch between S and X2")

      nameGa = colnames(aperm(X2,c(1,3,2)))
      Z = NULL
      for(t in 1:(TT-1)) Z = rbind(Z,X2[,t,])
      if(nc2==1) Z = as.vector(X2)
      out = aggr_data(Z,fort=fort); Zdis = out$data_dis; Zlab = out$label; Zndis = max(Zlab)
      if(nc2==1) Zdis=matrix(Zdis,length(Zdis),1)
    }
    if(param=="multilogit"){
      ZZdis = array(0,c(k,(k-1)*(nc2+1),Zndis,k))
      for(h in 1:k){
        if(k==2){
          if(h == 1) GGa = as.matrix(c(0,1)) else GGa = as.matrix(c(1,0))
        }else{
          GGa = diag(k); GGa = GGa[,-h]
        }
        for(i in 1:Zndis){
          if(nc2==0) zdis = 1 else zdis = c(1,Zdis[i,])
          ZZdis[,,i,h] = GGa%*%(diag(k-1)%x%t(zdis))
        }
      }
    }else if(param=="difflogit"){
      Zlab = (((Zlab-1)*k)%x%rep(1,k))+rep(1,ns*(TT-1))%x%(1:k)
      ZZdis = array(0,c(k,k*(k-1)+(k-1)*nc2,Zndis*k))
      j = 0
      for(i in 1:Zndis){
        for(h in 1:k){
          j = j+1
          if(k==2){
            if(h == 1) GGa = as.matrix(c(0,1)) else GGa = as.matrix(c(1,0))
          }else{
            GGa = diag(k); GGa = GGa[,-h]
          }
          u = matrix(0,1,k); u[1,h] = 1
          U = diag(k); U[,h] = U[,h]-1
          U = U[,-1]
          ZZdis[,,j] = cbind(u%x%GGa,U%x%t(Zdis[i,]))
        }
      }
    }

    # for information matrix
    if(out_se){
      Am = vector("list",r)
      for(j in 1:r) Am[[j]] = rbind(rep(0,b[j]),diag(b[j]))
    }
    # When there is just 1 latent class
    if(k == 1){
      Piv = rep(1,n); Pi = 1
      P = matrix(0,mb+1,r)
      for(t in 1:TT){
        for(j in 1:r){
          for(y in 0:b[j]){
            ind = which(S[,t,j]==y)
            P[y+1,j] = P[y+1,j]+sum(yv[ind])
          }
        }
      }
      Psi = P/(n*TT)
      pm = rep(1,ns)
      if (miss) for(t in 1:TT) for(j in 1:r)  pm = pm*(Psi[S[,t,j]+1,j]*R[,t,j]+(1-R[,t,j]))
      else for(t in 1:TT) for(j in 1:r)  pm = pm*Psi[S[,t,j]+1,j]

      lk = sum(yv*log(pm))
      if(r==1) np = k*mb*r else np = k*sum(b)
      aic = -2*lk+np*2
      bic = -2*lk+np*log(n)
      out = list(lk=lk,Piv=Piv,Pi=Pi,Psi=Psi,np=np,aic=aic,bic=bic,lkv=NULL,V=NULL,call=match.call())
      class(out)="LMlatent"
      (out)
    }
    time = proc.time()
    # Starting values: deterministic initialization
    if(start == 0){
      if(fixPsi==FALSE){
        P = matrix(NA,mb+1,r)
        for(j in 1:r) P[1:(b[j]+1),j] = 0
        for(t in 1:TT) for(j in 1:r) for(y in 0:mb){
          ind = which(S[,t,j]==y)
          if(miss) P[y+1,j] = P[y+1,j]+sum(t(R[ind,t,j])%*%yv[ind])
          else P[y+1,j] = P[y+1,j]+sum(yv[ind])
        }
        if(r==1) E = Co%*%log(Ma%*%P) else{
          E = matrix(NA,mb,r)
          for(j in 1:r){
            Co = Matr[[j]]$Co; Ma = Matr[[j]]$Ma
            E[1:b[j],j] = Co%*%log(Ma%*%P[1:(b[j]+1),j])
          }
        }
        Psi = array(NA,c(mb+1,k,r)); Eta = array(NA,c(mb,k,r))
        grid = seq(-k,k,2*k/(k-1))
        for(c in 1:k){
          for(j in 1:r){
            etac = E[1:b[j],j]+grid[c]
            Eta[1:b[j],c,j] = etac
            Psi[1:(b[j]+1),c,j] = invglob(etac)
          }
        }
      }
      # parameters on initial probabilities
      be = array(0,(nc1+1)*(k-1))
      out = prob_multilogit(XXdis,be,Xlab,fort)
      Piv = out$P; Pivdis = out$Pdis
      # parameters on transition probabilities
      if(param=="multilogit"){
        Ga = matrix(0,(nc2+1)*(k-1),k)
        Ga[1+(0:(k-2))*(nc2+1),] = -log(10)
        PIdis = array(0,c(Zndis,k,k)); PI = array(0,c(k,k,ns,TT))
        for(h in 1:k){
          tmp = ZZdis[,,,h]
          if(nc2==0) tmp = array(tmp,c(k,(k-1),Zndis))
          out = prob_multilogit(tmp,Ga[,h],Zlab,fort)
          PIdis[,,h] = out$Pdis; PI[h,,,2:TT] = array(as.vector(t(out$P)),c(1,k,ns,TT-1))
        }

      }else if(param=="difflogit"){
        Ga = matrix(0,k*(k-1)+(k-1)*nc2)
        Ga[1:((h-1)*k)] = -log(10)
        PI = array(0,c(k,k,ns,TT))
        out = prob_multilogit(ZZdis,Ga,Zlab,fort)
        PIdis = out$Pdis; PI[,,,2:TT] = array(as.vector(t(out$P)),c(k,k,ns,TT-1))
        PI = aperm(PI,c(2,1,3,4))
      }
    }

    # random initialization
    if(start==1){
      if(fixPsi==FALSE){
        Psi = array(NA,c(mb+1,k,r))
        # for(j in 1:r) Psi[1:(b[j]+1),,j] = 0
        for(j in 1:r){
          Psi[1:(b[j]+1),,j] = matrix(runif((b[j]+1)*k),b[j]+1,k)
          for(c in 1:k) Psi[1:(b[j]+1),c,j] = Psi[1:(b[j]+1),c,j]/sum(Psi[1:(b[j]+1),c,j])
        }
      }
      # parameters on initial probabilities
      be = c(rnorm(1),rep(0,nc1))
      if(k>2) for(h in 2:(k-1)) be = c(be,rnorm(1),rep(0,nc1))
      out = prob_multilogit(XXdis,be,Xlab,fort)
      Piv = out$P; Pivdis = out$Pdis
      # parameters on transition probabilities
      if(param=="multilogit"){
        #	Ga = matrix(-abs(rnorm((nc2+1)*(k-1),k)),(nc2+1)*(k-1),k)/2
        Ga = matrix(0,(nc2+1)*(k-1),k)
        Ga[1+(0:(k-2))*(nc2+1),] = -abs(rnorm((k-1)))
        PIdis = array(0,c(Zndis,k,k)); PI = array(0,c(k,k,ns,TT))
        for(h in 1:k){
          tmp = ZZdis[,,,h]
          if(nc2==0) tmp = array(tmp,c(k,(k-1),Zndis))
          out = prob_multilogit(tmp,Ga[,h],Zlab,fort)
          PIdis[,,h] = out$Pdis; PI[h,,,2:TT] = array(as.vector(t(out$P)),c(1,k,ns,TT-1))
        }
      }else if(param=="difflogit"){
        Ga = c(-abs(rnorm(k*(k-1))),rep(0,(k-1)*nc2))
        PI = array(0,c(k,k,ns,TT))
        out = prob_multilogit(ZZdis,Ga,Zlab,fort)
        PIdis = out$Pdis; PI[,,,2:TT] = array(as.vector(t(out$P)),c(k,k,ns,TT-1))
        PI = aperm(PI,c(2,1,3,4))
      }
    }
    # initialization as input
    if(start==2){
      # parameters on initial probabilities
      be = as.vector(Be)
      out = prob_multilogit(XXdis,be,Xlab,fort)
      Piv = out$P; Pivdis = out$Pdis
      # parameters on transition probabilities
      if(param=="multilogit"){
        if(is.list(Ga)) stop("invalid mode (list) for Ga")
        Ga = matrix(Ga,(nc2+1)*(k-1),k)
        PIdis = array(0,c(Zndis,k,k)); PI = array(0,c(k,k,ns,TT))
        for(h in 1:k){
          out = prob_multilogit(ZZdis[,,,h],Ga[,h],Zlab,fort)
          PIdis[,,h] = out$Pdis; PI[h,,,2:TT] = array(as.vector(t(out$P)),c(1,k,ns,TT-1))
        }
      }else if(param=="difflogit"){
        if(is.list(Ga)) Ga = c(as.vector(t(Ga[[1]])),as.vector(Ga[[2]]))
        if(length(Ga)!=k*(k-1)+(k-1)*nc2) stop("invalid dimensions for Ga")
        PI = array(0,c(k,k,ns,TT))
        out = prob_multilogit(ZZdis,Ga,Zlab,fort)
        PIdis = out$Pdis; PI[,,,2:TT] = array(as.vector(t(out$P)),c(k,k,ns,TT-1))
        PI = aperm(PI,c(2,1,3,4))
      }
    }

    ###### standard EM #####
    out = lk_comp_latent(S,R,yv,Piv,PI,Psi,k,fort=fort)
    lk = out$lk; Phi = out$Phi; L = out$L; pv = out$pv
    # 	if(is.nan(lk)) browser()
    it = 0; lko = lk-10^10; lkv = NULL
    par = c(as.vector(Piv),as.vector(PI),as.vector(Psi))
    if(any(is.na(par))) par = par[-which(is.na(par))]
    paro = par
    # Iterate until convergence
    # display output
    cat("------------|-------------|-------------|-------------|-------------|-------------|\n");
    cat("      k     |    start    |     step    |     lk      |    lk-lko   | discrepancy |\n");
    cat("------------|-------------|-------------|-------------|-------------|-------------|\n");
    cat(sprintf("%11g",c(k,start,0,lk)),"\n",sep=" | ")
    #cat("",sprintf("%11g",c(0,lk)),"\n",sep=" | ")
    while((lk-lko)/abs(lk)>tol & it<maxit){
      Psi0 = Psi; Piv0 = Piv; PI0 = PI
      it = it+1
      # ---- E-step ----
      # Compute V and U
      out = prob_post_cov(S,yv,Psi,Piv,PI,Phi,L,pv,fort=fort)
      U = out$U; V = out$V
      # If required store parameters
      # ---- M-step ----
      # Update Psi
      if(fixPsi==FALSE){
        Y1 = array(NA,c(mb+1,k,r))
        for(j in 1:r) Y1[1:(b[j]+1)] = 0
        Vv = matrix(aperm(V,c(1,3,2)),ns*TT,k)
        for(j in 1:r) for(jb in 0:b[j]) {
          ind = which(Sv[,j]==jb)
          if(length(ind)==1){
            if(miss) Y1[jb+1,,j] = Vv[ind,]*Rv[ind,j]
            else Y1[jb+1,,j] = Vv[ind,]
          }
          if(length(ind)>1){
            if(miss) Y1[jb+1,,j] = colSums(Vv[ind,]*Rv[ind,j])
            else Y1[jb+1,,j] = colSums(Vv[ind,])
          }


        }
        for(j in 1:r) for(c in 1:k){
          tmp = Y1[1:(b[j]+1),c,j]
          if(any(is.na(tmp))) tmp[is.na(tmp)] = 0
          tmp = pmax(tmp/sum(tmp),10^-10)
          Psi[1:(b[j]+1),c,j] = tmp/sum(tmp)
        }
      }
      # Update piv
      out = est_multilogit(V[,,1],XXdis,Xlab,be,Pivdis,fort=fort)
      be = out$be; Pivdis = out$Pdi; Piv = out$P
      # Update Pi
      if(param=="multilogit"){
        for(h in 1:k){
          UU = NULL
          for(t in 2:TT) UU = rbind(UU,t(U[h,,,t]))
          tmp = ZZdis[,,,h]
          if(nc2==0) tmp = array(tmp,c(k,(k-1),Zndis))
          tmp2 = PIdis[,,h]
          if(Zndis==1) tmp2 = matrix(tmp2,1,k)
          out = est_multilogit(UU,tmp,Zlab,Ga[,h],tmp2,fort=fort)
          PIdis[,,h] = out$Pdis; PI[h,,,2:TT] = array(as.vector(t(out$P)),c(1,k,ns,TT-1)); Ga[,h] = out$be
        }
      }else if(param=="difflogit"){
        Tmp = aperm(U[,,,2:TT,drop=FALSE],c(1,3,4,2))
        Tmp = matrix(Tmp,ns*k*(TT-1),k)
        out = est_multilogit(Tmp,ZZdis,Zlab,Ga,PIdis,fort=fort)
        PIdis = out$Pdis; Ga = out$be
        Tmp = array(out$P,c(k,ns,TT-1,k))
        PI[,,,2:TT] = aperm(Tmp,c(1,4,2,3))
      }
      # Compute log-likelihood
      paro = par; par = c(as.vector(Piv),as.vector(PI),as.vector(Psi))
      if(any(is.na(par))) par = par[-which(is.na(par))]
      lko = lk;
      out = lk_comp_latent(S,R,yv,Piv,PI,Psi,k,fort=fort)
      lk = out$lk; Phi = out$Phi; L = out$L; pv = out$pv
      # Display output
      if(it/10 == floor(it/10)){
        #cat("",sprintf("%11g",c(it,lk,lk-lko,max(abs(par-paro)))),"\n",sep=" | ")
        cat(sprintf("%11g",c(k,start,it,lk,lk-lko,max(abs(par-paro)))),"\n",sep=" | ")
      }
      lkv = c(lkv,lk)
    }
    if(it/10 > floor(it/10))  cat(sprintf("%11g",c(k,start,it,lk,lk-lko,max(abs(par-paro)))),"\n",sep=" | ")
    #### compute infomation matrix ####
    if(out_se){
      dlPsi = array(NA,c(mb+1,k,r,k*sb))
      for(j in 1:r) dlPsi[1:(b[j]+1),,j,] = 0
      count = 0
      for(c in 1:k) for(j in 1:r){
        ind = count+(1:b[j])
        temp = pmax(Psi[1:(b[j]+1),c,j],10^-50)
        dlPsi[1:(b[j]+1),c,j,ind] = (diag(b[j]+1)-rep(1,b[j]+1)%o%temp)%*%Am[[j]]
        count = count+b[j]
        th = c(th,log(temp[-1]/temp[1]))
      }
      dlPiv = array(0,c(ns,k,(1+nc1)*(k-1)))
      for(j in 1:Xndis){
        temp = pmax(Pivdis[j,],10^-50)
        Temp = (diag(k)-rep(1,k)%o%temp)%*%XXdis[,,j]
        for(i in which(Xlab==j)) dlPiv[i,,] = Temp
      }
      th = c(th,be)

      count = 0
      if(param=="multilogit"){
        dlPI = array(0,c(k,k,ns*TT,(1+nc2)*(k-1)*k))
        temp0 = rep(1,k); Temp0 = diag(k)
        for(h in 1:k){
          ind = count+(1:((1+nc2)*(k-1)))
          for(j in 1:Zndis){
            temp = pmax(PIdis[j,,h],10^-50)
            Temp = (Temp0-temp0%o%temp)%*%ZZdis[,,j,h]
            for(i in which(Zlab==j)) dlPI[h,,ns+i,ind] = Temp
          }
          count = count+((1+nc2)*(k-1))
          th = c(th,Ga[,h])
        }
        dlPI = array(dlPI,c(k,k,ns,TT,(1+nc2)*(k-1)*k))
      }else if(param=="difflogit"){
        dlPI = array(0,c(k,k*ns*TT,(k+nc2)*(k-1)))
        temp0 = rep(1,k); Temp0 = diag(k)
        for(j in 1:(Zndis*k)){
          temp = pmax(PIdis[j,],10^-50)
          Temp = (Temp0-temp0%o%temp)%*%ZZdis[,,j]
          for(i in which(Zlab==j)) dlPI[,k*ns+i,] = Temp
        }
        #		    Ga = c(as.vector(t(Ga[[1]])),as.vector(Ga[[2]]))
        th = c(th,Ga)
        dlPI = array(dlPI,c(k,k,ns,TT,(k+nc2)*(k-1)))
        dlPI = aperm(dlPI,c(2,1,3,4,5))
      }

      # Compute log-likelihood
      lk2 = lk
      out = lk_comp_latent(S,R,yv,Piv,PI,Psi,k,der=TRUE,fort=fort,dlPsi=dlPsi,dlPiv=dlPiv,dlPI=dlPI)
      sc = out$dlk; dlL = out$dlL; dlPhi = out$dlPhi; dlL2 = out$dlL2; dlpv = out$dlpv
      lk = out$lk; Phi = out$Phi; L = out$L; pv = out$pv
      sc2 = sc;
      #      if(is.nan(lk)) browser()
      it = 0; lko = lk-10^10; lkv = NULL; dev = NULL
      # backward recursion
      out = prob_post_cov(S,yv,Psi,Piv,PI,Phi,L,pv,der=TRUE,fort=fort,dlPhi=dlPhi,dlPiv,
                          dlPI=dlPI,dlL=dlL,dlL2=dlL2,dlpv=dlpv)
      U = out$U; V = out$V; dlU = out$dlU; dlV = out$dlV
      # ---- M-step ----
      # score and info Psi
      sc = NULL
      Y1 = array(NA,c(mb+1,k,r))
      for(j in 1:r) Y1[1:(b[j]+1),,j] = 0
      Vv = matrix(aperm(V,c(1,3,2)),ns*TT,k)
      for(j in 1:r) for(jb in 0:b[j]) {
        ind = which(Sv[,j]==jb)
        if(length(ind)==1){
          if(miss) Y1[jb+1,,j] = Vv[ind,]*Rv[ind,j]
          else Y1[jb+1,,j] = Vv[ind,]
        }
        if(length(ind)>1){
          if(miss) Y1[jb+1,,j] = colSums(Vv[ind,]*Rv[ind,j])
          else Y1[jb+1,,j] = colSums(Vv[ind,])
        }

      }
      for(c in 1:k) for(j in 1:r){
        sc = c(sc,t(Am[[j]])%*%(Y1[1:(b[j]+1),c,j]-sum(Y1[1:(b[j]+1),c,j])*Psi[1:(b[j]+1),c,j]))
        #Psi[,c,j] = Y1[,c,j]/sum(Y1[,c,j])
        tmp = Y1[1:(b[j]+1),c,j]
        tmp = pmax(tmp/sum(tmp),10^-10)
        Psi[1:(b[j]+1),c,j] = tmp/sum(tmp)

        temp = pmax(Psi[1:(b[j]+1),c,j],10^-50)
        Op = diag(temp)-temp%o%temp
        Temp  = sum(Y1[1:(b[j]+1),c,j])*t(Am[[j]])%*%Op%*%Am[[j]]
        if(j==1 & c==1) Fi = Temp else Fi = blkdiag(Fi,Temp)
      }
      # score and info piv
      out = est_multilogit(V[,,1],XXdis,Xlab,be,Pivdis,fort=fort,ex=TRUE)
      sc = c(sc,out$sc); Fi = blkdiag(Fi,out$Fi)
      # score and info Pi
      if(param=="multilogit"){
        for(h in 1:k){
          UU = NULL
          for(t in 2:TT) UU = rbind(UU,t(U[h,,,t]))
          tmp = ZZdis[,,,h]
          if(nc2==0) tmp = array(tmp,c(k,(k-1),Zndis))
          tmp2 = PIdis[,,h]
          if(Zndis==1) tmp2 = matrix(tmp2,1,k)
          out = est_multilogit(UU,tmp,Zlab,Ga[,h],tmp2,fort=fort,ex=TRUE)
          sc = c(sc,out$sc); Fi = blkdiag(Fi,out$Fi)
        }
      }else if(param=="difflogit"){
        Tmp = aperm(U[,,,2:TT,drop=FALSE],c(1,3,4,2))
        Tmp = matrix(Tmp,ns*k*(TT-1),k)
        out = est_multilogit(Tmp,ZZdis,Zlab,Ga,PIdis,fort=fort,ex=TRUE)
        sc = c(sc,out$sc); Fi = blkdiag(Fi,out$Fi)
      }
      Fi = as.matrix(Fi)
      # compute correction matrix for the information
      nal = dim(dlPhi)[4]; nbe = dim(dlPiv)[3]; nga = dim(dlPI)[5]
      npar = nal+nbe+nga
      Cor = matrix(0,npar,npar)
      dY1 = array(NA,c(mb+1,k,r,npar))
      for(j in 1:r) dY1[1:(b[j]+1),,j,] = 0
      dV = array(V,c(ns,k,TT,npar))*dlV
      dVv = array(aperm(dV,c(1,3,2,4)),c(ns*TT,k,nal+nbe+nga))
      for(j in 1:r) for(jb in 0:b[j]) {
        ind = which(Sv[,j]==jb)
        if(length(ind)==1){
          if(miss) for(h in 1:(nal+nbe+nga)) dY1[jb+1,,j,h] = dVv[ind,,h]*Rv[ind,j]
          else for(h in 1:(nal+nbe+nga)) dY1[jb+1,,j,h] = dVv[ind,,h]
        }
        if(length(ind)>1){
          if(miss) for(h in 1:(nal+nbe+nga)) dY1[jb+1,,j,h] = colSums(dVv[ind,,h]*Rv[ind,j])
          else for(h in 1:(nal+nbe+nga)) dY1[jb+1,,j,h] = colSums(dVv[ind,,h])
        }
      }
      for(h in 1:(npar)){
        count = 0
        for(c in 1:k) for(j in 1:r){
          #count = count+1
          #ind = (count-1)*mb+(1:mb)
          ind = count+(1:b[j])
          count = count+b[j]
          Cor[h,ind] = t(Am[[j]])%*%(dY1[1:(b[j]+1),c,j,h]-sum(dY1[1:(b[j]+1),c,j,h])*Psi[1:(b[j]+1),c,j])
        }
      }
      for(h in 1:(npar)){
        out = est_multilogit(dV[,,1,h],XXdis,Xlab,be,Pivdis,fort=fort,ex=TRUE)
        Cor[h,nal+(1:nbe)] = out$sc
      }
      dU = array(U,c(k,k,ns,TT,npar))*dlU
      if(param=="multilogit"){
        rGa = dim(Ga)[1]
        for(h in 1:k){
          for(h1 in 1:npar){
            UU = NULL
            for(t in 2:TT) UU = rbind(UU,t(dU[h,,,t,h1]))
            tmp = ZZdis[,,,h]
            if(nc2==0) tmp = array(tmp,c(k,(k-1),Zndis))
            tmp2 = PIdis[,,h]
            if(Zndis==1) tmp2 = matrix(tmp2,1,k)

            out = est_multilogit(UU,tmp,Zlab,Ga[,h],tmp2,fort=fort,ex=TRUE)
            ind = nal+nbe+(h-1)*rGa+(1:rGa)
            Cor[h1,ind] = out$sc
          }
        }
      }else if(param=="difflogit"){
        rGa = length(Ga)
        for(h1 in 1:npar){
          Tmp = aperm(dU[,,,2:TT,h1],c(1,3,4,2))
          Tmp = matrix(Tmp,ns*k*(TT-1),k)
          out = est_multilogit(Tmp,ZZdis,Zlab,Ga,PIdis,fort=fort,ex=TRUE)
          ind = nal+nbe+(1:rGa)
          Cor[h1,ind] = out$sc
        }
      }
      # check score and information
      if(check_der){
        lk0 = lk
        out = lk_obs_latent(th,S,R,b,yv,Am,XXdis,Xlab,ZZdis,Zlab,param,fort)
        print(lk-out$lk)
        sc1 = out$sc
        lth = length(th)
        scn = rep(0,lth); Fn = matrix(0,lth,lth)
        for(h in 1:lth){
          thh = th; thh[h] = thh[h]+10^-6
          outh = lk_obs_latent(thh,S,R,b,yv,Am,XXdis,Xlab,ZZdis,Zlab,param,fort=fort)
          scn[h] = (outh$lk-lk)*10^6
          Fn[,h] = -(outh$sc-sc)*10^6
        }
        print(round(cbind(sc,sc1,scn,sc-scn),4))
        print(round(cbind(diag(Fi-Cor),diag(Fn),diag(Fi-Cor-Fn)),4))
        browser()
      }
      # Information matrix and standard errors
      Fi = Fi-Cor
      iFi = ginv(Fi)
      se = sqrt(diag(iFi))
      # Divide parameters
      sepsi = se[1:nal]
      sebe = se[nal+(1:nbe)]
      sega = se[nal+nbe+(1:nga)]
    }
    # Compute number of parameters
    if(r==1) np = k*mb*r else np = k*sum(b)
    np = np+(k-1)*(nc1+1)
    if(param=="multilogit") np = np+(k-1)*(nc2+1)*k else if(param=="difflogit")  np = np+(k-1)*(nc2+k)
    aic = -2*lk+np*2
    bic = -2*lk+np*log(n)
    #	out = list(lk=lk,piv=piv,Pi=Pi,Psi=Psi,np=np,aic=aic,bic=bic,lkv=lkv,J=J,V=V1,th=th,sc=sc)
    # local decoding
    Ul = matrix(0,ns,TT)
    for(i in 1:ns) for(t in 1:TT){
      Ul[i,t] = which.max(V[i,,t])
    }
    if(all(yv==1)) V1=V else V1 = V/yv

    if(out_se){
      if(r==1){
        psi = as.vector(aperm(Psi,c(1,3,2)))
        dPsi = diag(psi)%*%matrix(aperm(dlPsi,c(1,3,2,4)),c((b+1)*k,nal))
        sePsi = sqrt(diag(dPsi%*%iFi[1:nal,1:nal]%*%t(dPsi)))
        sePsi = aperm(array(sePsi,c(b+1,r,c)),c(1,3,2))
        dimnames(sePsi)=list(category=0:b,state=1:k)
      }else{
        sePsi = array(NA,dim(Psi))
        for(j in 1:r) sePsi[1:(b[j]+1)]=0
        ind = 0
        for(c in 1:k) for(j in 1:r){
          Tmp = iFi[ind+(1:b[j]),ind+(1:b[j])]
          ind = ind+b[j]
          psi = Psi[1:(b[j]+1),c,j]
          Tmp1 = matrix((diag(psi)-psi%o%psi)[,-1],b[j]+1,b[j])
          sePsi[1:(b[j]+1),c,j] = sqrt(diag(Tmp1%*%Tmp%*%t(Tmp1)))
          dimnames(sePsi)=list(category=0:mb,state=1:k,item=1:r)
        }
      }
    }
    Be = matrix(be,nc1+1,k-1)
    if (is.null(nameBe)){
      if(nc1==0) nameBe = c("Intercept") else nameBe = c("intercept",paste("X1",1:nc1,sep=""))
    }else{
      nameBe = c("intercept",nameBe)
    }

    dimnames(Be) = list(nameBe,logit=2:k)
    if(out_se) {seBe = matrix(sebe,nc1+1,k-1); dimnames(seBe) = list(nameBe,logit=2:k)}
    if(param=="multilogit"){
      if(is.null(nameGa)){
        if(nc2==0) nameGa = c("Intercept") else nameGa = c("intercept", paste("X2",1:nc2,sep=""))
      }else{
        nameGa = c("intercept",nameGa)
      }
      if(k>2) {
        Ga = array(as.vector(Ga),c(nc2+1,k-1,k))
        dimnames(Ga) = list(nameGa,logit=2:k,logit=1:k)
      }else if(k==2){

        dimnames(Ga) = 	list(nameGa,logit=1:k)
      }
      if(out_se){
        if(k==2){
          seGa = matrix(sega,nc2+1,2)
          dimnames(seGa) = list(nameGa,logit=1:k)
        }else if(k>2){
          seGa = array(as.vector(sega),c(nc2+1,k-1,k))
          dimnames(seGa) = list(nameGa,logit=2:k,logit=1:k)
        }
      }
    }else if(param=="difflogit"){
      Ga0 = Ga
      Ga = vector("list",2)
      seGa = vector("list",2)
      Ga[[1]] = t(matrix(Ga0[1:(k*(k-1))],k-1,k))
      Ga[[2]] = matrix(Ga0[(k*(k-1))+(1:((k-1)*nc2))],nc2,k-1)
      if(is.null(nameGa)){
        nameGa2 = paste("X2",1:nc2,sep="")
      }else{
        nameGa2 = nameGa
      }
      if (k==2) {
        dimnames(Ga[[1]]) = list(intercept=1:k,logit=k)
        dimnames(Ga[[2]])=list(nameGa2,logit=k)
      } else if (k>2){
        dimnames(Ga[[1]]) = list(intercept=1:k,logit=2:k)
        dimnames(Ga[[2]])=list(nameGa2,logit=2:k)
      }
      if(out_se){
        seGa[[1]] = t(matrix(sega[1:(k*(k-1))],k-1,k))
        seGa[[2]] = matrix(sega[(k*(k-1))+(1:((k-1)*nc2))],nc2,k-1)
        if(k==2){
          dimnames(seGa[[1]]) = list(intercept=1:k,logit=k)
          dimnames(seGa[[2]])=list(nameGa2,logit=k)
        }else if (k>2){
          dimnames(seGa[[1]]) = list(intercept=1:k,logit=2:k)
          dimnames(seGa[[2]])=list(nameGa2,logit=2:k)
        }
      }
    }
    # adjust output
    lk = as.vector(lk)
    if(output){
      dimnames(Piv)=list(subject=1:ns,state=1:k)
      dimnames(PI)=list(state=1:k,state=1:k,subject=1:ns,time=1:TT)
    }
    if(r==1) dimnames(Psi) = list(category=0:b,state=1:k,item=1) else 		dimnames(Psi)=list(category=0:mb,state=1:k,item=1:r)
    out = list(lk=lk,Be=Be,Ga=Ga,Psi=Psi,np=np,aic=aic,bic=bic,lkv=lkv,
               call=match.call(),param=param)
    if(out_se){
      out$sePsi = sePsi
      out$seBe = seBe
      out$seGa = seGa
    }
    # final output
    if(output){
      out$V1 = V1
      out$PI = PI
      out$Piv = Piv
      out$Ul = Ul
    }
    #cat(" |-------------|-------------|-------------|-------------|\n");
    cat("------------|-------------|-------------|-------------|-------------|-------------|\n");
    class(out)="LMlatent"
    (out)
  }


est_lm_cov_manifest <- function(S,X,yv = rep(1,nrow(S)),k,q=NULL,mod=c("LM","FM"),tol=10^-8,maxit=1000,start=0,mu=NULL,al=NULL,
                                be=NULL,si=NULL,rho=NULL,la=NULL,PI=NULL,output=FALSE,out_se=FALSE){

  #
  warning("est_lm_cov_manifest function is no longer maintained. Please look at lmest function",call. = FALSE)
  # Fit the model of Bacci, Bartolucci and Pennoni (2014) with global logits
  # (when mod = 1)
  #
  # INPUT:
  # S:     array of available configurations (n x TT)
  # X:     array (n x TT x nc) of covariates with eventually includes lagged
  #        response
  # k:     number of latent states
  # q:     number of support points for AR
  # mod:   model (0 = LM with stationary transition, 1 = finite mixture)
  # tol:   tolerance for the convergence (optional) and tolerance of conditional probability
  #        if tol>1 then
  # start: equal to 1 for random starting values (optional)
  # mu:    starting value for mu (optional)
  # al:    starting value for al (optional)
  # be:    starting value for be (optional)
  # si:    starting value for si (optional)
  # rho:   starting value for rho (optional)
  # la:    starting value for la (optional)
  # PI:    starting value for PI (optional)
  # output:  TRUE to  additional output
  #out_se:   TRUE for computing information
  #
  # OUTPUT:
  #        mu:    vector of cuptpoints
  #        al:    support points for the latent states
  #        be:    estimate of the vector of regression parameters
  #        si:    sigma of the AR process
  #        rho:   parameter vector for AR
  #        la:    vector of initial probabilities
  #        PI:    transition matrix
  # lk:    maximum log-likelihood
  # np:   number of parameters
  # aic:   AIC index
  # bic:   BIC index
  # sebe:  standard errors for the regression parameters be
  # selrho: standard errors for logit type transformation of rho
  # PRED0: prediction of latent state
  # PRED1: prediction of the overall latent effect
  # preliminaries

  mod = match.arg(mod)
  if(mod=="LM") mod=0 else mod=1
  if(mod==0) q = 1
  if(mod==1 & is.null(q)) stop("value of q is missing")
  # *** organize response matrix ***
  lev = max(S)+1
  if(min(S)>0){
    cat("|------------------- WARNING -------------------|\n")
    cat("|The first response category must be coded as 0 |\n")
    cat("|-----------------------------------------------|\n")
  }
  nt = prod(lev)
  ns = nrow(S); TT = ncol(S)
  n = sum(yv)

  if(is.array(S)){
    r = dim(S)[3]
    if(!is.na(r) & r>1) warning("multivariate data are not allowed; only the first response variable is considered")
    S = matrix(S,ns,TT)
  }
  if(is.data.frame(S)) warning("Data frame not allowed for S")
  if(ns!= dim(X)[1]) stop("dimension mismatch between S and X")

  Y0 = S+1
  S = array(0,c(nt,ns,TT))
  for(i in 1:ns) for(t in 1:TT){
    ind = Y0[i,t]
    S[ind,i,t] = 1
  }
  if(is.matrix(X)) X = array(X,c(ns,TT,1))
  nc = dim(X)[3]
  ne = lev-1

  XX = X
  X = array(0,c(ne,nc,ns,TT))
  for(i in 1:ns) for(t in 1:TT){
    if(lev==2) X[,,i,t] = XX[i, t, ]
    else X[,,i,t] = rep(1,ne)%o%XX[i,t,]
  }
  opt = list(TolFun=10^-6,TolX=10^-6)
  opt1 = list(TolFun=10^-6,Display="iter")
  out = marg_param(lev,"g")
  Cm = out$C; Mm = out$M
  Gm = cbind(-rep(1,lev-1),diag(lev-1))
  Hm = rbind(rep(0,lev-1),diag(lev-1))
  GHt = t(Gm)%*%t(Hm)
  lm = c(1,rep(0,lev-1))
  Lm = rbind(rep(0,lev-1),diag(lev-1))-rbind(diag(lev-1),rep(0,lev-1))

  if(q==1) sup = 0 else{
    # lim = min(sqrt(q),5);
    lim = 5;
    sup = seq(-lim,lim,2*lim/(q-1))
  }
  Mar = diag(k)%x%matrix(1,1,q)
  G2 = NULL; H2 = NULL; IPI = NULL
  if(k>1){
    if(mod==0){
      for(c in 1:k){
        G2c = diag(k)[,-c]
        H2c = diag(k)[-c,]
        if (k==2) H2c[c]=-1 else H2c[,c]= -1
        if(is.null(G2)) G2 = G2c else if(k==2) G2 = blkdiag(matrix(G2,ncol=1),matrix(G2c,ncol=1)) else G2 = blkdiag(G2,G2c)
        if(is.null(H2)) H2 = H2c else if(k==2) H2 = blkdiag(matrix(H2,nrow=1),matrix(H2c,nrow=1))else H2 = blkdiag(H2,H2c)
        IPI = c(IPI,c+seq(0,k*(k-1),k))
      }
    }
    if(mod==1){
      G2 = diag(k)[,-1]; if(k==2) G2 = matrix(G2,ncol=1)
      H2 = diag(k); H2[,1] = -1; H2 = H2[-1,]
    }
  }
  # starting values
  mu_inp=mu
  if(is.null(mu)){
    Pim = apply(S,c(1,2),sum)+0.05*TT; Eta = Cm%*%log(Mm%*%Pim)
    Eta = Eta%x%matrix(1,1,TT)
    eta = as.vector(Eta)
    Z = matrix(aperm(X,c(1,4,3,2)),ns*ne*TT,dim(X)[2])
    Z = cbind(matrix(1,ns*TT,1)%x%diag(ne),Z)
    par = ginv(t(Z)%*%Z)%*%t(Z)%*%eta
    mu = par[1:ne]; par = par[-(1:ne)]; be = par
    if(k==1) al = NULL else{
      if(start==1) al = rnorm(k)*k else al = seq(-k,k,2*k/(k-1))
      mu = mu+al[1]
      al = al[-1]-al[1]
    }
    if(k==1) PI = 1 else{
      if(mod==0){
        PI = matrix(1,k,k)+9*diag(k)
        PI = diag(1/rowSums(PI))%*%PI
      }
      if(mod==1) PI = diag(k)
    }
    if(start==1){
      la = matrix(runif(k),k,1); la = la/sum(la)
      rho = 2*matrix(runif(k),k,1)-1
      if(mod==0) si = NULL
      else si = runif(1)*5
    }else{
      la = matrix(1,k,1)/k
      rho = matrix(0,k,1)
      if(mod==0) si = NULL
      else si = 3
    }
  }
  if(start==2){
    if(is.null(mu_inp)) stop("initial value of the cut-points (mu) must be given in input")
    mu=mu_inp
    if(is.null(be)) stop("initial value of the regression parameters (be) must be given in input")
    be=be
    if(is.null(al)) stop("initial value of the support points (al) must be given in input")

    mu = mu+al[1]
    al=al[-1]-al[1]
    if(is.null(la)) stop("initial value of the initial probabilities (la) must be given in input")
    la=la
    if(is.null(PI)) stop("initial value of the transition probabilities (PI) must be given in input")
    PI=PI
    if(mod==0){
      rho = matrix(0,k,1)
      si = NULL
    }
    if(mod==1){
      if(is.null(rho)) stop("initial value of the parameter vector for AR(1) process (rho) must be given in input")
      rho = rho
      if(is.null(si)) stop("initial value of sigma (si) must be given in input")
      si = si
    }

  }

  par = c(mu,al,si,be)

  if(k==1) tau = NULL else{
    if(mod==0) tau = H2%*%log(PI[IPI]) else tau = H2%*%log(la)
  }
  wei = dnorm(sup); wei = wei/sum(wei)
  las = as.vector(la%x%wei)
  lrho = (rho+1)/2
  lrho = log(lrho/(1-lrho))
  SUP = sup%o%rep(1,q)
  WEI = matrix(0,k*q,k*q)

  for(j in 1:k){
    ind = (j-1)*q+(1:q)
    Wei = dnorm(t(SUP),rho[j]*SUP,sqrt(1-rho[j]^2))
    Wei = Wei/rowSums(Wei)
    WEI[,ind] = matrix(1,k,1)%x%Wei
  }
  PIs = (PI%x%matrix(1,q,q))*WEI

  # to do in Fortran
  # find non-redundant X configurations (may be very slow)
  # Xd = array(X,c(ne,nc,n*TT))
  # indn = matrix(1:(n*TT),n,TT)
  # for(jd in 1:nd) INDN[[jd]]$ind = jd
  X1 = matrix(X,ne*nc,ns*TT)
  out1 = t(unique(t(X1)))
  nd = ncol(out1)
  indn = rep(0,ns*TT)
  INDN = vector("list",nd)
  tmp = ne*nc
  for(jd in 1:nd){
    ind = which(colSums(X1 == out1[,jd])==tmp)
    indn[ind] = jd
    INDN[[jd]]$ind = ind
  }
  indn = matrix(indn,ns,TT)
  Xd = array(out1,c(ne,nc,nd))
  #for(jd in 1:nd) INDN[[jd]]$ind = which(indn==jd)
  cat(c("n. distinct covariate conf. = ",nd))
  #Xd = Xd[,,1:nd]  #to allow for one covariate
  LLm1 = array(t(Lm),c(ncol(Lm),nrow(Lm),nd))
  # alternate between EM and NR
  itg = 0; cont = 1;

  while(cont && itg<5){

    cont = 0; itg = itg+1;
    # compute initial log-likelihood
    I = diag(ne)
    one = matrix(1,ne,1)
    Pio = array(0,c(ns,k*q,TT))
    if(mod==0){
      par0 = par[1:(lev-2+k)]
      Eta01 = prod_array(Xd,par[(lev+k-1):length(par)])
    }else{
      par0 = par[1:(lev-1+k)]
      Eta01 = prod_array(Xd,par[(lev+k):length(par)])
    }
    j = 0
    for(c in 1:k){
      u = matrix(0,1,k); u[c] = 1; u = u[-1]
      D0 = cbind(I,matrix(u,nrow=1)%x%one)
      for(d in 1:q){
        j = j+1;
        if(mod==0) D = D0
        else D = cbind(D0,sup[d]*one)
        agg = D%*%par0
        Eta1 = Eta01+agg%*%rep(1,nd)
        Qv1 = expit(Eta1); Qv1 = pmin(pmax(Qv1,10^-100),1-10^-100)
        Pv1 = lm%o%rep(1,nd)+Lm%*%Qv1; Pv1 = pmin(pmax(Pv1,10^-100),1-10^-100)
        for(t in 1:TT) Pio[,j,t] = colSums(S[,,t]*Pv1[,indn[,t]])
      }
    }
    Q = rec1(Pio,las,PIs)
    if(q*k==1) pim = Q[,,TT] else pim = rowSums(Q[,,TT])
    lk = sum(yv*log(pim))

    if(tol>1){
      est = NULL; (est)
    }
    # start EM
    t0 = proc.time()[1]; tdisp = 5;
    cat("\nEM step:\n")
    if(mod==0){
      cat("------------|-------------|-------------|-------------|-------------|-------------|\n");
      cat("      k     |    start    |     step    |     lk      |    lk-lko   | discrepancy |\n");
      cat("------------|-------------|-------------|-------------|-------------|-------------|\n");
      cat(sprintf("%11g",c(k,start,0,lk)),"\n",sep=" | ")
    }else{
      cat("----------|-----------|-----------|-----------|-----------|-----------|-----------|-----------|\n");
      cat("     k    |  n. sup   |  max(rho) |  sigma    |  step     |   lk      |  lk-lko   |discrepancy|\n");
      cat("----------|-----------|-----------|-----------|-----------|-----------|-----------|-----------|\n");
      cat(sprintf("%9g",c(k,q,max(rho),si,0,lk)),"\n",sep=" | ")
    }

    # iterate until convergence
    it = 0; lko = lk-10^10; dis = 0
    #	while((dis>tol || lk-lko>tol || it ==0) && it <5000){
    while((lk-lko)/abs(lk)>tol & it<maxit){
      it = it+1
      lko = lk; paro = par; tauo = tau; lrhoo = lrho
      # E-step
      out = rec3(Q,yv,PIs,Pio,pim)
      U = out$U; V = out$V
      # M-step: latent parameters
      if(k>1){
        if(mod==0){
          u1 = Mar%*%rowSums(U[,,1])
          V1 = Mar%*%V%*%t(Mar)
          out = optim(tau,lk_sta,gr=NULL,as.vector(u1),V1,G2,outl=FALSE,method = "BFGS")
          tau = out$par
          out = lk_sta(tau,as.vector(u1),V1,G2,outl=TRUE)
          flk = out$flk; la = out$la; PI = out$PI
        }
        if(mod==1){
          u1 = Mar%*%rowSums(U[,,1])
          la = u1/n;
          tau = H2%*%log(la)
        }
      }
      if(q>1){
        for(c in 1:k){
          Vc = matrix(0,q,q);
          ind = (c-1)*q+(1:q);
          for(d in 1:k){
            ind1 = (d-1)*q+(1:q)
            Vc = Vc+V[ind1,ind]
          }
          out = optim(lrho[c],lk_ar_rho,gr=NULL,SUP,Vc,outp=FALSE,method = "BFGS")
          lrho[c] = out$par
          out = lk_ar_rho(lrho[c],SUP,Vc,outp=TRUE)
          flk = out$flk; Wei = out$Wei; rho[c] = out$rho
          WEI[,ind] = matrix(1,k,1)%x%Wei
        }
      }
      las = as.vector(la%x%wei); PIs = (PI%x%matrix(1,q,q))*WEI
      # M-step: regression parameters
      U = aperm(U,c(2,1,3))
      s = 0; FF = 0; j = 0
      for(c in 1:k){
        u = matrix(0,1,k); u[c] = 1; u = u[-1]
        D0 = cbind(I,t(as.matrix(u))%x%one)
        for(d in 1:q){
          j = j+1
          if(mod==0) D = D0
          else D = cbind(D0,sup[d]*one)
          agg = as.vector(D%*%par0)
          Eta1 = Eta01+agg%o%rep(1,nd)
          Qv1 = expit(Eta1); Qv1 = pmin(pmax(Qv1,10^-100),1-10^-100)
          Pit1 = lm%o%rep(1,nd)+Lm%*%Qv1; Pit1 = pmin(pmax(Pit1,10^-100),1-10^-100)
          QQv1 = Qv1*(1-Qv1)
          DPv1 = 1/Pit1
          RRtc1 = array(0,c(ne,lev,nd))
          for(j1 in 1:ne) for(j2 in 1:lev) RRtc1[j1,j2,] = QQv1[j1,]*DPv1[j2,]
          RRtc1 = RRtc1*LLm1
          XXRi1 = array(0,c(dim(D)[1],dim(D)[2]+dim(Xd)[2],nd))
          for(h2 in 1:nd){
            if(lev==2) XXRi1[,,h2] = c(D, Xd[,, h2])
            else XXRi1[,,h2] = cbind(D,Xd[,,h2])
          }
          XXRi1 = aperm(XXRi1,c(2,1,3))
          pc = U[,j,]; pc = as.vector(pc)
          nt = dim(S)[1]
          YGP = matrix(S,nt,ns*TT)-Pit1[,as.vector(indn)]
          Om = array(0,c(lev,lev,nd))
          for(r1 in 1:lev) for(r2 in 1:lev){
            if(r2==r1){
              Om[r1,r2,] = Pit1[r1,]-Pit1[r1,]*Pit1[r2,]
            }else{
              Om[r1,r2,] = -Pit1[r1,]*Pit1[r2,]
            }
          }
          for(jd in 1:nd){
            ind = INDN[[jd]]$ind
            pci = pc[ind]
            if(lev==2) XRi = (XXRi1[, , jd] %o% RRtc1[, , jd]) %*% GHt
            else XRi = (XXRi1[,,jd]%*%RRtc1[,,jd])%*%GHt
            if(length(ind)==1){
              s = s+XRi%*%(YGP[,ind]*pci)
            }else{
              s = s+XRi%*%(YGP[,ind]%*%pci)
            }
            FF = FF+sum(pci)*(XRi%*%Om[,,jd])%*%t(XRi)
          }
        }
      }
      # update parameter vector
      dpar = ginv(FF)%*%s
      mdpar = max(abs(dpar))
      if(mdpar>1) dpar = dpar/mdpar
      par = par+dpar
      if(mod==1) si = par[ne+k]
      # compute new log-likelihood
      if(mod==0){
        par0 = par[1:(lev-2+k)]
        Eta01 = prod_array(Xd,par[(lev+k-1):length(par)])
      }else{
        par0 = par[1:(lev-1+k)]
        Eta01 = prod_array(Xd,par[(lev+k):length(par)])
      }
      j = 0
      for(c in 1:k){
        u = matrix(0,1,k); u[c] = 1; u = u[-1]
        D0 = cbind(I,t(as.matrix(u))%x%one)
        for(d in 1:q){
          j = j+1;
          if(mod==0) D = D0
          else D = cbind(D0,sup[d]*one)
          agg = as.vector(D%*%par0)
          Eta1 = Eta01+agg%o%rep(1,nd)
          Qv1 = expit(Eta1); Qv1 = pmin(pmax(Qv1,10^-100),1-10^-100);
          Pv1 = lm%o%rep(1,nd)+Lm%*%Qv1; Pv1 = pmin(pmax(Pv1,10^-100),1-10^-100);
          for(t in 1:TT) Pio[,j,t] = colSums(S[,,t]*Pv1[,indn[,t]])
        }
      }
      Q = rec1(Pio,las,PIs)
      if(k*q==1) pim = Q[,,TT] else pim = rowSums(Q[,,TT])
      lk = sum(yv*log(pim))
      # display results
      dis = max(abs(c(par-paro,tau-tauo,lrho-lrhoo)))
      #    if((proc.time()[1]-t0)>tdisp){
      #Display output
      if(it/10 == floor(it/10)){
        if(mod==0){
          cat(sprintf("%11g",c(k,start,it,lk,lk-lko,dis)),"\n",sep=" | ")
        }else{
          cat(sprintf("%9g",c(k,q,max(rho),si,it,lk,lk-lko,round(dis,7))),"\n",sep=" | ")
        }
      }
      #      tdisp = etime(clock,t0)+5
      #    }
    }
    # Newton-Rapshon
    par1 = NULL;
    if(k>1) par1 = tau
    if(q>1) par1 = c(par1,lrho)
    par1 = c(par1,par)
    # NR algorithm
    if(mod==1){
      cat("--------------------------------------------------------------------------------------\n");
      cat("\nNR step:\n")
      cat("----------|-----------|-----------|-----------|-----------|-----------|-----------|-----------|\n");
      cat("     k    |  n. sup   |  max(rho) |  sigma    |  step     |   lk      |  lk-lko   |discrepancy|\n");
      cat("----------|-----------|-----------|-----------|-----------|-----------|-----------|-----------|\n");
    }
    it = 0; t0 = proc.time()[1]
    while(abs(lk-lko)>10^-5 && it<100 && mod==1){
      lko = lk
      it = it+1
      out = lk_obs_manifest(par1,S,Xd,yv,indn,lev,k,sup,G2,IPI,mod,outp=TRUE)
      nx = length(par1)
      d0 = out$s
      ny = length(d0)
      D = matrix(0,nx,ny)
      for (i in 1:nx){
        o = matrix(0,nx,1); o[i] = 10^-6
        out = lk_obs_manifest(par1+o,S,Xd,yv,indn,lev,k,sup,G2,IPI,mod,outp=TRUE)
        d1 = out$s
        d = (d1-d0)/10^-6
        D[i,] = t(d)
      }
      s1 = d0
      J1 = D
      J1 = -(J1+t(J1))/2

      if(rcond(J1)<10^-15) print(c("rcond of information = ",toString(round(rcond(J1),3))))
      dpar1 = ginv(J1)%*%s1;
      mdpar1 = max(abs(dpar1));
      if(mdpar1>0.5) dpar1 = dpar1/mdpar1*0.5
      par1o = par1;
      par1 = par1+dpar1;
      lktmp = lk_obs_manifest(par1,S,Xd,yv,indn,lev,k,sup,G2,IPI,mod)
      lk = lktmp$lk
      cont = 0;
      while(lk<(lko-10^-6)){
        cont = 1;
        dpar1 = dpar1/2;
        par1 = par1o+dpar1;
        lktmp = lk_obs_manifest(par1,S,Xd,yv,indn,lev,k,sup,G2,IPI,mod)
        lk = lktmp$lk
        print(c('halved step',toString(lk-lko)))
      }
      out = trans_par(par1,lev,k,sup,G2,IPI,mod)
      la = out$la; PI = out$PI; rho = out$rho; si = out$si; par = out$par;
      lrho = out$lrho; tau = out$tau
      cat(sprintf("%9g",c(k,q,max(rho),si,it,lk,round(lk-lko,7),round(max(abs(par1-par1o)),7))),"\n",sep=" | ")
      #print(c(k,q,max(rho),si,it,lk,lk-lko,max(abs(par1-par1o)),(proc.time()[1]-t0)/it))
    }

    if(cont){
      las = as.vector(la%x%wei); PIs = (PI%x%matrix(1,q,q))*WEI
      #las = as.vector(kron(la,wei))
      WEI = matrix(0,k*q,k*q);

      for(j in 1:k){
        ind = (j-1)*q+(1:q);
        Wei = dnorm(t(SUP),rho[j]*SUP,sqrt(1-rho[j]^2))
        Wei = Wei/rowSums(Wei)
        WEI[,ind] = matrix(1,k,1)%x%Wei
      }

      PIs = (PI%x%matrix(1,q,q))*WEI
      tol = tol/2;
    }
  }
  # separate parameters and compute aic and bic
  mu = par[1:ne]
  al = 0
  if(k>1) al = c(al,par[(ne+1):(ne+k-1)])
  mu = as.vector(mu+al%*%la)
  al = as.vector(al-al%*%la)
  if(mod==0) be = par[(ne+k):length(par)]
  else be = par[(ne+k+1):length(par)]
  if(mod==0) np = k*(k-1)
  if(mod==1) np = k-1
  np = np + (ne+(k-1)+nc) + ((k+1)*(q>1))
  if(q==1) rho = NULL

  # compute aic, bic and prediction of latent structure
  aic = -2*lk+2*(np)
  bic = -2*lk+log(n)*(np)


  # prediction
  if(output){
    out = lk_obs_manifest(par1,S,Xd,yv,indn,lev,k,sup,G2,IPI,mod,outp=TRUE)
    lk = out$lk; U = out$U
    sup1 = t(Mar)%*%al
    # if(q>1) sup1 = sup1+kron(ones(k,1),sup*si)
    if(q>1) sup1 = sup1+matrix(1,k,1)%x%(sup*si)
    PRED0 = array(0,c(ns,k,TT)); PRED1 = matrix(0,ns,TT)
    for(t in 1:TT){
      PRED0[,,t] = U[,,t]%*%t(Mar)
      PRED1[,t] = U[,,t]%*%sup1
    }
    if(any(yv!=1)){
      PRED0=PRED0/yv
      PRED1=PRED1/yv
    }
  }
  # standard errors
  if(out_se){
    out = lk_obs_manifest(par1,S,Xd,yv,indn,lev,k,sup,G2,IPI,mod,outp=TRUE)
    nx = length(par1)
    d0 = out$s
    ny = length(d0)
    D = matrix(0,nx,ny)
    for (i in 1:nx){
      o = matrix(0,nx,1); o[i] = 10^-6
      out = lk_obs_manifest(par1+o,S,Xd,yv,indn,lev,k,sup,G2,IPI,mod,outp=TRUE)
      d1 = out$s
      d = (d1-d0)/10^-6
      D[i,] = t(d)
    }
    s1 = d0
    J1 = D
    J1 = -(J1+t(J1))/2
    if(rcond(J1)<10^-15) print(c("rcond of information = ",rcond(J1)))
    se1 = sqrt(diag(ginv(J1)))
    if(k>1){
      if(mod==0) se1 = se1[-(1:(k*(k-1)))]
      if(mod==1) se1 = se1[-(1:(k-1))]
    }
    if(q==1) lrho = NULL
    if(q>1){
      lrho = se1[1:k]
      se1 = se1[-(1:k)]
    }
    #se = list(lrho=lrho, be = se1[(ne+k+1):length(se1)])
    selrho = lrho
    if(mod==0) sebe = se1[(ne+k):length(se1)]
    else sebe = se1[(ne+k+1):length(se1)]
  }

  out = list(mu=mu,al=al,be=be,si=si,rho=rho,la=la,PI=PI,lk=lk,np=np,aic=aic,bic=bic,call=match.call())
  if(out_se){
    out$selrho=selrho
    out$sebe = sebe
    out$J1=J1
  }
  if(output){
    out$Phi = Pio
    out$PRED0 = PRED0
    out$PRED1 = PRED1
  }
  if(mod==0){
    cat("------------|-------------|-------------|-------------|-------------|-------------|\n");
  }else{
    cat("----------|-----------|-----------|-----------|-----------|-----------|-----------|-----------|\n");
  }
  class(out)="LMmanifest"
  (out)
}




est_lm_mixed <- function(S,yv=rep(1,nrow(S)),k1,k2,start=0,tol=10^-8,maxit=1000,out_se=FALSE){


  warning("est_lm_mixed function is no longer maintained. Please look at lmestMixed function",call. = FALSE)

  # EM algorithm for mixed HMM based
  # Y = matrix of data
  # k1 = number of latent classes
  # k2 = number of latent states
  # start = 0 for deterministic initialization, 1 for stochastic initialization
  # tol = tolerance level to stop

  # preliminaries
  k2 = as.integer(k2)
  n = sum(yv)
  sS = dim(S)
  ns = sS[1]
  TT = sS[2]
  if(is.data.frame(S)) warning("Data frame not allowed for S")

  if(min(S)>0){
    cat("|------------------------------------------ WARNING -----------------------------------------|\n")
    cat("|The first response category must be coded as 0                                              |\n")
    cat("|------------------------------------------ WARNING -----------------------------------------|\n")
  }

  if(ns!=length(yv)) stop("dimensions mismatch between S and yv")
  if(length(sS)==2) r = 1
  else r = sS[3]
  if(r==1) {
    if(is.matrix(S)) S = array(S,c(dim(S),1))
  }
  Sv = matrix(S,ns*TT,r)

  b = max(S)
  flag = FALSE
  for (j in 1:r) if (length(table(Sv[, j])) < b) flag = TRUE
  if(flag){
    cat("|------------------------------------------ WARNING -----------------------------------------|\n")
    cat("| Response variables must have the same number of categories                                 |\n")
    cat("|------------------------------------------ WARNING -----------------------------------------|\n")
  }

  Co = cbind(-diag(b),diag(b))
  Ma = cbind(lower.tri(matrix(1,b,b), diag = TRUE),rep(0,b))
  Ma = rbind(Ma,1-Ma)
  # starting parameters
  if(start==0){
    la = rep(1,k1)/k1
    Pi = matrix(1,k2,k2)+9*diag(k2); Pi = Pi/rowSums(Pi)
    Pi = array(Pi,c(k2,k2,k1))
    P = matrix(0,b+1,r)
    for(t in 1:TT) for(j in 1:r) for(y in 0:b){
      #if(r==1) ind = which(S[,t]==y) else ind = which(S[,t,j]==y)
      ind = which(S[,t,j]==y)
      P[y+1,j] = P[y+1,j]+sum(yv[ind])
    }
    E = Co%*%log(Ma%*%P)
    Psi = array(0,c(b+1,k2,r)); Eta = array(0,c(b,k2,r))
    if(k2 == 1) grid = k2 else grid = seq(-k2,k2,2*k2/(k2-1))
    for(c in 1:k2) for(j in 1:r){
      etac = E[,j]+grid[c]
      Eta[,c,j] = etac
      Psi[,c,j] = invglob(etac)
    }
  }else{
    Psi = array(runif((b+1)*k2*r),c(b+1,k2,r))
    for(j in 1:r) for(c in 1:k2) Psi[,c,j] = Psi[,c,j]/sum(Psi[,c,j])
    la = runif(k1); la = la/sum(la)
    Pi = array(0,c(k2,k2,k1))
    for(u in 1:k1){
      Pi[,,u] = matrix(runif(k2^2),k2,k2)
      Pi[,,u] = Pi[,,u]/rowSums(matrix(Pi[,,u],k2,k2))
    }
  }
  if(k1==1){
    if(start==0){
      Piv = matrix(1,k2,1)/k2
    }else{
      temp = runif(k2)
      Piv = matrix(temp/sum(temp),k2,1)
    }
  }else{
    if(start==0){
      if(k2==1){
        Piv = matrix(1,1,k1)
      }else{
        Piv = matrix(0,k2,k1)
        gl = log((k2-1):1)-log(1:(k2-1))
        for(u in 1:k1) Piv[,u] = diff(c(0,1/(1+exp(gl+u-(1+k1)/2)),1))
      }
    }else{
      Piv = matrix(0,k2,k1)
      for(u in 1:k1){
        temp = runif(k2)
        Piv[,u] = temp/sum(temp)
      }
    }
  }

  # EM algorithm for composite likelihood one-wise case row by row
  # *** compute log-likelihood ***
  # conditional distribution of any row given the row effect and corresponding joint
  Fc1 = matrix(0,ns,k1); Fc2 = array(0,c(ns,k1,k2)); Fc3 = array(0,c(ns,k1,k2,k2))
  PP1 = array(0,c(ns,k1,k2,TT))
  Phi = array(1,c(ns,k2,TT))
  for(t in 1:TT){
    #if(r==1) Phi[,,t] = Phi[,,t]*Psi[S[,t]+1,,1] else for(j in 1:r) Phi[,,t] = Phi[,,t]*Psi[S[,t,j]+1,,j]
    for(j in 1:r) Phi[,,t] = Phi[,,t]*Psi[S[,t,j]+1,,j]
  }
  for(i in 1:ns) for(u in 1:k1){
    o = .Fortran("BWforback", TT, k2, Phi[i,,], Piv[,u], Pi[,,u], lk=0, Pp1=matrix(0,k2,TT),
                 Pp2=array(0,c(k2,k2,TT)))
    Fc1[i,u] = exp(o$lk); Fc2[i,u,] = o$Pp1[,1]; Fc3[i,u,,] = apply(o$Pp2,c(1,2),sum)
    PP1[i,u,,] = o$Pp1
  }
  Fj1 = Fc1%*%diag(la)
  fm = rowSums(Fj1)
  fm = pmax(fm,10^-300)
  lk = yv%*%log(fm)
  W = (Fj1/matrix(fm,ns,k1))*yv

  # iterate until convergence
  it = 0; lko = -Inf

  cat("------------|-------------|-------------|-------------|-------------|-------------|\n");
  cat("     k1     |      k2     |    start    |     step    |     lk      |    lk-lko   |\n");
  cat("------------|-------------|-------------|-------------|-------------|-------------|\n");
  cat(sprintf("%11g",c(k1,k2,start,0,lk)),"\n",sep=" | ")

  while((lk-lko)/abs(lk)>tol & it<maxit){
    it = it+1
    lko = lk

    # # E-step update row-weights
    WZ1 = array(W,c(ns,k1,k2))*Fc2
    WZ2 = array(W,c(ns,k1,k2,k2))*Fc3
    PV = array(W,c(ns,k1,k2,TT))*PP1
    PV = apply(PV,c(1,3,4),sum)

    # # M-step
    la = colSums(W); la = la/sum(la)
    for(u in 1:k1){
      Piv[,u] = apply(matrix(WZ1[,u,],ns,k2),2,sum)
      Piv[,u] = pmax(Piv[,u],10^-20)
      Piv[,u] = Piv[,u]/sum(Piv[,u])
      Pi[,,u] = apply(array(WZ2[,u,,],c(ns,k2,k2)),c(2,3),sum)
      Pi = pmax(Pi,10^-20)
      Pi[,,u] = Pi[,,u]/rowSums(matrix(Pi[,,u],k2,k2))
    }

    Y1 = array(0,c(b+1,k2,r))
    Vv = matrix(aperm(PV,c(1,3,2)),ns*TT,k2)
    for(j in 1:r) for(y in 0:b) {
      ind = which(Sv[,j]==y)
      if(k2==1) Y1[y+1,,j] = sum(Vv[ind,]) else Y1[y+1,,j] = colSums(Vv[ind,])
    }
    for(j in 1:r) for(c in 1:k2) Psi[,c,j] = Y1[,c,j]/sum(Y1[,c,j])


    # # *** compute log-likelihood ***
    # # conditional distribution of any row given the row effect and corresponding joint
    Phi = array(1,c(ns,k2,TT))
    for(t in 1:TT){
      #if(r==1) Phi[,,t] = Phi[,,t]*Psi[S[,t]+1,,1]
      #else for(j in 1:r) Phi[,,t] = Phi[,,t]*Psi[S[,t,j]+1,,j]
      for(j in 1:r) Phi[,,t] = Phi[,,t]*Psi[S[,t,j]+1,,j]
    }
    for(i in 1:ns) for(u in 1:k1){
      o = .Fortran("BWforback", TT, k2, Phi[i,,], Piv[,u], Pi[,,u], lk=0, Pp1=matrix(0,k2,TT),
                   Pp2=array(0,c(k2,k2,TT)))
      Fc1[i,u] = exp(o$lk); Fc2[i,u,] = o$Pp1[,1]; Fc3[i,u,,] = apply(o$Pp2,c(1,2),sum)
      PP1[i,u,,] = o$Pp1
    }
    Fj1 = Fc1%*%diag(la)
    fm = rowSums(Fj1)
    fm = pmax(fm,10^-300)
    lk = yv%*%log(fm)
    W = (Fj1/matrix(fm,ns,k1))*yv
    # display output
    if(it/10 == floor(it/10)) cat(sprintf("%11g",c(k1,k2,start,it,lk,lk-lko)),"\n",sep=" | ")
  }
  if(it/10 > floor(it/10))  cat(sprintf("%11g",c(k1,k2,start,it,lk,lk-lko)),"\n",sep=" | ")

  # BIC
  np = (k1-1)+k1*(k2^2-1)+k2*r*b
  bic = -2*lk+np*log(n)

  # compute standard errors if required
  if(out_se){
    # structure of parameter vector
    lla = logit1(la)$lp
    Der = expit1(lla)$Der
    lPiv = matrix(0,k2-1,k1)
    for(u in 1:k1){
      lPiv[,u] = logit1(Piv[,u])$lp
      Der = blkdiag(Der,expit1(lPiv[,u])$Der)
    }
    lPiv = as.vector(lPiv)
    lPi = array(0,c(k2-1,k2,k1))
    for(u in 1:k1) for(v in 1:k2){
      lPi[,v,u] = logit1(Pi[v,,u],v)$lp
      Der = blkdiag(Der,expit1(lPi[,v,u])$Der)
    }
    lPi = as.vector(lPi)
    lPsi = array(0,c(b,k2,r))
    for(j in 1:r) for(v in 1:k2){
      lPsi[,v,j] = logit1(Psi[,v,j])$lp
      Der = blkdiag(Der,expit1(lPsi[,v,j])$Der)
    }
    lPsi = as.vector(lPsi)
    th = c(lla,lPiv,lPi,lPsi)
    nla = length(lla); nPiv = length(lPiv); nPi = length(lPi); nPsi = length(lPsi)
    out = lk_obs_mixed(th,nla,nPiv,nPi,nPsi,S,yv,r,k1,k2)
    scn = NULL; Jn = NULL
    for(j in 1:length(th)){
      th1 = th; th1[j] = th1[j]+10^-6
      out1 = lk_obs_mixed(th1,nla,nPiv,nPi,nPsi,S,yv,r,k1,k2)
      scn = c(scn,(out1$lk-out$lk)*10^6)
      Jn = cbind(Jn,(out1$sc-out$sc)*10^6)
    }
    Jn = (Jn+t(Jn))/2
    Vn = ginv(-Jn)
    if(any(diag(Vn)<0)) print("negative elements in the diagonal of inv(Information)")
    Vn1 = Der%*%Vn%*%t(Der)
    se1 = sqrt(abs(diag(Vn1)))
    sela = se1[1:k1]; se1 = se1[-(1:k1)]
    sePiv = matrix(0,k2,k1)
    for(u in 1:k1){
      sePiv[,u] = se1[1:k2]
      se1 = se1[-(1:k2)]
    }
    dimnames(sePiv)=list(v=1:k2,u=1:k1)
    sePi = array(0,c(k2,k2,k1))
    for(u in 1:k1) for(v in 1:k2){
      sePi[v,,u] = se1[1:k2]
      se1 = se1[-(1:k2)]
    }
    dimnames(sePi)=list(v0=1:k2,v1=1:k2,u=1:k1)
    sePsi = array(0,c(b+1,k2,r))
    for(j in 1:r) for(v in 1:k2){
      sePsi[,v,j] = se1[1:(b+1)]
      se1 = se1[-(1:(b+1))]
    }
    dimnames(sePsi)=list(y=0:b,v=1:k2,j=1:r)
    # print(c(lk,out$lk,lk-out$lk))
    # print(cbind(out$sc,scn,out$sc-scn))
  }

  # adjust output
  lk = as.vector(lk); bic = as.vector(bic)
  if(any(yv!=1)) W = W/yv

  dimnames(Piv)=list(v=1:k2,u=1:k1)
  dimnames(Pi)=list(v0=1:k2,v1=1:k2,u=1:k1)
  dimnames(Psi)=list(y=0:b,v=1:k2,j=1:r)
  out = list(la=la,Piv=Piv,Pi=Pi,Psi=Psi,lk=lk,W=W,np=np,bic=bic,call=match.call())
  if(out_se){out$sela = sela; out$sePiv = sePiv; out$sePi = sePi; out$sePsi = sePsi}
  cat("------------|-------------|-------------|-------------|-------------|-------------|\n");
  class(out)="LMmixed"
  out

}

est_mc_basic <-
  function(S,yv,mod=0,tol=10^-8,maxit=1000,out_se=FALSE){

    warning("est_mc_basic function is no longer maintained. Please look at lmestMc function",call. = FALSE)
    # Preliminaries
    check_der = FALSE  # to check derivatives
    n = sum(yv)
    sS = dim(S)
    ns = sS[1]
    TT = sS[2]
    if(min(S,na.rm=T)>0){
      cat("|------------------- WARNING -------------------|\n")
      cat("|The first response category must be coded as 0 |\n")
      cat("|-----------------------------------------------|\n")
    }

    if(is.data.frame(S)){
      warning("Data frame not allowed for S")
    }

    if(ns!=length(yv)) stop("dimensions mismatch between S and yv")

    b = max(S)
    U = Pi  = array(0,c(b+1,b+1,TT))

    piv = table(rep(S[,1],yv))/n

    if(out_se) sepiv = sqrt(piv*(1-piv)/n)

    for(t in 2:TT){
      for(j1 in 0:b) for(j2 in 0:b){
        U[j1+1,j2+1,t] = sum((rep(S[,t-1],yv)==j1)*(rep(S[,t],yv)==j2))
      }
    }
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
    out = list(lk=lk,piv=piv,Pi=Pi,np=np,aic=aic,bic=bic,Fy=Fy,call=match.call())
    if(out_se){

      dimnames(sePi) = list(category=0:b,category=0:b,time=1:TT)

      out$sepiv = sepiv
      out$sePi = sePi

    }

    class(out)="MCbasic"
    (out)
  }

est_mc_cov <-
  function(S,X1=NULL,X2=NULL,yv=rep(1,nrow(S)),start=0,tol=10^-8,maxit=1000,out_se=FALSE,output=FALSE,fort=TRUE){

    warning("est_mc_cov function is no longer maintained. Please look at lmestMc function",call. = FALSE)
    # Preliminaries
    check_der = FALSE  # to check derivatives
    n = sum(yv)
    sS = dim(S)
    ns = sS[1]
    TT = sS[2]
    b = max(S)

    if(min(S,na.rm=T)>0){
      cat("|------------------- WARNING -------------------|\n")
      cat("|The first response category must be coded as 0 |\n")
      cat("|-----------------------------------------------|\n")
    }

    if(is.data.frame(S)){
      warning("Data frame not allowed for S")
    }

    if(ns!=length(yv)) stop("dimensions mismatch between S and yv")

    # Covariate structure and related matrices: initial probabilities
    if((b+1) == 2) GBe = as.matrix(c(0,1)) else{
      GBe = diag(b+1); GBe = GBe[,-1]
    }
    if(is.null(X1)){
      nc1=0
      Xlab = rep(1,ns)
      nameBe = NULL
    }else{
      if(is.vector(X1)) X1 = matrix(X1,ns,1)
      nc1 = dim(X1)[2] # number of covariates on the initial probabilities
      if(ns!= dim(X1)[1]) stop("dimension mismatch between S and X1")

      nameBe = colnames(X1)
      out = aggr_data(X1,fort=fort)
      Xdis = out$data_dis
      if(nc1==1) Xdis = matrix(Xdis,length(Xdis),1)
      Xlab = out$label
    }
    Xndis = max(Xlab)
    XXdis = array(0,c(b+1,b*(nc1+1),Xndis))
    for(i in 1:Xndis){
      if(nc1==0) xdis = 1 else xdis = c(1,Xdis[i,])
      XXdis[,,i] = GBe%*%(diag(b)%x%t(xdis))
    }

    # for the transition probabilities
    if(is.null(X2)){
      nc2 = 0
      Zlab = rep(1,ns*(TT-1))
      nameGa = NULL
      Zndis = max(Zlab)
    }else{
      if(TT==2) X2 = array(X2,c(ns,1,dim(X2)[2]))
      if(is.matrix(X2)) X2 = array(X2,c(ns,TT-1,1))
      nc2 = dim(X2)[3] # number of covariates on the transition probabilities
      if(ns!= dim(X2)[1]) stop("dimension mismatch between S and X2")

      nameGa = colnames(aperm(X2,c(1,3,2)))
      Z = NULL
      for(t in 1:(TT-1)) Z = rbind(Z,X2[,t,])
      if(nc2==1) Z = as.vector(X2)
      out = aggr_data(Z,fort=fort); Zdis = out$data_dis; Zlab = out$label; Zndis = max(Zlab)
      if(nc2==1) Zdis=matrix(Zdis,length(Zdis),1)
    }
    ZZdis = array(0,c(b+1,(b)*(nc2+1),Zndis,b+1))
    for(h in 1:(b+1)){
      if((b+1)==2){
        if(h == 1) GGa = as.matrix(c(0,1)) else GGa = as.matrix(c(1,0))
      }else{
        GGa = diag(b+1); GGa = GGa[,-h]
      }
      for(i in 1:Zndis){
        if(nc2==0) zdis = 1 else zdis = c(1,Zdis[i,])
        ZZdis[,,i,h] = GGa%*%(diag(b)%x%t(zdis))
      }
    }


    # parameters on initial probabilities
    if(start==0) be = array(0,(nc1+1)*b)
    else if(start==1){
      be = c(rnorm(1),rep(0,nc1))
      if((b+1)>2) for(h in 2:b) be = c(be,rnorm(1),rep(0,nc1))
    }
    out = prob_multilogit(XXdis,be,Xlab,fort)
    Piv = out$P; Pivdis = out$Pdis


    # parameters on transition probabilities
    Ga = matrix(0,(nc2+1)*b,b+1)
    if(start==0) Ga[1+(0:(b-1))*(nc2+1),] = -log(10)
    else if(start==1) Ga[1+(0:(b-1))*(nc2+1),] = -abs(rnorm(b))

    PIdis = array(0,c(Zndis,b+1,b+1)); PI = array(0,c(b+1,b+1,ns,TT))
    for(h in 1:(b+1)){
      tmp = ZZdis[,,,h]
      if(nc2==0) tmp = array(tmp,c(b+1,b,Zndis))
      out = prob_multilogit(tmp,Ga[,h],Zlab,fort)
      PIdis[,,h] = out$Pdis; PI[h,,,2:TT] = array(as.vector(t(out$P)),c(1,b+1,ns,TT-1))
    }


    #updating be
    V = matrix(0,ns,b+1)
    for(i in 1:ns) V[i,S[i,1]+1]=yv[i]
    out = est_multilogit(V,XXdis,Xlab,be,Pivdis,fort=fort)
    be = out$be; Pivdis = out$Pdi; Piv = out$P
    if(out_se){
      iFi = ginv(out$Fi)
      sebe = sqrt(diag(iFi))
    }
    #Updating Ga
    U = array(0,c(b+1,b+1,ns,TT))
    for(i in 1:ns) for(t in 2:TT){
      U[S[i,t-1]+1,S[i,t]+1,i,t] = yv[i]
    }
    if(out_se) sega = matrix(0,(nc2+1)*b,b+1)
    for(h in 1:(b+1)){
      UU = NULL
      for(t in 2:TT) UU = rbind(UU,t(U[h,,,t]))
      tmp = ZZdis[,,,h]
      if(nc2==0) tmp = array(tmp,c(b+1,b,Zndis))
      tmp2 = PIdis[,,h]
      if(Zndis==1) tmp2 = matrix(tmp2,1,b+1)
      out = est_multilogit(UU,tmp,Zlab,Ga[,h],tmp2,fort=fort)
      PIdis[,,h] = out$Pdis; PI[h,,,2:TT] = array(as.vector(t(out$P)),c(1,b+1,ns,TT-1)); Ga[,h] = out$be
      if(out_se){
        iFi = ginv(out$Fi)
        sega[,h] = sqrt(diag(iFi))
      }
    }


    # Compute log-likelihood
    lk = sum(V*log(Piv))+sum(U[,,,2:TT]*log(PI[,,,2:TT]))


    # Compute number of parameters
    np = b*(nc1+1)
    np = np+b*(nc2+1)*(b+1)
    aic = -2*lk+np*2
    bic = -2*lk+np*log(n)


    Be = matrix(be,nc1+1,b)
    if (is.null(nameBe)){
      if(nc1==0) nameBe = c("Intercept") else nameBe = c("intercept",paste("X1",1:nc1,sep=""))
    }else{
      nameBe = c("intercept",nameBe)
    }

    dimnames(Be) = list(nameBe,logit=2:(b+1))
    if(out_se) {seBe = matrix(sebe,nc1+1,b); dimnames(seBe) = list(nameBe,logit=2:(b+1))}
    if(is.null(nameGa)){
      if(nc2==0) nameGa = c("Intercept") else nameGa = c("intercept", paste("X2",1:nc2,sep=""))
    }else{
      nameGa = c("intercept",nameGa)
    }
    if((b+1)>2) {
      Ga = array(as.vector(Ga),c(nc2+1,b,b+1))
      dimnames(Ga) = list(nameGa,logit=2:(b+1),logit=1:(b+1))
    }else if((b+1)==2){
      dimnames(Ga) = 	list(nameGa,logit=1:(b+1))
    }
    if(out_se){
      if((b+1)==2){
        seGa = matrix(sega,nc2+1,2)
        dimnames(seGa) = list(nameGa,logit=1:(b+1))
      }else if((b+1)>2){
        seGa = array(as.vector(sega),c(nc2+1,b,b+1))
        dimnames(seGa) = list(nameGa,logit=2:(b+1),logit=1:(b+1))
      }
    }
    # adjust output
    lk = as.vector(lk)
    if(output){
      dimnames(Piv)=list(subject=1:ns,category=0:b)
      dimnames(PI)=list(category=0:b,category=0:b,subject=1:ns,time=1:TT)
    }
    out = list(lk=lk,Be=Be,Ga=Ga,np=np,aic=aic,bic=bic,
               call=match.call())
    if(out_se){
      out$seBe = seBe
      out$seGa = seGa
    }
    # final output
    if(output){
      out$PI = PI
      out$Piv = Piv
    }
    #cat(" |-------------|-------------|-------------|-------------|\n");
    class(out)="MCcov"
    (out)

  }

# est_mc_cov_latent <-
#   function(S,X1=NULL,X2=NULL,yv=rep(1,nrow(S)),start=0,tol=10^-8,maxit=1000,out_se=FALSE,output=FALSE,fort=TRUE){
#
#     warning("est_mc_cov_latent function is no longer maintained. Please look at lmestMc function")
#     # Preliminaries
#     check_der = FALSE  # to check derivatives
#     n = sum(yv)
#     sS = dim(S)
#     ns = sS[1]
#     TT = sS[2]
#     b = max(S)
#
#     if(min(S,na.rm=T)>0){
#       cat("|------------------- WARNING -------------------|\n")
#       cat("|The first response category must be coded as 0 |\n")
#       cat("|-----------------------------------------------|\n")
#     }
#
#     if(is.data.frame(S)){
#       warning("Data frame not allowed for S")
#     }
#
#     if(ns!=length(yv)) stop("dimensions mismatch between S and yv")
#
#     # Covariate structure and related matrices: initial probabilities
#     if((b+1) == 2) GBe = as.matrix(c(0,1)) else{
#       GBe = diag(b+1); GBe = GBe[,-1]
#     }
#     if(is.null(X1)){
#       nc1=0
#       Xlab = rep(1,ns)
#       nameBe = NULL
#     }else{
#       if(is.vector(X1)) X1 = matrix(X1,ns,1)
#       nc1 = dim(X1)[2] # number of covariates on the initial probabilities
#       if(ns!= dim(X1)[1]) stop("dimension mismatch between S and X1")
#
#       nameBe = colnames(X1)
#       out = aggr_data(X1,fort=fort)
#       Xdis = out$data_dis
#       if(nc1==1) Xdis = matrix(Xdis,length(Xdis),1)
#       Xlab = out$label
#     }
#     Xndis = max(Xlab)
#     XXdis = array(0,c(b+1,b*(nc1+1),Xndis))
#     for(i in 1:Xndis){
#       if(nc1==0) xdis = 1 else xdis = c(1,Xdis[i,])
#       XXdis[,,i] = GBe%*%(diag(b)%x%t(xdis))
#     }
#
#     # for the transition probabilities
#     if(is.null(X2)){
#       nc2 = 0
#       Zlab = rep(1,ns*(TT-1))
#       nameGa = NULL
#       Zndis = max(Zlab)
#     }else{
#       if(TT==2) X2 = array(X2,c(ns,1,dim(X2)[2]))
#       if(is.matrix(X2)) X2 = array(X2,c(ns,TT-1,1))
#       nc2 = dim(X2)[3] # number of covariates on the transition probabilities
#       if(ns!= dim(X2)[1]) stop("dimension mismatch between S and X2")
#
#       nameGa = colnames(aperm(X2,c(1,3,2)))
#       Z = NULL
#       for(t in 1:(TT-1)) Z = rbind(Z,X2[,t,])
#       if(nc2==1) Z = as.vector(X2)
#       out = aggr_data(Z,fort=fort); Zdis = out$data_dis; Zlab = out$label; Zndis = max(Zlab)
#       if(nc2==1) Zdis=matrix(Zdis,length(Zdis),1)
#     }
#     ZZdis = array(0,c(b+1,(b)*(nc2+1),Zndis,b+1))
#     for(h in 1:(b+1)){
#       if((b+1)==2){
#         if(h == 1) GGa = as.matrix(c(0,1)) else GGa = as.matrix(c(1,0))
#       }else{
#         GGa = diag(b+1); GGa = GGa[,-h]
#       }
#       for(i in 1:Zndis){
#         if(nc2==0) zdis = 1 else zdis = c(1,Zdis[i,])
#         ZZdis[,,i,h] = GGa%*%(diag(b)%x%t(zdis))
#       }
#     }
#
#
#     # parameters on initial probabilities
#     if(start==0) be = array(0,(nc1+1)*b)
#     else if(start==1){
#       be = c(rnorm(1),rep(0,nc1))
#       if((b+1)>2) for(h in 2:b) be = c(be,rnorm(1),rep(0,nc1))
#     }
#     out = prob_multilogit(XXdis,be,Xlab,fort)
#     Piv = out$P; Pivdis = out$Pdis
#
#
#     # parameters on transition probabilities
#     Ga = matrix(0,(nc2+1)*b,b+1)
#     if(start==0) Ga[1+(0:(b-1))*(nc2+1),] = -log(10)
#     else if(start==1) Ga[1+(0:(b-1))*(nc2+1),] = -abs(rnorm(b))
#
#     PIdis = array(0,c(Zndis,b+1,b+1)); PI = array(0,c(b+1,b+1,ns,TT))
#     for(h in 1:(b+1)){
#       tmp = ZZdis[,,,h]
#       if(nc2==0) tmp = array(tmp,c(b+1,b,Zndis))
#       out = prob_multilogit(tmp,Ga[,h],Zlab,fort)
#       PIdis[,,h] = out$Pdis; PI[h,,,2:TT] = array(as.vector(t(out$P)),c(1,b+1,ns,TT-1))
#     }
#
#
#     #updating be
#     V = matrix(0,ns,b+1)
#     for(i in 1:ns) V[i,S[i,1]+1]=yv[i]
#     out = est_multilogit(V,XXdis,Xlab,be,Pivdis,fort=fort)
#     be = out$be; Pivdis = out$Pdi; Piv = out$P
#     if(out_se){
#       iFi = ginv(out$Fi)
#       sebe = sqrt(diag(iFi))
#     }
#     #Updating Ga
#     U = array(0,c(b+1,b+1,ns,TT))
#     for(i in 1:ns) for(t in 2:TT){
#       U[S[i,t-1]+1,S[i,t]+1,i,t] = yv[i]
#     }
#     if(out_se) sega = matrix(0,(nc2+1)*b,b+1)
#     for(h in 1:(b+1)){
#       UU = NULL
#       for(t in 2:TT) UU = rbind(UU,t(U[h,,,t]))
#       tmp = ZZdis[,,,h]
#       if(nc2==0) tmp = array(tmp,c(b+1,b,Zndis))
#       tmp2 = PIdis[,,h]
#       if(Zndis==1) tmp2 = matrix(tmp2,1,b+1)
#       out = est_multilogit(UU,tmp,Zlab,Ga[,h],tmp2,fort=fort)
#       PIdis[,,h] = out$Pdis; PI[h,,,2:TT] = array(as.vector(t(out$P)),c(1,b+1,ns,TT-1)); Ga[,h] = out$be
#       if(out_se){
#         iFi = ginv(out$Fi)
#         sega[,h] = sqrt(diag(iFi))
#       }
#     }
#
#
#     # Compute log-likelihood
#     lk = sum(V*log(Piv))+sum(U[,,,2:TT]*log(PI[,,,2:TT]))
#
#
#     # Compute number of parameters
#     np = b*(nc1+1)
#     np = np+b*(nc2+1)*(b+1)
#     aic = -2*lk+np*2
#     bic = -2*lk+np*log(n)
#
#
#     Be = matrix(be,nc1+1,b)
#     if (is.null(nameBe)){
#       if(nc1==0) nameBe = c("Intercept") else nameBe = c("intercept",paste("X1",1:nc1,sep=""))
#     }else{
#       nameBe = c("intercept",nameBe)
#     }
#
#     dimnames(Be) = list(nameBe,logit=2:(b+1))
#     if(out_se) {seBe = matrix(sebe,nc1+1,b); dimnames(seBe) = list(nameBe,logit=2:(b+1))}
#     if(is.null(nameGa)){
#       if(nc2==0) nameGa = c("Intercept") else nameGa = c("intercept", paste("X2",1:nc2,sep=""))
#     }else{
#       nameGa = c("intercept",nameGa)
#     }
#     if((b+1)>2) {
#       Ga = array(as.vector(Ga),c(nc2+1,b,b+1))
#       dimnames(Ga) = list(nameGa,logit=2:(b+1),logit=1:(b+1))
#     }else if((b+1)==2){
#       dimnames(Ga) = 	list(nameGa,logit=1:(b+1))
#     }
#     if(out_se){
#       if((b+1)==2){
#         seGa = matrix(sega,nc2+1,2)
#         dimnames(seGa) = list(nameGa,logit=1:(b+1))
#       }else if((b+1)>2){
#         seGa = array(as.vector(sega),c(nc2+1,b,b+1))
#         dimnames(seGa) = list(nameGa,logit=2:(b+1),logit=1:(b+1))
#       }
#     }
#     # adjust output
#     lk = as.vector(lk)
#     if(output){
#       dimnames(Piv)=list(subject=1:ns,category=0:b)
#       dimnames(PI)=list(category=0:b,category=0:b,subject=1:ns,time=1:TT)
#     }
#     out = list(lk=lk,Be=Be,Ga=Ga,np=np,aic=aic,bic=bic,
#                call=match.call())
#     if(out_se){
#       out$seBe = seBe
#       out$seGa = seGa
#     }
#     # final output
#     if(output){
#       out$PI = PI
#       out$Piv = Piv
#     }
#     #cat(" |-------------|-------------|-------------|-------------|\n");
#     class(out)="MClatent"
#     (out)
#
#   }
