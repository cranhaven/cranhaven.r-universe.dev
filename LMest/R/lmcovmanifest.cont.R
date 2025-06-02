lmcovmanifest.cont  <- function(Y,X,yv,k,start=0,modBasic=0,tol=10^-8,maxit=1000,
                                out_se=FALSE,piv=NULL,Pi=NULL,Mu=NULL,Si=NULL,
                                output=FALSE,fort=TRUE,ntry=0){

# t0 = proc.time()

# ---- Repeat estimation if necessary ----
  if(ntry>0){
    cat("* Deterministic inditialization *\n")
    out = lmcovmanifest.cont(Y,X,k,start=0,modBasic=modBasic,tol=tol,maxit=maxit,
                             out_se=out_se,output=output,fort=fort)
    lkv_glob = out$lk
    for(it0 in 1:(k*ntry)){
      cat("\n* Random inditialization (",it0,"/",k*ntry,") *\n",sep="")
      outr = try(lmcovmanifest.cont(Y,X,k,start=1,modBasic=modBasic,tol=tol,maxit=maxit,
                              out_se=out_se,output=output,fort=fort))
      if(!inherits(outr,"try-error")){
        lkv_glob = c(lkv_glob,outr$lk)
        if(outr$lk>out$lk) out = outr #SP out = outr
      }
    }
    out$lkv_glob = lkv_glob
    return(out)
  }

# ---- Preliminaries ----
  check_der = FALSE  # to check derivatives
  sY = dim(Y)
  ns = as.integer(sY[1])
  k = as.integer(k)
  n = sum(yv)
  ncov = ncol(X)-1
  TT = as.integer(sY[2])
  mod <- modBasic
  if(is.data.frame(Y)) warning("Data frame not allowed for Y")
  if(length(sY)==2){
    r = 1
    if(is.matrix(Y)) Y = array(Y,c(dim(Y),1))
  }else{
    r = sY[3]
  }
  r = as.integer(r)

# Check and impute for missing data
  miss = any(is.na(Y))
  R = NULL
  if(miss){
    R = (!is.na(Y))
    if(fort) RR = array(as.integer(1*R),c(ns,TT,r))
    Y[is.na(Y)] = 0
    cat("Missing data in the dataset dealt with as MAR\n")
    Rv = matrix(aperm(R,c(2,1,3)),ns*TT,r)
  }

# Preliminary objects
  Yv = matrix(aperm(Y,c(2,1,3)),ns*TT,r)
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

#---- When there is just 1 latent class ----
  if(k == 1){
    piv = 1; Pi = 1
    nt = nrow(Yv)
    # print(c(1,proc.time()-t0))
    if(miss){
      Yvimp = Yv
      for(j in 1:r) if(any(!Rv[,j])) Yvimp[!Rv[,j],j] = mean(Yv[,j],na.rm=TRUE)
      Be = solve(t(X)%*%X)%*%t(X)%*%Yvimp
      Mu = X%*%Be
      Tmp = Yvimp-Mu
      Si = (t(Tmp)%*%Tmp)/nt
      if(start==1){
        seBe = sqrt(diag(solve(t(X)%*%X))%o%diag(Si))
        Be = Be+rnorm(r*(ncov+1),0,seBe)
        Mu = X%*%Be
      }
      lk = 0
      for (i in 1:nt){
        indo = Rv[i,]
        if(sum(Rv[i,])==1){
          lk = lk + dnorm(Yv[i,indo],Mu[i,indo],sqrt(Si[indo,indo]),log=TRUE)
        }else if(sum(Rv[i,])>1){
          lk = lk + dmvnorm(Yv[i,indo],Mu[i,indo],Si[indo,indo],log=TRUE)
        }
      }
      names(lk) = NULL
      # print(c(2,proc.time()-t0))
      it = 0; lko = lk; lkv = lk; par = c(as.vector(Be),as.vector(Si))
      cat("------------|-------------|-------------|-------------|-------------|\n")
      cat("      k     |     step    |     lk      |    lk-lko   | discrepancy |\n")
      cat("------------|-------------|-------------|-------------|-------------|\n")
      cat(sprintf("%11g",c(k,0,lk)),"\n",sep=" | ")
      while(((lk-lko)/abs(lko)>tol | it==0) & it<1000){
        it = it+1
        Yvimp = Yv 
        Vc = matrix(0,r,r)
        for(i in 1:nt){
          indo = Rv[i,]
          if(sum(indo)==0){
            Yvimp[i,] = Mu[i,]
            Vc = Vc+Si
          }else if(sum(indo)<r){
            iSi = try(solve(Si[indo,indo]))
            if(!inherits(iSi,"try-error")) ginv(Si[indo,indo])
            Yvimp[i,!indo] = Mu[i,!indo]+Si[!indo,indo]%*%iSi%*%(Yv[i,indo]-Mu[i,indo])
            Vc[!indo,!indo] = Vc[!indo,!indo]+Si[!indo,!indo]-Si[!indo,indo]%*%iSi%*%Si[indo,!indo]
          }
        }
        Yimp = aperm(array(Yvimp,c(TT,ns,r)),c(2,1,3))
        Be = solve(t(X)%*%X)%*%t(X)%*%Yvimp
        Mu = X%*%Be
        iSi = solve(Si)
        Tmp = Yvimp-Mu
        Si = (Vc+t(Tmp)%*%Tmp)/nt
        # print(c(3,proc.time()-t0))
        lko = lk; lk = 0
        for (i in 1:nt){
          indo = Rv[i,]
          if(sum(Rv[i,])==1){
            lk = lk + dnorm(Yv[i,indo],Mu[i,indo],sqrt(Si[indo,indo]),log=TRUE)
          }else if(sum(Rv[i,])>1){
            lk = lk + dmvnorm(Yv[i,indo],Mu[i,indo],Si[indo,indo],log=TRUE)
          }
        }
        names(lk) = NULL
        lkv = c(lkv,lk)
        paro = par; par = c(as.vector(Be),as.vector(Si))
        if(it%%10 == 0) cat(sprintf("%11g",c(k,it,lk,lk-lko,max(abs(par-paro)))),"\n",sep=" | ")
        if(lk-lko<(-1)) browser()
        # print(c(4,proc.time()-t0))
      }
      if(it%%10>0) cat(sprintf("%11g",c(k,it,lk,lk-lko,max(abs(par-paro)))),"\n",sep=" | ")
      cat("------------|-------------|-------------|-------------|-------------|\n")
    }else{
      Be = solve(t(X)%*%X)%*%t(X)%*%Yv
      Mu = X%*%Be
      Tmp = Yv-Mu
      yvv = rep(yv,each=TT)
      Si = (t(Tmp)%*%(yvv*Tmp))/(n*TT)
      lk = 0
      for(i in 1:nt) lk = lk+dmvnorm(Yv[i,],Mu[i,],Si,log=TRUE)*yvv[i]
      lkv = lk
    }

# ---- model selection ----
    np = (ncov+1)*r+r*(r+1)/2
    aic = -2*lk+np*2
    bic = -2*lk+np*log(n)

# ---- Information matrix ----
    if(out_se){
      th = as.vector(Be)
      th = c(th,Si[upper.tri(Si,TRUE)])
      th0 = th
      out = lk_obs_covmanifest.cont(th0,yv,Bm,Cm,k,Y,R,X,TT,r,ncov,mod,fort)
      lk0 = out$lk; sc0 = out$sc
      lth = length(th)
      scn = rep(0,lth)
      J = matrix(0,lth,lth)
      for(j in 1:lth){
        thj = th0; thj[j] = thj[j]+10^-6
        out = lk_obs_covmanifest.cont(thj,yv,Bm,Cm,k,Y,R,X,TT,r,ncov,mod,fort)
        scn[j] = (out$lk-lk0)/10^-6
        J[,j] = (out$sc-sc0)/10^-6
      }
      J = -(J+t(J))/2
      if(check_der){
        print(c(lk,lk0))
        print(round(cbind(scn,sc0,round(scn-sc0,4)),5))
      }
      #  se = sqrt(diag(ginv(J)))
      Va = try(solve(J))
      if(inherits(Va,"try-error")){
        ind = NULL
        for(j in 1:nrow(J)){
          tmp = try(solve(J[c(ind,j),c(ind,j)]),silent = TRUE)
          if(!inherits(tmp,"try-error")) ind = c(ind,j)
        }
        Va = matrix(0,nrow(J),nrow(J))
        Va[ind,ind] = solve(J[ind,ind])
      }
      nBe = r*(ncov+1)
      nSi = r*(r+1)/2
      Va2 = Va[1:(nBe+nSi),1:(nBe+nSi)]
      if(any(diag(Va2)<0)) warning("negative elements in the estimated variance")
      se2 = sqrt(abs(diag(Va2)))
      se = se2
      seBe = se[1:nBe]
      seSi = se[nBe+(1:nSi)]
    }
    nameX = colnames(X)
    nameY <- dimnames(Y)[[3]]
    dimnames(Be) <- list(nameX,nameY)
    Al = Be[1,,drop=FALSE]
    Be = Be[-1,,drop=FALSE]
    out = list(lk=lk,piv=piv,Pi=Pi,Al=Al,Be=Be,Si=Si,np=np, k = k, aic=aic,bic=bic,
               lkv=lkv,V=array(1,c(n,k,TT)),n = n,TT = TT, modBasic = mod)
    if(out_se){
      seBe = matrix(seBe,ncov+1,r)
      dimnames(seBe) = list(nameX,nameY)
      seAl = seBe[1,,drop=FALSE]
      seBe = seBe[-1,,drop=FALSE]
      seSi2 = matrix(0,r,r)
      if(r==1){
        seSi2 = seSi
      }else{
        seSi2[upper.tri(seSi2,TRUE)]=seSi
        seSi2 = seSi2+t(seSi2-diag(diag(seSi2)))
      }
      seSi = seSi2
      # if(r==1) dimnames(seMu) = list(item=1,state=1:k) else dimnames(seMu)=list(item=1:r,state=1:k)
      out$seAl = seAl
      out$seBe = seBe
      out$seSi = seSi
    }
    if(miss){
      out$Y = Y
      out$Yimp = Yimp  ##SP:modificato qui
    }
    if(out_se){
      out$sc = sc0
      out$J = J
    }
    class(out)="LMmanifestcont"
    return(out)
  }

# ---- Starting values ----
  Yvimp = Yv
  nt = nrow(Yv)
  if(miss) for(j in 1:r) if(any(!Rv[,j])) Yvimp[!Rv[,j],j] = mean(Yv[,j],na.rm=TRUE)
  Be = solve(t(X)%*%X)%*%t(X)%*%Yvimp
  Tmp = Yv-X%*%Be
  Si = (t(Tmp)%*%Tmp)/nt
  if(start == 0){
    std = sqrt(diag(Si))
    qt = qnorm((1:k)/(k+1))
    Al = matrix(0,k,r)
    for(u in 1:k) Al[u,] = qt[u]*std+Be[1,]
    Be = Be[-1,,drop=FALSE]
    piv = rep(1,k)/k
    Pi = matrix(1,k,k)+9*diag(k); Pi = diag(1/rowSums(Pi))%*%Pi
    Pi = array(Pi,c(k,k,TT)); Pi[,,1] = 0
  }
  if(start==1){
    seBe = sqrt(diag(solve(t(X)%*%X))%o%diag(Si))
    Be = Be+rnorm(r*(ncov+1),0,seBe)
    Al = matrix(0,k,r)
    for(u in 1:k) Al[u,] = rmvnorm(1,Be[1,],Si)
    Be = Be[-1,,drop=FALSE]
    Pi = array(runif(k^2*TT),c(k,k,TT))
    for(t in 2:TT) Pi[,,t] = diag(1/rowSums(Pi[,,t]))%*%Pi[,,t]
    Pi[,,1] = 0
    piv = runif(k); piv = piv/sum(piv)
  }
  if(start==2){
    if(is.null(piv)) stop("initial value of the initial probabilities (piv) must be given in input")
    if(is.null(Pi)) stop("initial value of the transition probabilities (Pi) must be given in input")
    if(is.null(Al)) stop("initial value of the conditional intercepts of the response variables (Al) must be given in input")
    if(is.null(Be)) stop("initial value of the conditional regression parameters of the response variables (Be) must be given in input")
    if(is.null(Si)) stop("initial value of the var-cov matrix common to all states (Si) must be given in input")
  }

# ---- EM algorithm ----
  Mu = array(0,c(nt,r,k))
  for(u in 1:k) Mu[,,u] = X%*%rbind(Al[u,],Be)
  out = complk_covmanifest.cont(Y,R,yv,piv,Pi,Mu,Si,k, fort = fort)
  #out = complk_covmanifest.cont(Y,R,yv,piv,Pi,Mu,Si,k, fort = FALSE)
  lk = out$lk; Phi = out$Phi; L = out$L; pv = out$pv
  cat("------------|-------------|-------------|-------------|-------------|-------------|-------------|\n")
  cat("     mod    |      k      |    start    |     step    |     lk      |    lk-lko   | discrepancy |\n")
  cat("------------|-------------|-------------|-------------|-------------|-------------|-------------|\n")
  cat(sprintf("%11g",c(mod,k,start,0,lk)),"\n",sep = " | ")
  it = 0; lko = lk-10^10; lkv = lk
  par = c(piv,as.vector(Pi),as.vector(Mu),as.vector(Si))
  if(any(is.na(par))) par = par[-which(is.na(par))]
  paro = par
    # Iterate until convergence
  while((lk-lko)/abs(lk)>tol & it<maxit){
    # t0 = proc.time()
    Mu0 = Mu; Si0 = Si; piv0 = piv; Pi0 = Pi
    it = it+1

# ---- E-step ----
    # Compute V and U
    V = array(0,c(ns,k,TT)); U = array(0,c(k,k,TT))
    M = matrix(1,ns,k)
    if(n==1){
      V[,,TT] = L[,,TT]/sum(L[1,,TT])
    }else{
      V[,,TT] = yv*L[,,TT]/rowSums(L[,,TT])
    }
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

# ---- M-step ----
# Update Mu
    Vv = matrix(aperm(V,c(3,1,2)),ns*TT,k)
    if(miss){
      # print(c(3.5,proc.time()-t0))
      Bec = rbind(Al,Be)
      Bec00 = Bec; itc = 0  #FB: corretto update di Mu
      while((max(abs(Bec00-Bec))>10^-10 || itc==0) & itc<10){
        Bec00 = Bec; itc = itc+1
        Y1 = array(Y,c(ns,TT,r,k))
        Var = array(0,c(ns,TT,r,r))
        if(fort){
          out = .Fortran("updatevar2",Y,RR,ns,TT,r,k,Mu,Si,Y1=Y1,Var=Var)
          Y1 = out$Y1; Var = out$Var
          Y10 = Y1
        }else{
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
        }
        Y1v = array(aperm(Y1,c(2,1,3,4)),c(ns*TT,r,k))
        NUM = matrix(0,ncov+k,r)
        DEN = matrix(0,ncov+k,ncov+k)
        for(u in 1:k){
          uv = rep(0,k); uv[u] = 1
          Xu = cbind(rep(1,nt)%o%uv,X[,-1])
          NUM = NUM+t(Xu)%*%(Vv[,u]*Y1v[,,u])
          DEN = DEN+t(Xu)%*%(Vv[,u]*Xu)
        }
        Bec = solve(DEN,NUM)
        Al = Bec[1:k,,drop=FALSE]
        Be = Bec[-(1:k),,drop=FALSE]
        for(u in 1:k) Mu[,,u] = X%*%rbind(Al[u,],Be)
        Sitmp = matrix(0,r,r)
        for(u in 1:k){
          Var1 = array(Var,c(ns*TT,r,r))
          Tmp = Y1v[,,u]-Mu[,,u]
          Sitmp = Sitmp+t(Tmp)%*%(Vv[,u]*Tmp)+apply(Vv[,u]*Var1,c(2,3),sum)
        }
        Si = Sitmp/(n*TT)
      }
    }else{
      NUM = matrix(0,ncov+k,r)
      DEN = matrix(0,ncov+k,ncov+k)
      for(u in 1:k){
        uv = rep(0,k); uv[u] = 1
        Xu = cbind(rep(1,nt)%o%uv,X[,-1])
        NUM = NUM+t(Xu)%*%(Vv[,u]*Yv)
        DEN = DEN+t(Xu)%*%(Vv[,u]*Xu)
      }
      Bec = solve(DEN,NUM)
      Al = Bec[1:k,,drop=FALSE]
      Be = Bec[-(1:k),,drop=FALSE]
      for(u in 1:k) Mu[,,u] = X%*%rbind(Al[u,],Be)
      Si = matrix(0,r,r)
      for(u in 1:k){
        Tmp = Yv-Mu[,,u]
        Si = Si+t(Tmp)%*%(Vv[,u]*Tmp)/(n*TT)
      }
    }
    # print(c(4,proc.time()-t0))
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
    
# Compute log-likelihood
    paro = par; par = c(piv,as.vector(Pi),as.vector(Mu),as.vector(Si))
    if(any(is.na(par))) par = par[-which(is.na(par))]
    lko = lk
    # print(c(4.5,proc.time()-t0))
  #  out = complk_covmanifest.cont(Y,R,yv,piv,Pi,Mu,Si,k, fort = FALSE)
     out = complk_covmanifest.cont(Y,R,yv,piv,Pi,Mu,Si,k, fort = fort)
    # print(c(4.7,proc.time()-t0))
    lk = out$lk; Phi = out$Phi; L = out$L; pv = out$pv
    if(it%%10 == 0) cat(sprintf("%11g",c(mod,k,start,it,lk,lk-lko,max(abs(par-paro)))),"\n",sep=" | ")
    lkv = c(lkv,lk)
    # print(c(5,proc.time()-t0))
  }
  if(it%%10 > 0) cat(sprintf("%11g",c(mod,k,start,it,lk,lk-lko,max(abs(par-paro)))),"\n",sep=" | ")
  cat("------------|-------------|-------------|-------------|-------------|-------------|-------------|\n");
  V2 = aperm(V,c(1,3,2))
  V2 = aperm(array(V2,c(ns,TT,k,r)),c(1,2,4,3))
  if(miss) Yimp = apply(Y1*V2,c(1,2,3),sum) 

# ---- Information matrix ----
  if(out_se){
    th = as.vector(Bec)
    th = c(th,Si[upper.tri(Si,TRUE)])
    th = c(th,B%*%log(piv))
    if(mod==0) for(t in 2:TT) for(u in 1:k) th = c(th,C[,,u]%*%log(Pi[u,,t]))
    if(mod==1) for(u in 1:k) th = c(th,C[,,u]%*%log(Pi[u,,2]))
    th0 = th
    #   browser()
    out = lk_obs_covmanifest.cont(th0,yv,Bm,Cm,k,Y,R,X,TT,r,ncov,mod,fort)
    lk0 = out$lk; sc0 = out$sc

    lth = length(th)
    scn = rep(0,lth)
    J = matrix(0,lth,lth)
    for(j in 1:lth){
      thj = th0; thj[j] = thj[j]+10^-6
      out = lk_obs_covmanifest.cont(thj,yv,Bm,Cm,k,Y,R,X,TT,r,ncov,mod,fort)
      scn[j] = (out$lk-lk0)/10^-6
      J[,j] = (out$sc-sc0)/10^-6
    }
    J = -(J+t(J))/2
  
    if(check_der){
      print(c(lk,lk0))
      print(round(cbind(scn,sc0,round(scn-sc0,4)),5))
    }
    #  se = sqrt(diag(ginv(J)))
    Va = try(solve(J))
    if(inherits(Va,"try-error")){
      ind = NULL
      for(j in 1:nrow(J)){
        tmp = try(solve(J[c(ind,j),c(ind,j)]),silent = TRUE)
        if(!inherits(tmp,"try-error")) ind = c(ind,j)
      }
      Va = matrix(0,nrow(J),nrow(J))
      Va[ind,ind] = solve(J[ind,ind])
    }
    nBe = r*(k+ncov)
    nSi = r*(r+1)/2
    Va2 = Va[1:(nBe+nSi),1:(nBe+nSi)]
    if(any(diag(Va2)<0)) warning("negative elements in the estimated variance")
    se2 = sqrt(abs(diag(Va2)))
    Va = Va[-(1:(nBe+nSi)),-(1:(nBe+nSi))]
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
    seBe = se[1:nBe]
    seSi = se[nBe+(1:nSi)]
    sepiv = se[nBe+nSi+(1:k)]
    if(mod==0) sePi = se[nBe+nSi+k+(1:(k*k*(TT-1)))]
    if(mod==1) sePi = se[nBe+nSi+k+(1:(k*k))]
    if(mod>1) sePi = se[nBe+nSi+k+(1:(k*k*2))]
  }
  
# Compute number of parameters
  np = (k-1)+k*r*(ncov+1)+r*(r+1)/2
  if(mod==0) np = np+(TT-1)*k*(k-1)
  if(mod==1) np = np+k*(k-1)
  if(mod>1) np = np+2*k*(k-1)
  aic = -2*lk+np*2
  bic = -2*lk+np*log(n)
# local decoding
  Ul = matrix(0,n,TT)
  for(i in 1:ns) for(t in 1:TT) Ul[i,t] = which.max(V[i,,t])

# adjust output
    #	if(any(yv!=1)) V = V/yv

  lk = as.vector(lk)
  dimnames(Pi)=list(state=1:k,state=1:k,time=1:TT)
  nameX = colnames(X)
  nameY <- dimnames(Y)[[3]]
  dimnames(Al) <- list(state=1:k,nameY)
  dimnames(Be) <- list(nameX[-1],nameY)
  out = list(lk=lk, piv=piv, Pi=Pi, Al=Al, Be=Be, Si=Si, np=np, k=k, aic=aic, bic=bic, lkv=lkv,
             n = n, TT = TT, modBasic = mod, ns=ns, yv=yv)
  if(miss){
    out$Y = Y
    out$Yimp = Yimp  ##SP:modificato qui
  }
  if(out_se){
    seBe = matrix(seBe,ncov+k,r)
    seAl = seBe[1:k,,drop=FALSE]
    seBe = seBe[-(1:k),,drop=FALSE]
    dimnames(seAl) <- list(state=1:k,nameY)
    dimnames(seBe) <- list(nameX[-1],nameY)
    seSi2 = matrix(0,r,r)
    if(r==1){
      seSi2 = seSi
    }else{
      seSi2[upper.tri(seSi2,TRUE)]=seSi
      seSi2 = seSi2+t(seSi2-diag(diag(seSi2)))
    }
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
    # if(r==1) dimnames(seMu) = list(item=1,state=1:k) else dimnames(seMu)=list(item=1:r,state=1:k)
    out$sepiv = sepiv
    out$sePi = sePi
    out$seAl = seAl
    out$seBe = seBe
    out$seSi = seSi
  }
  # final output
  if(miss) out$Y = Y
  if(output){
    if(k>1){
      Pmarg <- as.matrix(piv)
      for(t in 2:TT) Pmarg= cbind(Pmarg,t(Pi[,,t])%*%Pmarg[,t-1])
    }else Pmarg<-NULL
    out <-c(out,list(V = V, Ul = Ul, Pmarg=Pmarg))
  }
  if(out_se){
    out$sc = sc0
    out$J = J
  }
  class(out)="LMmanifestcont"
  return(out)
}
