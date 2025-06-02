lmbasic.cont  <- function(Y,yv,k,start=0,modBasic=0,tol=10^-8,maxit=1000,out_se=FALSE,piv=NULL,
                          Pi=NULL,Mu=NULL,Si=NULL,output=FALSE,fort=TRUE,ntry=0){

# t0 = proc.time()

# ---- Repeat estimation if necessary ----
  if(ntry>0){
    cat("* Deterministic inditialization *\n")
    out = lmbasic.cont(Y,yv,k,start=0,modBasic=modBasic,tol=tol,maxit=maxit,
                            out_se=out_se,output=output,fort=fort)
    lkv_glob = out$lk
    for(it0 in 1:(k*ntry)){
      cat("\n* Random inditialization (",it0,"/",k*ntry,") *\n",sep="")
      outr = try(lmbasic.cont(Y,yv,k,start=1,modBasic=modBasic,tol=tol,maxit=maxit,
                              out_se=out_se,output=output,fort=fort))
      if(!inherits(outr,"try-error")){
        lkv_glob = c(lkv_glob,outr$lk)
        if(outr$lk>out$lk) out = outr #SP
        out = outr
      }
    }
    out$lkv_glob = lkv_glob
    return(out)
  }

# ---- Preliminaries ----
  check_der = FALSE  # to check derivatives
  sY = dim(Y)
  ns = as.integer(dim(Y)[1])
  n = sum(yv)
  k = as.integer(k)
  TT = as.integer(sY[2])
  mod <- modBasic
  if(is.data.frame(Y)){
    warning("Data frame not allowed for Y")
  }
  if(length(sY)==2){
    r = 1
    if(is.matrix(Y)) Y = array(Y,c(dim(Y),1))
  }else r = sY[3]
  r = as.integer(r)

# Check and inpute for missing data
  miss = any(is.na(Y))
  R= NULL
  if(miss){
    R = (!is.na(Y))
    if(fort) RR = array(as.integer(1*R),c(n,TT,r))
    Y[is.na(Y)] = 0
    cat("Missing data in the dataset dealt with as MAR\n")
  }

# Preliminary objects
  Yv = matrix(Y,ns*TT,r)
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
      Si = as.matrix(cov(Yv,use = "complete.obs"))
      if(start==0) Mu = as.matrix(colMeans(Yv,na.rm=TRUE))
      if(start==1) Mu = as.matrix(rmvnorm(1,colMeans(Yv,na.rm=TRUE),Si))
      lk = 0
      for (i in 1:ns) for(t in 1:TT){
        indo = R[i,t,]
        if(sum(R[i,t,])==1){
          lk = lk + dnorm(Y[i,t,][indo],Mu[indo],sqrt(Si[indo,indo]),log=TRUE)
        }else if(sum(R[i,t,])>1){
          lk = lk + dmvnorm(Y[i,t,][indo],Mu[indo],Si[indo,indo],log=TRUE)
        }
      }
      names(lk) = NULL
      # print(c(2,proc.time()-t0))
      it = 0; lko = lk; lkv = lk; par = c(as.vector(Mu),as.vector(Si))
      cat("------------|-------------|-------------|-------------|-------------|\n")
      cat("      k     |     step    |     lk      |    lk-lko   | discrepancy |\n")
      cat("------------|-------------|-------------|-------------|-------------|\n")
      cat(sprintf("%11g",c(k,0,lk)),"\n",sep=" | ")
      while(((lk-lko)/abs(lko)>tol | it==0) & it<maxit){
        it = it+1
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
        Yvimp = matrix(Yimp,ns*TT,r)
        Mu = as.matrix(colMeans(Yvimp,na.rm=TRUE))
        iSi = solve(Si)
        Tmp = Yvimp-rep(1,nt)%o%c(Mu)
        Si = (Vc+t(Tmp)%*%Tmp)/nt
        # print(c(3,proc.time()-t0))
        lko = lk; lk = 0
        for (i in 1:ns) for(t in 1:TT){
          indo = R[i,t,]
          if(sum(R[i,t,])==1){
            lk = lk + dnorm(Y[i,t,][indo],Mu[indo],sqrt(Si[indo,indo]),log=TRUE)
          }else if(sum(R[i,t,])>1){
            lk = lk + dmvnorm(Y[i,t,][indo],Mu[indo],Si[indo,indo],log=TRUE)
          }
        }
        names(lk) = NULL
        lkv = c(lkv,lk)
        paro = par; par = c(as.vector(Mu),as.vector(Si))
        if(it%%10 == 0) cat(sprintf("%11g",c(k,it,lk,lk-lko,max(abs(par-paro)))),"\n",sep=" | ")
        if(lk-lko<(-1)) browser()
        # print(c(4,proc.time()-t0))
      }
      if(it%%10>0) cat(sprintf("%11g",c(k,it,lk,lk-lko,max(abs(par-paro)))),"\n",sep=" | ")
      cat("------------|-------------|-------------|-------------|-------------|\n")
    }else{
      yvv = rep(yv,TT)
      Mu = as.matrix(colSums(yvv*Yv,na.rm=TRUE))/(n*TT)
      Tmp = Yv-rep(1,nt)%o%c(Mu)
      Si = (t(Tmp)%*%(yvv*Tmp))/(n*TT)
      lkv = lk = sum(yvv*dmvnorm(Yv,Mu,Si,log=TRUE))
    }

# ---- model selection ----
    np = k*r+r*(r+1)/2
    aic = -2*lk+np*2
    bic = -2*lk+np*log(n)

# ---- Information matrix ----
    if(out_se){
      th = NULL
      th = c(th,as.vector(Mu))
      th = c(th,Si[upper.tri(Si,TRUE)])
      th0 = th
      out = lk_obs_cont_miss(th0,yv,Bm,Cm,k,Y,R,TT,r,mod,fort)
      lk0 = out$lk; sc0 = out$sc
      lth = length(th)
      scn = rep(0,lth)
      J = matrix(0,lth,lth)
      for(j in 1:lth){
        thj = th0; thj[j] = thj[j]+10^-6
        out = lk_obs_cont_miss(thj,yv,Bm,Cm,k,Y,R,TT,r,mod,fort)
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
      nMu = r*k
      nSi = r*(r+1)/2
      Va2 = Va[1:(nMu+nSi),1:(nMu+nSi)]
      if(any(diag(Va2)<0)) warning("negative elements in the estimated variance")
      se2 = sqrt(abs(diag(Va2)))
      se = se2
      seMu = se[1:nMu]
      seSi = se[nMu+(1:nSi)]
    }
    Mu = matrix(Mu,r,k)
    nameY <- dimnames(Y)[[3]]
    dimnames(Mu) <- list(nameY,state=1:k)
    out = list(lk=lk,piv=piv,Pi=Pi,Mu=Mu,Si=Si,np=np, k = k, aic=aic,bic=bic,lkv=lkv,V=array(1,c(n,k,TT)),n = n,
               TT = TT, modBasic = mod, ns=ns, yv=yv)
    if(out_se){
      seMu = matrix(seMu,r,k)
      dimnames(seMu) = list(nameY,state=1:k)
      seSi2 = matrix(0,r,r)
      if(r==1){
        seSi2 = seSi
      }else{
        seSi2[upper.tri(seSi2,TRUE)]=seSi
        seSi2 = seSi2+t(seSi2-diag(diag(seSi2)))
      }
      seSi = seSi2
      # if(r==1) dimnames(seMu) = list(item=1,state=1:k) else dimnames(seMu)=list(item=1:r,state=1:k)
      out$seMu = seMu
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
    class(out)="LMbasiccont"
    return(out)
  }
  
# ---- Starting values ----
    if(start == 0){
      mu = colMeans(Yv,na.rm=TRUE)
      Si = cov(Yv,use = "complete.obs"); std = sqrt(diag(Si))
      qt = qnorm((1:k)/(k+1))
      Mu = matrix(0,r,k)
      for(u in 1:k) Mu[,u] = qt[u]*std+mu
      piv = rep(1,k)/k
      Pi = matrix(1,k,k)+9*diag(k); Pi = diag(1/rowSums(Pi))%*%Pi;
      Pi = array(Pi,c(k,k,TT)); Pi[,,1] = 0
    }
    if(start==1){
      Mu = matrix(0,r,k)
      mu = colMeans(Yv,na.rm=TRUE)
      Si = cov(Yv,use = "complete.obs")
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
    }

# ---- EM algorithm ----
    out = complk_cont_miss(Y,R,yv,piv,Pi,Mu,Si,k,fort=fort)
    lk = out$lk; Phi = out$Phi; L = out$L; pv = out$pv
    
    cat("------------|-------------|-------------|-------------|-------------|-------------|-------------|\n");
    cat("     mod    |      k      |    start    |     step    |     lk      |    lk-lko   | discrepancy |\n");
    cat("------------|-------------|-------------|-------------|-------------|-------------|-------------|\n");
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
      Vv = matrix(aperm(V,c(1,3,2)),ns*TT,k)
      if(miss){
        # print(c(3.5,proc.time()-t0))
        Mu00 = Mu; itc = 0  #FB: corretto update di Mu
        while((max(abs(Mu00-Mu))>10^-10 || itc==0) & itc<10){
          Mu00 = Mu; itc = itc+1
          Y1 = array(Y,c(ns,TT,r,k))
          Var = array(0,c(ns,TT,r,r))
          if(fort){
            out = .Fortran("updatevar",Y,RR,ns,TT,r,k,Mu,Si,Y1=Y1,Var=Var)
            Y1 = out$Y1; Var = out$Var
          }else{
            for(i in 1:ns) for(t in 1:TT){
              nr = sum(R[i,t,])
              if(nr==0){
                Y1[i,t,,] = Mu
                Var[i,t,,] = Si
              }else if(nr<r){
                indo = R[i,t,]; indm = !R[i,t,]
                Tmp = Si[indm,indo]%*%solve(Si[indo,indo])
                Var[i,t,indm,indm] = Si[indm, indm]-Tmp%*%Si[indo,indm]
                for(u in 1:k) Y1[i,t,indm,u] = Mu[indm,u]+Tmp%*%(Y[i,t,indo]-Mu[indo,u])
              }
            }
          }
          Mub = matrix(0,r,k)
          for(u in 1:k){
            Yv1 = matrix(Y1[,,,u],ns*TT)
            Mub[,u] = (t(Yv1)%*%Vv[,u])/sum(Vv[,u])
          }
          Mu = Mub
          Sitmp = matrix(0,r,r)
          for(u in 1:k){
            Yv1 = matrix(Y1[,,,u],ns*TT)
            Var1 = array(Var,c(ns*TT,r,r))
            Tmp = Yv1-rep(1,ns*TT)%*%t(Mu[,u])
            Sitmp = Sitmp+t(Tmp)%*%(Vv[,u]*Tmp)+apply(Vv[,u]*Var1,c(2,3),sum)
          }
          Si = Sitmp/(n*TT)
          Mu00 = Mu
        }
      #  print(c(itc,max(abs(Mu-Mu00))))
      }else{
        Mu00 = Mu; itc = 0
        Mub = matrix(0,r,k)
        for(u in 1:k) Mub[,u] = (t(Yv)%*%Vv[,u])/sum(Vv[,u])
        while((max(abs(Mu00-Mu))>10^-10 || itc==0) & itc<10){
          Mu00 = Mu; itc = itc+1
          Mu = Mub
          Si = matrix(0,r,r)
          for(u in 1:k) Si = Si+ t(Yv-rep(1,ns*TT)%*%t(Mu[,u]))%*%(Vv[,u]*as.matrix(Yv-rep(1,ns*TT)%*%t(Mu[,u]))) #FB: velocizzato togliendo diag
          Si = Si/(n*TT)
          Mu00 = Mu
        }
      }
      # print(c(4,proc.time()-t0))
# Update piv and Pi
      if(n==1) piv = (V[,,1])/n
      else piv = colSums(V[,,1])/n
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
      out = complk_cont_miss(Y,R,yv,piv,Pi,Mu,Si,k, fort = fort)
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
      th = NULL
      th = c(th,as.vector(Mu))
      th = c(th,Si[upper.tri(Si,TRUE)])
      th = c(th,B%*%log(piv))
      if(mod==0) for(t in 2:TT) for(u in 1:k) th = c(th,C[,,u]%*%log(Pi[u,,t]))
      if(mod==1) for(u in 1:k) th = c(th,C[,,u]%*%log(Pi[u,,2]))
      th0 = th
      out = lk_obs_cont_miss(th0,yv,Bm,Cm,k,Y,R,TT,r,mod,fort)
      lk0 = out$lk; sc0 = out$sc
      lth = length(th)
      scn = rep(0,lth)
      J = matrix(0,lth,lth)
      for(j in 1:lth){
        thj = th0; thj[j] = thj[j]+10^-6
        out = lk_obs_cont_miss(thj,yv,Bm,Cm,k,Y,R,TT,r,mod,fort)
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
      nMu = r*k
      nSi = r*(r+1)/2
      Va2 = Va[1:(nMu+nSi),1:(nMu+nSi)]
      if(any(diag(Va2)<0)) warning("negative elements in the estimated variance")
      se2 = sqrt(abs(diag(Va2)))
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
    rb = 0
    np = (k-1)+k*(r-rb)+rb+r*(r+1)/2
    if(mod==0) np = np+(TT-1)*k*(k-1)
    if(mod==1) np = np+k*(k-1)
    if(mod>1) np = np+2*k*(k-1)
    aic = -2*lk+np*2
    bic = -2*lk+np*log(n)
# local decoding
    Ul = matrix(0,ns,TT)
    for(i in 1:ns) for(t in 1:TT) Ul[i,t] = which.max(V[i,,t])

    # adjust output
    #	if(any(yv!=1)) V = V/yv

    lk = as.vector(lk)
    dimnames(Pi)=list(state=1:k,state=1:k,time=1:TT)
    #	dimnames(Mu) = list(dimnames(Y)[[3]],state=1:k)
    #	dimnames(Si) = list(dimnames(Y)[[3]],dimnames(Y)[[3]])
    if(r==1) dimnames(Mu) = list(item=1,state=1:k) else dimnames(Mu)=list(item=1:r,state=1:k)
    # dimnames(Si)=list(item=1:r,item=1:r)
    nameY = dimnames(Y)[[3]]
    rownames(Mu) <- nameY
    out = list(lk=lk,piv=piv,Pi=Pi,Mu=Mu,Si=Si,np=np,k = k,aic=aic,bic=bic,lkv=lkv, n = n, TT = TT, 
               modBasic = mod, ns=ns, yv=yv)
    if(miss){
      out$Y = Y
      out$Yimp = Yimp  ##SP:modificato qui
    }
    if(out_se){
      seMu = matrix(seMu,r,k)
      dimnames(seMu) = list(nameY,state=1:k)
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
      out$seMu = seMu
      out$seSi = seSi
    }
    # final output
    if(miss) out$Y = Y
    if(output){
      if(k>1){
        Pmarg <- as.matrix(piv)
        for(t in 2:TT) Pmarg= cbind(Pmarg,t(Pi[,,t])%*%Pmarg[,t-1])
      }else Pmarg<-NULL
      out = c(out,list(V = V, Ul = Ul, Pmarg=Pmarg))
    } 
    if(out_se){
      out$sc = sc0
      out$J = J
    }
    class(out)="LMbasiccont"
    return(out)
  }
