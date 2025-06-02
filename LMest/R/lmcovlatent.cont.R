lmcovlatent.cont <- function(Y,X1=NULL,X2=NULL,yv,k,start=0,tol=10^-8,maxit=1000,
                                  paramLatent="multilogit",Mu=NULL,Si=NULL,Be=NULL,Ga=NULL,
                                  output=FALSE, out_se=FALSE,fort=TRUE,ntry=0){

# Fit the LM model for continuous outcomes with individual covariates in the distribution of the latent process
#
# INPUT:
# Y = array of available continuous outcome (n x TT x r)
# X1 = matrix of covariates affecting the initial probabilities
# X2 = array of covariates affecting the transition probabilities
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
# output = to return additional output

  # ---- Repeat estimation if necessary ----
  if(ntry>0){
    cat("* Deterministic inditialization *\n")
    out = lmcovlatent.cont(Y,X1=X1,X2=X2,yv=yv,k=k,start=0,tol=tol,maxit=maxit,
                                paramLatent=paramLatent,
                                out_se=out_se,output=output,fort=fort)
    lkv_glob = out$lk
    for(it0 in 1:(k*ntry)){
      cat("\n* Random inditialization (",it0,"/",k*ntry,") *\n",sep="")
      outr = try(lmcovlatent.cont(Y,X1=X1,X2=X2,yv=yv,k=k,start=1,tol=tol,maxit=maxit,
                                       paramLatent=paramLatent,
                                       out_se=out_se,output=output,fort=fort))
      if(!inherits(outr,"try-error")){
        lkv_glob = c(lkv_glob,outr$lk)
        if(outr$lk>out$lk) out = outr #SP out = outr
      }
    }
    out$lkv_glob = lkv_glob
    return(out)
  }

# ---- When there is just 1 latent class ----
  if(k == 1){
    cat("Use basic model without covariates\n")
    out = lmbasic.cont(Y,yv,k,start=start,tol=tol,maxit=maxit,out_se=out_se,
                            Mu=Mu,Si=Si,output=output,fort=fort)
    return(out)
  }

# ---- Preliminaries ----
  if(paramLatent=="difflogit"){
    cat("\n* With difflogit is not possible to avoid the intercept for the transition probabilities*\n\n")
    X2 = X2[,,-1,drop=FALSE]
  }
  check_der = FALSE # to check score and info
  param <- paramLatent
  sY = dim(Y)
  ns = as.integer(sY[1])
  k = as.integer(k)
  TT = as.integer(sY[2])
  n = sum(yv)
  if(length(sY)==2) r = 1
  else r = sY[3]
  Yv = matrix(Y,ns*TT,r)
  if(is.null(X1)){
    X1 = matrix(1,ns,1)
    colnames(X1) = "(Intercept)"
  }
  if(is.null(X2)){
    X2 = array(1,c(ns,TT-1,1))
    dimnames(X2) = list(NULL,NULL,"(Intercept)")
  }

# Check and inpute for missing data
  miss = any(is.na(Y))
  R= NULL
  if(miss){
    R = (!is.na(Y))
    if(fort) RR = array(as.integer(1*R),c(n,TT,r))
    Y[is.na(Y)] = 0
    cat("Missing data in the dataset dealt with as MAR\n")
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
    if(is.vector(X1)) X1 = matrix(X1,ns,1)
    nc1 = dim(X1)[2] # number of covariates on the initial probabilities
    if(ns!= dim(X1)[1]) stop("dimension mismatch between S and X1")
    nameBe = colnames(X1)
    out = aggr_data(X1)
    Xdis = out$data_dis
    if(nc1==1) Xdis = matrix(Xdis,length(Xdis),1)
    Xlab = out$label
  }
  Xndis = max(Xlab)
  XXdis = array(0,c(k,(k-1)*nc1,Xndis))
  for(i in 1:Xndis){
    if(nc1==0) xdis = 1 else xdis = Xdis[i,]
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
    #if(TT==2) X2 = array(X2,c(n,1,dim(X2)[2]))
    #if(is.matrix(X2)) X2 = array(X2,c(n,TT-1,1))
    nc2 = dim(X2)[3] # number of covariates on the transition probabilities
    if(ns!= dim(X2)[1]) stop("dimension mismatch between S and X2")
    nameGa = colnames(aperm(X2,c(1,3,2)))
    Z = NULL
    for(t in 1:(TT-1)) Z = rbind(Z,X2[,t,])
    if(nc2==1) Z = as.vector(X2)
    out = aggr_data(Z); Zdis = out$data_dis; Zlab = out$label; Zndis = max(Zlab)
    if(nc2==1) Zdis=matrix(Zdis,length(Zdis),1)
  }
  if(param=="multilogit"){
    ZZdis = array(0,c(k,(k-1)*nc2,Zndis,k))
    for(h in 1:k){
      if(k==2){
        if(h == 1) GGa = as.matrix(c(0,1)) else GGa = as.matrix(c(1,0))
      }else{
        GGa = diag(k); GGa = GGa[,-h]
      }
      for(i in 1:Zndis){
        if(nc2==0) zdis = 1 else zdis = Zdis[i,]
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

# ---- Starting values ----
# deterministic initialization
  if(start == 0){
    mu = colMeans(Yv,na.rm=TRUE)
    Si = cov(Yv,use = "complete.obs"); std = sqrt(diag(Si))
    qt = qnorm((1:k)/(k+1))
    Mu = matrix(0,r,k)
    for(u in 1:k) Mu[,u] = qt[u]*std+mu
    # parameters on initial probabilities
    be = array(0,nc1*(k-1))
    out = prob_multilogit(XXdis,be,Xlab,fort=fort)
    Piv = out$P; Pivdis = out$Pdis
    # parameters on transition probabilities
    if(param=="multilogit"){
      Ga = matrix(0,nc2*(k-1),k)
      Ga[1+(0:(k-2))*nc2,] = -log(10)
      PIdis = array(0,c(Zndis,k,k)); PI = array(0,c(k,k,ns,TT))
      for(h in 1:k){
        tmp = ZZdis[,,,h]
        if(nc2==1) tmp = array(tmp,c(k,(k-1),Zndis))
        out = prob_multilogit(tmp,Ga[,h],Zlab,fort=fort)
        PIdis[,,h] = out$Pdis; PI[h,,,2:TT] = array(as.vector(t(out$P)),c(1,k,ns,TT-1))
      }
    }else if(param=="difflogit"){
      Ga = matrix(0,k*(k-1)+(k-1)*nc2)
      Ga[1:((h-1)*k)] = -log(10)
      PI = array(0,c(k,k,ns,TT))
      out = prob_multilogit(ZZdis,Ga,Zlab,fort=fort)
      PIdis = out$Pdis; PI[,,,2:TT] = array(as.vector(t(out$P)),c(k,k,ns,TT-1))
      PI = aperm(PI,c(2,1,3,4))
    }
  }

# random initialization
  if(start==1){
    Mu = matrix(0,r,k)
    mu = colMeans(Yv,na.rm=TRUE)
    Si = cov(Yv,use = "complete.obs")
    for(u in 1:k) Mu[,u] = rmvnorm(1,mu,Si)
    # parameters on initial probabilities
    be = c(rnorm(1),rep(0,nc1-1))
    if(k>2) for(h in 2:(k-1)) be = c(be,rnorm(1),rep(0,nc1-1))
    out = prob_multilogit(XXdis,be,Xlab,fort=fort)
    Piv = out$P; Pivdis = out$Pdis
    # parameters on transition probabilities
    if(param=="multilogit"){
      Ga = matrix(0,nc2*(k-1),k)
      Ga[1+(0:(k-2))*nc2,] = -abs(rnorm((k-1)))
      PIdis = array(0,c(Zndis,k,k)); PI = array(0,c(k,k,ns,TT))
      for(h in 1:k){
        tmp = ZZdis[,,,h]
        if(nc2<=1) tmp = array(tmp,c(k,(k-1),Zndis))
        out = prob_multilogit(tmp,Ga[,h],Zlab,fort=fort)
        PIdis[,,h] = out$Pdis; PI[h,,,2:TT] = array(as.vector(t(out$P)),c(1,k,ns,TT-1))
      }
    }else if(param=="difflogit"){
      Ga = c(-abs(rnorm(k*(k-1))),rep(0,(k-1)*nc2))
      PI = array(0,c(k,k,ns,TT))
      out = prob_multilogit(ZZdis,Ga,Zlab,fort=fort)
      PIdis = out$Pdis; PI[,,,2:TT] = array(as.vector(t(out$P)),c(k,k,ns,TT-1))
      PI = aperm(PI,c(2,1,3,4))
    }
  }
  # initialization as input
  if(start==2){
    if(is.null(Be)) stop("initial value of the parameters on the initial probabilities (Be) must be given in input")
    if(is.null(Ga)) stop("initial value of the parameters on the transition probabilities (Ga) must be given in input")
    if(is.null(Mu)) stop("initial value of the conditional means of the response variables (Mu) must be given in input")
    if(is.null(Si)) stop("initial value of the var-cov matrix common to all states (Si) must be given in input")
    
# parameters on initial probabilities
    be = as.vector(Be)
    out = prob_multilogit(XXdis,be,Xlab,fort=fort)
    Piv = out$P; Pivdis = out$Pdis
    # parameters on transition probabilities
    if(param=="multilogit"){
      if(is.list(Ga)) stop("invalid mode (list) for Ga")
      Ga = matrix(Ga,nc2*(k-1),k)
      PIdis = array(0,c(Zndis,k,k)); PI = array(0,c(k,k,ns,TT))
      for(h in 1:k){
        tmp = ZZdis[,,,h]
        if(nc2==0) tmp = array(tmp,c(k,(k-1),Zndis))
        out = prob_multilogit(tmp,Ga[,h],Zlab,fort=fort)
        PIdis[,,h] = out$Pdis; PI[h,,,2:TT] = array(as.vector(t(out$P)),c(1,k,ns,TT-1))
      }
    }else if(param=="difflogit"){
      if(is.list(Ga)) Ga = c(as.vector(t(Ga[[1]])),as.vector(Ga[[2]]))
      if(length(Ga)!=k*(k-1)+(k-1)*nc2) stop("invalid dimensions for Ga")
      PI = array(0,c(k,k,ns,TT))
      out = prob_multilogit(ZZdis,Ga,Zlab,fort=fort)
      PIdis = out$Pdis; PI[,,,2:TT] = array(as.vector(t(out$P)),c(k,k,ns,TT-1))
      PI = aperm(PI,c(2,1,3,4))
    }
  }
  
# ---- EM algorithm ----
  out = lk_comp_latent_cont(Y,R,yv,Piv,PI,Mu,Si,k,fort=fort)
  lk = out$lk; Phi = out$Phi; L = out$L; pv = out$pv
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
  while((lk-lko)/abs(lk)>tol & it<maxit){
    Mu0 = Mu; Si0 = Si; Piv0 = Piv; PI0 = PI
    it = it+1

# ---- E-step ----
    # Compute V and U
    out = prob_post_cov_cont(Y,yv,Mu,Si,Piv,PI,Phi,L,pv)
    U = out$U; V = out$V
    # If required store parameters

# ---- M-step ----
# Update Mu and Si
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
        for(u in 1:k) Si= Si+ t(Yv-rep(1,ns*TT)%*%t(Mu[,u]))%*%(Vv[,u]*as.matrix(Yv-rep(1,ns*TT)%*%t(Mu[,u]))) #FB: velocizzato togliendo diag
        Si = Si/(n*TT)
        Mu00 = Mu
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
        if(nc2==1) tmp = array(tmp,c(k,(k-1),Zndis))
        tmp2 = PIdis[,,h]
        if(Zndis==1) tmp2 = matrix(tmp2,1,k)
        out = est_multilogit(UU,tmp,Zlab,Ga[,h],tmp2,fort=fort)
        PIdis[,,h] = out$Pdis; PI[h,,,2:TT] = array(as.vector(t(out$P)),c(1,k,ns,TT-1)); Ga[,h] = out$be
      }
    }else if(param=="difflogit"){
      Tmp = aperm(U[,,,2:TT],c(1,3,4,2))
      Tmp = matrix(Tmp,ns*k*(TT-1),k)
      out = est_multilogit(Tmp,ZZdis,Zlab,Ga,PIdis,fort=fort)
      PIdis = out$Pdis; Ga = out$be
      Tmp = array(out$P,c(k,ns,TT-1,k))
      PI[,,,2:TT] = aperm(Tmp,c(1,4,2,3))
    }

# Compute log-likelihood
    paro = par; par = c(as.vector(Piv),as.vector(PI),as.vector(Mu),as.vector(Si))
    if(any(is.na(par))) par = par[-which(is.na(par))]
    lko = lk
    out = lk_comp_latent_cont(Y,R,yv,Piv,PI,Mu,Si,k)
    lk = out$lk; Phi = out$Phi; L = out$L; pv = out$pv
    # Display output
    if(it%%10==0) cat(sprintf("%11g",c(k,start,it,lk,lk-lko,max(abs(par-paro)))),"\n",sep=" | ")
    lkv = c(lkv,lk)
  }
  if(it%%10>0)  cat(sprintf("%11g",c(k,start,it,lk,lk-lko,max(abs(par-paro)))),"\n",sep=" | ")
  cat("------------|-------------|-------------|-------------|-------------|-------------|\n")
  if(out_se){
    th = NULL
    th = c(th,as.vector(Mu))
    th = c(th,Si[upper.tri(Si,TRUE)])
    th = c(th, be)
    if(param=="multilogit"){
      for(h in 1:k) th = c(th, Ga[,h])
    }else{
      if(param=="difflogit") th = c(th,Ga)
    }
    out = lk_obs_latent_cont(th,Y,R,yv,XXdis,Xlab,ZZdis,Zlab,param,fort=fort)
    lk0 = out$lk; sc0 = out$sc
    if(check_der) print(c(lk,lk0,lk-lk0))
    lth = length(th)
    scn = rep(0,lth); Fn = matrix(0,lth,lth)
    for(h in 1:lth){
      thh = th; thh[h] = thh[h]+10^-6
      outh = lk_obs_latent_cont(thh,Y,R,yv,XXdis,Xlab,ZZdis,Zlab,param,fort)
      scn[h] = (outh$lk-lk0)/10^-6
      Fn[,h] = (outh$sc-sc0)/10^-6
    }
    J = -(Fn+t(Fn))/2
    if(check_der) print(round(cbind(scn,sc0,round(scn-sc0,4)),5))
    iJ = try(solve(J))
    if(inherits(iJ,"try-error")){
      ind = NULL
      for(j in 1:nrow(J)){
        tmp = try(solve(J[c(ind,j),c(ind,j)]),silent = TRUE)
        if(!inherits(tmp,"try-error")) ind = c(ind,j)
      }
      iJ = matrix(0,nrow(J),nrow(J))
      iJ[ind,ind] = solve(J[ind,ind])
    }
    if(any(diag(iJ)<0)) warning("negative elements in the estimated variance")
    se = sqrt(abs(diag(iJ)))
    nMu = r*k
    nSi = r*(r+1)/2
    nbe = nc1*(k-1)
    if(param=="multilogit") nga = nc2*(k-1)*k else if(param=="difflogit") nga=(k+nc2)*(k-1)
    seMu = se[1:nMu]
    seSi = se[nMu+(1:nSi)]
    sebe = se[nMu+nSi+(1:nbe)]
    sega = se[nMu+nSi+nbe+(1:nga)]
  }

# Compute number of parameters
  np = k*r+r*(r+1)/2
  np = np+(k-1)*nc1
  if(param=="multilogit") np = np+(k-1)*nc2*k else if(param=="difflogit")  np = np+(k-1)*(nc2+k)
  aic = -2*lk+np*2
  bic = -2*lk+np*log(n)
# local decoding
  Ul = matrix(0,ns,TT)
  for(i in 1:ns) for(t in 1:TT) Ul[i,t] = which.max(V[i,,t])

  Be = matrix(be,nc1,k-1)
  if(is.null(nameBe)){
    if(nc1==0) nameBe = c("(Intercept)") else nameBe = c("(Intercept)",paste("X1",1:(nc1-1),sep=""))
  }
  dimnames(Be) = list(nameBe,logit=2:k)
  if(out_se){
    seBe = matrix(sebe,nc1,k-1)
    dimnames(seBe) = list(nameBe,logit=2:k)
  }
  if(param=="multilogit"){
    if(is.null(nameGa)){
      if(nc2==0) nameGa = c("(Intercept)") else nameGa = c("(Intercept)", paste("X2",1:(nc2-1),sep=""))
    }
    if(k>2) {
      Ga = array(as.vector(Ga),c(nc2,k-1,k))
      dimnames(Ga) = list(nameGa,logit=2:k,logit=1:k)
    }else if(k==2){
      dimnames(Ga) = 	list(nameGa,logit=1:k)
    }
    if(out_se){
      if(k==2){
        seGa = matrix(sega,nc2,2)
        dimnames(seGa) = list(nameGa,logit=1:k)
      }else if(k>2){
        seGa = array(as.vector(sega),c(nc2,k-1,k))
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
      dimnames(Ga[[1]]) = list("(Intercept)"=1:k,logit=k)
      dimnames(Ga[[2]])=list(nameGa2,logit=k)
    } else if (k>2){
      dimnames(Ga[[1]]) = list("(Intercept)"=1:k,logit=2:k)
      dimnames(Ga[[2]])=list(nameGa2,logit=2:k)
    }
    if(out_se){
      seGa[[1]] = t(matrix(sega[1:(k*(k-1))],k-1,k))
      seGa[[2]] = matrix(sega[(k*(k-1))+(1:((k-1)*nc2))],nc2,k-1)
      if(k==2){
        dimnames(seGa[[1]]) = list("(Intercept)"=1:k,logit=k)
        dimnames(seGa[[2]])=list(nameGa2,logit=k)
      }else if (k>2){
        dimnames(seGa[[1]]) = list("(Intercept)"=1:k,logit=2:k)
        dimnames(seGa[[2]])=list(nameGa2,logit=2:k)
      }
    }
  }

# adjust output
  lk = as.vector(lk)
  dimnames(Piv)=list(subject=1:ns,state=1:k)
  dimnames(PI)=list(state=1:k,state=1:k,subject=1:ns,time=1:TT)
  nameY <- dimnames(Y)[[3]]
  dimnames(Mu) <- list(nameY,state=1:k)
  # if(r==1) colnames(Si) <- nameY else dimnames(Si) <- list(nameY,nameY)
  out = list(lk=lk,Be=Be,Ga=Ga,Mu=Mu,Si=Si,Piv=Piv,PI=PI,np=np,k = k,aic=aic,bic=bic,lkv=lkv, 
             n=n,TT=TT,yv=yv,ns=ns,paramLatent=param)
  if(out_se){
    seMu = matrix(seMu,r,k)
    seSi2 = matrix(0,r,r)
    seSi2[upper.tri(seSi2,TRUE)]=seSi
    if(r>1) seSi2 = seSi2+t(seSi2-diag(diag(seSi2)))
    seSi = seSi2
    dimnames(seMu)=list(nameY,state=1:k)
    # if(r==1) colnames(seSi) <- nameY else dimnames(seSi) <- list(nameY,nameY)
    out$seMu = seMu
    out$seSi = seSi
    out$seBe = seBe
    out$seGa = seGa
  }
  # final output
  if(miss) out$Y = Y
  if(output){
    if(k>1){
      PMarg <- array(0,c(ns,k,TT))
      PMarg[,,1] <- as.matrix(Piv)
      for(i in 1:ns) for(t in 2:TT) PMarg[i,,t]= t(PI[,,i,t])%*%PMarg[i,,t-1]
      Pmarg <-apply(PMarg,c(2,3),mean)
    }else Pmarg <- NULL
    out = c(out,list(V = V, Ul = Ul, Pmarg=Pmarg))
  } 
  if(out_se){
    out$sc = sc0
    out$J = J
  }
  class(out)="LMlatentcont"
  return(out)
}
