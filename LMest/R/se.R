se <- function(est,...) {
  UseMethod("se")
}

se.LMbasic <- function(est, ...){

  check_der = FALSE
  if(!is.null(est$sePsi)){
    out = list(sepiv = est$sepiv,sePi = est$sePi,sePsi = est$sePsi)
    return(out)
  }

  data <- est$data
  responsesFormula = attributes(est)$responsesFormula
  latentFormula = attributes(est)$latentFormula

  tv.which <- attributes(est)$whichtv
  id.which <- attributes(est)$whichid
  id <- attributes(est)$id
  tv <- attributes(est)$time
  k <- est$k
  mod <- est$modBasic

  data.new <- data[,-c(id.which,tv.which), drop = FALSE]

  if(is.null(responsesFormula)){
    Y <- data.new
    Xmanifest <- NULL
    Xinitial <- NULL
    Xtrans <- NULL
  }else{
    temp <- getResponses(data = data.new,formula = responsesFormula)
    Y <- temp$Y
    Xmanifest <- temp$X
    Xinitial <- NULL
    Xtrans <- NULL
  }
  if(min(Y,na.rm=T)>0){
    Y <- apply(Y,2,function(x) x - min(x, na.rm = TRUE))
  }

  yv = est$yv
  tmp <- long2matrices.internal(Y = Y, id = id, time = tv, yv = yv,
                                Xinitial = Xinitial, Xmanifest = Xmanifest, Xtrans = Xtrans)
  #model <- tmp$model
  S <- tmp$Y
  # yv = tmp$freq

  miss = any(is.na(S))
  if(miss){
    R = 1 * (!is.na(S))
    Y[is.na(S)] = 0
  }else{
    R = NULL
  }

  piv = est$piv
  Pi = est$Pi
  Psi = est$Psi

  n = sum(yv)
  sS = dim(S)
  ns = sS[1]
  TT = sS[2]

  if(k==1){
    b = nrow(Psi)-1
    r = ncol(Psi)
    sePsi = matrix(0,b+1,r)
    for(j in 1:r) sePsi[,j] = sqrt(diag(ginv(n*(diag(Psi[,j])-Psi[,j]%o%Psi[,j]))))
    dimnames(sePsi)=list(category=0:b,item=1:r)
    out = list(sePsi=sePsi)
    return(out)
  }

  if(length(sS)==2){
    r = 1
    if(is.matrix(S)) S = array(S,c(dim(S),1))
  }else{
    r = sS[3]
  }

  Sv = matrix(S,ns*TT,r)

  bv = apply(Sv,2,max)
  b = max(bv)
  m = vector("list",r)

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

# Compute information matrix if required
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
      J =  blkdiag(J,sum(Ut[u,])*t(Cm[,,u])%*%(diag(Pi[u,,2])-Pi[u,,2]%o%Pi[u,,2])%*%Cm[,,u])
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
    } else M =  blkdiag(M,Om%*%m[[j]]$Am)
  }
  Om = diag(piv)-tcrossprod(piv,piv)
  M =  blkdiag(M,Om%*%Bm)
  if(mod==0){
    for(t in 2:TT) for(u in 1:k){
      Om = diag(Pi[u,,t])-Pi[u,,t]%o%Pi[u,,t]
      M =  blkdiag(M,Om%*%Cm[,,u])
    }
  }
  if(mod==1){
    for(u in 1:k){
      Om = diag(Pi[u,,2])-Pi[u,,2]%o%Pi[u,,2]
      M =  blkdiag(M,Om%*%Cm[,,u])
    }
  }
  if(mod>1){
    for(u in 1:k){
      Om = diag(Pi[u,,2])-Pi[u,,2]%o%Pi[u,,2]
      M =  blkdiag(M,Om%*%Cm[,,u])
    }
    for(u in 1:k){
      Om = diag(Pi[u,,mod+1])-Pi[u,,mod+1]%o%Pi[u,,mod+1]
      M =  blkdiag(M,Om%*%Cm[,,u])
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
  # if(check_der){
  #   J0 = J
  #   th0 = th-10^-5/2
  #   out = lk_obs(th0,m,Bm,Cm,bv,k,S,R=R,yv,TT,r,mod)
  #   lk0 = out$lk; sc0 = out$sc
  #   lth = length(th)
  #   scn = rep(0,lth)
  #   J = matrix(0,lth,lth)
  #   for(j in 1:lth){
  #     thj = th0; thj[j] = thj[j]+10^-5
  #     out = lk_obs(thj,m,Bm,Cm,bv,k,S,R=R,yv,TT,r,mod)
  #     scn[j] = (out$lk-lk0)/10^-5
  #     J[,j] = (out$sc-sc0)/10^-5
  #   }
  #   J = -(J+t(J))/2
  #   print(c(lk,lk0))
  #   print(round(cbind(sc,scn,sc0),5))
  #   print(round(cbind(diag(J),diag(J0),diag(J)-diag(J0)),4))
  #   browser()
  # }


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

  out = list(sepiv = sepiv, sePi = sePi,sePsi = sePsi)
  return(out)
}

se.LMlatent <- function(est,...){

  out_se = TRUE
  fort = TRUE
  check_der = FALSE

  if(!is.null(est$seBe))
  {
    out = list(sePsi = est$sePsi, seBe = est$seBe,seGa = est$seGa)
    return(out)
  }

  data <- est$data
  responsesFormula = attributes(est)$responsesFormula
  latentFormula = attributes(est)$latentFormula

  tv.which <- attributes(est)$whichtv
  id.which <- attributes(est)$whichid
  id <- attributes(est)$id
  tv <- attributes(est)$time
  k <- est$k
  param <- est$paramLatent

  data.new <- data[,-c(id.which,tv.which), drop = FALSE]

  if(is.null(responsesFormula))
  {
    Y <- data.new
    Xmanifest <- NULL
    Xinitial <- NULL
    Xtrans <- NULL
  }else{
    temp <-  getResponses(data = data.new,formula = responsesFormula)
    Y <- temp$Y
    Xmanifest <- temp$X
    Xinitial <- NULL
    Xtrans <- NULL
  }
  if(min(Y,na.rm=T)>0){
    Y <- apply(Y,2,function(x) x - min(x, na.rm = TRUE))
  }

  if(!is.null(latentFormula))
  {
    temp <-  getLatent(data = data.new,latent = latentFormula, responses = responsesFormula)
    Xinitial <- temp$Xinitial
    Xtrans <- temp$Xtrans
  }
  tmp <-  long2matrices.internal(Y = Y, id = id, time = tv, yv = NULL,
                                        Xinitial = Xinitial, Xmanifest = Xmanifest, Xtrans = Xtrans)
  model <- tmp$model
  X1 <- tmp$Xinitial
  X2 <- tmp$Xtrans
  S <- tmp$Y
  yv = tmp$freq
  
  miss = any(is.na(S))
  if(miss){
    R = 1 * (!is.na(S))
    Y[is.na(S)] = 0
  }else{
    R = NULL
  }

  n = sum(yv)
  sS = dim(S)
  ns = sS[1]
  TT = sS[2]
  
  if(param=="difflogit"){
    cat("\n* With difflogit is not possible to avoid the intercept for the transition probabilities*\n\n")
    X2 = X2[,,-1,drop=FALSE]
  }
  
  if(is.null(X1)){
    X1 = matrix(1,ns,1)
    colnames(X1) = "(Intercept)"
  }
  if(is.null(X2)){
    X2 = array(1,c(ns,TT-1,1))
    dimnames(X2) = list(NULL,NULL,"(Intercept)")
  }
  
  nc1 = dim(X1)[2]
  nc2 = dim(X2)[3]

  if(length(sS)==2){
    r = 1
    if(is.matrix(S)) S = array(S,c(dim(S),1))
  }else
  {
    r = sS[3]
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
    out =  aggr_data(X1,fort=fort)
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
  if(is.null(X2) | any(dimnames(X2)[2] == "(Intercept)")){
    if(param=="difflogit"){
      warning("with X2=NULL parametrization difflogit not considered")
      param="multilogit"
    }
    nc2 = 0
    Zlab = rep(1,ns*(TT-1))
    nameGa = NULL
    Zndis = max(Zlab)
  }else{
   # if(TT==2) X2 = array(X2,c(ns,1,dim(X2)[2]))
  #  if(is.matrix(X2)) X2 = array(X2,c(ns,TT-1,1))
    nc2 = dim(X2)[3] # number of covariates on the transition probabilities
    if(ns!= dim(X2)[1]) stop("dimension mismatch between S and X2")

    nameGa = colnames(aperm(X2,c(1,3,2)))
    Z = NULL
    for(t in 1:(TT-1)) Z = rbind(Z,X2[,t,])
    if(nc2==1) Z = as.vector(X2)
    out =  aggr_data(Z,fort=fort); Zdis = out$data_dis; Zlab = out$label; Zndis = max(Zlab)
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
  Piv = est$Piv
  PI = est$PI
  Psi = est$Psi
  Be = est$Be
  Ga = est$Ga
  V = est$V
  if(is.null(V)) stop("V must be available in the output of lmest (use output=TRUE option)")

  Am = vector("list",r)
  for(j in 1:r) Am[[j]] = rbind(rep(0,b[j]),diag(b[j]))

  be = as.vector(Be)
  out =  prob_multilogit(XXdis,be,Xlab,fort)
  Piv = out$P; Pivdis = out$Pdis
  # parameters on transition probabilities
  if(param=="multilogit"){
    if(is.list(Ga)) stop("invalid mode (list) for Ga")
    Ga = matrix(Ga,nc2*(k-1),k)
    PIdis = array(0,c(Zndis,k,k)); PI = array(0,c(k,k,ns,TT))
    for(h in 1:k){
      out =  prob_multilogit(ZZdis[,,,h],Ga[,h],Zlab,fort)
      PIdis[,,h] = out$Pdis; PI[h,,,2:TT] = array(as.vector(t(out$P)),c(1,k,ns,TT-1))
    }
  }else if(param=="difflogit"){
    
    if(is.list(Ga)) Ga = c(as.vector(t(Ga[[1]])),as.vector(Ga[[2]]))
    if(length(Ga)!=k*(k-1)+(k-1)*nc2) stop("invalid dimensions for Ga")
    PI = array(0,c(k,k,ns,TT))
    out =  prob_multilogit(ZZdis,Ga,Zlab,fort)
    PIdis = out$Pdis; PI[,,,2:TT] = array(as.vector(t(out$P)),c(k,k,ns,TT-1))
    PI = aperm(PI,c(2,1,3,4))
  }

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
  dlPiv = array(0,c(ns,k,nc1*(k-1)))
  be = as.vector(Be)

  out =  est_multilogit(V[,,1],XXdis,Xlab,be,Pivdis,fort=fort)
  Pivdis = out$Pdi
  for(j in 1:Xndis){
    temp = pmax(Pivdis[j,],10^-50)
    Temp = (diag(k)-rep(1,k)%o%temp)%*%XXdis[,,j]
    for(i in which(Xlab==j)) dlPiv[i,,] = Temp
  }
  th = c(th,be)

  count = 0
  if(param=="multilogit"){
    dlPI = array(0,c(k,k,ns*TT,nc2*(k-1)*k))
    temp0 = rep(1,k); Temp0 = diag(k)
    for(h in 1:k){
      ind = count+(1:(nc2*(k-1)))
      for(j in 1:Zndis){
        temp = pmax(PIdis[j,,h],10^-50)
        Temp = (Temp0-temp0%o%temp)%*%ZZdis[,,j,h]
        for(i in which(Zlab==j)) dlPI[h,,ns+i,ind] = Temp
      }
      count = count+(nc2*(k-1))
      th = c(th,Ga[,h])
    }
    dlPI = array(dlPI,c(k,k,ns,TT,nc2*(k-1)*k))
  }else if(param=="difflogit"){
    dlPI = array(0,c(k,k*ns*TT,(k+nc2)*(k-1)))
    temp0 = rep(1,k); Temp0 = diag(k)
    for(j in 1:(Zndis*k)){
      temp = pmax(PIdis[j,],10^-50)
      Temp = (Temp0-temp0%o%temp)%*%ZZdis[,,j]
      for(i in which(Zlab==j)) dlPI[,k*ns+i,] = Temp
    }
    th = c(th,Ga)
    dlPI = array(dlPI,c(k,k,ns,TT,(k+nc2)*(k-1)))
    dlPI = aperm(dlPI,c(2,1,3,4,5))
  }

  # Compute log-likelihood
  lk = est$lk
  lk2 = lk
  out =  lk_comp_latent(S,R,yv,Piv,PI,Psi,k,der=TRUE,fort=fort,dlPsi=dlPsi,dlPiv=dlPiv,dlPI=dlPI)
  sc = out$dlk; dlL = out$dlL; dlPhi = out$dlPhi; dlL2 = out$dlL2; dlpv = out$dlpv
  lk = out$lk; Phi = out$Phi; L = out$L; pv = out$pv
  sc2 = sc;
  it = 0; lko = lk-10^10; lkv = NULL; dev = NULL
  # backward recursion
  out =  prob_post_cov(S,yv,Psi,Piv,PI,Phi,L,pv,der=TRUE,fort=fort,dlPhi=dlPhi,dlPiv,
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
    tmp = Y1[1:(b[j]+1),c,j]
    tmp = pmax(tmp/sum(tmp),10^-10)
    Psi[1:(b[j]+1),c,j] = tmp/sum(tmp)

    temp = pmax(Psi[1:(b[j]+1),c,j],10^-50)
    Op = diag(temp)-temp%o%temp
    Temp  = sum(Y1[1:(b[j]+1),c,j])*t(Am[[j]])%*%Op%*%Am[[j]]
    if(j==1 & c==1) Fi = Temp else Fi =  blkdiag(Fi,Temp)
  }

  # score and info piv
  out =  est_multilogit(V[,,1],XXdis,Xlab,be,Pivdis,fort=fort,ex=TRUE)
  sc = c(sc,out$sc); Fi =  blkdiag(Fi,out$Fi)
  # score and info Pi
  if(param=="multilogit"){
    for(h in 1:k){
      UU = NULL
      for(t in 2:TT) UU = rbind(UU,t(U[h,,,t]))
      tmp = array(ZZdis[,,,h],dim(ZZdis)[1:3]) #SP: tmp = ZZdis[,,,h]
      if(nc2==0) tmp = array(tmp,c(k,(k-1),Zndis))
      tmp2 = PIdis[,,h]
      if(Zndis==1) tmp2 = matrix(tmp2,1,k)
      out =  est_multilogit(UU,tmp,Zlab,Ga[,h],tmp2,fort=fort,ex=TRUE)
      sc = c(sc,out$sc); Fi =  blkdiag(Fi,out$Fi)
    }
  }else if(param=="difflogit"){
    Tmp = aperm(U[,,,2:TT],c(1,3,4,2))
    Tmp = matrix(Tmp,ns*k*(TT-1),k)
    out =  est_multilogit(Tmp,ZZdis,Zlab,Ga,PIdis,fort=fort,ex=TRUE)
    sc = c(sc,out$sc); Fi =  blkdiag(Fi,out$Fi)
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
      ind = count+(1:b[j])
      count = count+b[j]
      Cor[h,ind] = t(Am[[j]])%*%(dY1[1:(b[j]+1),c,j,h]-sum(dY1[1:(b[j]+1),c,j,h])*Psi[1:(b[j]+1),c,j])
    }
  }
  for(h in 1:(npar)){
    out =  est_multilogit(dV[,,1,h],XXdis,Xlab,be,Pivdis,fort=fort,ex=TRUE)
    Cor[h,nal+(1:nbe)] = out$sc
  }
  dU = array(U,c(k,k,ns,TT,npar))*dlU
  if(param=="multilogit"){
    rGa = dim(Ga)[1]
    for(h in 1:k){
      for(h1 in 1:npar){
        UU = NULL
        for(t in 2:TT) UU = rbind(UU,t(dU[h,,,t,h1]))
        tmp = array(ZZdis[,,,h],dim(ZZdis)[1:3]) #SP: tmp = ZZdis[,,,h]
        if(nc2==0) tmp = array(tmp,c(k,(k-1),Zndis))
        tmp2 = PIdis[,,h]
        if(Zndis==1) tmp2 = matrix(tmp2,1,k)

        out =  est_multilogit(UU,tmp,Zlab,Ga[,h],tmp2,fort=fort,ex=TRUE)
        ind = nal+nbe+(h-1)*rGa+(1:rGa)
        Cor[h1,ind] = out$sc
      }
    }
  }else if(param=="difflogit"){
    rGa = length(Ga)
    for(h1 in 1:npar){
      Tmp = aperm(dU[,,,2:TT,h1],c(1,3,4,2))
      Tmp = matrix(Tmp,ns*k*(TT-1),k)
      out =  est_multilogit(Tmp,ZZdis,Zlab,Ga,PIdis,fort=fort,ex=TRUE)
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

  if(out_se) {seBe = matrix(sebe,nc1,k-1); dimnames(seBe) = list(nameBe,logit=2:k)}
  if(param=="multilogit"){
    if(is.null(nameGa)){
      if(nc2==0) nameGa = "(Intercept)" else nameGa = c("(Intercept)", paste("X2",1:(nc2-1),sep=""))
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

  out = list(sePsi = sePsi, seBe = seBe,seGa = seGa)
  return(out)
}

se.LMbasiccont <- function(est,...){

  if(!is.null(est$seMu)){
    out = list(sepiv = est$sepiv,sePi = est$sePi,seMu = est$seMu,seSi = est$seSi)
    return(out)
  }

  data <- est$data
  responsesFormula = attributes(est)$responsesFormula
  latentFormula = attributes(est)$latentFormula

  tv.which <- attributes(est)$whichtv
  id.which <- attributes(est)$whichid
  id <- attributes(est)$id
  tv <- attributes(est)$time
  k <- est$k
  mod <- est$modBasic

  data.new <- data[,-c(id.which,tv.which), drop = FALSE]

  if(is.null(responsesFormula)){
    Y <- data.new
    Xmanifest <- NULL
    Xinitial <- NULL
    Xtrans <- NULL
  }else{
    temp <-  getResponses(data = data.new,formula = responsesFormula)
    Y <- temp$Y
    Xmanifest <- temp$X
    Xinitial <- NULL
    Xtrans <- NULL
  }
  if(min(Y,na.rm=T)>0) Y <- apply(Y,2,function(x) x - min(x, na.rm = TRUE))

  tmp <-  long2matrices.internal(Y = Y, id = id, time = tv, yv = est$yv,
                                        Xinitial = Xinitial, Xmanifest = Xmanifest, Xtrans = Xtrans)
  #model <- tmp$model
  Y <- tmp$Y
  yv = est$yv
  ns = dim(Y)[1]
  TT = dim(Y)[2]
  r = dim(Y)[3]
  n = sum(yv)
  miss = any(is.na(Y))

  if(miss){
    Yv = matrix(Y,ns*TT,r)
    Yv = cbind(1,Yv)
    pYv = prelim.mix(Yv,1)
    thhat = em.mix(prelim.mix(Yv,1))
    rngseed(1)
    Yv = as.matrix(imp.mix(pYv, da.mix(pYv,thhat,steps=100), Yv)[,-1])
    Y = array(Yv,c(ns,TT,r))
    cat("Missing data in the dataset. imp.mix function (mix package) used for imputation.\n")
  }

  piv = est$piv
  Pi = est$Pi
  Mu = est$Mu
  Si = est$Si

  sY = dim(Y)
  ns = as.integer(sY[1])
  TT = as.integer(sY[2])
  n = sum(yv)

  if(length(sY)==2){
    r = 1
    if(is.matrix(Y)) Y = array(Y,c(dim(Y),1))
  }else{
    r = sY[3]
  }

  Yv = matrix(Y,ns*TT,r)

  th = NULL; sc = NULL; J = NULL
  
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
  th = NULL
  th = c(th,as.vector(Mu))
  th = c(th,Si[upper.tri(Si,TRUE)])
  if(k>1){
    th = c(th,B%*%log(piv))
    if(mod==0) for(t in 2:TT) for(u in 1:k) th = c(th,C[,,u]%*%log(Pi[u,,t]))
    if(mod==1) for(u in 1:k) th = c(th,C[,,u]%*%log(Pi[u,,2]))
  }

  th0 = th-10^-5/2
  out =  lk_obs_cont(th0,yv,Bm,Cm,k,Y,TT,r,mod)
  lk0 = out$lk; sc0 = out$sc
  lth = length(th)
  scn = rep(0,lth)
  J = matrix(0,lth,lth)
  for(j in 1:lth){
    thj = th0; thj[j] = thj[j]+10^-5
    out =  lk_obs_cont(thj,yv,Bm,Cm,k,Y,TT,r,mod)
    scn[j] = (out$lk-lk0)/10^-5
    J[,j] = (out$sc-sc0)/10^-5
  }
  J = -(J+t(J))/2
  Va = ginv(J)
  nMu = r*k
  nSi = r*(r+1)/2
  Va2 = Va[1:(nMu+nSi),1:(nMu+nSi)]
  se2 = sqrt(diag(Va2))

  if(k>1){
    Va = Va[-(1:(nMu+nSi)),-(1:(nMu+nSi))]
    Om = diag(piv)-tcrossprod(piv,piv)
    M = Om%*%Bm
    if(mod==0){
      for(t in 2:TT) for(u in 1:k){
        Om = diag(Pi[u,,t])-Pi[u,,t]%o%Pi[u,,t]
        M =  blkdiag(M,Om%*%Cm[,,u])
      }
    }
    if(mod==1){
      for(u in 1:k){
        Om = diag(Pi[u,,2])-Pi[u,,2]%o%Pi[u,,2]
        M =  blkdiag(M,Om%*%Cm[,,u])
      }
    }
    if(mod>1){
      for(u in 1:k){
        Om = diag(Pi[u,,2])-Pi[u,,2]%o%Pi[u,,2]
        M =  blkdiag(M,Om%*%Cm[,,u])
      }
      for(u in 1:k){
        Om = diag(Pi[u,,mod+1])-Pi[u,,mod+1]%o%Pi[u,,mod+1]
        M =  blkdiag(M,Om%*%Cm[,,u])
      }
    }
    M = as.matrix(M)
    Va = M%*%Va%*%t(M)
    dVa = diag(Va)
    if(any(dVa<0)) warning("Negative elements in the estimated variance-covariance matrix for the parameters estimates")
    se = sqrt(abs(dVa))
  }

# Divide parameters
  if(k==1) se = se2 else se = c(se2,se)
  seMu = se[1:nMu]
  seSi = se[nMu+(1:nSi)]
  if(k==1){
    sepiv = NULL; sePi = NULL
  }else{
    sepiv = se[nMu+nSi+(1:k)]
    if(mod==0) sePi = se[nMu+nSi+k+(1:(k*k*(TT-1)))]
    if(mod==1) sePi = se[nMu+nSi+k+(1:(k*k))]
    if(mod>1) sePi = se[nMu+nSi+k+(1:(k*k*2))]
  }

  if(miss) out$Y = Y

  seMu = matrix(seMu,r,k)
  seSi2 = matrix(0,r,r)
  seSi2[upper.tri(seSi2,TRUE)]=seSi
  if(nrow(seSi2)>1) seSi2 = seSi2+t(seSi2-diag(diag(seSi2)))
  seSi = seSi2
  if(k>1){
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
  }
  if(r==1) dimnames(seMu) = list(item=1,state=1:k) else dimnames(seMu)=list(item=1:r,state=1:k)

  out = list(sepiv = sepiv,sePi = sePi,seMu = seMu,seSi = seSi)
  return(out)

}

se.LMlatentcont <- function(est,...){

  out_se = TRUE
  #fort = TRUE
  #check_der = FALSE

  if(!is.null(est$seBe)){
    out = list(seMu = est$seMu,seSi = est$seSi, seBe = est$seBe,seGa = est$seGa)
    return(out)
  }

  data <- est$data
  responsesFormula = attributes(est)$responsesFormula
  latentFormula = attributes(est)$latentFormula

  tv.which <- attributes(est)$whichtv
  id.which <- attributes(est)$whichid
  id <- attributes(est)$id
  tv <- attributes(est)$time
  k <- est$k
  param <- est$paramLatent

  data.new <- data[,-c(id.which,tv.which), drop = FALSE]

  if(is.null(responsesFormula)){
    Y <- data.new
    Xmanifest <- NULL
    Xinitial <- NULL
    Xtrans <- NULL
  }else{
    temp <-  getResponses(data = data.new,formula = responsesFormula)
    Y <- temp$Y
    Xmanifest <- temp$X
    Xinitial <- NULL
    Xtrans <- NULL
  }
  if(min(Y,na.rm=T)>0) Y <- apply(Y,2,function(x) x - min(x, na.rm = TRUE))

  if(!is.null(latentFormula)){
    temp <-  getLatent(data = data.new,latent = latentFormula, responses = responsesFormula)
    Xinitial <- temp$Xinitial
    Xtrans <- temp$Xtrans
  }
  tmp <-  long2matrices.internal(Y = Y, id = id, time = tv, yv = NULL,
                                        Xinitial = Xinitial, Xmanifest = Xmanifest, Xtrans = Xtrans)
  model <- tmp$model
  X1 <- tmp$Xinitial
  X2 <- tmp$Xtrans
  Y <- tmp$Y
  R = array(1,dim(Y))
  yv = est$yv

  miss = any(is.na(Y))

  if(miss){
    ns = dim(Y)[1]
    TT = dim(Y)[2]
    r = dim(Y)[3]
    Yv = matrix(Y,ns*TT,r)
    Yv = cbind(1,Yv)
    pYv = prelim.mix(Yv,1)
    thhat = em.mix(prelim.mix(Yv,1))
    rngseed(1)
    Yv = as.matrix(imp.mix(pYv, da.mix(pYv,thhat,steps=100), Yv)[,-1])
    Y = array(Yv,c(ns,TT,r))
    cat("Missing data in the dataset. imp.mix function (mix package) used for imputation.\n")
  }

  Be = est$Be
  Ga = est$Ga
  Mu = est$Mu
  Si = est$Si

  sY = dim(Y)
  ns = as.integer(sY[1])
  TT = as.integer(sY[2])
  n = sum(yv)

  if(length(sY)==2){
    r = 1
    if(is.matrix(Y)) Y = array(Y,c(dim(Y),1))
  }else{
    r = sY[3]
  }
  Yv = matrix(Y,ns*TT,r)

  if(k == 2){
    GBe = as.matrix(c(0,1))
  }else{
    GBe = diag(k); GBe = GBe[,-1]
  }

  if(is.vector(X1)) X1 = matrix(X1,ns,1)
  nc1 = dim(X1)[2] # number of covariates on the initial probabilities
  if(ns!= dim(X1)[1]) stop("dimension mismatch between S and X1")
  nameBe = colnames(X1)
  out =  aggr_data(X1)
  Xdis = out$data_dis
  if(nc1==1) Xdis = matrix(Xdis,length(Xdis),1)
  Xlab = out$label

  Xndis = max(Xlab)
  XXdis = array(0,c(k,(k-1)*nc1,Xndis))
  for(i in 1:Xndis){
    if(nc1==0) xdis = 1 else xdis = Xdis[i,]
    XXdis[,,i] = GBe%*%(diag(k-1)%x%t(xdis))
  }
  if(TT==2) X2 = array(X2,c(ns,1,dim(X2)[2]))
  if(is.matrix(X2)) X2 = array(X2,c(ns,TT-1,1))
  nc2 = dim(X2)[3] # number of covariates on the transition probabilities
  if(ns!= dim(X2)[1]) stop("dimension mismatch between S and X2")
  nameGa = colnames(aperm(X2,c(1,3,2)))
  Z = NULL
  for(t in 1:(TT-1)) Z = rbind(Z,X2[,t,])
  if(nc2==1) Z = as.vector(X2)
  out =  aggr_data(Z); Zdis = out$data_dis; Zlab = out$label; Zndis = max(Zlab)
  if(nc2==1) Zdis=matrix(Zdis,length(Zdis),1)

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

  be = as.vector(Be)
  out =  prob_multilogit(XXdis,be,Xlab)
  Piv = out$P; Pivdis = out$Pdis
  # parameters on transition probabilities
  if(param=="multilogit"){
    if(is.list(Ga)) stop("invalid mode (list) for Ga")
    Ga = matrix(Ga,nc2*(k-1),k)
    PIdis = array(0,c(Zndis,k,k)); PI = array(0,c(k,k,ns,TT))
    for(h in 1:k){
      out =  prob_multilogit(ZZdis[,,,h],Ga[,h],Zlab)
      PIdis[,,h] = out$Pdis; PI[h,,,2:TT] = array(as.vector(t(out$P)),c(1,k,ns,TT-1))
    }
  }else if(param=="difflogit"){
    if(is.list(Ga)) Ga = c(as.vector(t(Ga[[1]])),as.vector(Ga[[2]]))
    if(length(Ga)!=k*(k-1)+(k-1)*nc2) stop("invalid dimensions for Ga")
    PI = array(0,c(k,k,n,TT))
    out =  prob_multilogit(ZZdis,Ga,Zlab)
    PIdis = out$Pdis; PI[,,,2:TT] = array(as.vector(t(out$P)),c(k,k,ns,TT-1))
    PI = aperm(PI,c(2,1,3,4))
  }

  th = NULL
  th = c(th,as.vector(Mu))
  th = c(th,Si[upper.tri(Si,TRUE)])
  th = c(th, be)
  if(param=="multilogit"){
    for(h in 1:k) th = c(th, Ga[,h])
  }else if(param=="difflogit") th = c(th,Ga)
  out = lk_obs_latent_cont(th,Y,R,yv,XXdis,Xlab,ZZdis,Zlab,param)
  lk0 = out$lk; sc0 = out$sc

  lth = length(th)
  scn = rep(0,lth); Fn = matrix(0,lth,lth)

  for(h in 1:lth){
    thh = th; thh[h] = thh[h]+10^-5
    outh =  lk_obs_latent_cont(thh,Y,R,yv,XXdis,Xlab,ZZdis,Zlab,param)
    scn[h] = (outh$lk-lk0)/10^-5
    Fn[,h] = (outh$sc-sc0)/10^-5
  }

  J = -(Fn+t(Fn))/2
  iJ = ginv(J)
  se = sqrt(diag(iJ))

  nMu = r*k
  nSi = r*(r+1)/2
  nbe = nc1*(k-1)
  if(param=="multilogit") nga=nc2*(k-1)*k else if(param=="difflogit") nga=(k+nc2)*(k-1)
  seMu = se[1:nMu]
  seSi = se[nMu+(1:nSi)]
  sebe = se[nMu+nSi+(1:nbe)]
  sega = se[nMu+nSi+nbe+(1:nga)]

  if (is.null(nameBe)){
    if(nc1==0) nameBe = "(Intercept)" else nameBe = c("(Intercept)",paste("X1",1:(nc1-1),sep=""))
  }
  seBe = matrix(sebe,nc1,k-1); dimnames(seBe) = list(nameBe,logit=2:k)


  if(param=="multilogit"){
    if(is.null(nameGa)){
      if(nc2==0) nameGa = "(Intercept)" else nameGa = c("(Intercept)", paste("X2",1:(nc2-1),sep=""))
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

  seMu = matrix(seMu,r,k)
  if(r==1) dimnames(seMu) = list(item=1,state=1:k) else dimnames(seMu)=list(item=1:r,state=1:k)
  seSi2 = matrix(0,r,r)
  seSi2[upper.tri(seSi2,TRUE)]=seSi
  if(nrow(seSi2)>1) seSi2 = seSi2+t(seSi2-diag(diag(seSi2)))
  seSi = seSi2
  dimnames(seSi)=list(item=1:r,item=1:r)

  out <- list(seMu = seMu,seSi = seSi,seBe = seBe,seGa = seGa)
  return(out)

}







