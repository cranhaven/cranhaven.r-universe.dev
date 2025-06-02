bootstrap <- function(est,...){
  UseMethod("bootstrap")
}

bootstrap.LMbasic <- function(est, B = 100, seed = NULL, ...){

  if(any(est$yv!=1)) warning("* weights are ignored within bootstrap *")

  piv = est$piv
  Pi = est$Pi
  Psi = est$Psi
  # mod = object$mod
  # start = object$start
  mod <- est$modBasic
  #start = 0
  #tol = est$tol
  

  if(!is.null(seed)) set.seed(seed)
  k = est$k
  c = est$n
  TT = est$TT
  n = est$n
  ns = est$ns
  
# Reparametrize
  mPsi = mpiv = mPi = 0
  m2Psi = m2piv = m2Pi = 0
  #    mth = 0; m2th = 0;
  for(b in 1:B){
    out = draw(est = est, n = ns, format = "matrices")
    Sb = out$S
    yvb = out$yv
    ns = dim(Sb)[1]
    out =  lmbasic(S = Sb, yv = yvb, k = k, modBasic = mod, start = 2, piv = piv,
                   Pi=Pi, Psi=Psi, ...)
    mPsi = mPsi+out$Psi/B
    mpiv = mpiv+out$piv/B
    mPi = mPi+out$Pi/B
    m2Psi = m2Psi+out$Psi^2/B
    m2piv = m2piv+out$piv^2/B
    m2Pi = m2Pi+out$Pi^2/B
    #	    	mth = mth+out$th/B; m2th = m2th+out$th/B
  }
  sePsi = sqrt(m2Psi-mPsi^2); sepiv = sqrt(m2piv-mpiv^2); sePi = sqrt(m2Pi-mPi^2)
  #    seth = sqrt(m2th-mth^2)
  out = list(mPsi=mPsi,mpiv=mpiv,mPi=mPi,sePsi=sePsi,sepiv=sepiv,sePi=sePi)
}

bootstrap.LMbasiccont <- function(est, B=100, seed = NULL, ...){

# Preliminaries
  if(any(est$yv!=1)) warning("* weights are ignored within bootstrap *")
  piv = est$piv
  Pi = est$Pi
  Mu = est$Mu
  Si = est$Si

  mod <- est$modBasic
  if(!is.null(seed)) set.seed(seed)
  # mod = object$mod
  # start = object$start

  # Preliminaries
  k = est$k
  n = est$n
  ns = est$ns
  
# Reparametrize
  mMu = mSi = mpiv = mPi = 0
  m2Mu = m2Si=  m2piv = m2Pi = 0
  #    mth = 0; m2th = 0;
  for(b in 1:B){
    cat("bootstrap sample n. ",b,"\n")
    out = draw(est = est, format = "matrices")
    Yb = out$Y
    yvb = out$yv
    out =  lmbasic.cont(Y = Yb, yv = yvb, k = k, modBasic = mod, start = 2, piv = piv, Pi = Pi,
                        Mu = Mu, Si = Si, ...)
    mMu = mMu + out$Mu/B
    mSi = mSi + out$Si/B
    mpiv = mpiv+out$piv/B
    mPi = mPi+out$Pi/B
    m2Mu = m2Mu + out$Mu^2/B; m2Si = m2Si + out$Si^2/B
    m2piv = m2piv+out$piv^2/B; m2Pi = m2Pi+out$Pi^2/B
  }
  seMu = sqrt(m2Mu - mMu^2)
  seSi = sqrt(m2Si - mSi^2)
  sepiv = sqrt(m2piv-mpiv^2)
  sePi = sqrt(m2Pi-mPi^2)
  out = list(mMu=mMu,mSi=mSi,mpiv=mpiv,mPi=mPi,seMu=seMu,seSi=seSi,sepiv=sepiv,sePi=sePi)
}

bootstrap.LMlatent <- function(est, B=100, seed = NULL, ...){
  
  if(any(est$yv!=1)) warning("* weights are ignored within bootstrap *")

  if(!is.null(seed)) set.seed(seed)
  Psi = est$Psi
  Be = est$Be
  Ga = est$Ga
  param = est$paramLatent

  latentFormula = attributes(est)$latentFormula
  responsesFormula = attributes(est)$responsesFormula
  Y = est$data
  id <- attributes(est)$id
  tv <- attributes(est)$time

  tv.which <- attributes(est)$whichtv
  id.which <- attributes(est)$whichid

  temp <-  getLatent(data = Y[,-c(tv.which,id.which)],latent = latentFormula, responses = responsesFormula)
  Xinitial <- temp$Xinitial
  Xtrans <- temp$Xtrans


  tmp <-  long2matrices.internal(Y = Y, id = id, time = tv,
                          Xinitial = Xinitial, Xmanifest = NULL, Xtrans = Xtrans)

  X1 <- tmp$Xinitial
  X2 <- tmp$Xtrans

  mPsi = 0
  mBe = 0
  m2Psi = 0
  m2Be = 0
  if(param=="multilogit"){
    mGa = 0
    m2Ga = 0
  }else if(param=="difflogit"){
    mGa = vector("list",2)
    m2Ga = vector("list",2)
    mGa[[1]] = array(0,dim(Ga[[1]]))
    mGa[[2]] = array(0,dim(Ga[[2]]))
    m2Ga[[1]] = array(0,dim(Ga[[1]]))
    m2Ga[[2]] = array(0,dim(Ga[[2]]))
  }

  dPsi = dim(Psi)
  if(length(dPsi)==1) k = 1
  else k = dPsi[2]
  for (b in 1:B) {
    cat("bootstrap sample n. ",b,"\n")
    out = draw(est = est, data = est$data, index = eval(est$call$index),
               fort=TRUE, format = "matrices")
    Yb = out$Y
    yvb = out$yv
    out =  lmcovlatent(S = Yb, X1 = X1, X2 = X2, yv=yvb, paramLatent=param, k=k, start=2,
                       Be=Be, Ga=Ga, Psi=Psi, ...)
    mPsi = mPsi + out$Psi/B
    mBe = mBe + out$Be/B
    if(param=="multilogit"){
      mGa = mGa + out$Ga/B
      m2Ga = m2Ga + out$Ga^2/B
    }else if(param=="difflogit"){
      mGa[[1]] = mGa[[1]]+out$Ga[[1]]/B
      mGa[[2]] = mGa[[2]]+out$Ga[[2]]/B
      m2Ga[[1]] = m2Ga[[1]] + out$Ga[[1]]^2/B
      m2Ga[[2]] = m2Ga[[2]] + out$Ga[[2]]^2/B
    }
    m2Psi = m2Psi + out$Psi^2/B
    m2Be = m2Be + out$Be^2/B

  }
  sePsi = sqrt(m2Psi - mPsi^2)
  seBe = sqrt(m2Be - mBe^2)
  if(param=="multilogit"){
    seGa = sqrt(m2Ga - mGa^2)
  }else if(param=="difflogit"){
    seGa = vector("list",2)
    seGa[[1]] = sqrt(m2Ga[[1]] - mGa[[1]]^2)
    seGa[[2]] = sqrt(m2Ga[[2]] - mGa[[2]]^2)
  }
  out = list(mPsi = mPsi, mBe = mBe, mGa = mGa, sePsi = sePsi,
             seBe = seBe, seGa = seGa)

}

bootstrap.LMlatentcont <- function(est, B=100, seed = NULL, ...){

  if(!is.null(seed)) set.seed(seed)
  Mu = est$Mu
  Be = est$Be
  Ga = est$Ga
  Si = est$Si
  param = est$paramLatent

  latentFormula = attributes(est)$latentFormula
  responsesFormula = attributes(est)$responsesFormula
  Y = est$data
  id <- attributes(est)$id
  tv <- attributes(est)$time

  tv.which <- attributes(est)$whichtv
  id.which <- attributes(est)$whichid
  temp <-  getLatent(data = Y[,-c(tv.which,id.which)],latent = latentFormula, responses = responsesFormula)
  Xinitial <- temp$Xinitial
  Xtrans <- temp$Xtrans
  Xinitial0 = Xinitial; Xtrans0 = Xtrans
  tmp <-  long2matrices.internal(Y = Y, id = id, time = tv,
                            Xinitial = Xinitial, Xmanifest = NULL, Xtrans = Xtrans)

  Xinitial <- tmp$Xinitial
  Xtrans <- tmp$Xtrans
  if(any(is.na(Xinitial)) & !any(is.na(Xinitial0)))
    for(j in 1:ncol(Xinitial)) if(any(is.na(Xinitial[,j]))) Xinitial[is.na(Xinitial[,j]),j] = mean(Xinitial[,j],na.rm=TRUE)
  if(any(is.na(Xtrans)) & !any(is.na(Xtrans0)))
    for(h in 1:dim(Xtrans)[3]) for(j in 1:ncol(Xtrans)) if(any(is.na(Xtrans[,j,h]))) Xtrans[is.na(Xtrans[,j,h]),j,h] = mean(Xtrans[,j,h],na.rm=TRUE)

  X1 <- Xinitial
  X2 <- Xtrans
  # preliminaries
  mMu = mSi = mBe = 0
  m2Mu = m2Si = m2Be = 0
  if(param=="multilogit"){
    mGa = 0
    m2Ga = 0
  }else if(param=="difflogit"){
    mGa = vector("list",2)
    m2Ga = vector("list",2)
    mGa[[1]] = array(0,dim(Ga[[1]]))
    mGa[[2]] = array(0,dim(Ga[[2]]))
    m2Ga[[1]] = array(0,dim(Ga[[1]]))
    m2Ga[[2]] = array(0,dim(Ga[[2]]))
  }

  if(is.vector(Mu)){
    r =1
    k = length(Mu)
  }else{
    r = nrow(Mu)
    k = ncol(Mu)
  }

  for (b in 1:B) {
    cat("bootstrap sample n. ",b,"\n")
    out = draw(est = est, data = est$data, index = eval(est$call$index), format = "matrices")
    Yb = out$Y
    yvb = out$yv
    out =  lmcovlatent.cont(Yb, X1, X2, yvb, paramLatent=param, k=k, start=2, 
                            Mu=Mu, Si=Si, Be=Be, Ga=Ga)
    mMu = mMu + out$Mu/B
    mSi = mSi + out$Si/B
    mBe = mBe + out$Be/B
    if(param=="multilogit"){
      mGa = mGa + out$Ga/B
      m2Ga = m2Ga + out$Ga^2/B
    }else if(param=="difflogit"){
      mGa[[1]] = mGa[[1]]+out$Ga[[1]]/B
      mGa[[2]] = mGa[[2]]+out$Ga[[2]]/B
      m2Ga[[1]] = m2Ga[[1]] + out$Ga[[1]]^2/B
      m2Ga[[2]] = m2Ga[[2]] + out$Ga[[2]]^2/B
    }
    m2Mu = m2Mu + out$Mu^2/B
    m2Si = m2Si + out$Si^2/B
    m2Be = m2Be + out$Be^2/B

  }
  seMu = sqrt(m2Mu - mMu^2)
  seSi = sqrt(m2Si - mSi^2)
  seBe = sqrt(m2Be - mBe^2)
  if(param=="multilogit"){
    seGa = sqrt(m2Ga - mGa^2)
  }else if(param=="difflogit"){
    seGa = vector("list",2)
    seGa[[1]] = sqrt(m2Ga[[1]] - mGa[[1]]^2)
    seGa[[2]] = sqrt(m2Ga[[2]] - mGa[[2]]^2)
  }
  out = list(mMu = mMu, mSi = mSi, mBe = mBe, mGa = mGa,
             seMu = seMu, seSi = seSi, seBe = seBe, seGa = seGa)
}



