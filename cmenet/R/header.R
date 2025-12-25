# acomb <- function(...) abind(..., along=1)

full.model.mtx <- function(xme){

  memtx <- xme

  #Assigning names to ME matrix
  nn <- nrow(memtx)
  pp <- ncol(memtx)
  colnret <- rep(NA,pp+2*pp*(pp-1))
  ret <- matrix(NA, nrow = nn, ncol = 2 * pp * (pp - 1) + pp)
  ininames <- rep(NA, pp)
  if (is.null(colnames(memtx))) {
    for (i in 1:pp) {
      ininames[i] <- paste0("V", i)
    }
    colnames(memtx) <- ininames
  }
  ret[, 1:pp] <- memtx
  colnret[1:pp] <- colnames(memtx)

  cmenames <- rep(NA, 2 * pp * (pp - 1))
  count <- pp + 1
  intmtx <- matrix(NA,nrow=2*pp*(pp-1),ncol=2)
  for (i in 1:pp) {
    for (j in 1:pp) {
      if (i != j) {
        ret[, count] <- memtx[, i] * as.numeric(memtx[,j] >= 0) #I|J+
        cmenames[count - pp] <- paste0(colnames(memtx)[i],"|", colnames(memtx)[j], "+")
        intmtx[count-pp,1] <- i
        intmtx[count-pp,2] <- j
        count <- count + 1

        ret[, count] <- memtx[, i] * as.numeric(memtx[,j] < 0) #I|J-
        cmenames[count - pp] <- paste0(colnames(memtx)[i],"|", colnames(memtx)[j], "-")
        intmtx[count-pp,1] <- i
        intmtx[count-pp,2] <- j
        count <- count + 1
      }
    }
  }
  colnret[(pp + 1):ncol(ret)] <- cmenames

  colnames(ret) <- colnret
  return(list(model.mtx=ret,cme.mtx=intmtx))
}


#Maximum lambda
lambda0.cme=function(x,y){
  max(abs(t(x)%*%(y-mean(y))))/length(y)
}

#Prediction method for CME
predictcme <- function(fit.cme,newx){

  # xnew - no need to normalize
  dm <- dim(fit.cme$coefficients)
  # print(dm)
  # print(dim(cme.obj$inter))
  ret <- array(NA, c(nrow(newx), dm[2], dm[3]))
  for (i in 1:dm[2]){
    for (j in 1:dm[3]){
      # print(paste0(i,j))
      ret[,i,j] <- fit.cme$inter[i,j] + newx%*%matrix(fit.cme$coefficients[,i,j],ncol=1)
    }
  }

  return(ret)
}

#Fitting cmenet
cmenet <- function(xme, xcme, y,
                   lambda.sib=exp(seq(from=log(max.lambda),to=log(max.lambda*1e-6),length=20)),
                   lambda.cou=exp(seq(from=log(max.lambda),to=log(max.lambda*1e-6),length=20)),
                   max.lambda=lambda0.cme(cbind(xme,xcme),y),
                   gamma=1/(0.5-tau)+0.001, tau=0.01,
                   act.vec=rep(1,ncol(xme)+ncol(xcme)),
                   beta0=rep(0,ncol(xme)+ncol(xcme)),
                   it.max=250, lambda.flg=T){

  #lambda.flg - T if grid over lambda, F otherwise

  #Remove all constant columns if present
  idx.constme <- which(apply(xme,2,function(xx){all(xx==mean(xx))}))
  idx.constcme <- which(apply(xcme,2,function(xx){all(xx==mean(xx))}))
  xme.sc <- scale(xme,center=T,scale=T)
  xme.sl <- attr(xme.sc,"scaled:scale")
  xme.ce <- attr(xme.sc,"scaled:center")
  xcme.sc <- scale(xcme,center=T,scale=T)
  xcme.sl <- attr(xcme.sc,"scaled:scale")
  xcme.ce <- attr(xcme.sc,"scaled:center")
  xme[,idx.constme] <- 0
  xcme[,idx.constcme] <- 0
  xme.sl[idx.constme] <- 1 #otherwise 1/0 error
  xcme.sl[idx.constcme] <- 1 #otherwise 1/0 error

  ret <- cme(xme.sc, xcme.sc, y,
             lambda.sib, lambda.cou, gamma, tau,
             xme.sl, xcme.sl, beta0, act.vec,
             max.lambda, it.max, 3, 1, F)

  #Compute intercept
  if (lambda.flg){
    inter <- matrix(NA,nrow=length(lambda.sib),ncol=length(lambda.cou))
  }else{
    # inter <- matrix(NA,nrow=length(gamma),ncol=length(tau)) #ch
    inter <- matrix(NA,nrow=length(tau),ncol=length(gamma))
  }
  xmat <- cbind(xme,xcme)
  for (a in 1:nrow(inter)){
    for (b in 1:ncol(inter)){
      inter[a,b] <- mean(y - xmat%*%ret$coefficients[,a,b])
    }
  }

  #Save in object
  ret$inter <- inter
  ret$xme <- xme
  ret$xcme <- xcme
  ret$y <- y

  return(ret)
}


#CV tuning for CME
cv.cmenet <- function(xme, xcme, y,
                      nfolds = 10, var.names = NULL,
                      nlambda.sib=20, nlambda.cou=20, lambda.min.ratio=1e-6,
                      ngamma=10, max.gamma=150, ntau=20,
                      max.tau=0.01, tau.min.ratio=0.01,
                      it.max=250, it.max.cv=25, warm.str="lasso"){

  #Set parameter limits
  min.tau <- max.tau*tau.min.ratio
  min.gamma <- 1/(0.5-min.tau)+0.001

  #center and scale model matrices
  pme <- ncol(xme)
  pcme <- ncol(xcme)
  n <- nrow(xme)
  xmat <- cbind(xme,xcme) #Full model matrix

  # Warm start active set:
  act.vec <- rep(-1,ncol(xme)+ncol(xcme)) #warm starts
  # act.vec <- rep(1,ncol(xme)+ncol(xcme)) #all variables
  if (warm.str=="lasso"){
    cvlas <- cv.glmnet(cbind(xme,xcme),y)
    lasfit <- glmnet(cbind(xme,xcme),y)
    # lasind <- which(lasfit$beta[,which(cvlas$lambda==cvlas$lambda.min)]!=0)
    lasind <- which(lasfit$beta[,which(cvlas$lambda==cvlas$lambda.1se)]!=0)
    # lasind <- which(lasfit$beta[,ncol(lasfit$beta)]!=0)
    act.vec <- rep(-1,ncol(xme)+ncol(xcme))
    act.vec[lasind] <- 1
  }else if (warm.str=="hierNet"){
    act.vec <- rep(-1,ncol(xme)+ncol(xcme))
    warm.hn <- hierNet.path(xme,y)
    warm.cv <- hierNet.cv(warm.hn,xme,y)
    l.opt <- which(warm.hn$lamlist==warm.cv$lamhat.1se)
    me.sel <- (warm.hn$bp-warm.hn$bn)[,l.opt]
    me.idx <- which(me.sel!=0)
    int.sel <- warm.hn$th[,,l.opt]

    int.pidx <- which(int.sel>0.0,arr.ind=T) #positive interactions
    int.pidx <- t(apply(int.pidx,1,function(xx){sort(xx)})) #sort by first ME...
    int.pidx <- unique(int.pidx) #... then take unique indices
    if (nrow(int.pidx)>0){
      for (ii in 1:nrow(int.pidx)){ # set active flags for positive interactions
        inta <- int.pidx[ii,1]
        intb <- int.pidx[ii,2]
        if (inta%in%me.idx){ #A and AB -> A|B+ and A|B-
          cmeind = (inta-1)*(2*(pme-1)) + (intb-2)*2 + 1;
          # colnames(model.mtx)[pp+cmeind] #check: keep in mind renumbering of vars
          # colnames(model.mtx)[pp+cmeind+1]
          act.vec[pme+cmeind] = 1;
          act.vec[pme+cmeind+1] = 1;
        }
        if (inta%in%me.idx){ #B and AB -> B|A+ and B|A-
          cmeind = (intb-1)*(2*(pme-1)) + (inta-1)*2 + 1;
          act.vec[pme+cmeind] = 1;
          act.vec[pme+cmeind+1] = 1;
        }
      }
    }

    int.nidx <- which(int.sel<0.0,arr.ind=T)
    int.nidx <- t(apply(int.nidx,1,function(xx){sort(xx)})) #sort by first ME...
    int.nidx <- unique(int.nidx) #... then take unique indices
    if (nrow(int.nidx)>0){
      for (ii in 1:nrow(int.nidx)){ # set active flags for positive interactions
        inta <- int.nidx[ii,1]
        intb <- int.nidx[ii,2]
        if (inta%in%me.idx){ #A and AB -> A|B+ and A|B-
          cmeind = (inta-1)*(2*(pme-1)) + (intb-2)*2 + 1;
          # colnames(model.mtx)[pp+cmeind] #check: keep in mind renumbering of vars
          # colnames(model.mtx)[pp+cmeind+1]
          act.vec[pme+cmeind] = 1;
          act.vec[pme+cmeind+1] = 1;
        }
        if (inta%in%me.idx){ #B and AB -> B|A+ and B|A-
          cmeind = (intb-1)*(2*(pme-1)) + (inta-1)*2 + 1;
          act.vec[pme+cmeind] = 1;
          act.vec[pme+cmeind+1] = 1;
        }
      }
    }

  }

  # Initialize lambda vector
  # xme.sc <- scale(xme,center=T,scale=T)
  # xcme.sc <- scale(xcme,center=T,scale=T)
  # max.lambda <- lambda0.cme(cbind(xme.sc,xcme.sc),y)
  max.lambda <- lambda0.cme(cbind(xme,xcme),y)
  lambda.sib <- exp(seq(from = log(max.lambda), to = log(max.lambda * lambda.min.ratio), length = nlambda.sib))
  lambda.cou <- exp(seq(from = log(max.lambda), to = log(max.lambda * lambda.min.ratio), length = nlambda.cou))
  gamma_vec = exp(seq(from = log(max.gamma), to = log(min.gamma), length = ngamma - 1))
  gamma_vec = c(9.9e+35, gamma_vec)
  tau_vec = rev(exp(seq(from = log(max.tau), to = log(min.tau), length = ntau))) #bottom up better

  # For each replicate ...
  parms.min.bst <- c()
  min.err <- 1e30;
  cvm.gt.bst <- c()
  cvm.lambda.bst <- c()
  # if (warm.lam=="median"){
  parms1.min <- c(median(lambda.sib), median(lambda.cou))
  # }else{
  #   parms1.min <- c(min(lambda.sib), min(lambda.cou))
  # }

  ## Resample folds
  foldid = sample(rep(seq(nfolds), length = n))
  if (nfolds < 3){
    stop("nfolds must be bigger than 3; nfolds=10 recommended")
  }
  beta0 <- rep(0,ncol(xme)+ncol(xcme))

  # for (kk in 1:nrep){

  ## (1) Tuning gammas and tau:
  predmat = array(NA, c(n, length(tau_vec), length(gamma_vec)))

  #Perform N-fold CV
  print(paste0("Tuning gamma & tau: "))
  pb <- txtProgressBar(style=3, width=floor(getOption("width")/2))
  for (i in seq(nfolds)){
    setTxtProgressBar(pb,i/nfolds)
    which = (foldid == i)
    fitobj <- cmenet(xme=xme[!which,,drop=F],xcme=xcme[!which,,drop=F],y=y[!which],
                     lambda.sib=parms1.min[1], lambda.cou=parms1.min[2], lambda.flg=F,
                     gamma=gamma_vec, tau=tau_vec,
                     act.vec=act.vec, max.lambda=max.lambda,
                     it.max=it.max.cv)
    xtest <- xmat[which,,drop=F]
    predmat[which,,] <- (predictcme(fitobj,xtest) - y[which])^2
  }
  cat('\n')

  #Compute mean of cv error
  cvm.gt <- t(apply(predmat,c(2,3),mean))
  # cvm.gt <- apply(predmat,c(2,3),mean) #ch
  whichmin = argmin(cvm.gt) #select tau and gamma
  ind2.min = whichmin
  parms2.min = c(gamma_vec[whichmin[1]], tau_vec[whichmin[2]])

  ## (2) Tuning lambdas:
  lambda.sib <- exp(seq(from = log(max.lambda), to = log(max.lambda * lambda.min.ratio), length = nlambda.sib))
  lambda.cou <- exp(seq(from = log(max.lambda), to = log(max.lambda * lambda.min.ratio), length = nlambda.cou))
  predmat = array(NA, c(n, nlambda.sib, nlambda.cou))
  cvm <- matrix(NA,nlambda.sib,nlambda.cou)

  #Perform N-fold CV
  if (nfolds < 3){
    stop("nfolds must be bigger than 3; nfolds=10 recommended")
  }
  print(paste0("Tuning lambdas: "))
  pb <- txtProgressBar(style=3, width=floor(getOption("width")/2))
  for (i in seq(nfolds)){
    setTxtProgressBar(pb,i/nfolds)
    which = (foldid == i)
    fitobj <- cmenet(xme=xme[!which,,drop=F], xcme=xcme[!which,,drop=F], y=y[!which],
                     lambda.sib=lambda.sib, lambda.cou=lambda.cou, lambda.flg=T,
                     gamma=parms2.min[1], tau=parms2.min[2],
                     act.vec=act.vec, max.lambda=max.lambda,
                     it.max=it.max.cv)
    xtest <- xmat[which,,drop=F]
    predmat[which,,] <- (predictcme(fitobj,xtest) - y[which])^2
  }
  cat('\n')

  #Compute mean of cv error
  cvm.lambda <- apply(predmat,c(2,3),mean)
  whichmin = argmin(cvm.lambda) #select lambdas
  ind1.min = whichmin
  parms1.min = c(lambda.sib[whichmin[1]], lambda.cou[whichmin[2]])
  parms.min <- c(parms1.min,parms2.min)

  #Pick the replicate with best cv err
  # print(min(cvm.lambda))
  # print(min.err)
  # if (min(cvm.lambda)<min.err){
  #   parms.min.bst <- parms.min
  #   min.err <- min(cvm.lambda)
  #   cvm.gt.bst <- cvm.gt
  #   cvm.lambda.bst <- cvm.lambda
  # }
  # }

  ##Summarize into list
  obj = list(y=y, lambda.sib = lambda.sib, lambda.cou = lambda.cou,
             gamma = gamma_vec, tau = tau_vec,
             cvm.gt = cvm.gt, cvm.lambda = cvm.lambda)
  obj$params = parms.min
  names(obj$params) <- c("lambda.sib","lambda.cou","gamma","tau")

  #Perform selection on full data with final parameters
  print(paste0("Fitting full data ..."))
  fitall <- cmenet(xme=xme, xcme=xcme, y,
                   lambda.sib=lambda.sib, lambda.cou=lambda.cou, lambda.flg=T,
                   gamma=obj$params[3], tau=obj$params[4],
                   act.vec=act.vec, max.lambda=max.lambda,
                   it.max=it.max)
  obj$cme.fit <- fitall
  obj$select.idx <- which(fitall$coefficients[,which(lambda.sib==obj$params[1]),which(lambda.cou==obj$params[2])] != 0)
  obj$select.names <- var.names[obj$select.idx]
  class(obj) = "cv.cme"

  return(obj)

}
argmin <- function (x){ #function from package "sparsenet"
  vx = as.vector(x)
  imax = order(vx)[1]
  if (!is.matrix(x))
    imax
  else {
    d = dim(x)
    c1 = as.vector(outer(seq(d[1]), rep(1, d[2])))[imax]
    c2 = as.vector(outer(rep(1, d[1]), seq(d[2])))[imax]
    c(c1, c2)
  }
}
