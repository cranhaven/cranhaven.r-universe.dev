datagenerator <- function(n, beta0, family, seed=NA){
  if(!is.na(seed)){set.seed(seed+1111)}
  p <- ncol(beta0); K <- nrow(beta0);
  if(length(n)!=1 & length(n)!=K){ stop("n has to be scalar or a vector of length K = nrow(beta0).") }
  groupvec <- rep(c(1:K), times=n); N <- length(groupvec);
  betamat <- matrix(NA, N, p); for(i in 1:p){ betamat[,i] <- rep(beta0[,i], times=n) }
  if(p==2){ X <- matrix(c(rep(1,N), rnorm((p-1)*N)), N, p) }
  if(p>=3){
    sigma <- matrix(0.3, p-1, p-1); diag(sigma) <- 1
    X <- cbind(rep(1,N), mvrnorm(N, mu=rep(0, (p-1)), Sigma=sigma))
  }
  if(family=="gaussian"){ y <- rowSums(X*betamat) + rnorm(N) }
  if(family=="binomial"){ y <- rbinom(N, 1, expit(rowSums(X*betamat))) }
  if(family=="poisson"){  y <- rpois(N, exp(rowSums(X*betamat))) }
  if(family=="cox"){
    tt <- - rowSums(X*betamat) - rgev(N); tt <- tt - min(tt) + 0.5
    cc <- runif(N, min=max(tt)/3, max=max(tt))
    y <- data.frame(time=apply(cbind(tt,cc), 1, min), status=(tt < cc) + 0)
  }
  colnames(X) <- paste("x", c(0:(p-1)), sep="")
  data <- data.frame(y,X[,-1],group=groupvec);
  data <- data[sample(1:N, N, replace=F),]; row.names(data) <- NULL
  rm(betamat, i, n, p, K, N, groupvec, y, family, beta0)
  return(data)
}

metafuse <- function(X=X, y=y, sid=sid, fuse.which=c(0:ncol(X)), family="gaussian", intercept=TRUE, alpha=0,
                     criterion="EBIC", verbose=TRUE, plots=FALSE, loglambda=TRUE){
  # argument check
  argcheck(X=X, y=y, sid=sid, fuse.which=fuse.which, family=family, intercept=intercept)
  if(missing(X)){X = NULL}

  # only complete cases
  complete <- complete.cases(cbind(X,y,sid))
  X        <- subset(X,   subset=complete)
  y        <- subset(y,   subset=complete)
  sid      <- subset(sid, subset=complete); rm(complete)

  studies  <- sort(unique(sid))
  K        <- length(studies)
  n.k      <- table(sid)  #sapply(studies, function(x){sum(sid==x)})
  N        <- length(sid)
  if(intercept==T){ X <- data.frame( cbind(Intercept=rep(1,N), X) ) }
  X.names    <- names(X)
  p          <- ncol(X)
  fuse.which <- fuse.which+1*intercept
  weights    <- as.numeric((N / (K * n.k))[sid])  #sapply(sid, function(x){((N/K) / n.k)[x]})

  X.b      <- X_trans(X=X, fuse.which=fuse.which, K=K, X.names=X.names, sid=sid, studies=studies)
  beta.glm <- glm_wrap(X=X.b, y=y, family=family, weights=weights)

  parg     <- pargroup(p=p, fuse.which=fuse.which, K=K)
  S        <- Sgen(p=p, parg=parg, fuse.which=fuse.which, K=K, beta=beta.glm)
  B        <- Bgen(p=p, parg=parg, fuse.which=fuse.which, K=K, beta=S%*%beta.glm)

  theta.glm<- drop(B %*% S %*% beta.glm)
  X.t      <- X.b %*% solve(B%*%S)

  center   <- centerlist(p=p, parg=parg)
  al.r     <- 1
  al.weights <- 1/(abs(theta.glm)^al.r)
  al.weights[center] <- alpha * al.weights[center]
  # # WHEN THE BELOW TWO LINES ARE COMMENTED, WE DON'T USE THE GROUP-WISE ADAPTIVE WEIGHT
  # al.fusion.weights <- 1 / sapply(1:p, function(x){ diff(range(beta.glm[parg==x])) })[parg]
  # al.weights[-center] <- (al.weights * al.fusion.weights)[-center]

  currentDF <- betaDF(beta=beta.glm, parg=parg, p=p, X.names=X.names)
  if(verbose==T){
    cat("************************** verbose (start) **************************\n")
    cat(paste("Lambda = ", round(0.00,4), "\n", sep="")); print(currentDF)
  }
  groupPath <- getGrouping(beta.glm, parg=parg, p=p, fuse.which=fuse.which)
  lambdaCP  <- c(0)

  # list of summaries
  solPath <- c(); lamlist <- c(); dflist <- c()

  l <- 0; ll <- -4; dd <- 0.05
  while(!all(currentDF == 1)){

    if(family=="cox"){
      glmnet.ada.fit <- glmnet(X.t, as.matrix(y), family=family, lambda=l, weights=weights, standardize=F, penalty.factor=al.weights)
      thetahat       <- drop(coef(glmnet.ada.fit))
    } else if(all(X.t[,1]==1)){
      glmnet.ada.fit <- glmnet(X.t[,-1], y, family=family, lambda=l, weights=weights, intercept=T, standardize=F, penalty.factor=al.weights[-1])
      thetahat       <- drop(coef(glmnet.ada.fit))
    } else{
      glmnet.ada.fit <- glmnet(X.t, y, family=family, lambda=l, weights=weights, intercept=F, standardize=F, penalty.factor=al.weights)
      thetahat       <- drop(coef(glmnet.ada.fit)[-1])
    }
    betahat   <- drop(solve(B%*%S)%*%thetahat)

    tempDF <- betaDF(beta=betahat, parg=parg, p=p, X.names=X.names)
    if(any(currentDF != tempDF)) {
      currentDF <- tempDF
      if(verbose==T){ cat(paste("Lambda = ", round(l,4), "\n", sep="")); print(currentDF) }
      groupPath <- cbind(groupPath, getGrouping(betahat, parg=parg, p=p, fuse.which=fuse.which))
      lambdaCP  <- c(lambdaCP, l)
    }
    solPath   <- cbind(solPath, as.vector(betahat))
    lamlist   <- c(lamlist,l)

    l        <- 10^(ll)
    ll       <- ll+dd
    dflist   <- cbind(dflist, tempDF)
  }
  if(verbose==T){ cat("************************** verbose (end) **************************\n") }

  critPath          <- apply(solPath, 2, critfunc, family=family, criterion=criterion, N=N,X.b=X.b,y=y,K=K,p=p,sid=sid,parg=parg,fuse.which=fuse.which)
  colnames(solPath) <- round(lamlist,5); rownames(solPath) <- names(beta.glm)
  colnames(dflist)  <- round(lamlist,5)
  names(critPath)   <- round(lamlist,5)

  lambda_opt    <- lamlist[which.min(critPath)]
  lambda_opt_t  <- log(lambda_opt+1, base=10)

  beta_opt      <- solPath[,which.min(critPath)]
  betaopt_out   <- betahatReformat(beta=beta_opt, parg=parg, p=p, K=K)

  beta_opt_DF   <- dflist[,which.min(critPath)]

  lambda_fuse   <- lamlist[apply(dflist, 1, function(x){which(x==1)[1]})]
  lambda_fuse_t <- log(lambda_fuse+1, base=10)

  friction      <- sapply(lambda_fuse, function(x){ 1 - min(x, lambda_opt)/x })
  friction_t    <- sapply(lambda_fuse_t, function(x){ 1 - min(x, lambda_opt_t)/x })

  if.fuse       <- rep(0,p); if.fuse[fuse.which] <- 1; names(if.fuse) <- X.names

  out.beta      <- betaopt_out; colnames(out.beta) <- X.names
  out.info.o    <- rbind(DF=beta_opt_DF, lambda_opt, lambda_fuse, friction); colnames(out.info.o) <- X.names
  out.info.t    <- rbind(DF=beta_opt_DF, lambda_opt=lambda_opt_t, lambda_fuse=lambda_fuse_t, friction=friction_t); colnames(out.info.t) <- X.names
  out.info      <- list(original_scale=out.info.o, log10_scale=out.info.t)

  out           <- list(family=family, criterion=criterion, alpha=alpha, if.fuse=if.fuse, betahat=out.beta, betainfo=out.info)

  # generate all the plots
  if(plots==T){
    plotSolPath(solPath=solPath, lamlist=lamlist, parg=parg, p=p, lambda_opt=lambda_opt, loglambda=loglambda, X.names=X.names, fuse.which=fuse.which)
    readkey()
    plotCrit(critPath=critPath, lamlist=lamlist, lambda_opt=lambda_opt, loglambda=loglambda, criterion=criterion)
    for(plot.which in fuse.which){
      readkey()
      fusiogram(groupPath=groupPath[parg==plot.which,], lambdaCP=lambdaCP, beta=beta_opt, K=K, parg=parg,
                plot.which=plot.which, lambda_opt=min(lambda_opt,lambda_fuse[plot.which]),
                loglambda=loglambda, X.names=X.names)
    }
  }

  return(out)
}

metafuse.l <- function(X=X, y=y, sid=sid, fuse.which=c(0:ncol(X)), family="gaussian", intercept=TRUE, alpha=0, lambda=lambda){
  # argument check
  argcheck(X=X, y=y, sid=sid, fuse.which=fuse.which, family=family, intercept=intercept)
  if(missing(X)){X = NULL}

  # only complete cases
  complete <- complete.cases(cbind(X,y,sid))
  X        <- subset(X,   subset=complete)
  y        <- subset(y,   subset=complete)
  sid      <- subset(sid, subset=complete); rm(complete)

  studies  <- sort(unique(sid))
  K        <- length(studies)
  n.k      <- table(sid)  #sapply(studies, function(x){sum(sid==x)})
  N        <- length(sid)
  if(intercept==T){ X <- data.frame( cbind(Intercept=rep(1,N), X) ) }
  X.names    <- names(X)
  p          <- ncol(X)
  fuse.which <- fuse.which+1*intercept
  weights    <- as.numeric((N / (K * n.k))[sid])  #sapply(sid, function(x){((N/K) / n.k)[x]})


  X.b      <- X_trans(X=X, fuse.which=fuse.which, K=K, X.names=X.names, sid=sid, studies=studies)
  beta.glm <- glm_wrap(X=X.b, y=y, family=family, weights=weights)

  parg     <- pargroup(p=p, fuse.which=fuse.which, K=K)
  S        <- Sgen(p=p, parg=parg, fuse.which=fuse.which, K=K, beta=beta.glm)
  B        <- Bgen(p=p, parg=parg, fuse.which=fuse.which, K=K, beta=S%*%beta.glm)

  theta.glm<- drop(B %*% S %*% beta.glm)
  X.t      <- X.b %*% solve(B%*%S)

  center   <- centerlist(p=p, parg=parg)
  al.r     <- 1
  al.weights <- 1/(abs(theta.glm)^al.r)
  al.weights[center] <- alpha * al.weights[center]
  # # WHEN THE BELOW TWO LINES ARE COMMENTED, WE DON'T USE THE GROUP-WISE ADAPTIVE WEIGHT
  #   al.fusion.weights <- 1 / sapply(1:p, function(x){ diff(range(beta.glm[parg==x])) })[parg]
  #   al.weights[-center] <- (al.weights * al.fusion.weights)[-center]

  if(family=="cox"){
    glmnet.ada.fit <- glmnet(X.t, as.matrix(y), family=family, lambda=lambda, weights=weights, standardize=F, penalty.factor=al.weights)
    thetahat       <- drop(coef(glmnet.ada.fit))
  } else if(all(X.t[,1]==1)){
    glmnet.ada.fit <- glmnet(X.t[,-1], y, family=family, lambda=lambda, weights=weights, intercept=T, standardize=F, penalty.factor=al.weights[-1])
    thetahat       <- drop(coef(glmnet.ada.fit))
  } else{
    glmnet.ada.fit <- glmnet(X.t, y, family=family, lambda=lambda, weights=weights, intercept=F, standardize=F, penalty.factor=al.weights)
    thetahat       <- drop(coef(glmnet.ada.fit)[-1])
  }
  betahat   <- drop(solve(B%*%S)%*%thetahat)

  if.fuse       <- rep(0,p); if.fuse[fuse.which] <- 1; names(if.fuse) <- X.names
  out.beta      <- betahatReformat(beta=betahat, parg=parg, p=p, K=K); colnames(out.beta) <- X.names
  out.DF        <- as.matrix(betaDF(beta=betahat, parg=parg, p=p, X.names=X.names)); colnames(out.DF) <- "DF"

  out           <- list(family=family, alpha=alpha, if.fuse=if.fuse, betahat=out.beta, betainfo=t(out.DF))

  return(out)
}



expit <- function(x){ return( exp(x)/(exp(x)+1) ) }

argcheck <- function(X, y, sid, fuse.which, family, intercept){
  if(missing(X) & intercept==FALSE){ stop("Need to specify X, otherwise, set intercept=TRUE.") }
  if(!missing(X)){
    if(!is.data.frame(X)){stop("X must be a data frame.")}
    if(!all(fuse.which %in% c(0:ncol(X)))){ stop("fuse.which must be an ordered ingeter vector with values from 0 to ncol(X).") }
  }
  if(missing(y)){ stop("Need to specify y.") }
  if(missing(sid)){ stop("Need to specify sid, the study ID for each subject, numbered consecutively starting from 1.") }
  if(family=="cox" & intercept==TRUE){ stop("Cox model does not allow intercept, set intercept=FALSE.") }
  if(intercept==F & any(fuse.which==0)){ stop("When intercept=FALSE, argument fuse.which cannot contain 0.") }
}

X_trans <- function(X, fuse.which, K, X.names, sid, studies){
  p <- ncol(X)
  N <- nrow(X)
  X.b <- c()
  X.b.names <- c()
  for(i in 1:p){
    if(i %in% fuse.which){
      for(j in studies){
        temp <- rep(0,N)
        temp[(sid==j)] <- X[(sid==j),i]
        X.b <- cbind(X.b, temp)
      }
      X.b.names <- c(X.b.names, paste(X.names[i],"_",studies,sep=""))
    } else{
      X.b <- cbind(X.b, X[,i])
      X.b.names <- c(X.b.names, X.names[i])
    }
  }
  colnames(X.b) <- X.b.names
  return(X.b)
}

glm_wrap <- function(X, y, family, weights){
  if(family=="cox"){
    beta <- coef(glmnet(x=as.matrix(X), y=as.matrix(y), family=family, weights=weights, lambda=0))

  } else{
    beta <- glm(y~.-1, data=data.frame(y,X), family=family, weights=weights)$coef
  }
  return(drop(beta))
}

pargroup <- function(p, fuse.which, K){
  parg <- c()
  for(i in 1:p){
    if(i %in% fuse.which){
      parg <- c(parg, rep(i,K))
    } else{
      parg <- c(parg, i)
    }
  }
  return(parg)
}

Bgen <- function(p=p, parg=parg, fuse.which=fuse.which, K=K, beta=beta){
  B <- c()
  for(i in 1:p){
    if(i %in% fuse.which){
      temp <- diag(c(0,rep(1,K-1)))
      temp[1,which.min(abs(beta[parg==i]))] <- 1
      for(i in 2:K){temp[i,i-1] <- -1}
      if(length(B)==0){B<-temp} else{B <- bdiag(B,temp)}
    } else{
      if(length(B)==0){B<-1} else{B <- bdiag(B,1)}
    }
  }
  return(as.matrix(B))
}

Sgen <- function(p=p, parg=parg, fuse.which=fuse.which, K=K, beta=beta){
  S <- c()
  for(i in 1:p){
    if(i %in% fuse.which){
      btemp <- beta[parg==i]
      order <- order(btemp)
      Stemp <- matrix(0,K,K)
      for(j in 1:K){
        Stemp[j,order[j]] <- 1
      }
      if(length(S)==0){S<-Stemp} else{S <- bdiag(S,Stemp)}
    } else{
      if(length(S)==0){S<-1} else{S <- bdiag(S,1)}
    }
  }
  return(as.matrix(S))
}

centerlist <- function(p=p, parg=parg){
  center <- sapply(c(1:p), function(x){ which(parg == x)[1] })
  return(center)
}

betaDF <- function(beta=beta, parg=parg, p=p, X.names=X.names){
  out <- c()
  for(i in 1:p){ out <- c(out, length(unique(beta[parg==i]))) }
  names(out) <- c(X.names)
  return(out)
}

getGrouping <- function(betahat=betahat, parg=parg, p=p, fuse.which=fuse.which){
  out <- c()
  for(i in 1:p){
    beta <- betahat[parg==i]
    group <- rep(NA, length(beta))
    for(j in 1:length(unique(beta))) group[beta==unique(beta)[j]] <- j
    out <- c(out, group)
  }
  return(out)
}

critfunc <- function(betahat=betahat, family=family, criterion=criterion, N=N, X.b=X.b, y=y, K=K, p=p, sid=sid, parg=parg, fuse.which=fuse.which){
  mu <- 0; tau <- 0;
  for(i in c(1:p)){
    mu <- mu + length(unique(betahat[parg==i]))
    # mu <- mu + sum(unique(betahat[parg==i])!=0)
    tau <- tau + choose(K, length(unique(betahat[parg==i])))
  }
  if(criterion == "AIC" ){ extra <- 2*mu }
  if(criterion == "BIC" ){ extra <- mu*log(N) }
  if(criterion == "EBIC"){ extra <- mu*log(N) + 2*1*log(tau)  }
  nk <- table(sid); nbar <- mean(nk)
  if(family=="gaussian"){LL <- sapply(c(1:K), function(kk) {-log(sum((y[sid==kk] - X.b[sid==kk,]%*%betahat)^2)/(nk[kk]))*nk[kk]/2})}
  if(family=="binomial"){LL <- sapply(c(1:K), function(kk) {t(y[sid==kk])%*%X.b[sid==kk,]%*%betahat - sum(log(1+exp(X.b[sid==kk,]%*%betahat)))})}
  if(family=="poisson") {LL <- sapply(c(1:K), function(kk) {t(y[sid==kk])%*%X.b[sid==kk,]%*%betahat - sum(exp(X.b[sid==kk,]%*%betahat))})}
  #   if(family=="cox")     {LL <- sapply(c(1:K), function(kk) {sum(y[sid==kk,2] * ((X.b[sid==kk,]%*%betahat) -
  #     (log((t(y[sid==kk,1] %*% t(rep(1,nk[kk]))) >= (y[sid==kk,1] %*% t(rep(1,nk[kk])))) %*% exp(X.b[sid==kk,]%*%betahat)))))   })}
  if(family=="cox")     {LL <- sapply(c(1:K), function(kk) {sum(y[sid==kk,2] * ((X.b[sid==kk,]%*%betahat) -
                                                                                  (log((t(y[,1] %*% t(rep(1,nk[kk]))) >= (y[sid==kk,1] %*% t(rep(1,sum(nk))))) %*% exp(X.b%*%betahat)))))   })}
  return(-2 * sum(LL * (nbar/nk)) + extra)
}

readkey <- function(){
  cat ("Press [enter] to continue")
  line <- readline()
}

plotCrit <- function(critPath=critPath, lamlist=lamlist, lambda_opt=lambda_opt, loglambda=F, criterion=criterion){
  xlabel <- expression(lambda)
  ylabel <- criterion
  if(loglambda==T) {lamlist <- log(lamlist+1, base=10)
  lambda_opt <- log(lambda_opt+1, base=10)
  xlabel <- expression(paste(log[10], "(", lambda, "+1)"))}
  plot(lamlist,critPath,type="l",pch=19,lwd=1.5, xlab=xlabel, ylab=ylabel, main="Model Selection")
  abline(v=lambda_opt, lty=3, lwd=1) # black dotted line
}

plotSolPath <- function(solPath=solPath, lamlist=lamlist, parg=parg, p=p, lambda_opt=lambda_opt, loglambda=F, X.names=X.names, fuse.which=fuse.which){
  sp <- solPath
  xlabel <- expression(lambda)
  if(loglambda==T) {lamlist <- log(lamlist+1, base=10)
  lambda_opt <- log(lambda_opt+1, base=10)
  xlabel <- expression(paste(log[10], "(", lambda, "+1)"))}
  plot(lamlist, sp[1,], type="n", ylim=range(sp), ylab=expression(beta), xlab=xlabel, main="Solution Path")
  allcolor <- rainbow(p+2)
  alltype  <- c(1:p)
  for(i in 1:p){
    # if(i==1) next
    for(j in which(parg==i)){
      # lines(lamlist,sp[j,],type="l", lwd=1.5, col=allcolor[i], lty=alltype[i]) # colored lines
      lines(lamlist,sp[j,],type="l", lwd=1, col=i) # colored lines
      # lines(lamlist,sp[j,], type="l", lwd=1.5, lty=alltype[i]) # dotted lines
    }
  }
  abline(v=lambda_opt, lty=3, lwd=1) # black dotted line
  # legend("topright", legend=X.names, lwd=1, col=allcolor, lty=alltype[i], cex=0.8)
  legend("topright", legend=X.names, lwd=1, col=c(1:p), cex=0.8)
  # legend("topright", legend=X.names, lwd=1, lty=alltype, cex=0.8)
}

betahatReformat <- function(beta=beta, parg=parg, p=p, K=K){
  beta_out <- list();
  for(i in 1:p){
    value <- beta[parg==i]
    if(length(value)==1){value <- rep(value,K)}
    group <- rep(NA, K)
    for(j in 1:length(unique(value))) group[value==unique(value)[j]] <- j
    listitem <- rbind(value, group)
    beta_out[[i]] <- listitem
  }
  beta_out_copy <- beta_out
  beta_out <- c()
  for(i in 1:length(beta_out_copy)){beta_out <- cbind(beta_out, beta_out_copy[[i]][1,])}
  rownames(beta_out) <- paste("study", c(1:nrow(beta_out)), sep="")
  return(beta_out)
}


fusiogram <- function(groupPath=groupPath, lambdaCP=lambdaCP, beta=beta, K=K, parg=parg,
                      plot.which=plot.which, lambda_opt=lambda_opt, loglambda=F, X.names=X.names){
  groupPath <- groupPath[,rev(1:ncol(groupPath))]
  if(loglambda==T) {lambdaCP <- log(lambdaCP+1, base=10)}
  lambdaCP <- rev(lambdaCP)
  temp <- rep(0, nrow(groupPath))
  distmat <- c()
  addvalue <- rep(max(lambdaCP), nrow(groupPath))
  for(i in 2:(ncol(groupPath)-1)){
    changeindex <- rep(F, nrow(groupPath))
    for(j in 1:length(unique(groupPath[,i]))){
      if(length(unique(groupPath[groupPath[,i]==unique(groupPath[,i])[j],i] -
                       groupPath[groupPath[,i]==unique(groupPath[,i])[j],i+1])) != 1){
        changeindex <- (changeindex | groupPath[,i]==unique(groupPath[,i])[j])
      }
    }
    temp <- addvalue*changeindex
    distmat <- cbind(distmat, temp)
    addvalue[changeindex] <- lambdaCP[i]
    if(i == (ncol(groupPath)-1)){
      temp1 <- rep(0, nrow(groupPath))
      for(k in 1:length(unique(addvalue))){
        temp11 <- which(addvalue == unique(addvalue)[k])
        if(length(temp11) <= 2) {
          temp1[temp11[1]] <- addvalue[temp11[1]]
        } else {
          for(kk in 1:length(unique(groupPath[temp11,i]))){
            temp111 <- which(groupPath[,i] == unique(groupPath[temp11,i])[kk])
            temp1[temp111[1]] <- addvalue[temp111[1]]
          }
        }
      }
      distmat <- cbind(distmat, temp1)
    }
  }
  rownames(distmat) <- names(beta)[parg==plot.which]
  ylabel <- expression(lambda)
  if(loglambda == T){
    lambda_opt <- log(lambda_opt+1, 10)
    ylabel <- expression(paste(log[10], "(", lambda, "+1)"))
  }
  plot(hclust(dist(distmat, method="maximum"), method="single"), hang=-1, main=paste(X.names[plot.which], sep=""),
       ylab=ylabel, xlab="", sub="", ylim=range(lambdaCP), lwd=1)
  abline(h=lambda_opt, lty=3, lwd=1)
}
