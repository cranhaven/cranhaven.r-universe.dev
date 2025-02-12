## ----initialsetup, include=FALSE----------------------------------------------
knitr::opts_chunk$set(cache=FALSE)

## ---- Box1, eval=FALSE--------------------------------------------------------
#  rm(list = ls())
#  setwd("~/Dropbox/projects/R_packages/test/pipeline")
#  
#  site <- "https://github.com/MarcooLopez/Data_for_Lopez-Cruz_et_al_2020/raw/main"
#  filename <- "wheatHTP.E3.RData"
#  
#  # Download file
#  download.file(paste0(site,"/",filename), filename, mode="wb")
#  load(filename)
#  
#  trials <- as.numeric(as.character(Y$trial))
#  
#  index <- trials %in% unique(trials)[1:6]   # For ease, only 6 trials
#  trials <- trials[index]
#  Y <- Y[index,]
#  X <- lapply(X,function(x)x[index,])
#  
#  Y$gid <- factor(as.character(Y$gid))
#  Z <- model.matrix(~0+gid,data=Y)
#  K <- tcrossprod(Z)               # Connection of replicates
#  y <- as.vector(Y[,"YLD"])
#  
#  # Save file
#  save(y, trials, K, X, file="prepared_data.RData")

## ---- Box2, eval=FALSE--------------------------------------------------------
#  library(SFSI)
#  
#  # Load data
#  load("prepared_data.RData")
#  EVD <- eigen(K)                 # Decomposition of K
#  
#  # Fit model
#  y <- as.vector(scale(y))
#  fm0 <- fitBLUP(y, EVD=EVD, BLUP=FALSE)
#  c(fm0$varU,fm0$varE,fm0$h2)
#  
#  save(fm0, EVD, file="varComps.RData")

## ---- Box3, eval=FALSE--------------------------------------------------------
#  load("prepared_data.RData") # load data
#  
#  #---------- parameters ------------#
#  pTST <- 1/3           # Perc of the trials to assign to testing
#  nPart <- 3            # Number of TRN-TST partitions to perform
#  timepoints <- 6:9     # Time-points to analyze
#  #----------------------------------#
#  
#  nTrial <- length(unique(trials))
#  nTST <- ceiling(pTST*nTrial)  # Number of trials in TST set
#  
#  seeds <- round(seq(1E3, .Machine$integer.max, length = 500))
#  partitions <- matrix(1,nrow=length(y),ncol=length(seeds))   # Object to store partitions
#  for(k in 1:length(seeds))
#  {   set.seed(seeds[k])
#      tst <- sample(unique(trials),nTST,replace=FALSE)
#      partitions[which(trials %in% tst),k] <- 2
#  }
#  save(partitions, pTST, nPart, timepoints, file="parameters.RData")

## ---- Box4, eval=FALSE--------------------------------------------------------
#  load("prepared_data.RData"); load("parameters.RData")
#  
#  for(tp in timepoints){
#    gencov <- phencov <- c()   # Matrices to store covariances
#  
#    for(k in 1:nPart)
#    { cat("  partition = ",k,"of",nPart,"\n")
#      indexTRN <- which(partitions[,k]==1)
#  
#      # Training set
#      xTRN <- scale(X[[tp]][indexTRN,])
#      yTRN <- as.vector(scale(y[indexTRN]))
#      KTRN <- K[indexTRN,indexTRN]   # Relationship matrix (given by replicates)
#  
#      # Get genetic variances and covariances
#      fm <- getGenCov(y=cbind(yTRN,xTRN), K=KTRN, scale=FALSE, pairwise=FALSE, verbose=FALSE)
#  
#      gencov <- cbind(gencov,fm$covU)
#      phencov <- cbind(phencov,fm$covU + fm$covE)
#    }
#    save(gencov, phencov, file=paste0("covariances_tp_",tp,".RData"))
#    cat("Time-point=",tp,". Done \n")
#  }

## ---- Box5a, eval=FALSE-------------------------------------------------------
#  load("prepared_data.RData"); load("parameters.RData")
#  
#  for(tp in timepoints){
#    load(paste0("covariances_tp_",tp,".RData"))
#  
#    # Objects to store regression coefficients
#    bSI <- bPCSI <- bL1PSI <- vector("list",nPart)
#  
#    for(k in 1:nPart)
#    { cat("  partition = ",k,"of",nPart,"\n")
#      indexTRN <- which(partitions[,k]==1)
#  
#      # Training set
#      xTRN <- scale(X[[tp]][indexTRN,])
#      VARx <- var(xTRN)
#      EVDx <- eigen(VARx)
#  
#      # Standard SI
#      VARinv <- EVDx$vectors %*% diag(1/EVDx$values) %*% t(EVDx$vectors)
#      bSI[[k]] <- VARinv %*%  gencov[,k]
#  
#      # PC-based SI
#      VARinv <- diag(1/EVDx$values)
#      gamma <- as.vector(VARinv %*% t(EVDx$vectors) %*%  gencov[,k])
#      beta <- apply(EVDx$vectors %*% diag(gamma),1,cumsum)
#      bPCSI[[k]] <- data.frame(I(beta),nsup=1:nrow(beta),lambda=NA)
#  
#      # L1-PSI
#      fm <- solveEN(VARx, gencov[,k], nlambda=100)
#      # fm <- LARS(VARx, gencov[,k])  # Second option
#      beta <- t(as.matrix(fm$beta)[,-1])
#      bL1PSI[[k]] <- data.frame(I(beta),nsup=fm$nsup[-1],lambda=fm$lambda[-1])
#    }
#    save(bSI, bPCSI, bL1PSI, file=paste0("coefficients_tp_",tp,".RData"))
#    cat("Time-point=",tp,". Done \n")
#  }

## ---- Box5b, eval=FALSE-------------------------------------------------------
#  load("prepared_data.RData"); load("parameters.RData")
#  
#  for(tp in timepoints){
#    load(paste0("coefficients_tp_",tp,".RData"))
#    accSI <- c()    # Object to store accuracy components
#  
#    for(k in 1:nPart)
#    { cat("  partition = ",k,"of",nPart,"\n")
#      indexTST <- which(partitions[,k]==2)
#  
#      # Testing set
#      xTST <- scale(X[[tp]][indexTST,])
#      yTST <- as.vector(scale(y[indexTST]))
#      KTST <- K[indexTST,indexTST]   # Connection given by replicates
#  
#      # Calculate the indices
#      SI <- xTST %*% bSI[[k]]
#      PCSI <- tcrossprod(xTST, bPCSI[[k]]$beta)
#      L1PSI <- tcrossprod(xTST, bL1PSI[[k]]$beta)
#  
#      # Fit genetic models
#      fitSI <- as.matrix(data.frame(I(SI),I(PCSI),I(L1PSI)))
#      fm <- getGenCov(y=cbind(yTST,fitSI), K=KTST, pairwise=FALSE, verbose=FALSE)
#      fm$varU <- ifelse(fm$varU<0.05,0.05,fm$varU)
#  
#      # Retrieve accuracy components
#      h <- sqrt(fm$varU/(fm$varU + fm$varE))[-1]
#      gencor <- fm$covU/sqrt(fm$varU[1]*fm$varU[-1])
#      acc2 <- fm$covU/sqrt(fm$varU[1])
#      accuracy <- abs(gencor)*h
#  
#      nsup <- c(nrow(bSI[[k]]),bPCSI[[k]]$nsup,bL1PSI[[k]]$nsup)
#      lambda <- c(min(bL1PSI[[k]]$lambda),bPCSI[[k]]$lambda,bL1PSI[[k]]$lambda)
#      accSI <- rbind(accSI,data.frame(rep=k,SI=colnames(fitSI),h,gencor,accuracy,nsup,lambda))
#    }
#    save(accSI, file=paste0("accuracy_tp_",tp,".RData"))
#    cat("Time-point=",tp,". Done \n")
#  }

## ---- Box6, eval=FALSE--------------------------------------------------------
#  tp <- 9      # Time-point
#  load(paste0("accuracy_tp_",tp,".RData"))
#  
#  accSI < split(accSI,as.character(accSI$SI))
#  accSI <- data.frame(do.call(rbind,lapply(accSI,function(x)apply(x[,-c(1,2)],2,mean,na.rm=T))))
#  accSI$SI <- unlist(lapply(strsplit(rownames(accSI),"\\."),function(x)x[1]))
#  
#  # Plot of PC-SI
#  if(requireNamespace("reshape2") & requireNamespace("ggplot2")){
#   dat <- reshape2::melt(accSI[accSI$SI=="PCSI",],id=c("nsup","lambda","SI"))
#   plot1 <- ggplot2::ggplot(dat,ggplot2::aes(nsup,value,group=variable,color=variable)) +
#    ggplot2::theme_bw() + ggplot2::geom_line() +
#    ggplot2::labs(title="PC-SI",y="correlation",x="Number of PCs")
#  
#  # Plot of L1-PSI
#   dat <- reshape2::melt(accSI[accSI$SI=="L1PSI",],id=c("nsup","lambda","SI"))
#   plot2 <- ggplot2::ggplot(dat,ggplot2::aes(nsup,value,group=variable,color=variable)) +
#    ggplot2::theme_bw() + ggplot2::geom_line() +
#    ggplot2::labs(title="L1-PSI",y="correlation",x="Number of active predictors")
#  
#   plot1; plot2
#  }

## ---- Box7, eval=FALSE--------------------------------------------------------
#  tp <- 9      # Time-point
#  load(paste0("accuracy_tp_",tp,".RData"))
#  
#  accSI$Model <- unlist(lapply(strsplit(as.character(accSI$SI),"\\."),function(x)x[1]))
#  accSI <- split(accSI,paste(accSI$rep,"_",accSI$Model))
#  accSI <- do.call(rbind,lapply(accSI,function(x)x[which.max(x$accuracy),]))
#  
#  dat <- aggregate(accuracy ~ Model, mean, data=accSI)
#  dat$sd <- aggregate(accuracy ~ Model,sd,data=accSI)$accuracy
#  dat$n <- aggregate(accuracy ~ Model,length,data=accSI)$accuracy
#  dat$se <- qnorm(0.975)*dat$sd/sqrt(dat$n)
#  
#  if(requireNamespace("ggplot2")){
#   ggplot2::ggplot(dat,ggplot2::aes(Model,accuracy)) + ggplot2::theme_bw() +
#     ggplot2::geom_bar(stat="identity",width=0.6,fill="orange") +
#     ggplot2::geom_errorbar(ggplot2::aes(ymin=accuracy-se,ymax=accuracy+se),width=0.2) +
#     ggplot2::geom_text(ggplot2::aes(label=sprintf("%.2f",accuracy),y=accuracy*0.5))
#  }

## ---- Box8, eval=FALSE--------------------------------------------------------
#  load("parameters.RData")
#  AccSI <- c()
#  for(tp in timepoints)
#  { load(paste0("accuracy_tp_",tp,".RData"))
#    AccSI <- rbind(AccSI,data.frame(Timepoint=tp,accSI))
#  }
#  
#  AccSI$Model <- unlist(lapply(strsplit(as.character(AccSI$SI),"\\."),function(x)x[1]))
#  AccSI <- split(AccSI,paste(AccSI$Timepoint,"_",AccSI$rep,"_",AccSI$Model))
#  AccSI <- do.call(rbind,lapply(AccSI,function(x)x[which.max(x$accuracy),]))
#  AccSI$Timepoint <- factor(as.character(AccSI$Timepoint))
#  
#  dat <- aggregate(accuracy ~ Model+Timepoint,mean,data=AccSI)
#  dat$sd <- aggregate(accuracy ~ Model+Timepoint,sd,data=AccSI)$accuracy
#  dat$n <- aggregate(accuracy ~ Model+Timepoint,length,data=AccSI)$accuracy
#  dat$se <- qnorm(0.975)*dat$sd/sqrt(dat$n)
#  
#  if(requireNamespace("ggplot2")){
#   ggplot2::ggplot(dat,ggplot2::aes(Timepoint,accuracy,color=Model,group=Model)) +
#      ggplot2::theme_bw() + ggplot2::geom_line() + ggplot2::geom_point() #+ geom_errorbar(aes(ymin=accuracy-se,ymax=accuracy+se),width=0.2)
#  }

## ---- Box9a, eval=FALSE-------------------------------------------------------
#  load("prepared_data.RData"); load("parameters.RData")
#  
#  Gencov <- c()   # To stack all covariances from all time-points
#  for(tp in timepoints)
#  { load(paste0("covariances_tp_",tp,".RData"))
#    Gencov <- rbind(Gencov,gencov)
#  }
#  
#  # Objects to store regression coefficients
#  bPCSI <- bL1PSI <- vector("list",nPart)
#  
#  for(k in 1:nPart)
#  {   cat("  partition = ",k,"of",nPart,"\n")
#      indexTRN <- which(partitions[,k]==1)
#  
#      # Training set
#      xTRN <- scale(do.call(cbind,X[timepoints])[indexTRN,])
#      VARx <- var(xTRN)
#      EVDx <- eigen(VARx)
#  
#      # PC-based SI
#      gamma <- t(sweep(EVDx$vectors,2,1/EVDx$values,FUN="*")) %*%  Gencov[,k]
#      beta <- apply(sweep(EVDx$vectors,2,as.vector(gamma),FUN="*"),1,cumsum)[1:500,]
#      bPCSI[[k]] <- data.frame(I(beta),nsup=1:nrow(beta),lambda=NA)
#  
#      # L1-PSI
#      fm <- solveEN(VARx, Gencov[,k],nlambda=100,maxiter=200,tol=1E-3)
#      beta <- t(as.matrix(fm$beta)[,-1])
#      bL1PSI[[k]] <- data.frame(I(beta),nsup=fm$nsup[-1],lambda=fm$lambda[-1])
#  }
#  save(bPCSI, bL1PSI, nPart, file="multi_timepoint_coefficients.RData")

## ---- Box9b, eval=FALSE-------------------------------------------------------
#  load("parameters.RData")
#  load("multi_timepoint_coefficients.RData")
#  
#  AccSI <- c()     # Objects to store accuracy components
#  
#  for(k in 1:nPart)
#  {   cat("  partition = ",k,"of",nPart,"\n")
#      indexTST <- which(partitions[,k]==2)
#  
#      # Testing set
#      xTST <- scale(do.call(cbind,X[timepoints])[indexTST,])
#      yTST <- as.vector(scale(y[indexTST]))
#      KTST <- K[indexTST,indexTST]   # Connection given by replicates
#  
#      # Calculate the indices
#      PCSI_multi <- tcrossprod(xTST, bPCSI[[k]]$beta)
#      L1PSI_multi <- tcrossprod(xTST, bL1PSI[[k]]$beta)
#  
#      # Fit genetic models
#      fitSI <- as.matrix(data.frame(I(PCSI_multi),I(L1PSI_multi)))
#      fm <- getGenCov(y=cbind(yTST,fitSI), K=KTST, pairwise=FALSE, verbose=FALSE)
#      fm$varU <- ifelse(fm$varU<0.05,0.05,fm$varU)
#  
#      # Retrieve accuracy components
#      h <- sqrt(fm$varU/(fm$varU + fm$varE))[-1]
#      gencor <- fm$covU/sqrt(fm$varU[1]*fm$varU[-1])
#      accuracy <- gencor*h
#  
#      nsup <- c(bPCSI[[k]]$nsup,bL1PSI[[k]]$nsup)
#      lambda <- c(bPCSI[[k]]$lambda,bL1PSI[[k]]$lambda)
#      AccSI <- rbind(AccSI,data.frame(rep=k,SI=colnames(fitSI),h,gencor,accuracy,nsup,lambda))
#  }
#  save(AccSI, file="multi_timepoint_accuracy.RData")

## ---- Box10, eval=FALSE-------------------------------------------------------
#  tp <- 9      # Time-point
#  load(paste0("accuracy_tp_",tp,".RData"))
#  load("multi_timepoint_accuracy.RData")
#  
#  AccSI <- rbind(accSI,AccSI)
#  AccSI$Model <- unlist(lapply(strsplit(as.character(AccSI$SI),"\\."),function(x)x[1]))
#  AccSI <- split(AccSI,paste(AccSI$rep,"_",AccSI$Model))
#  AccSI <- do.call(rbind,lapply(AccSI,function(x)x[which.max(x$accuracy),]))
#  
#  dat <- aggregate(accuracy ~ Model, mean,data=AccSI)
#  dat$sd <- aggregate(accuracy ~ Model, sd, data=AccSI)$accuracy
#  dat$n <- aggregate(accuracy ~ Model, length, data=AccSI)$accuracy
#  dat$se <- qnorm(0.975)*dat$sd/sqrt(dat$n)
#  
#  if(requireNamespace("ggplot2")){
#   ggplot2::ggplot(dat,ggplot2::aes(Model,accuracy)) + ggplot2::theme_bw() +
#    ggplot2::geom_bar(stat="identity",width=0.65,fill="orange") +
#    ggplot2::geom_errorbar(ggplot2::aes(ymin=accuracy-se,ymax=accuracy+se),width=0.2) +
#    ggplot2::geom_text(ggplot2::aes(label=sprintf("%.2f",accuracy),y=accuracy*0.5))
#  }

## ---- Box11, eval=FALSE-------------------------------------------------------
#  load("varComps.RData")    # Load the SVD of ZZ' to speed computation
#  load("multi_timepoint_accuracy.RData")
#  
#  # Get genetic covariances
#  x <- scale(do.call(cbind,X))
#  y <- as.vector(scale(y))
#  fm <- getGenCov(y=cbind(y,x), EVD=EVD, scale=FALSE, pairwise=FALSE, verbose=FALSE)
#  gencov <- fm$covU
#  
#  # Get a value of lambda across partitions
#  AccSI$Model <- unlist(lapply(strsplit(as.character(AccSI$SI),"\\."),function(x)x[1]))
#  AccSI <- split(AccSI,paste0(AccSI$Model,"_",AccSI$rep))
#  AccSI <- do.call(rbind,lapply(AccSI,function(x)x[which.max(x$accuracy),]))
#  lambda <- mean(AccSI$lambda[AccSI$Model=="L1PSI_multi"])
#  
#  # Get regression coefficients
#  VARx <- var(x)
#  beta <- solveEN(VARx, gencov, lambda=lambda)$beta
#  
#  wl <- factor(rep(gsub("wl","",colnames(X[[1]])),length(X)))
#  Timepoint <- factor(rep(seq(length(X)),each=ncol(X[[1]])))
#  dat <- data.frame(beta=as.vector(beta),Timepoint,wl)
#  dat$beta[abs(dat$beta) < .Machine$double.eps] <- NA
#  
#  if(requireNamespace("ggplot2")){
#   ggplot2::ggplot(dat, ggplot2::aes(x=wl,y=Timepoint,fill = abs(beta))) +
#    ggplot2::theme_bw() + ggplot2::geom_tile(height=0.8) + ggplot2::labs(x="Wavelength (nm)") +
#    ggplot2::scale_x_discrete(breaks=levels(wl)[seq(1,nlevels(wl),25)]) +
#    ggplot2::scale_fill_gradientn(na.value='gray35', colours=c(rev(heat.colors(15))))
#  }

