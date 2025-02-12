## ----initialsetup, include=FALSE----------------------------------------------
knitr::opts_chunk$set(cache=FALSE)

## ----eval=FALSE---------------------------------------------------------------
#  library(SFSI)
#  if(requireNamespace("BGLR")){
#   data(wheat, package="BGLR")     # Load data from the BGLR package
#  }
#  
#  # Select the environment 1 to work with
#  y <- as.vector(scale(wheat.Y[,1]))
#  
#  # Calculate G matrix
#  G <- tcrossprod(scale(wheat.X))/ncol(wheat.X)
#  
#  # Save data
#  save(y, G, file="geno_pheno.RData")

## ----eval=FALSE---------------------------------------------------------------
#  load("geno_pheno.RData") # Load data
#  
#  # Fit model
#  fm0 <- fitBLUP(y, K=G)
#  fm0$theta <- fm0$varE/fm0$varU          # Residual/genetic variances ratio
#  fm0$h2 <- fm0$varU/(fm0$varU+fm0$varE)  # Heritability
#  c(fm0$varU,fm0$varE,fm0$theta,fm0$h2)   # Print variance components
#  
#  save(fm0, file="varComps.RData")

## ----eval=FALSE---------------------------------------------------------------
#  nPart <- 5                        # Number of partitions
#  load("geno_pheno.RData")          # Load data
#  
#  nTST <- ceiling(0.3*length(y))    # Number of elements in TST set
#  partitions <- matrix(1,nrow=length(y),ncol=nPart)   # Matrix to store partitions
#  seeds <- round(seq(1E3, .Machine$integer.max/10, length=nPart))
#  
#  for(k in 1:nPart){
#     set.seed(seeds[k])
#     partitions[sample(1:length(y), nTST),k] <- 0
#  }
#  save(partitions, file="partitions.RData")    # Save partitions

## ----eval=FALSE---------------------------------------------------------------
#  # Load data
#  load("geno_pheno.RData"); load("varComps.RData"); load("partitions.RData")
#  
#  accSSI <- mu <- varU <- varE <- c()       # Objects to store results
#  
#  for(k in 1:ncol(partitions))
#  { cat("  partition = ",k,"\n")
#    trn <- which(partitions[,k]==1)
#    tst <- which(partitions[,k]==0)
#  
#    # G-BLUP model
#    fm1 <- fitBLUP(y, K=G, trn=trn)
#    mu[k] <- fm1$b        # Retrieve mu estimate
#    varU[k] <- fm1$varU   # Retrieve varU
#    varE[k] <- fm1$varE   # Retrieve varE
#  
#    # Sparse SI
#    fm2 <- SGP(y, K=G, b=mu[k], varU=varU[k], varE=varE[k],
#               trn=trn, tst=tst, mc.cores=1, nlambda=100)
#    fm3 <- summary(fm2)   # Useful function to get results
#  
#    accuracy <- c(GBLUP=cor(fm1$yHat[tst],y[tst]), fm3$accuracy[,1])/sqrt(fm0$h2)
#    lambda <- c(min(fm3$lambda),fm3$lambda[,1])
#    nsup <- c(max(fm3$nsup),fm3$nsup[,1])
#    accSSI <- rbind(accSSI,data.frame(rep=k,SSI=names(accuracy),accuracy,lambda,nsup))
#  }
#  save(mu,varU,varE,accSSI,file="results_accuracy.RData")

## ----eval=FALSE---------------------------------------------------------------
#  load("results_accuracy.RData")
#  
#  dat <- data.frame(do.call(rbind,lapply(split(accSSI,accSSI$SSI),
#           function(x) apply(x[,-c(1:2)],2,mean))))
#  dat$Model <- unlist(lapply(strsplit(rownames(dat),"\\."),function(x)x[1]))
#  
#  dat2 <- do.call(rbind,lapply(split(dat,dat$Mod),function(x)x[which.max(x$acc),]))
#  
#  if(requireNamespace("ggplot2")){
#   ggplot2::ggplot(dat[dat$nsup>1,],ggplot2::aes(-log(lambda),accuracy)) +
#     ggplot2::geom_hline(yintercept=dat["GBLUP",]$accuracy, linetype="dashed") +
#     ggplot2::geom_line(ggplot2::aes(color=Model),size=1.1) + ggplot2::theme_bw() +
#     ggplot2::geom_point(data=dat2,ggplot2::aes(color=Model),size=2.5)
#  }

## ----eval=FALSE---------------------------------------------------------------
#  load("geno_pheno.RData");   load("varComps.RData")
#  load("partitions.RData");   load("results_accuracy.RData")
#  
#  lambdaCV <- accSSI_CV <- nsupCV <- c()   # Objects to store results
#  
#  for(k in 1:ncol(partitions))
#  {   cat("  partition = ",k,"\n")
#      trn <- which(partitions[,k]==1)
#  
#      # Cross-validating the training set
#      fm1 <- SGP.CV(y, K=G, trn=trn, nlambda=100, mc.cores=1, nfolds=5, nCV=1)
#      lambdaCV[k] <- summary(fm1)$optCOR["lambda"]
#  
#      # Fit a SSI with the estimated lambda
#      fm2 <- SGP(y, K=G, b=mu[k], varU=varU[k], varE=varE[k],
#                 trn=trn, tst=tst, lambda=lambdaCV[k])
#  
#      accSSI_CV[k] <- summary(fm2)$accuracy/sqrt(fm0$h2)
#      nsupCV <- cbind(nsupCV, fm2$nsup)
#  }
#  save(accSSI_CV,lambdaCV,nsupCV,file="results_accuracyCV.RData")

## ----eval=FALSE---------------------------------------------------------------
#  load("results_accuracy.RData"); load("results_accuracyCV.RData")
#  
#  dat <- data.frame(GBLUP=accSSI[accSSI$SSI=="GBLUP",]$acc,SSI=accSSI_CV)
#  rg <- range(dat)
#  tmp <- c(mean(rg),diff(rg)*0.4)
#  
#  if(requireNamespace("ggplot2")){
#   ggplot2::ggplot(dat,ggplot2::aes(GBLUP,SSI)) +
#    ggplot2::geom_abline(slope=1,linetype="dotted") + ggplot2::theme_bw() +
#    ggplot2::geom_point(shape=21,color="orange") + ggplot2::lims(x=rg,y=rg) +
#    ggplot2::annotate("text",tmp[1],tmp[1]-tmp[2],label=round(mean(dat$GBLUP),3)) +
#    ggplot2::annotate("text",tmp[1]-tmp[2],tmp[1],label=round(mean(dat$SSI),3))
#  }

## ----eval=FALSE---------------------------------------------------------------
#  load("results_accuracyCV.RData")
#  
#  dat <- data.frame(nsup=as.vector(nsupCV))
#  
#  bw <- round(diff(range(dat$nsup))/40)
#  if(requireNamespace("ggplot2")){
#   ggplot2::ggplot(data=dat,ggplot2::aes(nsup,stat(count)/length(nsupCV))) +
#      ggplot2::theme_bw() +
#      ggplot2::geom_histogram(color="gray20",fill="lightblue",binwidth=bw) +
#      ggplot2::labs(x=bquote("Support set size(" *n[sup]*")"),y="Frequency")
#  }

## ----eval=FALSE---------------------------------------------------------------
#  # Load data
#  load("geno_pheno.RData"); load("partitions.RData"); load("results_accuracyCV.RData")
#  
#  part <- 1      # Choose any partition from 1,…,nPart
#  trn <- which(partitions[,k]==1)
#  tst <- which(partitions[,k]==0)
#  
#  # Fit SSI with lambda previously estimated using CV
#  fm <- SGP(y, K=G, trn=trn, tst=tst, lambda=lambdaCV[part])
#  
#  plot(net(fm, K=G), i=1:16, unified=FALSE, main=NULL, bg.col="white",
#       set.size=c(3,1.5,0.2), point.color="gray40", axis.labels=FALSE)

