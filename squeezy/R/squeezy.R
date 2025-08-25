#main function: fit group-adaptive elastic net linear or logistic model
squeezy <- function(Y,X,groupset,alpha=1,model=NULL,
                      X2=NULL,Y2=NULL,unpen=NULL,intrcpt=TRUE,
                      method=c("ecpcEN","MML","MML.noDeriv","CV"),
                      fold=10,compareMR = TRUE,selectAIC=FALSE,
                      fit.ecpc=NULL,
                      lambdas=NULL,lambdaglobal=NULL,
                      lambdasinit=NULL,sigmasq=NULL,
                      ecpcinit=TRUE,SANN=FALSE,minlam=10^-3,
                      standardise_Y=NULL,reCV=NULL,opt.sigma=NULL,
                      resultsAICboth=FALSE,silent=FALSE){
  #Y: response
  #X: observed data
  #groupset: list with index of covariates of co-data groups
  #alpha: elastic net penalty parameter (as in glmnet)
  #X2: independent observed data
  #Y2: independent response data
  #unpen: vector with index of unpenalised variables
  #intrcpt: should intercept be included?
  #fold: number of folds used in global lambda cross-validation
  #sigmasq: (linear regression) noise level
  #method: "CV","MML.noDeriv", or "MML"
  #compareMR: TRUE/FALSE to return betas for multiridge estimate, and predictions for Y2 if X2 is given
  #selectAIC: TRUE/FALSE to compare AIC of multiridge model and ordinary ridge model. Return best one.
  
  #Set-up variables ---------------------------------------------------------------------------
  n <- dim(X)[1] #number of samples
  p <- dim(X)[2] #number of covariates 
  if(length(lambdas)==p && missing(groupset)) groupset <- lapply(1:p, function(x) x)
  groupsets <- list(groupset)
  if(!is.null(X2)) n2<-dim(X2)[1] #number of samples in independent data set x2 if given
  
  if(is.null(model)){
    if(all(is.element(Y,c(0,1))) || is.factor(Y)){
      model <- "logistic" 
    } else if(all(is.numeric(Y)) & !(is.matrix(Y) && dim(Y)[2]==2)){
      model <- "linear"
    }else{
      model <- "cox"
    }
  }
  
  #settings default methods
  if(length(method)==4){
    method <- "MML"
  }
  
  switch(method,
         "ecpcEN"={
           if(is.null(fit.ecpc)) stop("provide ecpc fit results")
           if(is.null(lambdas)) lambdas <- fit.ecpc$sigmahat/(fit.ecpc$gamma*fit.ecpc$tauglobal)
           if(is.null(lambdaglobal)) lambdaglobal <- fit.ecpc$sigmahat/fit.ecpc$tauglobal
           if(is.null(sigmasq)) sigmasq <- fit.ecpc$sigmahat
           if(is.null(standardise_Y)) standardise_Y <- TRUE
           if(is.null(reCV)) reCV <- TRUE
           if(is.null(opt.sigma)) opt.sigma <- FALSE
         },
         "MML"={
           if(!is.null(fit.ecpc)){
             if(is.null(lambdasinit)) lambdasinit <- fit.ecpc$sigmahat/(fit.ecpc$gamma*fit.ecpc$tauglobal)
             if(is.null(lambdaglobal)) lambdaglobal <- fit.ecpc$sigmahat/fit.ecpc$tauglobal
             if(is.null(sigmasq)) sigmasq <- fit.ecpc$sigmahat
           } 
           if(is.null(standardise_Y)) standardise_Y <- FALSE
           if(is.null(opt.sigma)) opt.sigma <- TRUE
           if(is.null(reCV)){
             reCV <- FALSE; if(model!="linear" | !opt.sigma) reCV <- TRUE
           } 
           
         },
         "MML.noDeriv"={
           if(!is.null(fit.ecpc)){
             if(is.null(lambdasinit)) lambdasinit <- fit.ecpc$sigmahat/(fit.ecpc$gamma*fit.ecpc$tauglobal)
             if(is.null(lambdaglobal)) lambdaglobal <- fit.ecpc$sigmahat/fit.ecpc$tauglobal
             if(is.null(sigmasq)) sigmasq <- fit.ecpc$sigmahat
           } 
           if(is.null(standardise_Y)) standardise_Y <- FALSE
           if(is.null(opt.sigma)) opt.sigma <- TRUE
           if(is.null(reCV)){
             reCV <- FALSE; if(!opt.sigma) reCV <- TRUE
           } 
         },
         "CV"={
           if(!is.null(fit.ecpc)){
             if(is.null(lambdasinit)) lambdasinit <- fit.ecpc$sigmahat/(fit.ecpc$gamma*fit.ecpc$tauglobal)
             if(is.null(lambdaglobal)) lambdaglobal <- fit.ecpc$sigmahat/fit.ecpc$tauglobal
             if(is.null(sigmasq)) sigmasq <- fit.ecpc$sigmahat
           } 
           if(is.null(standardise_Y)) standardise_Y <- FALSE
           if(is.null(opt.sigma)) opt.sigma <- FALSE
           if(is.null(reCV)) reCV <- TRUE
         }
  )
  

  switch(model,
         'linear'={
           fml <- 'gaussian'
           sd_y <- sqrt(var(Y)*(n-1)/n)[1]
           if(standardise_Y){
             Y <- Y/sd_y
             sd_y_former <- sd_y
             sd_y <- 1
             if(!is.null(sigmasq)) sigmasq <- sigmasq/sd_y_former
           }
           if(method=="MML") minlam <- max(minlam,10^-4*var(Y))
         },
         'logistic'={
           fml <- 'binomial'
           opt.sigma <- FALSE
           standardise_Y <- FALSE
           sd_y <- 1 #do not standardise y in logistic setting
           sd_y_former <- sd_y
           
           #set response to numeric 0,1
           levelsY<-cbind(c(0,1),c(0,1))
           if(!all(is.element(Y,c(0,1)))){
             oldLevelsY<-levels(Y)
             levels(Y)<-c("0","1")
             Y<-as.numeric(Y)-1
             levelsY<-cbind(oldLevelsY,c(0,1))
             colnames(levelsY)<-c("Old level names","New level names")
             if(!is.null(Y2)){
               levels(Y2)<-c("0","1")
               Y2<-as.numeric(Y2)-1
             }
             if(!silent) print("Y is put in 0/1 format, see levelsY in output for new names")
           }
         },
         'cox'={
           fml <- 'cox'
           opt.sigma <- FALSE
           standardise_Y <- FALSE
           sd_y <- 1 #do not standardise y in cox regression setting
           sd_y_former <- sd_y
           intrcpt <- FALSE #Cox does not use intercept
         }
  )
  # check whether unpenalised covariates are not in partition
  # and set penalty.factor of unpenalised covariates to 0 for glmnet
  penfctr <- rep(1,p) #factor=1 for penalised covariates
  if(length(unpen)>0){
    penfctr[unpen] <- 0 #factor=0 for unpenalised covariates
    if(any(unlist(groupsets)%in%unpen)){
      warning("Unpenalised covariates removed from group set")
      for(i in 1:length(groupsets)){
        for(j in 1:length(groupsets[[i]])){
          if(all(groupsets[[i]][[j]]%in%unpen)){
            groupsets[[i]][[j]] <- NULL #remove whole group if all covariates unpenalised
          }else{
            groupsets[[i]][[j]] <- groupsets[[i]][[j]][!(groupsets[[i]][[j]]%in%unpen)]
          }
        }
      }
    }
  }
  
  G <- sapply(groupsets,length) #1xm vector with G_i, number of groups in partition i
  m <- length(G) #number of partitions
  
  indGrpsGlobal <- list(1:G[1]) #global group index in case we have multiple partitions
  if(m>1){
    for(i in 2:m){
      indGrpsGlobal[[i]] <- (sum(G[1:(i-1)])+1):sum(G[1:i])
    }
  }
  Kg <- lapply(groupsets,function(x)(sapply(x,length))) #m-list with G_i vector of group sizes in partition i
  #ind1<-ind
  
  #ind <- (matrix(1,G,1)%*%ind)==(1:G)#sparse matrix with ij element TRUE if jth element in group i, otherwise FALSE
  i<-unlist(sapply(1:sum(G),function(x){rep(x,unlist(Kg)[x])}))
  j<-unlist(unlist(groupsets))
  ind <- sparseMatrix(i,j,x=1) #sparse matrix with ij element 1 if jth element in group i (global index), otherwise 0
  
  Ik <- lapply(1:m,function(i){
    x<-rep(0,sum(G))
    x[(sum(G[1:i-1])+1):sum(G[1:i])]<-1
    as.vector(x%*%ind)}) #list for each partition with px1 vector with number of groups beta_k is in
  #sparse matrix with ij element 1/Ij if beta_j in group i
  
  #make co-data matrix Z (Zt transpose of Z as in paper, with co-data matrices stacked for multiple groupsets)
  Zt<-ind; 
  if(G[1]>1){
    Zt[1:G[1],]<-t(t(ind[1:G[1],])/apply(ind[1:G[1],],2,sum))
  }
  if(m>1){
    for(i in 2:m){
      if(G[i]>1){
        Zt[indGrpsGlobal[[i]],]<-t(t(ind[indGrpsGlobal[[i]],])/
                                     apply(ind[indGrpsGlobal[[i]],],2,sum))
      }
    }
  }
  
  if(dim(Zt)[2]<p) Zt <- cbind(Zt,matrix(rep(NaN,(p-dim(Zt)[2])*sum(G)),c(sum(G),p-dim(Zt)[2])))
  
  
  #Extend data to make artifical non-overlapping groups----
  Xxtnd <- do.call(cbind,lapply(groupsets[[1]],function(group){t(t(X[,group])/sqrt(Ik[[1]][group]))}))
  #create new group indices for Xxtnd
  Kg2 <- c(1,Kg[[1]]) 
  G2 <- length(Kg2)-1
  groupxtnd <- lapply(2:length(Kg2),function(i){sum(Kg2[1:(i-1)]):(sum(Kg2[1:i])-1)}) #list of indices in each group
  groupxtnd2 <- unlist(sapply(1:G2,function(x){rep(x,Kg2[x+1])})) #vector with group number
 
  Xunpen <- NULL
  if(sum((1:p)%in%unpen)>0) Xunpen<-X[,(1:p)%in%unpen] #seperate matrix for unpenalised variables
  
  #datablockNo <- groupxtnd2
  #datablocks <- groupxtnd (or groupset if no overlapping groups)
  
  #datablocks: list with each element a data type containing indices of covariates with that data type 
  Xbl <- createXblocks(lapply(groupxtnd,function(ind) Xxtnd[,ind,drop=FALSE]))
  XXbl <- createXXblocks(lapply(groupxtnd,function(ind) Xxtnd[,ind,drop=FALSE]))
  
  #Find global lambda if not given for initial penalty and/or for AIC comparison----
  if(is.null(lambdaglobal)){
    #find rough estimate of initial global lambda
    # cvperblock <- fastCV2(list(Xxtnd),Y=Y,kfold=fold,fixedfolds = FALSE,X1=Xunpen,intercept=intrcpt)
    # lambda <- cvperblock$lambdas
    # lambda[lambda==Inf] <- 10^6
    
    lambdaseq <- 10^c(-10:10)
    XXbl1 <- list(apply(simplify2array(XXbl),c(1,2),sum))
    ML <- sapply(log(lambdaseq),function(lam){
      temp <- try(minML.LA.ridgeGLM(loglambdas = lam,XXblocks = XXbl1,sigmasq=sd_y^2,
                                    Y=Y,Xunpen=Xunpen,intrcpt = intrcpt,model=model),
                  silent=TRUE)
      if(class(temp)[1]!="try-error"){
        return(temp)
      }else return(NaN)
      }
    )
    
    
    if(all(is.nan(ML))) stop("Error in estimating global lambda, try standardising data")
    lambdaseq <- lambdaseq[!is.nan(ML)&(ML!=Inf)]
    ML <- ML[!is.nan(ML)&(ML!=Inf)]
    #if multiple lambda with same ML, take least extreme lambda, 
    #that is within 1 percent of minimal value of difference no/full penalty
    se <- abs(ML[1]-rev(ML)[1])/100
    if(ML[1]<rev(ML)[1]){
      lambda <- rev(lambdaseq)[which.min(rev(ML))]
      if(lambda==lambdaseq[1]){
        lambda <- max(lambdaseq[ML<=(min(ML)+se)])
      }
    } 
    else{
      lambda <- lambdaseq[which.min(ML)]
      if(lambda==rev(lambdaseq)[1]){
        lambda <- min(lambdaseq[ML<=(min(ML)+se)])
      }
    } 
  }else{
    lambda <- lambdaglobal
  }
    
  #Further optimise global lambda with same method as multi-group for AIC comparison
  if(selectAIC | compareMR){
    if(method=="CV"){
      leftout <- CVfolds(Y=Y,kfold=fold,nrepeat=3,fixedfolds = FALSE) #Create (repeated) CV-splits of the data
      lambda1groupfit <- optLambdasWrap(penaltiesinit=lambda, 
                                     XXblocks=list(apply(simplify2array(XXbl),c(1,2),sum)),
                                     Y=Y,folds=leftout,
                                     X1=Xunpen,intercept=intrcpt,
                                     score=ifelse(model == "linear", "mse", "loglik"),model=model,
                                     maxItropt2 = 500,reltol = 10^-3)
      lambda <- lambda1groupfit$optpen
    }else if(method=="MML.noDeriv"){
      lambda1groupfit <- optLambdas_mgcv(penaltiesinit=lambda,Y=Y,
                                         XXblocks=list(apply(simplify2array(XXbl),c(1,2),sum)),
                                         model=model,reltol=1e-3,
                                         maxItropt=500,tracescore=FALSE,fixedseed =FALSE,
                                         optmethod = "Nelder-Mead",
                                         sigmasq=sigmasq) #TD: intercept?
      lambda <- lambda1groupfit$optpen
    }else if(method=="MML"){
      sigmahat<-sd_y 
      if(!is.null(sigmasq) & model=="linear") sigmahat <- sigmasq
      
      if(opt.sigma){
        lambda1groupfit <- optim(par=c(log(sigmahat),log(lambda)), fn=minML.LA.ridgeGLM,
                                 gr=dminML.LA.ridgeGLM, method="BFGS",minlam=minlam,
                                 XXblocks = list(apply(simplify2array(XXbl),c(1,2),sum)) ,
                                 Y=Y,sigmasq=sigmahat,model=model,
                                 intrcpt=intrcpt, Xunpen=Xunpen,opt.sigma=opt.sigma)
        sigmasq <- exp(lambda1groupfit$par[1])+minlam
        lambda <- exp(lambda1groupfit$par[-1])+minlam
      }else{
        lambda1groupfit <- optim(par=log(lambda), fn=minML.LA.ridgeGLM,
                                 gr=dminML.LA.ridgeGLM, method="BFGS",minlam=minlam,
                                 XXblocks = list(apply(simplify2array(XXbl),c(1,2),sum)) ,
                                 Y=Y,sigmasq=sigmahat,model=model,
                                 intrcpt=intrcpt, Xunpen=Xunpen,opt.sigma=opt.sigma)
        lambda <- exp(lambda1groupfit$par)+minlam
      }
      
    }
  }
  
  #find estimate for sigma for 1 group for AIC comparison
  sigmahat <- sd_y
  if(model=="linear"){
    if(!is.null(sigmasq)) sigmahat <- sigmasq
    else{
      XXT1grp <- SigmaFromBlocks(XXblocks = list(apply(simplify2array(XXbl),c(1,2),sum)),lambda)
      if(length(unpen)>0 | intrcpt){
        Xunpen2 <- Xunpen
        if(intrcpt) Xunpen2 <- cbind(Xunpen,rep(1,n))
        if(intrcpt && length(unpen)==0){
          betaunpenML1grp <- sum(Y)/n
        }else{
          temp <- solve(XXT1grp+diag(rep(1,n)),Xunpen2)
          betaunpenML1grp <- solve(t(Xunpen2)%*%temp , t(temp)%*%Y)
        }
        sigmahat <- c(t(Y-Xunpen2%*%betaunpenML1grp)%*%
                              solve(XXT1grp+diag(rep(1,n)),Y-Xunpen2%*%betaunpenML1grp)/n)
      }else{
        sigmahat <- c(t(Y)%*%solve(XXT1grp+diag(rep(1,n)),Y)/n)
      }
    }
  }
  if(selectAIC){
    sigmahat1group <- sigmahat
  }
  
  #Compute ridge penalties and corresponding group variance estimates----
  if(is.null(lambdas)){
    #If not given, set initial lambdas to global lambda
    if(is.null(lambdasinit)|!ecpcinit){
      lambdasinit <- rep(lambda,G)
    }
    if(any(lambdasinit==Inf)){
      lambdasinit[lambdasinit>2*lambda] <- 2*lambda #truncate at 2 times the global lambda
    }
    
    #Find joint lambdas:
    if(method=="CV"){
      leftout <- CVfolds(Y=Y,kfold=fold,nrepeat=3,fixedfolds = FALSE) #Create (repeated) CV-splits of the data
      jointlambdas <- optLambdasWrap(penaltiesinit=lambdasinit, XXblocks=XXbl,Y=Y,folds=leftout,
                                     X1=Xunpen,intercept=intrcpt,
                                     score=ifelse(model == "linear", "mse", "loglik"),model=model,
                                     maxItropt2 = 500,reltol = 10^-3,traceCV=FALSE)
      lambdas <- jointlambdas$optpen
    }else if(method=="MML.noDeriv"){
      #browser()
      if(ecpcinit){
        if(SANN){
          jointlambdas <- optLambdas_mgcvWrap(penaltiesinit=lambdasinit, XXblocks=XXbl,Y=Y,
                                              model=model,reltol=1e-4,
                                              maxItropt2=1000,tracescore=FALSE,fixedseed =FALSE,
                                              optmethod2 = "Nelder-Mead",
                                              sigmasq=sigmahat,opt.sigma=opt.sigma) #TD: intercept?
        }else{
          jointlambdas <- optLambdas_mgcv(penaltiesinit=lambdasinit, XXblocks=XXbl,Y=Y,
                                          model=model,reltol=1e-4,
                                          maxItropt=1000,tracescore=FALSE,fixedseed =FALSE,
                                          optmethod = "Nelder-Mead",
                                          sigmasq=sigmahat,opt.sigma=opt.sigma) #TD: intercept?
        }
        
      }else{
        if(SANN){
          jointlambdas <- optLambdas_mgcvWrap(penaltiesinit=rep(lambda,G), XXblocks=XXbl,Y=Y,
                                              model=model,reltol=1e-4,
                                              maxItropt2=1000,tracescore=FALSE,fixedseed =FALSE,
                                              optmethod2 = "Nelder-Mead",
                                              sigmasq=sigmahat,opt.sigma=opt.sigma) #TD: intercept?
        }else{
          jointlambdas <- optLambdas_mgcv(penaltiesinit=rep(lambda,G), XXblocks=XXbl,Y=Y,
                                          model=model,reltol=1e-4,
                                          maxItropt=1000,tracescore=FALSE,fixedseed =FALSE,
                                          optmethod = "Nelder-Mead",
                                          sigmasq=sigmahat,opt.sigma=opt.sigma) #TD: intercept?
        }
      }
      
      # print("-log(ML) for initial penalties")
      # if(ecpcinit){
      #   MLinit <- mgcv_lambda(penalties=lambdasinit, XXblocks=XXbl,Y=Y, model=model, sigmasq = sigmasq) #-log(ml) for ecpc
      # }else{
      #   MLinit <- mgcv_lambda(penalties=rep(lambda,G), XXblocks=XXbl,Y=Y, model=model, sigmasq = sigmasq) #-log(ml) for global
      # }

      if(opt.sigma){
        sigmasq <- jointlambdas$optpen[1]
        lambdas <- jointlambdas$optpen[-1]
      }else{
        lambdas <- jointlambdas$optpen
      }
      # print("-log(ML) for final penalties")
      # MLfinal <- mgcv_lambda(penalties=lambdas, XXblocks=XXbl,Y=Y, model=model, sigmasq = sigmasq) #-log(ml) for ecpc

      
    }else if(method=="MML"){
      if(opt.sigma){
        jointlambdas <- optim(par=c(log(sigmahat),log(lambdasinit)), 
                              fn=minML.LA.ridgeGLM, 
                              gr=dminML.LA.ridgeGLM, method="BFGS",minlam=minlam,
                              XXblocks = XXbl , Y=Y,opt.sigma=opt.sigma,model=model,
                              intrcpt=intrcpt, Xunpen=Xunpen)
        sigmasq <- exp(jointlambdas$par[1])+minlam
        lambdas <- exp(jointlambdas$par[-1])+minlam
        
        # MLinit <- minML.LA.ridgeGLM(loglambdas=c(log(sigmahat),log(lambdasinit)),opt.sigma = TRUE,
        #                             XXblocks = XXbl , Y=Y,model=model,intrcpt=intrcpt,minlam=0)
        # MLfinal <- minML.LA.ridgeGLM(loglambdas=c(log(sigmasq),log(lambdas)),
        #                              opt.sigma = TRUE,
        #                              XXblocks = XXbl , Y=Y,model=model,intrcpt=intrcpt,minlam=0)
        
      }else{
        jointlambdas <- optim(par=log(lambdasinit), fn=minML.LA.ridgeGLM, 
                              gr=dminML.LA.ridgeGLM, method="BFGS",minlam=minlam,
                              XXblocks = XXbl , Y=Y,sigmasq=sigmahat,model=model,
                              intrcpt=intrcpt, Xunpen=Xunpen)
        lambdas <- exp(jointlambdas$par)+minlam
        
        # MLinit <- minML.LA.ridgeGLM(loglambdas=c(log(lambdasinit)),opt.sigma = FALSE,
        #                             sigmasq=sigmahat,
        #                             XXblocks = XXbl , Y=Y,model=model,intrcpt=intrcpt,minlam=0)
        # MLfinal <- minML.LA.ridgeGLM(loglambdas=c(log(lambdas)),
        #                              opt.sigma = FALSE,sigmasq=sigmasq,
        #                              XXblocks = XXbl , Y=Y,model=model,intrcpt=intrcpt,minlam=0)
      }
        
      
    }
  }else{
    lambdasinit <- lambdas
  }
  
  sigmahat <- 1 #sigma not in model for logistic: set to 1
  if(model=="linear"){
    if(!is.null(sigmasq)) sigmahat <- sigmasq
    else{
      XXT <- SigmaFromBlocks(XXblocks = XXbl,lambdas)
      if(length(unpen)>0 | intrcpt){
        Xunpen2 <- Xunpen
        if(intrcpt) Xunpen2 <- cbind(Xunpen,rep(1,n))
        if(intrcpt && length(unpen)==0){
          betaunpenML <- sum(Y)/n
        }else{
          temp <- solve(XXT+diag(rep(1,n)),Xunpen2)
          betaunpenML <- solve(t(Xunpen2)%*%temp , t(temp)%*%Y)
        }
        sigmahat <- c(t(Y-Xunpen2%*%betaunpenML)%*%solve(XXT+diag(rep(1,n)),Y-Xunpen2%*%betaunpenML)/n)
      }else{
        sigmahat <- c(t(Y)%*%solve(XXT+diag(rep(1,n)),Y)/n)
      }
    }
  }
  
  MLinit <- minML.LA.ridgeGLM(loglambdas=log(lambdasinit),opt.sigma = FALSE,sigmasq=sigmahat,
                              XXblocks = XXbl , Y=Y,model=model,intrcpt=intrcpt,minlam=0)
  MLfinal <- minML.LA.ridgeGLM(loglambdas=log(lambdas),
                               opt.sigma = FALSE, sigmasq=sigmahat,
                               XXblocks = XXbl , Y=Y,model=model,intrcpt=intrcpt,minlam=0)
  # print("-log(ML) for initial penalties"); print(MLinit)
  # print("-log(ML) for final penalties"); print(MLfinal)
  
  #May compare 1 group versus multiple groups with AIC
  if(selectAIC){
    # res1group <- squeezy(Y=Y,X=X,groupset=list(1:p),alpha=0,model=model,
    #                        unpen=unpen,intrcpt=intrcpt,
    #                        fold=fold,sigmasq=sigmasq,method=method,
    #                        compareMR = FALSE,selectAIC=FALSE)
    # 
    # lambda1group <- res1group$lambdaMR
    # sigmahat1group <- res1group$sigmahat
    lambda1group <- lambda
    if(sum((1:p)%in%unpen)>0){
      AIC1group <- mAIC.LA.ridgeGLM(log(lambda1group),XXblocks=list(apply(simplify2array(XXbl),c(1,2),sum)),
                                   Y=Y,sigmasq=sigmahat1group,Xunpen=Xunpen,intrcpt=intrcpt,model=model)
      AICmultigroup <- mAIC.LA.ridgeGLM(log(lambdas),XXblocks=XXbl,
                                       Y=Y,sigmasq=sigmahat,Xunpen=Xunpen,intrcpt=intrcpt,model=model)
    }else{
      AIC1group <- mAIC.LA.ridgeGLM(log(lambda1group),
                                   XXblocks=list(apply(simplify2array(XXbl),c(1,2),sum)),
                                   Y=Y,sigmasq=sigmahat1group,intrcpt=intrcpt,model=model)
      AICmultigroup <- mAIC.LA.ridgeGLM(log(lambdas),XXblocks=XXbl,
                                       Y=Y,sigmasq=sigmahat,intrcpt=intrcpt,model=model)
    }
    #If model with only one group has lower AIC, select that model
    if(AIC1group <= AICmultigroup){
      lambdasNotOptimalAIC <- lambdas
      sigmahatNotOptimalAIC <- sigmahat
      lambdas <- rep(lambda1group,G)
      sigmahat <- sigmahat1group
      modelbestAIC <- "onegroup"
    }else{
      lambdasNotOptimalAIC <- lambda1group
      sigmahatNotOptimalAIC <- sigmahat1group
      modelbestAIC <- "multigroup"
    }
  }
  
  tauglobal<- sigmahat/lambda #set global group variance
  gamma <- lambda/lambdas #= (sigmahat/lambdas)/tauglobal
  lambdap<- lambda/(as.vector(gamma%*%Zt)) #=sigmahat/(tauglobal*as.vector(gamma%*%Zt)) #specific ridge penalty for beta_k
  lambdap[lambdap<0]<-Inf 
  
  
  #Compute regression model----
  glmGR <- NA
  if(compareMR||alpha==0){
    #Compute ridge betas with multiridge
    XXT <- SigmaFromBlocks(XXbl,penalties=lambdas) #create nxn Sigma matrix = sum_b [lambda_b)^{-1} X_b %*% t(X_b)]
    if(sum((1:p)%in%unpen)>0){
      fit <- IWLSridge(XXT,Y=Y, model=model,intercept=intrcpt,X1=X[,(1:p)%in%unpen]) #Fit. fit$etas contains the n linear predictors
    }else{
      fit <- IWLSridge(XXT,Y=Y, model=model,intercept=intrcpt) #Fit. fit$etas contains the n linear predictors
    }
    betafit <- betasout(fit, Xblocks=Xbl, penalties=lambdas) #Find betas.
    a0MR <- 0
    if(intrcpt) a0MR <- c(betafit[[1]][1]) #intercept
    betaMR <- rep(0,p) 
    betaMR[(1:p)%in%unpen] <- betafit[[1]][-1] #unpenalised variables
    for(i in 1:length(groupsets[[1]])){
      betaMR[groupsets[[1]][[i]]] <- betaMR[groupsets[[1]][[i]]] + betafit[[1+i]]/sqrt(Ik[[1]][groupsets[[1]][[i]]])
    }  
    rm(betafit)
  }
  
  #Transform group variance estimates to desired group prior estimates----
  
  pen <- which(!((1:p)%in%unpen))
  if(any(is.nan(sqrt(lambdap[pen])))){browser()}
  
  # #Compute ridge betas with glmnet
  # 
  # penfctr2 <- penfctr
  # penfctr2[pen] <- lambdap[pen]
  # not0 <- which(penfctr2!=Inf)
  # lambdaoverall <- sum(penfctr2[not0])/length(penfctr2[not0]) #global penalty parameter
  # penfctr2 <- penfctr2/lambdaoverall #scale penalty factor such that sums to p
  # #all(penfctr2*lambdaoverall==lambdap)
  # 
  # Xacc <- X
  # Xacc[,pen] <- as.matrix(X[,pen] %*% sparseMatrix(i=1:length(pen),j=1:length(pen),
  #                                                  x=c(1/sqrt(penfctr2[pen]))))
  # if(model=="cox"){
  #   glmGR <- glmnet(Xacc[,not0],Y,alpha=0,#lambda = lambdaoverall/n*sd_y,
  #                   family=fml,standardize = FALSE,
  #                   penalty.factor=penfctr[not0], thresh=10^-10)
  #   temp <- coef(glmGR,s=lambdaoverall/n*sd_y,exact=TRUE,x=Xacc[,not0],y=Y,
  #                penalty.factor=penfctr[not0],family=fml,
  #                thresh=10^-10)
  # }else{
  #   glmGR <- glmnet(Xacc[,not0],Y,alpha=0,#lambda = lambdaoverall/n*sd_y,
  #                   family=fml, intercept = intrcpt, standardize = FALSE,
  #                   penalty.factor=penfctr[not0], thresh=10^-10)
  #   #betaGLM <- rep(0,p); betaGLM[not0] <- as.vector(glmGR$beta) 
  #   temp <- coef(glmGR,s=lambdaoverall/n*sd_y,exact=TRUE,x=Xacc[,not0],y=Y,
  #                penalty.factor=penfctr[not0],family=fml,
  #                intercept=intrcpt, thresh=10^-10)
  # }
  # betaGLM <- rep(0,p); betaGLM[not0] <- temp[-1]
  # betaGLM[pen] <- c(1/sqrt(penfctr2[pen])) * betaGLM[pen]
  # a0GLM <- temp[1]
  # 
  # plot(betaMR,betaGLM); abline(0,1)
  
  # #with penfctr and glmnet: does not always converge
  # penfctr2 <- penfctr
  # penfctr2[pen] <- lambdap[pen]
  # not0 <- which(penfctr2!=Inf)
  # lambdaoverall <- sum(penfctr2[not0])/length(penfctr2[not0]) #global penalty parameter
  # penfctr2 <- penfctr2/lambdaoverall #scale penalty factor such that sums to p
  # if(model=="cox"){
  #   glmGR <- glmnet(X[,not0],Y,alpha=0,family=fml,standardize = FALSE,
  #                   penalty.factor=penfctr2[not0], thresh=10^-10)
  #   temp <- coef(glmGR,s=lambdaoverall/n*sd_y,exact=TRUE,x=X[,not0],y=Y,
  #                penalty.factor=penfctr2[not0],family=fml,
  #                thresh=10^-10)
  # }else{
  #   glmGR <- glmnet(X[,not0],Y,alpha=0,family=fml,#lambda=lambdaoverall/n*sd_y,
  #                   intercept = intrcpt, standardize = FALSE,
  #                   penalty.factor=penfctr2[not0], thresh=10^-10)
  #   #betaGLM2 <- rep(0,p); betaGLM2[not0] <- as.vector(glmGR$beta)
  #   
  #   temp <- coef(glmGR,s=lambdaoverall/n*sd_y,exact=TRUE,x=X[,not0],y=Y,
  #                penalty.factor=penfctr2[not0],family=fml,
  #                intercept=intrcpt, thresh=10^-12)
  # }
  # betaGLM2 <- rep(0,p); betaGLM2[not0] <- temp[-1]
  # plot(betaGLM2,betaGLM); abline(0,1); 
  # points(betaGLM2[groupsets[[1]][[1]]],betaGLM[groupsets[[1]][[1]]],col="red")
  
  #Transform penalties and compute elastic net betas with glmnet
  if(alpha <= 1){
    varFunc <- function(tauEN,alpha=alpha,tauR){
      #tauR: ridge prior variance (equals sigma^2/lambda_R)
      #tauEN: elastic net prior variance (equals sigma^2/lambda_{EN})
      #alpha: fixed elastic net mixing parameter
      #return variance of elastic net prior minus the ridge variance tauR;
      #p(\beta)\propto exp(-1/(2*tauEN)*(alpha*|\beta|+(1-\alpha)\beta^2))
      
      t2 <- - alpha/2/(1-alpha)^(3/2)*sqrt(tauEN)*
        exp(dnorm(alpha/2/sqrt(tauEN)/sqrt(1-alpha),log=TRUE) -
              pnorm(-alpha/2/sqrt(tauEN)/sqrt(1-alpha),log.p=TRUE))
      varBeta <- tauEN/(1-alpha) + alpha^2/4/(1-alpha)^2 + t2
      f <- varBeta - tauR
      return(f)
    }
    lamEN <- function(alpha,tauR){
      if(alpha==0){
        lamEN <- sigmahat/tauR
      }else if(alpha==1){
        lamEN <- sigmahat*sqrt(8/tauR)
      }else if(tauR/sigmahat>10^6){
        lamEN <- sigmahat/tauR
        if(alpha>0.2){
          ub2 <- 10^6*sigmahat
          lb2 <- sqrt(10^6*sigmahat/8)
          temp <- try(uniroot(varFunc,c(0.9*lb2,1.1*ub2),alpha=alpha,tauR=10^6*sigmahat,tol=10^-6))
          if(class(temp)[1]!="try-error"){
            if(temp$root<lamEN) lamEN <- sigmahat/temp$root
          }
        }
      }else if(tauR/sigmahat<10^-6){
        lamEN <- sigmahat*sqrt(8/tauR)
        if(alpha<0.8){
          ub2 <- sqrt(10^-7*sigmahat/8)
          lb2 <- 10^-7*sigmahat
          temp <- try(uniroot(varFunc,c(0.9*lb2,1.1*ub2),alpha=alpha,tauR=10^-7*sigmahat,tol=10^-6))
          if(class(temp)[1]!="try-error"){
            if(temp$root>lamEN) lamEN <- temp$root
          }
        }
      }else{
        lb <- min(tauR,sqrt(tauR/8))
        ub <- max(tauR,sqrt(tauR/8))
        #if(sign(varFunc(0.9*lb,alpha=alpha,tauR=tauR))==sign(varFunc(1.1*ub,alpha=alpha,tauR=tauR))) browser()
        temp <- try(uniroot(varFunc,c(0.9*lb,1.1*ub),alpha=alpha,tauR=tauR,tol=10^-6))
        if(class(temp)[1]=="try-error"){
          if(tauR<0.5) lamEN <- sigmahat*sqrt(8/tauR)
          if(tauR>0.5) lamEN <- sigmahat/tauR
        }else{
          lamEN <- sigmahat/temp$root
        }
      }
      return(lamEN)
    }
    alphat <- 1/(1+2*sd_y*(1-alpha)/alpha) #alpha used in glmnet
    lambdasEN <- sapply(sigmahat/lambdas,function(tau) lamEN(tauR = tau,alpha=alpha))
    tauEN <- sigmahat/lambdasEN
    lambdasENhat <- lambdasEN/2*(alpha/sd_y+ 2*(1-alpha))
    
    uniqueTaus <- unique(as.vector(c(sigmahat/lambdas)%*%Zt))
    if(dim(X)[2]==dim(Xxtnd)[2]){
      lambdap<- (as.vector(lambdasENhat%*%Zt))
      lambdapApprox<- (as.vector(lambdasEN%*%Zt))
    }else{ #some groups are overlapping
      lambdasENunique <- sapply(uniqueTaus,function(tau) lamEN(tauR = tau,alpha=alpha))
      lambdasENuniquehat <- lambdasENunique/2*(alpha/sd_y+ 2*(1-alpha))
      indUnique <- sapply(as.vector(c(sigmahat/lambdas)%*%Zt),function(x)which(uniqueTaus==x))
      lambdap <- lambdasENuniquehat[indUnique]
      lambdapApprox <- lambdasENunique[indUnique]
    }
    
    if(alpha==0){
      beta <- betaMR
      a0 <- a0MR
      lambdaMR_reCV <- lambdas
    }else{
      penfctr2 <- penfctr
      penfctr2[pen] <- lambdap[pen] 
      not0 <- which(penfctr2!=Inf)
      lambdaEN <- sum(penfctr2[not0])/length(penfctr2[not0])
      penfctr2 <- penfctr2/lambdaEN
      if(model=="cox"){
        glmGR <- glmnet(X[,not0],Y,alpha=alphat,family=fml,standardize = FALSE,
                        penalty.factor=penfctr2[not0], thresh=10^-10)
      }else{
        glmGR <- glmnet(X[,not0],Y,alpha=alphat,family=fml,
                        intercept = intrcpt, standardize = FALSE,
                        penalty.factor=penfctr2[not0], thresh=10^-10)
      }
      
      if(reCV){
        glmGR.cv <- cv.glmnet(X[,not0],Y,alpha=alphat,family=fml,
                              intercept = intrcpt, standardize = FALSE,
                              penalty.factor=penfctr2[not0], thresh=10^-10)
        sopt <- glmGR.cv$lambda.min
        
        #if 0 model returned, first try to rerun CV
        tempItr <- 1
        while(glmGR.cv$lambda.min==glmGR.cv$lambda[1] & tempItr<=10){
          glmGR.cv <- cv.glmnet(X[,not0],Y,alpha=alphat,family=fml,
                                intercept = intrcpt, standardize = FALSE,
                                penalty.factor=penfctr2[not0], thresh=10^-10)
          sopt <- glmGR.cv$lambda.min
          tempItr <- tempItr+1
        }
        # #if still 0 model returned (for first lambda in the sequence as run by glmnet), 
        # #take smaller lambda with cvm within 1 se
        # if(glmGR.cv$lambda.min==glmGR.cv$lambda[1]){
        #   warning("Recalibrated global lambda returns intercept-only model. 
        #           Model for smaller global lambda is returned")
        #   #se <- sd(glmGR.cv$cvm) 
        #   se <- abs(glmGR.cv$cvm[1]-rev(glmGR.cv$cvm)[1])/100
        #   sopt <- min(glmGR.cv$lambda[glmGR.cv$cvm <= (glmGR.cv$lambda + se)])
        # },
        
        lambdasEN_reCV <- sopt/lambdaEN*n/sd_y * lambdasEN
        lambdaMR_reCV <- sigmahat/sapply(sigmahat/lambdasEN_reCV,varFunc,alpha=alpha,tauR=0)
        
      } else{
        sopt <- lambdaEN/n*sd_y
        lambdaMR_reCV <- lambdas
      } 
      
      temp <- coef(glmGR,s=sopt,exact=TRUE,x=X[,not0],y=Y,alpha=alphat,
                   penalty.factor=penfctr2[not0],family=fml,intercept=intrcpt) 
      beta <- rep(0,p); beta[not0] <- temp[-1]
      a0 <- temp[1]
      #plot(beta)
      #print(lambdaEN/n*sd_y)
    }
  }else{stop("alpha should be between 0 and 1")}
  
  if(standardise_Y){
    beta <- beta*sd_y_former
    a0 <- a0*sd_y_former
    if(compareMR){
      betaMR <- betaMR*sd_y_former
      a0MR <- a0MR*sd_y_former
    }
  }  
  
  #Compute predictions for independent data if given----
  if(!is.null(X2)){
    #Ypredridge <- predict(glmR,newx=X2)
    #browser()
    
    if(compareMR){
      if(model=="linear"){
        X2c <- cbind(X2,rep(1,n2))
        YpredMR <- X2c %*% c(betaMR,a0MR)
        MSEMR <- sum((YpredMR-Y2)^2)/n2
      } 
      if(model=='logistic'){
        X2c <- cbind(X2,rep(1,n2))
        YpredMR <- 1/(1+exp(-X2c %*% c(betaMR,a0MR)))
        MSEMR <- sum((YpredMR-Y2)^2)/n2
      }else if(model=="cox"){
        expXb<-exp(X %*% c(betaMR))
        h0 <- sapply(1:length(Y[,1]),function(i){Y[i,2]/sum(expXb[Y[,1]>=Y[i,1]])})#updated baseline hazard in censored times for left out samples
        H0 <- sapply(Y2[,1],function(Ti){sum(h0[Y[,1]<=Ti])})
        YpredMR <- H0*exp(X2 %*% betaMR)
        MSEMR<- sum((YpredMR-Y2[,2])^2)/n2
      }
    }
    
    #Compute test error for elastic net/lasso estimate
    if(model=="linear"){
      X2c <- cbind(X2,rep(1,n2))
      YpredApprox <- X2c %*% c(beta,a0)
      MSEApprox <- sum((YpredApprox-Y2)^2)/n2
    } 
    if(model=='logistic'){
      X2c <- cbind(X2,rep(1,n2))
      YpredApprox <- 1/(1+exp(-X2c %*% c(beta,a0)))
      MSEApprox <- sum((YpredApprox-Y2)^2)/n2
    }else if(model=="cox"){
      expXb<-exp(X %*% c(beta))
      h0 <- sapply(1:length(Y[,1]),function(i){Y[i,2]/sum(expXb[Y[,1]>=Y[i,1]])})#updated baseline hazard in censored times for left out samples
      H0 <- sapply(Y2[,1],function(Ti){sum(h0[Y[,1]<=Ti])})
      YpredApprox <- H0*exp(X2 %*% beta)
      MSEApprox<- sum((YpredApprox-Y2[,2])^2)/n2
    }
    
  }
  
  #Return output----
  output <- list(betaApprox = beta, #lasso
                 a0Approx = a0,
                 #tauApprox = tauEN, #prior parameter estimates
                 lambdaApprox = lambdasEN, #elastic net group penalty
                 lambdapApprox = lambdapApprox, #p-dimensional vector with EN penalties
                 tauMR = sigmahat/lambdas, #prior parameter estimates
                 lambdaMR = lambdas,
                 lambdaglobal = lambda,
                 sigmahat = sigmahat,
                 MLinit=MLinit,
                 MLfinal=MLfinal,
                 alpha=alpha,#elastic net parameter alpha value used
                 glmnet.fit = glmGR, #glmnet fit for the coefficients 
                 lambdaMR_reCV=lambdaMR_reCV) 
                 #betaMR2 = betaGLM, #multiridge penalty with glmnet
                 #a0MR2 = a0GLM)
  if(compareMR){
    output$betaMR <- betaMR #multiridge package
    output$a0MR <- a0MR
  }
  if(!is.null(X2)){
    output$YpredApprox<-YpredApprox #predictions for test set
    output$MSEApprox <- MSEApprox #MSE on test set
    if(compareMR){
      output$YpredMR <-YpredMR #predictions for test set
      output$MSEMR <- MSEMR #MSE on test set
    }
  }
  if(selectAIC){
    output$AICmodels <- list(
      "multigroup"=list(
        "lambdas"=lambdas,
        "sigmahat"=sigmahat,
        "AIC"=AICmultigroup
      ),
      "onegroup"=list(
        "lambdas"=lambda1group,
        "sigmahat"=sigmahat1group,
        "AIC"=AIC1group
      )
    )
    if(modelbestAIC!="multigroup"){
      output$AICmodels$multigroup$lambdas <- lambdasNotOptimalAIC
      output$AICmodels$multigroup$sigmahat <- sigmahatNotOptimalAIC
    }
    if(resultsAICboth){
        output$AICmodels$onegroup$fit <- squeezy(Y,X,groupset,alpha=alpha,model=model,
                X2=X2,Y2=Y2,unpen=unpen,intrcpt=intrcpt,
                method="MML",fold=fold,
                compareMR = compareMR,selectAIC=FALSE,
                fit.ecpc=NULL,
                lambdas=rep(lambda1group,G),lambdaglobal=lambda1group,
                sigmasq=sigmahat1group,
                standardise_Y=standardise_Y,reCV=reCV,resultsAICboth=FALSE)
        output$AICmodels$multigroup$fit <- squeezy(Y,X,groupset,alpha=alpha,model=model,
                       X2=X2,Y2=Y2,unpen=unpen,intrcpt=intrcpt,
                       method="MML",fold=fold,
                       compareMR = compareMR,selectAIC=FALSE,
                       fit.ecpc=NULL,
                       lambdas=output$AICmodels$multigroup$lambdas,
                       lambdaglobal=lambda1group,
                       sigmasq=output$AICmodels$multigroup$sigmahat,
                       standardise_Y=standardise_Y,reCV=reCV,resultsAICboth=FALSE)
    }
    
    output$modelbestAIC <- modelbestAIC
  }
  
  return(output)
}

#Other functions----
#Minus log marginal likelihood of the Laplace approximation for ridge penalised GLMs----
minML.LA.ridgeGLM<- function(loglambdas,XXblocks,Y,sigmasq=1,
                             Xunpen=NULL,intrcpt=TRUE,model,minlam=0,opt.sigma=FALSE){
  #compute Laplace approximation (LA) of the minus log marginal likelihood of ridge penalised GLMs
  #(NOTE: for now only implemented for linear and logistic)
  #Input:
  #loglambdas: logarithm of group penalties
  #XXblocks: nxn matrices for each group
  #Y: response vector
  #sigmasq: noise level in linear regression (1 for logistic)
  #Xunpen: unpenalised variables
  #intercept: TRUE/FALSE to include intercept
  #model: "linear" or "logistic" 
  #opt.sigma: (linear case) TRUE/FALSE if log(sigma^2) is given as first argument of 
  #           loglambdas for optimisation purposes
  if(model!="linear"){
    opt.sigma <- FALSE
    sigmasq <- 1
  } 
  
  n<-length(Y)
  lambdas <- exp(loglambdas)+minlam
  if(opt.sigma){
    sigmasq <- lambdas[1]
    lambdas <- lambdas[-1]
  }
  Lam0inv <- .SigmaBlocks(XXblocks,lambdas) 
  if(!is.null(Xunpen)){
    fit <- IWLSridge(XXT=Lam0inv,Y=Y, model=model,intercept=intrcpt,
                     X1=Xunpen,maxItr = 500,eps=10^-12) #Fit. fit$etas contains the n linear predictors
  }else{
    fit <- IWLSridge(XXT=Lam0inv,Y=Y, model=model,intercept=intrcpt,
                     maxItr = 500,eps=10^-12) #Fit. fit$etas contains the n linear predictors
  }
  eta <- fit$etas + fit$eta0
  
  if(model=="linear"){
    mu <- eta
    W <- diag(rep(1,n))
    Hpen <- Lam0inv - Lam0inv %*% solve(solve(W)+Lam0inv, Lam0inv)
    
    t1 <- dmvnorm(c(Y),mean=eta,sigma=sigmasq*diag(rep(1,n)),log=TRUE) #log likelihood in linear predictor
  }else{
    mu <- 1/(1+exp(-eta))
    W <- diag(c(mu)*c(1-mu))
    
    #Hpen <- Lam0inv - Lam0inv %*% solve(diag(1/diag(W))+Lam0inv, Lam0inv)
    Hpen <- Lam0inv - Lam0inv %*% solve(diag(1,n)+W%*%Lam0inv, W%*%Lam0inv)
    
    t1 <- sum(Y*log(mu)+(1-Y)*log(1-mu)) #log likelihood in linear predictor
  }
  
  t2 <- -1/2/sigmasq*sum(c(Y-mu)*eta)
  t3 <- 1/2 * .logdet(diag(rep(1,n))-W%*%Hpen)
  #return(-t2)
  return(-(t1+t2+t3))
}

#Derivative of minus log marginal likelihood of the Laplace approximation for ridge penalised GLMs----
dminML.LA.ridgeGLM<- function(loglambdas,XXblocks,Y,sigmasq=1,
                              Xunpen=NULL,intrcpt=TRUE,model,minlam=0,opt.sigma=FALSE){
  #compute Laplace approximation (LA) of the first derivative of the minus log 
  #  marginal likelihood of ridge penalised GLMs
  #(NOTE: for now only implemented for linear and logistic)
  #Input:
  #loglambdas: logarithm of group penalties 
  #XXblocks: nxn matrices for each group
  #Y: response vector
  #sigmasq: noise level in linear regression (1 for logistic)
  #Xunpen: unpenalised variables
  #intercept: TRUE/FALSE to include intercept
  #model: "linear" or "logistic" 
  #minlam: minimum of lambda that is added to the value given (useful in optimisation settings)
  #opt.sigma: (linear case) TRUE/FALSE if derivative to log(sigma^2) should be given for optimisation purposes
  #           Note that sigma^2 should then be given as the first argument of loglambdas     
  
  if(model!="linear") opt.sigma <- FALSE
  
  n<-length(Y)
  lambdas <- exp(loglambdas)+minlam
  if(opt.sigma){
    sigmasq <- lambdas[1]
    lambdas <- lambdas[-1]
  }
  rhos<-log(lambdas)
  Lam0inv <- .SigmaBlocks(XXblocks,lambdas) 
  if(!is.null(Xunpen)){
    fit <- IWLSridge(XXT=Lam0inv,Y=Y, model=model,intercept=intrcpt,
                     X1=Xunpen,maxItr = 500,eps=10^-12) #Fit. fit$etas contains the n linear predictors
  }else{
    fit <- IWLSridge(XXT=Lam0inv,Y=Y, model=model,intercept=intrcpt,
                     maxItr = 500,eps=10^-12) #Fit. fit$etas contains the n linear predictors
  }
  eta <- fit$etas + fit$eta0
  
  if(intrcpt) Xunpen <- cbind(Xunpen,rep(1,n))
  #eta0 <- fit$eta0 #TD: add unpenalised variables/intercept
  
  #GLM-type specific terms
  if(model=="linear"){
    mu <- eta
    W <- diag(rep(1,n))
    dwdeta <- rep(0,n)
  }else if(model=="logistic"){
    mu <- 1/(1+exp(-eta))
    W <- diag(c(mu)*c(1-mu))
    dwdeta <- c(mu*(1-mu)*(1-2*mu))
  }else{
    stop(print(paste("Only model type linear and logistic supported")))
  }
  
  #Hpen <- Lam0inv - Lam0inv %*% solve(solve(W)+Lam0inv, Lam0inv)
  Hpen <- Lam0inv - Lam0inv %*% solve(diag(1,n)+W%*%Lam0inv, W%*%Lam0inv)
  
  if(!is.null(Xunpen)){
    WP1W <- diag(rep(1,n)) - Xunpen%*%solve(t(Xunpen)%*%W%*%Xunpen,t(Xunpen)%*%W)
  }else{
    WP1W <- diag(rep(1,n))
  }
  #L2 <- WP1W%*%(diag(rep(1,n))-t(solve(solve(W)+WP1W%*%Lam0inv,WP1W%*%Lam0inv)))
  L2 <- WP1W%*%(diag(rep(1,n))-t(solve(diag(1,n)+W%*%WP1W%*%Lam0inv,W%*%WP1W%*%Lam0inv)))
  Hj <- lapply(1:length(XXblocks),function(i){
    L2%*%XXblocks[[i]]/lambdas[i]
  })
  
  if(is.null(Xunpen)) {Hres <- .Hpen(diag(W),Lam0inv);Hmat <- Hres$Hmat} else {
    Hres <- .Hunpen(diag(W),Lam0inv,Xunpen); Hmat <- Hres$Hmat
  }
  # detadrho <- lapply(1:length(rhos),function(j){
  #   -(diag(rep(1,n))-L2%*%Lam0inv%*%W)%*%t(Hj[[j]])%*%c(Y-mu+W%*%eta)})
  detadrho <- lapply(1:length(rhos),function(j){
    -exp(rhos[j])*(diag(rep(1,n))-Hmat%*%W)%*%t(Hj[[j]]/lambdas[j])%*%c(Y-mu+W%*%eta)})
  dWdrho <- lapply(1:length(rhos),function(j){dwdeta*c(detadrho[[j]])})
  
  t1 <- sapply(1:length(rhos),function(j){-1/sigmasq*c((Y-mu))%*%detadrho[[j]]})
  t2 <- sapply(1:length(rhos),function(j){1/2/sigmasq*c(-W%*%eta+Y-mu)%*%detadrho[[j]]})
  #t3 <- sapply(1:length(rhos),function(j){-0.5*sum(diag(solve(solve(W)+Lam0inv,XXblocks[[j]]/sigmasq/lambdas[j])))})
  t3 <- sapply(1:length(rhos),function(j){-0.5*sum(diag(solve(diag(1,n)+W%*%Lam0inv,W%*%XXblocks[[j]]/lambdas[j])))})
  t4 <- sapply(1:length(rhos),function(j){0.5*sum(diag(Hpen)*dWdrho[[j]])})
  
  #return(t2)
  if(!opt.sigma) return((t1+t2+t3+t4)) #derivative to rho
  
  #for linear model, may want to return derivative to log(sigma^2) as well
  # browser()
  # invMat <- solve(diag(rep(sigmasq,n))+Lam0inv*sigmasq)
  # ts.1 <- -0.5*sum(diag(invMat))
  # ts.2 <- -0.5*t(Y-mu)%*%invMat%*%invMat%*%(Y-mu)
  
  ts.1 <- n/2/sigmasq
  ts.2 <- -1/2/sigmasq^2*t(Y-eta)%*%Y
  
  return(c(c(ts.1+ts.2)*sigmasq,(t1+t2+t3+t4))) #derivative to rho
}

#Marginal AIC for ridge penalised GLMs----
mAIC.LA.ridgeGLM<- function(loglambdas,XXblocks,Y,sigmasq=1,
                            Xunpen=NULL,intrcpt=TRUE,model,minlam=0){
  
  minLL <- minML.LA.ridgeGLM(loglambdas,XXblocks,Y,sigmasq=sigmasq,
                             Xunpen=Xunpen,intrcpt=intrcpt,model=model,minlam=minlam)
  k <- length(XXblocks) + intrcpt #number of parameters
  if(model=="linear") k <- k+1 #sigma estimated in linear model as well
  if(is.matrix(Xunpen)) k <- k+dim(Xunpen)[2]
  if(is.vector(Xunpen)) k <- k + 1
  
  mAIC <- 2*minLL + 2*k
  return(mAIC)
}

#Normality check----
normalityCheckQQ <- function(X,groupset,fit.squeezy,nSim=500){
  lambdapApprox <- fit.squeezy$lambdapApprox
  alpha <- fit.squeezy$alpha
  sigmahat <- fit.squeezy$sigmahat
  tauMR <- fit.squeezy$tauMR
  n <- dim(X)[1]
  
  #sample linear predictors according to elastic net prior distribution
  sampleEtas <- function(lambdapApprox=lambdapApprox,alpha=alpha,sigmahat=sigmahat,nSim=nSim){
    #X: data matrix
    #lambdapApprox: p-dimensional vector with elastic net penalties
    #alpha: elastic net mixing parameter
    #sigmahat: (linear regression only) noise variable
    p<-length(lambdapApprox)
    
    if(alpha==0){ 
      #gaussian distribution
      etas <- sapply(1:nSim,function(x){
        #simulate betas
        betas <- rnorm(p, mean=0, sd=sqrt(sigmahat/lambdapApprox))
        
        #compute linear predictor eta
        eta <- X%*%betas
        return(eta)
      })
    }else if(alpha==1){
      #A Laplace(0,b) variate can also be generated as the difference of two i.i.d. 
      #Exponential(1/b) random variables
      etas <- sapply(1:nSim,function(x){
        #simulate betas
        betas <- rexp(p, rate=lambdapApprox/2/sigmahat) - rexp(p, rate=lambdapApprox/2/sigmahat)
        
        #compute linear predictor eta
        eta <- X%*%betas
        return(eta)
      })
    }else if(alpha=="horseshoe"){
      #generate horseshoe prior for comparison
      etas <- sapply(1:nSim,function(x){
        #simulate betas
        tausLatent <- abs(rcauchy(p))
        betas <- rnorm(p,mean=0,sd=tausLatent*sqrt(sigmahat/lambdapApprox))
        
        #compute linear predictor eta
        eta <- X%*%betas
        return(eta)
      })
    }else{
      #bayesian elastic net parameters (given sigmasq)
      lam1 <- alpha*lambdapApprox
      lam2 <- (1-alpha)*lambdapApprox
      
      shape <- 0.5
      scale <- 8*lam2*sigmahat/lam1^2
      a <- 1
      b <- Inf
      
      etas <- sapply(1:nSim,function(x){
        #simulate betas
        tausLatent <- .rtgamma(p,shape = shape, scale= scale, a=a, b=b)
        varsBeta <- sigmahat*(tausLatent-1)/lam2/tausLatent
        varsBeta[tausLatent==Inf] <- sigmahat/lam2[tausLatent==Inf]
        betas <- rnorm(p,mean=0,sd=sqrt(varsBeta)) 
        
        #compute linear predictor eta
        eta <- X%*%betas
        return(eta)
      })
    }
    
    return(etas)
  }
  
  etas <- sampleEtas(lambdapApprox=lambdapApprox,alpha=alpha,sigmahat=sigmahat,nSim=nSim)
  
  #create covariance matrix
  covMat <- function(X=X,groupset=groupset,tauMR=tauMR){
    p <- dim(X)[2]
    groupsets <- list(groupset)
    G <- sapply(groupsets,length) #1xm vector with G_i, number of groups in partition i
    m <- length(G) #number of partitions
    
    indGrpsGlobal <- list(1:G[1]) #global group index in case we have multiple partitions
    if(m>1){
      for(i in 2:m){
        indGrpsGlobal[[i]] <- (sum(G[1:(i-1)])+1):sum(G[1:i])
      }
    }
    Kg <- lapply(groupsets,function(x)(sapply(x,length))) #m-list with G_i vector of group sizes in partition i
    #ind1<-ind
    
    #ind <- (matrix(1,G,1)%*%ind)==(1:G)#sparse matrix with ij element TRUE if jth element in group i, otherwise FALSE
    i<-unlist(sapply(1:sum(G),function(x){rep(x,unlist(Kg)[x])}))
    j<-unlist(unlist(groupsets))
    ind <- sparseMatrix(i,j,x=1) #sparse matrix with ij element 1 if jth element in group i (global index), otherwise 0
    
    Ik <- lapply(1:m,function(i){
      x<-rep(0,sum(G))
      x[(sum(G[1:i-1])+1):sum(G[1:i])]<-1
      as.vector(x%*%ind)}) #list for each partition with px1 vector with number of groups beta_k is in
    #sparse matrix with ij element 1/Ij if beta_j in group i
    
    #make co-data matrix Z (Zt transpose of Z as in paper, with co-data matrices stacked for multiple groupsets)
    Zt<-ind; 
    if(G[1]>1){
      Zt[1:G[1],]<-t(t(ind[1:G[1],])/apply(ind[1:G[1],],2,sum))
    }
    if(m>1){
      for(i in 2:m){
        if(G[i]>1){
          Zt[indGrpsGlobal[[i]],]<-t(t(ind[indGrpsGlobal[[i]],])/
                                       apply(ind[indGrpsGlobal[[i]],],2,sum))
        }
      }
    }
    
    if(dim(Zt)[2]<p) Zt <- cbind(Zt,matrix(rep(NaN,(p-dim(Zt)[2])*sum(G)),c(sum(G),p-dim(Zt)[2])))
    
    
    #Extend data to make artifical non-overlapping groups----
    Xxtnd <- do.call(cbind,lapply(groupsets[[1]],function(group){t(t(X[,group])/sqrt(Ik[[1]][group]))}))
    #create new group indices for Xxtnd
    Kg2 <- c(1,Kg[[1]]) 
    G2 <- length(Kg2)-1
    groupxtnd <- lapply(2:length(Kg2),function(i){sum(Kg2[1:(i-1)]):(sum(Kg2[1:i])-1)}) #list of indices in each group
    groupxtnd2 <- unlist(sapply(1:G2,function(x){rep(x,Kg2[x+1])})) #vector with group number
    
    #datablocks: list with each element a data type containing indices of covariates with that data type 
    Xbl <- createXblocks(lapply(groupxtnd,function(ind) Xxtnd[,ind]))
    XXbl <- createXXblocks(lapply(groupxtnd,function(ind) Xxtnd[,ind]))
    
    covMat <- SigmaFromBlocks(XXblocks=XXbl,penalties=1/tauMR)
    
    return(covMat)
  }
  
  covMatX <- covMat(X,groupset,tauMR)
  
  #Compute Mahalanobis distance
  mahala <- function(eta,covMat) {
    eta <- matrix(eta,nrow=1)
    dist <- try(eta %*% solve(covMat, t(eta)),silent = TRUE)
    if(class(dist)[1]=="try-error"){
      svdcovMat <- svd(covMat)
      svdd <- svdcovMat$d
      #reci <- 1/svdd[1:n]
      reci <- c(1/svdd[1:n-1],0)
      invforcovMat <- svdcovMat$v %*% (reci * t(svdcovMat$u))
      dist <- c(eta%*% invforcovMat %*% t(eta))
    }
    return(dist)
  }
  
  #qq-plot
  dfs <- nSim
  mahaladists <- apply(etas,2,mahala,covMat=covMatX)
  
  if(requireNamespace("ggplot2")){
    Sample <- NULL
    df <- data.frame("Sample"=mahaladists)
    p <- ggplot2::ggplot(df)+ggplot2::aes(sample=Sample)+
      ggplot2::geom_qq(distribution=stats::qchisq, dparams = dfs)+
      ggplot2::geom_qq_line(distribution=stats::qchisq, dparams = dfs,line.p = c(0.25,0.75),col="red")+
      ggplot2::labs(x="Theoretical quantiles",y="Empirical quantiles")+
      ggplot2::theme_bw()
    
    return(p)
  }else{
    qqplot(qchisq(ppoints(500), df = dfs), mahaladists, ylab="Empirical quantiles", xlab="Theoretical quantiles")
    qqline(mahaladists, distribution = function(p) qchisq(p, df = dfs),
           probs = c(0.1, 0.6), col = 2)
    
    return(NULL)
  }
  
}

#Internal functions----
#Log-determinant; needed below----
.logdet <- function(mat) return(determinant(mat,logarithm=TRUE)$modulus[1])

.SigmaBlocks <- function(XXblocks,lambdas){ #computes X Laminv X^T from block cross-products
  nblocks <- length(XXblocks)
  if(nblocks != length(lambdas)){
    print("Error: Number of penalty parameters should equal number of blocks")
    return(NULL)
  } else {
    Sigma<-Reduce('+', lapply(1:nblocks,function(i) XXblocks[[i]] * 1/lambdas[i]))
    return(Sigma)
  }
}





#Computation of weighted Hat matrix, penalized variables only; needed in IWLS functions----
.Hpen <- function(WV,XXT){ #WV:weigths as vector (n); XXT: (penalized) sample cross-product (nxn)
  n <-length(WV)
  Winv <- diag(1/WV)
  inv <- solve(Winv + XXT)
  Mmat <- diag(n) - inv %*% XXT
  Hmat <- XXT %*% Mmat
  return(list(Hmat=Hmat,Mmat=Mmat))
}

#Computation of weighted Hat matrix, possibly with unpenalized variables----
.Hunpen <- function(WV,XXT,X1){
  #Function from multiridge-package written by Mark van de Wiel
  #WV:weigths as vector (n)
  #XXT: sample cross-product (nxn) penalized variables
  #X1 (p1xn) design matrix unpenalized variables
  n <- length(WV)
  WsqrtV <- sqrt(WV)
  WinvsqrtV <- 1/WsqrtV
  X1W <- WsqrtV * X1
  X1aux <- solve(t(X1W) %*% X1W) %*% t(X1W)
  X1Wproj <- X1W %*% X1aux
  P1W <- diag(n) - X1Wproj
  GammaW <- t(t(WsqrtV * XXT) * WsqrtV) #faster
  P1GammaW <- P1W %*% GammaW
  # cons <- mean(abs(P1GammaW))
  # solve(diag(n)/cons+P1GammaW/cons)
  invforH2<-try(solve(diag(n) + P1GammaW))
  if(class(invforH2)[1]=="try-error"){
    svdXXT <- svd(diag(n) + P1GammaW)
    svdd <- svdXXT$d
    #reci <- 1/svdd[1:n]
    reci <- c(1/svdd[1:n-1],0)
    invforH2 <- svdXXT$v %*% (reci * t(svdXXT$u))
  }
  #invforH2 <- solve(diag(n) + P1GammaW)
  #H2 <- Winvsqrt %*% GammaW %*% (diag(n) -invforH2 %*% P1GammaW) %*% P1W %*% Winvsqrt
  MW <- WsqrtV * t(t((diag(n) - invforH2 %*% P1GammaW) %*% P1W) * WinvsqrtV)
  H20 <- GammaW %*% (WinvsqrtV * MW)
  H2 <- t(t(WinvsqrtV * H20)) #faster
  #Hboth <- Winvsqrt %*% X1Wproj %*% (diag(n) - Wsqrt %*% H2 %*% Wsqrt) %*% Winvsqrt + H2
  KW <- t(t(X1aux %*% (diag(n) - t(t(WsqrtV * H2) * WsqrtV))) * WinvsqrtV)
  Hmat <-(WinvsqrtV * X1W) %*% KW  + H2
  return(list(Hmat=Hmat,MW=MW,KW=KW))
}


#Generate from truncated gamma distribution----
#Code from: https://rdrr.io/rforge/TruncatedDistributions/man/tgamma.html
.rtgamma <- function(n, shape, scale = 1, a = 0, b = Inf){
  stopifnot(n > 0 & all(shape > 0) & all(scale > 0))
  output <- rep(Inf,n)
  maxIt <- 50; It <- 1
  while(any(output==Inf) & It <= maxIt){
    x <- runif(n)
    Fa <- pgamma(a, shape, scale = scale)
    Fb <- pgamma(b, shape, scale = scale)
    y <- (1 - x) * Fa + x * Fb
    if(sum(output==Inf)!=length(y)) browser()
    temp <- qgamma(y, shape, scale = scale)
    output[output==Inf] <- temp
    It <- It + 1
    if(length(shape)>1) shape <- shape[temp==Inf]
    if(length(scale)>1) scale <- scale[temp==Inf]
    n <- sum(output==Inf)
  }
  return(output)
}