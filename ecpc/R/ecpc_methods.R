#Methods for ecpc class

print.ecpc <- function(x, ...){
  cat("ecpc fit\n")
  cat("\n")
  cat("Estimated co-data variable weights: \n")
  cat(x$gamma*x$tauglobal," \n")
  cat("\n")
  cat("Estimated co-data weights: \n")
  cat(x$w)
}

summary.ecpc <- function(object, ...){
  cat("Summary estimated prior variances: \n")
  print(summary(object$sigmahat/object$penalties))
  cat("\n")
  cat("Summary estimated regression coefficients: \n")
  print(summary(object$beta))
  cat("\n")
  cat("Estimated intercept: ",object$intercept," \n")
  cat("\n")
  # cat("Summary ridge penalties: \n")
  # print(summary(x$penalties))
  # cat("\n")
  # cat("Co-data variable weights: \n")
  # cat(x$gamma," \n")
  # cat("\n")
  # cat("Co-data weights: \n")
  # cat(x$w)
}

plot.ecpc <- function(x, show=c("coefficients","priorweights"),
                      Z=NULL, values=NULL,
                      groupsets=NULL, codataweights=FALSE, ...){
  show_ggplot <- FALSE
  y<-NULL
  if(requireNamespace("ggplot2")&requireNamespace("ggpubr")) show_ggplot <- TRUE
  if(length(x$tauglobal)>1){
    print("For multiple datablocks of X (multiple tauglobal), only the group 
          contributions are plotted, (i.e. for tauglobal set to 1)")
    x$tauglobal <- 1
  }
  
  if(length(show)==2) show <- "coefficients"
  if(show=="coefficients"){
    if(show_ggplot){
      p1 <- ggplot2::ggplot(data.frame(x=x$sigmahat/x$penalties, y=x$beta^2))+
        ggplot2::aes(x=x,y=y)+
        ggplot2::geom_point()+
        ggplot2::labs(x="Prior variance", y="Squared regression coefficients")+
        ggplot2::geom_abline(intercept=0,slope=1)
      p1
      return(p1)
    }
    par(mfrow=c(1,1))
    #par(mfrow=c(1,3))
    #plot(log(x$penalties), x$beta)
    #lot(x$sigmahat/x$penalties, x$beta)
    plot(x$sigmahat/x$penalties, x$beta^2,
         xlab= "Prior variance", ylab="Squared regression coefficients")
    abline(0,1)
  }else if(show=="priorweights"){
    if(is.null(Z)&&is.null(groupsets)){
      stop("Provide Z or groupsets used to fit ecpc")
    }else if(!is.null(Z)&!is.null(groupsets)){
      stop("Provide either Z or groupsets, not both")
    }else{
      if(!is.null(groupsets)) Z <- lapply(groupsets,createZforGroupset)
      if(is.null(values)){
        print("Prior variance contribution per group and co-data source is plotted.")
        print("Note that for splines, the original continuous co-data values should be provided in values")
      }else{
        print("Prior variance contribution per group/continuous values and co-data source is plotted.")
      }
      if(is.null(names(Z))) names(Z) <- paste("Co-data set",1:length(Z))
      
      df <- data.frame()
      p1 <- list()
      nrows <- floor(sqrt(length(Z)))
      ncols <- ceiling(length(Z)/nrows)
      par(mfrow=c(nrows,ncols))
      for(g in 1:length(Z)){
        ind_g <- attributes(x$gamma)$codataSource==g
        if(is.null(values[[g]])){
          if(is.null(colnames(Z[[g]])) || any(colnames(Z[[g]])=="")){
            if(codataweights){
              if(show_ggplot){
                temp <- data.frame(x=factor(1:dim(Z[[g]])[2]),
                                   y=x$gamma[ind_g]*x$tauglobal* x$w[g],
                                   Codatasource=names(Z)[g])
                #df <- rbind(df,temp)
                p1[[g]] <- ggplot2::ggplot(temp)+ggplot2::aes(x=x,y=y)+
                  ggplot2::geom_point()+
                  ggplot2::labs(x="Co-data variable",y="Prior variance weight", title=names(Z)[g])
              }else{
                plot(factor(1:dim(Z[[g]])[2]), x$gamma[ind_g]*x$tauglobal* x$w[g],
                     main=names(Z)[g], xlab="Co-data variable",
                     ylab="Prior variance weight")
              }
            }else{
              if(show_ggplot){
                temp <- data.frame(x=factor(1:dim(Z[[g]])[2]),
                                   y=x$gamma[ind_g]*x$tauglobal,
                                   Codatasource=names(Z)[g])
                #df <- rbind(df,temp)
                p1[[g]] <- ggplot2::ggplot(temp)+ggplot2::aes(x=x,y=y)+
                  ggplot2::geom_point()+
                  ggplot2::labs(x="Co-data variable",y="Prior variance weight", title=names(Z)[g])
              }else{
                plot(factor(1:dim(Z[[g]])[2]), x$gamma[ind_g]*x$tauglobal,
                     main=names(Z)[g], xlab="Co-data variable",
                     ylab="Prior variance weight")
              }
            }
          }else{
            if(codataweights){
              if(show_ggplot){
                temp <- data.frame(x=factor(colnames(Z[[g]])),
                                   y=x$gamma[ind_g]*x$tauglobal* x$w[g],
                                   Codatasource=names(Z)[g])
                #df <- rbind(df,temp)
                p1[[g]] <- ggplot2::ggplot(temp)+ggplot2::aes(x=x,y=y)+
                  ggplot2::geom_point()+
                  ggplot2::labs(x="Co-data variable",y="Prior variance weight", title=names(Z)[g])
              }else{
                plot(factor(colnames(Z[[g]]), levels=colnames(Z[[g]])), 
                     x$gamma[ind_g] * x$tauglobal * x$w[g],
                     main=names(Z)[g], xlab="Co-data variable",
                     ylab="Prior variance weight")
              }
            }else{
              if(show_ggplot){
                temp <- data.frame(x=factor(colnames(Z[[g]])),
                                   y=x$gamma[ind_g]*x$tauglobal,
                                   Codatasource=names(Z)[g])
                #df <- rbind(df,temp)
                p1[[g]] <- ggplot2::ggplot(temp)+ggplot2::aes(x=x,y=y)+
                  ggplot2::geom_point()+
                  ggplot2::labs(x="Co-data variable",y="Prior variance weight", title=names(Z)[g])
              }else{
                plot(factor(colnames(Z[[g]]), levels=colnames(Z[[g]])), 
                     x$gamma[ind_g] * x$tauglobal,
                     main=names(Z)[g], xlab="Co-data variable",
                     ylab="Prior variance weight")
              }
            }
          }
        }else{
          if(codataweights){
            if(show_ggplot){
              temp <- data.frame(x=values[[g]],
                                 y= Z[[g]]%*%x$gamma[ind_g] * x$tauglobal * x$w[g],
                                 Codatasource=names(Z)[g])
              #df <- rbind(df,temp)
              p1[[g]] <- ggplot2::ggplot(temp)+ggplot2::aes(x=x,y=y)+
                ggplot2::geom_line()+
                ggplot2::labs(x="Continuous co-data variable",y="Prior variance weight", title=names(Z)[g])
            }else{
              plot(values[[g]], Z[[g]]%*%x$gamma[ind_g] * x$tauglobal * x$w[g],
                   main=names(Z)[g], xlab="Continuous co-data variable",
                   ylab="Prior variance weight")
            }
          }else{
            if(show_ggplot){
              temp <- data.frame(x=values[[g]],
                                 y= Z[[g]]%*%x$gamma[ind_g] * x$tauglobal,
                                 Codatasource=names(Z)[g])
              #df <- rbind(df,temp)
              p1[[g]] <- ggplot2::ggplot(temp)+ggplot2::aes(x=x,y=y)+
                ggplot2::geom_line()+
                ggplot2::labs(x="Continuous co-data variable",y="Prior variance weight", title=names(Z)[g])
            }else{
              plot(values[[g]], Z[[g]]%*%x$gamma[ind_g] * x$tauglobal,
                   main=names(Z)[g], xlab="Continuous co-data variable",
                   ylab="Prior variance weight")
            }
          }
        } 
      }
      
      if(show_ggplot){
        #df$Codatasource <- factor(df$Codatasource, levels=unique(df$Codatasource),
        #                          labels=unique(df$Codatasource))
        ylims <- sapply(1:length(Z), function(g){
          ind_g <- attributes(x$gamma)$codataSource==g
          if(is.null(values[[g]])){
            if(codataweights){
              return(range(x$gamma[ind_g] * x$tauglobal *x$w[g]))
            }else{
              return(range(x$gamma[ind_g]* x$tauglobal))
            }
          }else{
            if(codataweights){
              return(range(Z[[g]]%*%x$gamma[ind_g] * x$tauglobal *x$w[g]))
            }else{
              return(range(Z[[g]]%*%x$gamma[ind_g]* x$tauglobal))
            }
          }
          })
        ylims <- range(c(ylims))
        #ylims <- ylims + 0.01*diff(ylims)*c(-1,1)
        for(g in 1:length(Z)){
          p1[[g]] <- p1[[g]]+ggplot2::ylim(ylims)
        }
        p2 <- ggpubr::ggarrange(plotlist=p1, nrow=nrows)
        p2
        return(p2)
      }
    }
  }
}

predict.ecpc <- function(object, X2, X=NULL, Y=NULL, ...){
  beta <- object$beta
  a0 <- object$intercept
  model <- object$model
  
  if(model=="linear"){
    predictions <- X2 %*% beta + a0
  }else if(model=='logistic'){
    predictions <- 1/(1+exp(-X2 %*% beta - a0))
  }else if(model=='cox'){ 
    predictions<-exp(X2 %*% c(beta))
    if(!is.null(X) & !is.null(Y)){
      expXb<-exp(X %*% c(beta))
      h0 <- sapply(1:length(Y[,1]),function(i){Y[i,2]/sum(expXb[Y[,1]>=Y[i,1]])})#updated baseline hazard in censored times for left out samples
      H0 <- sapply(sort(c(Y[,1])),function(Ti){sum(h0[Y[,1]<=Ti])})
      predictions <- outer(c(predictions),c(H0))
    }
  }else{
    warning("This type of model is not supported")
    predictions <- NULL
  }
  return(predictions)
}

coef.ecpc <- function(object,
                      penalties=NULL,X=NULL, Y=NULL,
                      unpen=NULL,intrcpt=TRUE,model=c("linear", "logistic", "cox"),
                      est_beta_method=c("glmnet","multiridge"), ...){
  #Return intercept+ regression coefficients of fitted ecpc-object given in x,
  #or retrieve ridge penalised coefficients for newly given penalties
  if(is.null(penalties)){
    betas <- object[c('intercept','beta')]
  }else{
    lambdap <- penalties
    #Check whether X and Y are provided
    if(is.null(X) | is.null(Y)){
      stop("To refit regression coefficients for given penalties, provide original X and Y")
    }
    #other variables needed for fitting----
    n <- dim(X)[1]
    p <- dim(X)[2]
    muhatp <- rep(0,p)
    if(length(est_beta_method)==2) est_beta_method <- "multiridge"
    if(class(model)[1]=="family"){
      #use glmnet package and cross-validation to compute initial global lambda en beta estimates
      fml <- model
      model <- "family"
      est_beta_method <- "glmnet"
    } 
    if(length(model)>1){
      if(all(is.element(Y,c(0,1))) || is.factor(Y)){
        model <- "logistic" 
      } else if(all(is.numeric(Y)) & !(is.matrix(Y) && dim(Y)[2]>1)){
        model <- "linear"
      }else{
        model <- "cox"
      }
    }
    levelsY<-NaN
    if(model=="logistic"){
      levelsY<-cbind(c(0,1),c(0,1))
      if(!all(is.element(Y,c(0,1)))){
        oldLevelsY<-levels(Y)
        levels(Y)<-c("0","1")
        Y<-as.numeric(Y)-1
        levelsY<-cbind(oldLevelsY,c(0,1))
        colnames(levelsY)<-c("Old level names","New level names")
        print("Y is put in 0/1 format:")
        print(levelsY)
      }
    }
    if(model=='cox'){
      intrcpt <- FALSE
      if(class(Y)[1]!="Surv"){
        Y <- survival::Surv(Y)
      }
    }
    switch(model,
           'linear'={
             Y <- c(Y) #make numeric vector 
             fml <- 'gaussian'
             sd_y <- sqrt(var(Y)*(n-1)/n)[1]
           },
           'logistic'={
             Y <- c(Y)
             fml <- 'binomial'
             sd_y <- 1 #do not standardise y in logistic setting
           },
           'cox'={
             fml <- 'cox'
             sd_y <- 1 #do not standardise y in cox regression setting
           },
           "family"={
             sd_y <- 1
             if(fml$family%in%c("gaussian")) sd_y <- sqrt(var(Y)*(n-1)/n)[1]
           }
    )
    pen <- (1:p)[!(1:p)%in%unpen]
    penfctr <- rep(1,p) #factor=1 for penalised covariates
    if(length(unpen)>0){
      penfctr[unpen] <- 0 #factor=0 for unpenalised covariates
    }
    
    
    
    #Compute beta using glmnet or multiridge----
    if(all(lambdap==Inf)){
      beta <- muhatp
      if(intrcpt){
        if(model=="linear"){
          glmGR <- list(a0=sum(Y-X%*%beta)/n)
          a0 <- sum(Y-X%*%beta)/n
        }else if(model=='logistic'){
          glmGR <- list(a0=sum(Y-exp(X%*%beta)/(1+exp(X%*%beta)))/n) 
          a0 <- sum(Y-exp(X%*%beta)/(1+exp(X%*%beta)))/n
        }else{
          glmGR <- list(a0=NULL)
          a0 <- NULL
        }
      }else{
        glmGR <- list(a0=NULL)
        a0 <- NULL
      }
      warning("All regression coefficients (set to) 0 due to too large penalties")
    }else{
      if(model=="linear"){
        sd_y2 <- sqrt(var(Y-X %*% muhatp)*(n-1)/n)[1]
      }else if(model%in%c('logistic','cox',"family")){
        sd_y2 <- 1 #do not standardize Y-offset for logistic/cox model
        if(model=="family" && fml$family=="gaussian"){
          sd_y2 <- sqrt(var(Y-X %*% muhatp)*(n-1)/n)[1]
        }
      }
      if(any(is.nan(sqrt(lambdap[pen])))){browser()}
      
      if(est_beta_method=="glmnet"){ #glmnet
        lambdaoverall <- exp(mean(log(lambdap[pen][lambdap[pen]<Inf])))
        minlam <- max(mean(lambdap[pen][lambdap[pen]<Inf]) , 10^-5)
        if(lambdaoverall<minlam) lambdaoverall <- minlam
        Xacc <- X
        Xacc[,pen] <- as.matrix(X[,pen] %*% Matrix::sparseMatrix(i=1:length(pen),j=1:length(pen),
                                                                 x=c(1/sqrt(lambdap[pen]/lambdaoverall))))
        
        if(model=="cox"){
          glmGR <- glmnet::glmnet(Xacc,as.matrix(Y),alpha=0,
                                  #lambda = lambdaoverall/n*sd_y2*2,
                                  family=fml,
                                  offset = X[,!((1:p)%in%unpen)] %*% muhatp[!((1:p)%in%unpen)], standardize = FALSE,
                                  penalty.factor=penfctr)
          
          beta <- coef(glmGR, s=lambdaoverall/n*sd_y2*2,thresh = 10^-10, exact=TRUE,
                       x=Xacc,y=as.matrix(Y),
                       family=fml,
                       offset = X[,!((1:p)%in%unpen)] %*% muhatp[!((1:p)%in%unpen)],
                       penalty.factor=penfctr) 
          beta[pen] <- c(1/sqrt(lambdap[pen]/lambdaoverall)) * beta[pen] + muhatp[pen]
          a0 <- NULL
        }else{
          glmGR <- glmnet::glmnet(Xacc,Y,alpha=0,
                                  #lambda = lambdaoverall/n*sd_y2,
                                  family=fml,
                                  offset = X[,!((1:p)%in%unpen)] %*% muhatp[!((1:p)%in%unpen)], intercept = intrcpt, standardize = FALSE,
                                  penalty.factor=penfctr)
          
          beta <- coef(glmGR, s=lambdaoverall/n*sd_y2,thresh = 10^-10, exact=TRUE,
                       x=Xacc, y=Y, family=fml,
                       offset = X[,!((1:p)%in%unpen)] %*% muhatp[!((1:p)%in%unpen)], intercept = intrcpt,
                       penalty.factor=penfctr)[-1]
          beta[pen] <- c(1/sqrt(lambdap[pen]/lambdaoverall)) * beta[pen] + muhatp[pen]
          a0 <- coef(glmGR, s=lambdaoverall/n*sd_y2,thresh = 10^-10, exact=TRUE,
                     x=Xacc, y=Y, family=fml,
                     offset = X[,!((1:p)%in%unpen)] %*% muhatp[!((1:p)%in%unpen)], intercept = intrcpt,
                     penalty.factor=penfctr)[1]
        }
      }else{ #use multiridge package to estimate betas
        lambdaoverall <- exp(mean(log(lambdap[pen][lambdap[pen]<Inf])))
        minlam <- max(mean(lambdap[pen][lambdap[pen]<Inf]) , 10^-5)
        if(lambdaoverall<minlam) lambdaoverall <- minlam
        Xacc <- X
        Xacc[,pen] <- as.matrix(X[,pen] %*% Matrix::sparseMatrix(i=1:length(pen),j=1:length(pen),
                                                                 x=c(1/sqrt(lambdap[pen]/lambdaoverall))))
        XXbl <- list(Xacc%*%t(Xacc))
        #Compute betas
        XXT <- multiridge::SigmaFromBlocks(XXbl,penalties=lambdaoverall) #create nxn Sigma matrix = sum_b [lambda_b)^{-1} X_b %*% t(X_b)]
        if(model!="cox"){
          if(sum((1:p)%in%unpen)>0){
            fit <- multiridge::IWLSridge(XXT,Y=Y, model=model,intercept=intrcpt,X1=X[,(1:p)%in%unpen]) #Fit. fit$etas contains the n linear predictors
          }else{
            fit <- multiridge::IWLSridge(XXT,Y=Y, model=model,intercept=intrcpt) #Fit. fit$etas contains the n linear predictors
          }
        }else{
          if(sum((1:p)%in%unpen)>0){
            fit <- multiridge::IWLSCoxridge(XXT,Y=Y, model=model,X1=X[,(1:p)%in%unpen]) #Fit. fit$etas contains the n linear predictors
          }else{
            fit <- multiridge::IWLSCoxridge(XXT,Y=Y) #Fit. fit$etas contains the n linear predictors
          }
        }
        
        betas <- multiridge::betasout(fit, Xblocks=list(Xacc[,pen]), penalties=lambdaoverall) #Find betas.
        a0 <- c(betas[[1]][1]) #intercept
        beta <- rep(0,p)
        beta[(1:p)%in%unpen] <- betas[[1]][-1] #unpenalised variables
        beta[pen] <- betas[[2]]
        beta[pen] <- c(1/sqrt(lambdap[pen]/lambdaoverall)) * beta[pen]
        rm(betas)
        
      }
    }
    betas <- list('intercept'=a0, 'beta'=beta)
  }
  
  return(betas)
}


#Obtain penalties for given co-data and prior parameters
penalties <- function(object,
                      tauglobal=NULL,sigmahat=NULL,gamma=NULL,gamma0=NULL,w=NULL,
                      Z=NULL, groupsets=NULL,
                      unpen=NULL,
                      datablocks=NULL){
  if(missing(object)) object <- NULL
  if(is.null(tauglobal) & is.null(sigmahat) & is.null(gamma) & is.null(w)){
    penalties <- object$penalties
  }else{
    #checks input----
    if(length(tauglobal)>1 && is.null(datablocks)){
      stop("Provide datablocks for multiple global tau")
    }
    if(!is.null(Z)&!is.null(groupsets)){
      stop("Provide co-data in either Z or groupsets, not both")
    }else if(is.null(Z)&is.null(groupsets)){
      stop("Provide co-data in either Z or groupsets to compute penalties")
    }
    if(is.null(Z)) Z <- lapply(groupsets,createZforGroupset)
    
    #set variables given as in fitted ecpc-object, or to given values----
    if(is.null(tauglobal)) tauglobal <- object$tauglobal
    if(is.null(sigmahat)) sigmahat <- object$sigmahat
    if(is.null(gamma)) gamma <- object$gamma
    if(is.null(gamma0)){
      if(is.null(object)){
        gamma0 <- 0
      }else{
        gamma0 <- object$gamma0
      } 
    } 
    if(is.null(w)) w <- object$w
    
    #other variables needed for fitting----
    G <- sapply(Z,function(x)dim(x)[2])
    p <- dim(Z[[1]])[1]
    
    pen <- (1:p)[!(1:p)%in%unpen]
    datablockNo <- rep(1,p) #in case only one data type
    if(!is.null(datablocks)){
      datablockNo <- c(unlist(lapply(1:length(datablocks),function(x){rep(x,length(datablocks[[x]]))}))) #p-dimensional vector with datablock number
    }else{
      datablocks <- list(1:p)
    }
    datablockNo[(1:p)%in%unpen]<-NA
    codataSource <- unlist(lapply(1:length(Z),function(x){rep(x,dim(Z[[x]])[2])}))
    partWeightsTauG <- w[codataSource]
    m <- length(Z)
    Zt <- Matrix::t(Z[[1]])
    if(m>1){
      for(i in 2:m){
        Zt <- rbind(Zt,Matrix::t(Z[[i]]))
      }
    }
    
    #Compute penalties----
    if(all(gamma==0) & gamma0==0){
      warning("All group weights estimated at 0 and set to 1 to retrieve ordinary ridge performance")
      gamma<-rep(1,sum(G))
    }
    if(all(partWeightsTauG==0)){#set all partition/group weights to 1 (i.e. no difference in partitions/groups)
      lambdap<-sigmahat/(tauglobal[datablockNo]*as.vector(gamma%*%Zt[,pen,drop=FALSE]+gamma0)) #target tau/overall
    }else{
      lambdap<-sigmahat/(tauglobal[datablockNo]*as.vector(c(partWeightsTauG*gamma)%*%Zt[,pen,drop=FALSE]+gamma0)) #specific penalty for beta_k
      lambdap[lambdap<0]<-Inf 
    } 
    
    lambdap[(1:p)%in%unpen] <- 0 #set penalty of unpenalised variables to 0
    penalties <- lambdap
  }
  
  return(penalties)
}

