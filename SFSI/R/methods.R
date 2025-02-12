
#====================================================================
#====================================================================
coef.LASSO <- function(object, ...)
{
  args0 <- list(...)
  if(ifelse(length(args0)==0,FALSE,is.null(names(args0))|any(names(args0)==""))){
    message("Some arguments have no names and were ignored")
  }

  iy <- ilambda <- nsup <-  NULL
  if(length(dim(object$nsup)) == 2L){
    nsup0 <- colMeans(object$nsup)
  }else{
    nlambda <- unique(unlist(lapply(object$nsup,length)))
    if(length(nlambda)==1L){
      if(object$q == 1L){
        nsup0 <- object$nsup
      }else{
        nsup0 <- colMeans(do.call(rbind, object$nsup))
      }
    }else{ # This is the case of a LAR-LASSO
      nsup0 <- object$nsup
    }
  }

  if("iy" %in% names(args0)){
    iy <- args0$iy
  }
  if(is.null(iy)){
    iy <- seq(object$q)
  }
  if( any(!iy %in% seq(object$q)) ){
    stop("All elements in 'iy' should be between 1 and ",object$q)
  }

  if("ilambda" %in% names(args0)){
    if(length(args0$ilambda)>1L){
      message("Only the first element of 'ilambda' is considered")
    }
    ilambda <- rep(args0$ilambda[1],length(iy))
    if( ilambda[1] < 1L | ilambda[1] > min(object$nlambda) ){
      stop("Input 'ilambda' should be between 1 and ",min(object$nlambda))
    }
  }

  if("nsup" %in% names(args0)){
    if(is.null(ilambda)){
      if(length(args0$nsup)>1L){
        message("Only the first element of 'nsup' is considered")
      }
      nsup <- args0$nsup[1]
      if( nsup < 1L | nsup > object$p ){
        stop("Input 'nsup' should be between 1 and ", object$p)
      }
      if(is.list(nsup0)){ # LAR-LASSO case
        ilambda <- lapply(nsup0,function(x)which.min(abs(x-nsup)))
        message("A different column was selected for each 'iy' for the case of a LAR-LASSO")
      }else{
        ilambda <- rep(which.min(abs(nsup0-nsup)),length(iy))
      }
    }else{
      message("Input 'nsup' is ignored when 'ilambda' is provided")
    }
  }

  #message("'nsup' is NULL: ",is.null(nsup))
  #message("'ilambda' is NULL: ",is.null(ilambda))
  #message("'ilambda' length: ",length(ilambda))
  #message("ilambda=: ",ilambda)

  if(is.null(object$file_beta)){
    if(is.null(object$beta)){
      stop("No regression coefficients were found for the input object")
    }
    if(object$q == 1L){
      BETA <- object$beta
      if(!is.null(ilambda)){
        BETA <- BETA[,ilambda, drop=FALSE]
      }
    }else{
      BETA <- object$beta[iy]
      if(!is.null(ilambda)){
        BETA <- lapply(1:length(BETA), function(k) BETA[[k]][,ilambda[k], drop=FALSE])
      }
    }

  }else{
    BETA <- vector("list",length(iy))
    for(k in 1:length(iy)){
      tmp <- paste0("i_",object$fileID[iy[k]],".bin")
      indexcol <- NULL
      if(!is.null(ilambda)){
        indexcol <- ilambda[k]
      }
      BETA[[k]] <- readBinary(gsub("i_\\*.bin$",tmp,object$file_beta),
                              cols=indexcol, verbose=FALSE)
    }
  }

  if(is.list(BETA)){
    if(length(BETA) == 1L){
      BETA <- BETA[[1]]
    }else{
      if(!is.null(ilambda)){
        BETA <- t(do.call(cbind,BETA))
      }
    }
  }

  BETA
}

#====================================================================
#====================================================================
predict.LASSO <- function(object, ...)
{
  args0 <- list(...)

  if(!'X' %in% names(args0)){
    stop("A matrix 'X' of ",object$p," predictors in columns should be provided")
  }

  if(length(dim(args0$X)) == 2L){
    X <- as.matrix(args0$X)
  }else{
    X <- matrix(args0$X, nrow=1L)
  }
  stopifnot(ncol(X) == object$p)

  yHat <- lapply(seq(object$q),function(i){
    tmp <- X%*%coef.LASSO(object, iy=i)
    colnames(tmp) <- paste0("yHat",1:ncol(tmp))
    tmp
  })
  if(nrow(X)==1L | object$q==1L){
    yHat <- do.call(rbind, yHat)
  }

  return(yHat)
}

#====================================================================
#====================================================================

coef.SGP <- function(object, ...){
  coef.LASSO(object, ...)
}

#====================================================================
#====================================================================
predict.SGP <- function(object, ...)
{
  args0 <- list(...)
  if(ifelse(length(args0)==0,FALSE,is.null(names(args0))|any(names(args0)==""))){
    message("Some arguments have no names and were ignored")
  }

  if(!inherits(object, "SGP")){
     stop("The input object is not of the class 'SGP'")
  }
  if("SGP.CV" %in% attr(object,"type")){
     stop("'predict' method cannot be applied after cross-validation")
  }

  if(("y" %in% names(args0)) | ("yTRN" %in% names(args0))){
    if(all(c("y","yTRN") %in% names(args0))){
      message("Both vectors 'y' and 'yTRN' are passed. Training data is taken from 'yTRN'")
    }
    if("yTRN" %in% names(args0)){
      if(length(args0$yTRN) != object$nTRN){
        stop("A vector 'yTRN' of length nTRN = ",object$nTRN," should be provided")
      }
      yTRN <- as.vector(args0$yTRN)
    }else{
      if(length(args0$y) != object$n){
        stop("A vector 'y' of length n = ",object$n," should be provided")
      }
      yTRN <- as.vector(args0$y)[object$trn]
    }

    if(!is.null(object$Xb)){
      yTRN <- yTRN - as.vector(object$Xb)[object$trn]
    }

    u <- predict.LASSO(object, X=yTRN)
    dimnames(u) <- list(object$tst, paste0("SSI.",1:ncol(u)))

    if(is.null(object$Xb)){
      yHat <- u[]
    }else{
      yHat <- sweep(u, 1L, object$Xb[object$tst], FUN="+")
    }

  }else{
    yTRN <- object$y[object$trn]
    yHat <- object$yHat
  }

  if(ifelse(is.null(yTRN),TRUE,any(is.na(yTRN)))){
    stop("A vector 'y' of length n = ",object$n," or a vector 'yTRN' of length",
         " nTRN = ",object$nTRN,"\n  with non-NA training entries should be provided")
  }

  return(yHat)
}

#====================================================================
#====================================================================
summary.SGP <- function(object, ...)
{
  args0 <- list(...)
  if(ifelse(length(args0)==0,FALSE,is.null(names(args0))|any(names(args0)==""))){
    message("Some arguments have no names and were ignored")
  }

  if(!inherits(object, "SGP")){
    stop("The input object is not of the class 'SGP'")
  }
  if("summary" %in% attr(object,"type")){
    stop("'summary' method was already applied to the input object")
  }

  map <- map_trn <- map_tst <- nsup_trait <- NULL

  nTST <- c("overall"=length(object$tst))
  nTRN <- c("overall"=length(object$trn))

  verbose <- ifelse("verbose" %in% names(args0), args0$verbose, TRUE)
  simplify <- ifelse("simplify" %in% names(args0), args0$simplify, FALSE)
  if("save.at" %in% names(args0)){
    save.at <- args0$save.at
  }else{
    save.at <- NULL
  }
  flag_accuracy <- TRUE

  if(attr(object,"type") == "SGP.CV")
  {
    if("y" %in% names(args0) & verbose){
      message("Response vector 'y' is ignored in CV")
    }
    fold_size <- do.call(rbind,lapply(object$CV,function(x)x$nTST))
    nfolds_CV <- length(object$CV)
    stopifnot(nfolds_CV <= object$nfolds_CV)
    if(nfolds_CV == object$nfolds_CV){
       tag <- ifelse(is.null(object$tag),ifelse(object$isLOOCV,"LOO-CV",
                      paste0(object$nCV,"x",object$nfolds,"F-CV")),object$tag)
    }else{
      if(verbose){
        message("CV results correspond to a subset of ",nfolds_CV," of ",object$nfolds_CV,
                ifelse(object$isLOOCV," singleton","")," fold-CV sets")
      }
      tag <- ifelse(is.null(object$tag),ifelse(object$isLOOCV,"LOO-CV",
                     names(object$CV)),object$tag)
    }

    if(object$isLOOCV){
      nsup_trait <- NULL
      yHat <- do.call(rbind,lapply(object$CV,function(x)x$yHat))
      trn <- as.numeric(rownames(yHat))
      stopifnot(length(trn) <= object$TRN)
      if(length(trn) == object$nTRN){
        stopifnot(all(trn == object$trn))
      }
      yTRN <- as.vector(object$y)[trn]
      tmp <- list(colnames(yHat), "overall")
      nsup <- matrix(colMeans(do.call(rbind,lapply(object$CV,function(x)x$nsup))),
                     ncol=1,dimnames=tmp)
      lambda <- matrix(colMeans(do.call(rbind,lapply(object$CV,function(x)x$lambda))),
                       ncol=1,dimnames=tmp)
      accuracy <- matrix(suppressWarnings(stats::cor(yHat,yTRN)),ncol=1,dimnames=tmp)
      MSE <- matrix(suppressWarnings(apply(sweep(yHat,1L,yTRN,FUN="-")^2,2,mean)),
                    ncol=1L,dimnames=tmp)

    }else{ # Cross-validation results
      # Average across all folds-CV combinations
      nsup <- Reduce("+",lapply(object$CV,function(x)x$nsup))/nfolds_CV
      lambda <- Reduce("+",lapply(object$CV,function(x)x$lambda))/nfolds_CV
      accuracy <- Reduce("+",lapply(object$CV,function(x)x$accuracy))/nfolds_CV
      MSE <- Reduce("+",lapply(object$CV,function(x)x$MSE))/nfolds_CV

      if(object$ntraits > 1L){
        names_nsup <- paste0("nsup_",1:object$ntraits)
        nsup_trait <- Reduce("+",lapply(object$CV,function(x)x$nsup_trait[,names_nsup]))/nfolds_CV
        tmp <- object$CV[[1]]$nsup_trait[,c("SSI","trait")]
        nsup_trait <- data.frame(tmp, nsup_trait)

        #map <- map_set_old(object$n, object$ntraits, x=object$trn, y=NULL,
        #             xlab="trn", ylab="tst")
        map <- map_set(i=object$ID_geno, j=object$ID_trait, x=object$trn, xlab="trn")
        map_trn <- map[object$trn,]

        tt <- factor(as.character(map_trn$j), levels=seq(object$ntraits))
        nTRN <- c(nTRN, table(tt))
      }
    }

  }else{
    if("y" %in% names(args0)){
      y <- as.vector(args0$y)
      yHat <- predict.SGP(object, y=y)
    }else{
      y <- object$y
      yHat <- predict.SGP(object) # Should be equal to object$yHat
    }
    tst <- as.numeric(rownames(yHat))
    nTST <- c("overall"=nrow(yHat))
    stopifnot(nTST <= object$nTST)
    if(nTST == object$nTST){
      stopifnot(all(tst == object$tst))
    }else{
      if(verbose){
        message("Fitted values correspond to a subset of ",nTST," of ",object$nTST," testing subjects")
      }
    }

    if(any(is.na(y[tst])) & verbose){
       message("Some testing entries in the response vector are NA.\n",
               "Provide a full response vector 'y' to compute accuracy in testing data")
       flag_accuracy <- FALSE
    }

    yTST <- y[tst]
    tmp <- list(colnames(yHat), "overall")
    nsup <- matrix(colMeans(object$nsup),ncol=1,dimnames=tmp)
    lambda <- matrix(colMeans(object$lambda),ncol=1,dimnames=tmp)
    accuracy <- matrix(suppressWarnings(stats::cor(yHat,yTST)),ncol=1,dimnames=tmp)
    MSE <- matrix(suppressWarnings(apply(sweep(yHat,1L,yTST,FUN="-")^2,2,mean)),
                  ncol=1L,dimnames=tmp)

    nsup1 <- lambda1 <- accuracy1 <- MSE1 <- NULL
    if(object$ntraits > 1L){  # Calculate MSE and accuracy within response variable
      map <- map_set(i=object$ID_geno, j=object$ID_trait, x=object$trn, y=object$tst,
                     xlab="trn", ylab="tst")
      map_trn <- map[object$trn,]
      map_tst <- map[tst,]  # map[object$tst,]

      nsup1 <- lambda1 <- accuracy1 <- MSE1 <- matrix(NA,nrow=object$nlambda,ncol=object$ntraits)
      colnames(nsup1) <- colnames(lambda1) <- seq(object$ntraits)
      colnames(accuracy1) <- colnames(MSE1) <- seq(object$ntraits)

      for(j in 1:object$ntraits){
        index <- which(map_tst$j == j)
        map0 <- map_tst[index,]
        nsup1[,j] <- colMeans(object$nsup[index,,drop=F])
        lambda1[,j] <- colMeans(object$lambda[index,,drop=F])

        #yHat0 <- yHat[map0$index_set,,drop=F]
        yHat0 <- yHat[as.character(map0$index),,drop=F]
        accuracy1[,j] <- drop(suppressWarnings(stats::cor(yHat0,y[map0$index])))
        MSE1[,j] <- suppressWarnings(apply(sweep(yHat0,1L,y[map0$index],FUN="-")^2,2,mean))
      }

      # Get nsup_trait: nsup for each trait in tst corresponding to each trait in trn
      nsup_trait <- get_summary_nsup(object, tst=tst, map=map)

      nsup <- cbind(nsup, nsup1)
      lambda <- cbind(lambda, lambda1)
      accuracy <- cbind(accuracy, accuracy1)
      MSE <- cbind(MSE, MSE1)

      tt <- factor(as.character(map_trn$j), levels=seq(object$ntraits))
      nTRN <- c(nTRN, table(tt))
      tt <- factor(as.character(map_tst$j), levels=seq(object$ntraits))
      nTST <- c(nTST, table(tt))
    }

    tag <- ifelse(is.null(object$tag),attr(object,"type"),object$tag)
  }

  out <- list(nTST=nTST, nTRN=nTRN, accuracy=accuracy, MSE=MSE, nsup=nsup,
              nsup_trait=nsup_trait, lambda=lambda)

  if(!flag_accuracy){
    out$accuracy <- NULL
    out$MSE <- NULL
  }

  if(object$ntraits == 1L){
    out$nsup_trait <- NULL
  }

  # Outputting
  if(!simplify){
    # Search for the optimum SGP (across all traits)
    index <- which(colnames(accuracy)=="overall")
    TMP <- data.frame(accuracy=accuracy[,index], MSE=MSE[,index],
                      nsup=nsup[,index], lambda=lambda[,index])

    # Detect maximum accuracy
    index <- which.max(TMP$accuracy)
    if(length(index) == 0L){
      optCOR <- TMP[1, ,drop=FALSE]
      if(nrow(TMP)>1) optCOR[1,] <- NA
    }else{
      optCOR <- TMP[index, ,drop=FALSE]
    }
    out$optCOR <- as.matrix(optCOR)[1,]

    # Detect minimum MSE
    index <- which.min(TMP$MSE)
    if(length(index) == 0L){
      optMSE <- TMP[1, ,drop=FALSE]
      if(nrow(TMP)>1) optMSE[1,] <- NA
    }else{
      optMSE <- TMP[index, ,drop=FALSE]
    }
    out$optMSE <- as.matrix(optMSE)[1,]

    names0 <- c("n","ntraits","trait_names","b","varU","varE","h2","nlambda")
    tt <- object[names(object) %in% names0]
    out <- do.call(c, list(tt,list(tag=tag),out))
  }

  class(out) <- class(object)
  attr(out,"type") <- c(attr(object,"type"), "summary")

  if(is.null(save.at)){
    return(out)
  }else{
    outfile <- normalizePath(paste0(save.at,paste(attr(out,"type"),collapse="."),".RData"),
                             mustWork=FALSE)
    save(out, file=outfile)
    if(verbose){
      message("Results were saved at file: \n   '",outfile,"'")
    }
  }
}

#====================================================================
#====================================================================
# x.stat=label="nsup"; y.stat="accuracy"; nbreaks.x=6
plot.SGP <- function(..., x.stat = c("nsup","lambda"),
                     y.stat = c("accuracy","MSE"),
                     label = x.stat, nbreaks.x = 6)
{
    x.stat <- match.arg(x.stat)
    y.stat <- match.arg(y.stat)
    if(!is.null(label)){
      label <- match.arg(label, choices=c("nsup","lambda"))
    }
    x <- tag <- obj <- lambda <- NULL
    args0 <- list(...)

    object <- list()
    if(length(args0) > 0L){
      for(i in 1:length(args0)){
        if(inherits(args0[[i]], "SGP")) object[[length(object)+1]] <- args0[[i]]
      }
    }
    np <- length(object) # Number of objects to plot
    if(np == 0L){
       stop("No input object of the class 'SGP' was provided")
    }

    trait <- ifelse("trait" %in% names(args0), as.character(args0$trait), "overall")
    xlab <- ifelse(x.stat=="nsup",expression("Support set size ("*n[sup]*")"),
                   expression("-log("*lambda*")"))
    ylab <- capitalize(y.stat)
    lwd <- ifelse("lwd" %in% names(args0), args0$lwd, 0.65)
    if("xlab" %in% names(args0)) xlab <- args0$xlab
    if("ylab" %in% names(args0)) ylab <- args0$ylab

    if("y" %in% names(args0)){
      y <- as.vector(args0$y)
    }else{
      y <- NULL
    }
    for(k in 1:np){
      if(!"summary" %in% attr(object[[k]],"type")){
        if(is.null(object[[k]]$yHat)){
          object[[k]] <- summary.SGP(object[[k]], y=y, verbose=FALSE)
        }else{
          object[[k]] <- summary.SGP(object[[k]], verbose=FALSE)
        }
      }
    }
    object_names <- unlist(lapply(object,function(fm) fm$tag))

    # Treat repeated fm$tag
    index <- table(object_names)[table(object_names)>1L]
    if(length(index) > 0L){
      for(i in seq_along(index)){
        tmp <- which(object_names == names(index[i]))
        for(j in seq_along(tmp)) object_names[[tmp[j]]] <- paste0(object_names[[tmp[j]]],"-",j)
      }
    }

    ntraits <- unlist(lapply(object,function(fm)fm$ntraits))
    if(any(ntraits == 1) & trait != "overall"){
      trait <- "overall"
      message("Single-trait SGP: input 'trait' was changed to 'overall'")
    }

    for(k in 1:length(object)){
      if(object[[k]]$ntraits>1){
        if(is.null(object[[k]]$trait_names)){
          trait_names <- as.character(seq(object[[k]]$ntraits))
        }else{
          trait_names <- object[[k]]$trait_names
        }
        if(!trait %in% c("overall",trait_names)){
          if(!is.null(object[[k]]$trait_names)){
            trait_names <- paste0("'",trait_names,"'")
          }
          stop("Input 'trait' should be one of ",paste(trait_names,collapse=", "))
        }
      }
    }
    nlambda <- unlist(lapply(object,function(fm)fm$nlambda))
    nTRN <- unlist(lapply(object,function(fm)fm$nTRN[trait]))
    nTST <- unlist(lapply(object,function(fm)fm$nTST[trait]))
    isCV <- unlist(lapply(object,function(fm)"SGP.CV" %in% attr(fm,"type")))

    if(length(table(isCV)) > 1L){
       stop("All 'SGP' class objects should be of the same type:\n",
            "  either a trn-tst prediction or a cross-validation")
    }
    isCV <- isCV[1]
    tt <- as.vector(c(nTRN[1],nTST[1]))

    if(length(unique(nTRN))>1L){
      msg <- "Training set size is not same across all 'SGP' class objects"
      if(isCV){
        stop(msg)
      }else{
        message(msg)
      }
      tt[1] <- paste(range(nTRN),collapse="-")
    }
    if(length(unique(nTST))>1L){
       message("Testing set size is not same across all 'SGP' class objects")
       tt[2] <- paste(range(nTST),collapse="-")
    }

    if(isCV){
      main <- bquote(.(ifelse(np==1,object_names[1],"SGP CV"))*" ("*n[TRN]==.(tt[1])*")")
    }else{
      #main <- bquote("SGP ("*n[TRN]*"="*.(tt[1])*", "*n[TST]==.(tt[2])*")")
      main <- parse(text=paste0("'SGP ('*n[TRN]==",tt[1],"*', '*paste(n[TST]==",tt[2],")*')'"))
    }
    if("main" %in% names(args0)) main <- args0$main

    legend.justification <- c(1,ifelse(tolower(y.stat)=="mse",1,0))
    legend.position <- c(0.99,ifelse(tolower(y.stat)=="mse",0.99,0.01))
    if("legend.justification" %in% names(args0)){
      legend.justification <- args0[['legend.justification']]
    }
    if("legend.position" %in% names(args0)){
      legend.position <- args0[['legend.position']]
    }

    theme0 <- mytheme() + ggplot2::theme(legend.justification = legend.justification,
                                         legend.position = legend.position)
    if(np == 1L){
      theme0 <- theme0 + ggplot2::theme(legend.position = "none")
    }

    eps <- .Machine$double.eps^0.5
    dat <- data.frame(matrix(nrow=0,ncol=9))
    for(k in 1:np)
    {
      fm <- object[[k]]

      if(nlambda[k] > 1L & !is.null(fm$accuracy) & !is.null(fm$MSE))
      {
        tt <- reshape2::melt(fm$lambda)
        colnames(tt) <- c("SSI","trait","lambda")
        tt <- tt[as.character(tt$trait) == trait,]
        tt$x <- -log(tt$lambda)

        tmp <- reshape2::melt(fm[[which(tolower(names(fm)) == tolower(y.stat))]])
        tmp <- tmp[as.character(tmp$Var2) == trait,]
        tt$y <- tmp[match(tt$SSI, tmp$Var1),"value"]

        tmp <- reshape2::melt(fm$nsup)
        tmp <- tmp[as.character(tmp$Var2) == trait,]
        tt$nsup <- tmp[match(tt$SSI, tmp$Var1),"value"]

        if(length(which(tt$lambda < eps))>0){
          tmp <- tt$lambda[which(tt$lambda >= eps)]
          tt[which(tt$lambda < eps),'lambda'] <- ifelse(length(tmp)>0,min(tmp),eps)
        }

        if(isCV){
          tt$n <- fm$nTRN[as.character(tt$trait)]
        }else{
          tt$n <- fm$nTST[as.character(tt$trait)]
        }

        tt <- data.frame(object=k,tag=object_names[k],tt,stringsAsFactors=FALSE)
        dat <- rbind(dat,tt)
      }
    }

    dat$tag <- factor(as.character(dat$tag))

    if(any(nlambda == 1L)){
      message("Object(s) ",paste(which(nlambda==1L),collapse=",")," contain a single SGP point.",
              "They are excluded from the plot")
    }

    if(nrow(dat) == 0){
      stop("The plot can not be generated with the provided data")
    }

    dat <- dat[!is.na(dat$y),]   # Remove NA values
    dat$trait <- factor(as.character(dat$trait))

    if("ylim" %in% names(args0)){
       ylim <- args0$ylim
    }else{
       ylim <- c(NA, NA)
    }

    breaksx <- labelsx <- NULL
    if(x.stat == "nsup")
    {
      if("xlim" %in% names(args0)){
         xlim <- args0$xlim
      }else{
         xlim <- c(1,max(dat$nsup, na.rm=TRUE))
      }

      index <- (dat$nsup >= xlim[1]) & (dat$nsup <= xlim[2])
      dat <- dat[index,]

      # Labels and breaks for the nsup axis
      breaks0 <- get_breaks(x=dat$x, y=dat$nsup, nbreaks.x=nbreaks.x, ymin=xlim[1])
      breaksx <- breaks0$breaks.x
      labelsx <- breaks0$breaks.y

    }else{
      if("xlim" %in% names(args0)){
        xlim <- args0$xlim
      }else{
        tmp <- dat[dat$nsup>=1,]
        tmp <- tmp[abs(tmp$nsup-min(tmp$nsup))<1E-8,,drop=F]
        xlim <- c(mean(tmp$x, na.rm=T), max(dat$x))
      }

      dat <- dat[(dat$x >= xlim[1]) & (dat$x <= xlim[2]),]
    }

    dat0 <- do.call(rbind,lapply(split(dat, paste(dat$trait,dat$object)),function(dt){
      dt[ifelse(tolower(y.stat)=="mse",which.min(dt$y),which.max(dt$y)),]
    }))

    pp <- ggplot2::ggplot(dat, ggplot2::aes(x,y,group=object,color=tag)) +
          ggplot2::geom_line(linewidth=lwd) + ggplot2::theme_bw() +
          ggplot2::labs(title=main, x=xlab, y=ylab) + theme0 +
          ggplot2::geom_vline(data=dat0, ggplot2::aes(xintercept=x), linewidth=0.5,
                     linetype="dotted", color="gray50") +
          ggplot2::scale_y_continuous(limits=ylim)

    if(!is.null(label) & (nrow(dat0)==1L)){
      dat0$label <- unlist(lapply(1:nrow(dat0),function(i){
        if(label == "nsup"){
          bquote(n[sup]==.(round(dat0$nsup[i])))
        }else{
          bquote(lambda==.(sprintf('%.6f',dat0$lambda[i])))
        }
      }))

      pp <- pp + ggplot2::geom_text(data=dat0, ggplot2::aes(x=x,y=-Inf,label=label),
                                    angle=90, parse=TRUE, vjust=-0.25, hjust=-0.25,
                                    size=2.6, color="gray20")
    }

    if(x.stat == "nsup"){
       pp <- pp + ggplot2::scale_x_continuous(breaks=breaksx, labels=round(labelsx))
    }else{
       pp <- pp +
        ggplot2::scale_x_continuous(breaks=scales::extended_breaks(n=nbreaks.x),
                                    limits=xlim)
    }

    return(pp)
}
