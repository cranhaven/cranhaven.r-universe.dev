lmestSearch <- function(responsesFormula = NULL, latentFormula = NULL,
                        data, index, k,
                        version = c("categorical", "continuous"),
                        weights = NULL, nrep = 2, tol1 = 10^-5,
                        tol2 = 10^-10, out_se = FALSE, miss.imp = FALSE, seed = NULL, ...){
# function that search for the global maximum of the log-likelihood
# vector of kv to try for
# nrep = number repetitions with random starting values
# version = model to be estimated ("basic" = basic LM model (est_lm_basic function); "LMlatent" = LM model with covariates in the distribution of the latent process (est_lm_cov_latent function); "LMmanifest" = LM model with covariates in the measurement model (est_lm_cov_maifest function))

  if(inherits(data, "lmestData")){
    data <- data$data
  }else if(!is.data.frame(data)){
    data <- as.data.frame(data)
    stop("A data.frame must be provided")
  }

  if(!is.null(seed)) set.seed(seed)

  if(length(index) !=2) stop("id and time must be provided")

  id.el <- names(data) == index[1]
  tv.el <- names(data) == index[2]

  id.which <- which(id.el == TRUE)
  tv.which <- which(tv.el == TRUE)

  if(!any(id.el)) stop("the id column does not exist")

  if(!any(tv.el)) stop("the time column does not exist")

  id <- data[,id.which]
  tv <- data[,tv.which]

  if(is.character(id) | is.factor(id)){
    warning("conversion of id colum in numeric. The id column must be numeric")
    id <- as.numeric(id)
  }
  if(is.character(tv) | is.factor(tv)){
    warning("conversion of time column in numeric. The time column must be numeric")
    tv <- as.numeric(tv)
  }
  data.new <- data[,-c(id.which,tv.which), drop = FALSE]
  Datatype <- version
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

  if(!is.null(latentFormula)){
    temp <- getLatent(data = data.new,latent = latentFormula, responses = responsesFormula)
    Xinitial <- temp$Xinitial
    Xtrans <- temp$Xtrans
  }

  cont <- ifelse(version == "categorical", FALSE, TRUE)
  if(is.null(weights)) weights = rep(1,length(unique(id)))
  tmp <- long2matrices.internal(Y = Y, id = id, time = tv, yv = weights,
                                Xinitial = Xinitial, Xmanifest = Xmanifest, Xtrans = Xtrans,cont = cont)
  version <- tmp$model
  Xinitial <- tmp$Xinitial
  Xmanifest <- tmp$Xmanifest
  Xtrans <- tmp$Xtrans
  Y <- tmp$Y

  if(is.null(weights)){
    freq = tmp$freq
  }else{
    freq = weights
    if(nrow(Y)!=length(weights)) {stop("dimensions mismatch between data and weights")}
  }

  miss = any(is.na(Y))
  if(Datatype == "categorical"){
    if(miss){
      if(version == "LMmanifest") stop("Missing data in the dataset")
      cat("Missing data in the dataset, treated as missing at random\n")
      R = 1*(!is.na(Y))
      Y[is.na(Y)] = 0
    }else{
      R = NULL
    }
  }

  if(min(Y,na.rm=T)>0 & Datatype == "categorical"){
    cat("|------------------- WARNING -------------------|\n")
    cat("|The first response category must be coded as 0 |\n")
    cat("|-----------------------------------------------|\n")
    for(i in 1:dim(Y)[3]) Y[,,i] <- Y[,,i]-min(Y[,,i],na.rm = TRUE)
  }

  if(!is.null(Xmanifest)){
    if(any(is.na(Xmanifest))) stop("missing data in the covariates affecting the measurement model are not allowed")
  }

  if(!is.null(Xinitial)){
    if(any(is.na(Xinitial))) stop("missing data in the covariates affecting the initial probabilities are not allowed")
  }

  if(!is.null(Xtrans)){
    if(any(is.na(Xtrans))) stop("missing data in the covariates affecting the transition probabilities are not allowed")
  }

  miss = any(is.na(Y))
  if(miss & miss.imp){
    Yv = cbind(1,Y)
    pYv = prelim.mix(Yv,1)
    thhat = em.mix(prelim.mix(Yv,1))
    rngseed(1)
    Yv = as.matrix(imp.mix(pYv, da.mix(pYv,thhat,steps=100), Yv)[,-1])
    Y = array(Yv,dim(Y))
    Yimp = Y
    cat("Missing data in the dataset. imp.mix function (mix package) used for imputation.\n")
  }

  data.new <- data[,-c(id.which,tv.which), drop = FALSE]

  kv = k
  out = vector("list",max(kv))
  lkv = Aic = Bic = errv = rep(NA,max(kv))
  
  for(k in kv){
    cat("***************************************************************************\n")
    cat(k,"\n")
    if(version=="LMbasic") out[[k]] = try(lmbasic(S = Y,yv = freq,k = k,start = 0,
                                                  tol = tol1,miss = miss, R = R, ...))
    if(version=="LMlatent") out[[k]] = try(lmcovlatent(S = Y,X1 = Xinitial,X2 = Xtrans,
                                                       yv = freq,start = 0, k = k,tol = tol1, miss = miss, R = R, ...))
    if(version=="LMmanifest") out[[k]] = try(lmcovmanifest(S = as.matrix(Y[,,1]),
                                                           X = Xmanifest, yv = freq, k = k,tol = tol1,start = 0,...))
    if(version=="LMbasiccont")
      out[[k]] = try(lmbasic.cont(Y=Y, k=k, yv=freq, start = 0, tol = tol1, ntry = 0, ...))
    if(version=="LMlatentcont") out[[k]] = try(lmcovlatent.cont(Y = Y,X1 = Xinitial,
                                                                X2 = Xtrans, yv=freq, k = k,start = 0,tol = tol1, ntry = 0, ...))
    if(!inherits(out[[k]],"try-error")){
      errv[[k]] = FALSE
    }else{
      errv[[k]] = TRUE
      if(k>1) out[[k]] = out[[k-1]]
    }
    if(version=="LMbasic") outh = try(lmbasic(S = Y,yv = freq,k = k,start = 0,tol = tol1,
                                              miss = miss, R = R, ...))
    if(version=="LMlatent") outh = try(lmcovlatent(S = Y,X1 = Xinitial,X2 = Xtrans,
                                                     yv = freq,start = 0, k = k,tol = tol1, miss = miss, R = R, ...))
    if(version=="LMmanifest") outh = try(lmcovmanifest(S = as.matrix(Y[,,1]),X = Xmanifest, 
                                                       yv = freq, k = k,tol = tol1,start = 0, ...))
    if(version=="LMbasiccont")
      outh = try(lmbasic.cont(Y = Y, yv=freq, k = k,start = 0, tol = tol1, ntry = 0 , ...))
    if(version=="LMlatentcont") outh = try(lmcovlatent.cont(Y = Y,X1 = Xinitial,
                                                            X2 = Xtrans, yv=freq,
                                                            k = k,start = 0,tol = tol1, 
                                                            ntry = 0, ...))
    lktrace = out[[k]]$lk
    lkv[k] = out[[k]]$lk
    Aic[k] = out[[k]]$aic
    Bic[k] = out[[k]]$bic
    cat("lktrace = ",sort(lktrace),"\n")
    cat("lk = ",lkv,"\n")
    cat("aic = ",Aic,"\n")
    cat("bic = ",Bic,"\n")
    if(k>1){
      if(nrep==0){
        cat("***************************************************************************\n")
        cat(c(k,1),"\n")
        if(version=="LMbasic") outh = try(lmbasic(S = Y,yv = freq,k = k,start = 1, 
                                                  tol = tol1,miss = miss, R = R, ...))
        if(version=="LMlatent") outh = try(lmcovlatent(S = Y,X1 = Xinitial,X2 = Xtrans,
                                                       yv = freq,start = 1, k = k,tol = tol1, miss = miss, R = R, ...))
        if(version=="LMmanifest") outh = try(lmcovmanifest(S = as.matrix(Y[,,1]),X = Xmanifest,
                                                           yv=freq, k = k,tol = tol1,start = 1, ...))
        if(version=="LMbasiccont")
          outh = try(lmbasic.cont(Y = Y, yv=freq, k = k,start = 1, tol = tol1, ntry = 0, ...))
        if(version=="LMlatentcont") outh = try(lmcovlatent.cont(Y = Y,X1 = Xinitial,
                                                                X2 = Xtrans, yv=freq,
                                                                k = k,start = 1,tol = tol1, ntry = 0, ...))
        if(!inherits(outh,"try-error")){
          lktrace = c(lktrace,outh$lk)
          if(outh$lk>out[[k]]$lk) out[[k]] = outh
        }
        lkv[k] = out[[k]]$lk
        Aic[k] = out[[k]]$aic
        Bic[k] = out[[k]]$bic
        cat("lktrace = ",sort(lktrace),"\n")
        cat("lk = ",lkv,"\n")
        cat("aic = ",Aic,"\n")
        cat("bic = ",Bic,"\n")
      }else{
        for(h in 1:(nrep*(k-1))){
          cat("***************************************************************************\n")
          cat(c(k,h),"\n")
          if(version=="LMbasic") outh = try(lmbasic(S = Y,yv = freq,k = k,start = 1, 
                                                    tol = tol1,miss = miss, R = R, ...))
          if(version=="LMlatent") outh = try(  lmcovlatent(S = Y,X1 = Xinitial,X2 = Xtrans,
                                                           yv = freq,start = 1, k = k,tol = tol1, miss = miss, R = R, ...))
          if(version=="LMmanifest") outh = try(lmcovmanifest(S = as.matrix(Y[,,1]),X = Xmanifest, 
                                                               yv = freq, k = k,tol = tol1,start = 1, ...))
          if(version=="LMbasiccont")
            outh = try(lmbasic.cont(Y = Y, yv=freq, k = k, start = 1, tol = tol1, ntry = 0 , ...))
          if(version=="LMlatentcont") outh = try(lmcovlatent.cont(Y = Y,X1 = Xinitial,
                                                                    yv=freq, X2 = Xtrans,k = k,start = 1,tol = tol1, ntry = 0, ...))
          if(!inherits(outh,"try-error")){
            lktrace = c(lktrace,outh$lk)
            if(outh$lk>out[[k]]$lk) out[[k]] = outh
          }
          lkv[k] = out[[k]]$lk
          Aic[k] = out[[k]]$aic
          Bic[k] = out[[k]]$bic
          cat("lktrace = ",sort(lktrace),"\n")
          cat("lk = ",lkv,"\n")
          cat("aic = ",Aic,"\n")
          cat("bic = ",Bic,"\n")
        }
      }
      if(version=="LMbasic") outn = try(lmbasic(S = Y,yv = freq,k = k,start = 2,
                                                tol=tol2,piv=out[[k]]$piv,Pi=out[[k]]$Pi,Psi=out[[k]]$Psi,out_se=out_se,miss = miss, R = R, ...))
      if(version=="LMlatent") outn = try(lmcovlatent(S = Y,X1 = Xinitial,X2 = Xtrans,
                                                       yv = freq,start = 2, k = k,tol=tol2,Psi=out[[k]]$Psi,Be=out[[k]]$Be,Ga=out[[k]]$Ga,out_se=out_se, miss = miss, R = R, ...))
      if(version=="LMmanifest") 
        outn = try( lmcovmanifest(S = as.matrix(Y[,,1]),X = Xmanifest, yv = freq,
                                  k = k,tol=tol2,mu=out[[k]]$mu,al=out[[k]]$al,be=out[[k]]$be,
                                  la=out[[k]]$la,PI=out[[k]]$PI,rho=out[[k]]$rho,
                                  si=out[[k]]$si,out_se=out_se,start = 2, ...))
      if(version=="LMbasiccont")
        outn = try(lmbasic.cont(Y=Y, yv=freq, k = k,start = 2, tol=tol2,piv=out[[k]]$piv,
                                Pi=out[[k]]$Pi, Mu=out[[k]]$Mu, Si=out[[k]]$Si, ntry = 0, ...))
      if(version=="LMlatentcont")
        outn = try(lmcovlatent.cont(Y = Y,X1 = Xinitial,X2 = Xtrans, yv=freq,
                                    k = k,start = 2,tol=tol2,Mu=out[[k]]$Mu,
                                    Si=out[[k]]$Si,Be=out[[k]]$Be,Ga=out[[k]]$Ga, 
                                    ntry = 0, ...))
      if(!inherits(outn,"try-error")){
        lktrace = c(lktrace,outn$lk)
        out[[k]] = outn
        out[[k]]$lktrace = lktrace
        lkv[k] = out[[k]]$lk
        Aic[k] = out[[k]]$aic
        Bic[k] = out[[k]]$bic
      }
    }
    out[[k]]$data = data
    if(miss & miss.imp) out[[k]]$Yimp = Yimp
    attributes(out[[k]])$responsesFormula = responsesFormula
    attributes(out[[k]])$latentFormula = latentFormula
    attributes(out[[k]])$whichid = id.which
    attributes(out[[k]])$whichtv = tv.which
    attributes(out[[k]])$id = id
    attributes(out[[k]])$time = tv
  }
  Aic <- Aic[kv]
  Bic <- Bic[kv]
  lkv <- lkv[kv]
  errv <- errv[kv]
  names(Aic) <- names(Bic) <- names(lkv) <- names(errv) <- kv
  out = list(out.single=lapply(kv,function(x) out[[x]]),Aic=Aic,Bic=Bic,lkv=lkv,errv=errv,k=kv,call=match.call())
  class(out)="LMsearch"
  return(out)

}
