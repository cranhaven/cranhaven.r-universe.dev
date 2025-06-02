lmestCont <- function(responsesFormula = NULL, latentFormula = NULL,
                      data, index, k = 1:4, start = 0,
                      modSel = c("BIC", "AIC"), modBasic = 0,
                      paramLatent = c("multilogit", "difflogit"),
                      weights = NULL, tol = 10^-10,
                      maxit = 5000, out_se = FALSE, output = FALSE,
                      parInit = list(piv = NULL, Pi = NULL, Mu = NULL, Si = NULL,
                                     Be = NULL, Ga = NULL),
                      fort = TRUE, seed = NULL, ntry = 0, miss.imp = FALSE){

  data <- as.data.frame(data)
  if(!is.data.frame(data)) stop("A data.frame must be provided")
  if(start == 2){
    if(is.null(parInit)){
      stop("With start = 2, initial parameters must be provided")
    }else if(!is.null(parInit$Mu)){
      k <- dim(parInit$Mu)[2]
    }
  }

  if(!is.null(seed)) set.seed(seed)

  k <- sort(unique(k))
  nkv <- length(k)

  if(length(index) !=2) stop("id and time must be provided")
  id.which <- which(names(data) == index[1])
  tv.which <- which(names(data) == index[2])

  if(length(id.which) == 0) stop("the id column does not exist")
  if(length(tv.which) == 0) stop("the time column does not exist")

  modSel <- match.arg(modSel, choices = eval(formals(lmestCont)$modSel))
  paramLatent <- match.arg(paramLatent, choices = eval(formals(lmestCont)$paramLatent))
  id <- data[,id.which]
  tv <- data[,tv.which]

  if(is.character(id) | is.factor(id)){
    warning("id column must be numeric. Coerced in numeric.")
    id <- as.numeric(id)
  }

  if(is.character(tv) | is.factor(tv)){
    warning("time column must be numeric. Coerced in numeric.")
    tv <- as.numeric(tv)
  }

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

  if(!is.null(latentFormula)){
    temp <- getLatent(data = data.new,latent = latentFormula, responses = responsesFormula)
    Xinitial <- temp$Xinitial
    Xtrans <- temp$Xtrans
  }
  Xinitial0 = Xinitial; Xtrans0 = Xtrans
  if(is.null(weights)) weights = rep(1,length(unique(id)))
  tmp <- long2matrices.internal(Y = Y, id = id, time = tv, yv = weights, 
                                Xinitial = Xinitial, Xmanifest = Xmanifest, Xtrans = Xtrans, cont = TRUE)
  model <- tmp$model
  Xinitial <- tmp$Xinitial
  Xtrans <- tmp$Xtrans
  if(!is.null(Xmanifest))  Xmanifest = matrix(aperm(tmp$Xmanifest,c(2,1,3)),
                                              dim(tmp$Xmanifest)[1]*dim(tmp$Xmanifest)[2],
                                              dim(tmp$Xmanifest)[3])
  Y <- tmp$Y
  if(is.null(weights)){
    freq = tmp$freq
  }else{
    freq = weights
    if(nrow(Y)!=length(weights)) stop("dimensions mismatch between data and weights")
  }
  if(any(is.na(Xinitial)) & !any(is.na(Xinitial0)))
    for(j in 1:ncol(Xinitial)) if(any(is.na(Xinitial[,j]))) Xinitial[is.na(Xinitial[,j]),j] = mean(Xinitial[,j],na.rm=TRUE)
  if(any(is.na(Xtrans)) & !any(is.na(Xtrans0)))
    for(h in 1:dim(Xtrans)[3]) for(j in 1:ncol(Xtrans)) if(any(is.na(Xtrans[,j,h]))) Xtrans[is.na(Xtrans[,j,h]),j,h] = mean(Xtrans[,j,h],na.rm=TRUE)
  out = vector("list",nkv)
  if(!is.null(Xinitial)){
    if(any(is.na(Xinitial)))stop("missing data in the covariates affecting the initial probabilities are not allowed")
  }

  if(!is.null(Xtrans)){
    if(any(is.na(Xtrans))) stop("missing data in the covariates affecting the transition 
                                probabilities are not allowed")
  }
  miss = any(is.na(Y))
  if(miss & miss.imp) {
    Yv = cbind(1, Y)
    pYv = prelim.mix(Yv, 1)
    thhat = em.mix(prelim.mix(Yv, 1))
    rngseed(1)
    Yv = as.matrix(imp.mix(pYv, da.mix(pYv, thhat, steps = 100),Yv)[, -1])
    Y = array(Yv, dim(Y))
    cat("Missing data in the dataset. imp.mix function (mix package) used for imputation\n")
    Yimp = Y  
  }
  npv = lkv = aicv = bicv = rep(NA,nkv)
#  model <- paste(model, covariance, sep = "")
  if(any(is.na(tmp$Xmanifest))){
    Xmanifest = matrix(aperm(tmp$Xmanifest,c(2,1,3)),dim(tmp$Xmanifest)[1]*dim(tmp$Xmanifest)[2],dim(tmp$Xmanifest)[3])
    for(j in 1:ncol(Xmanifest)) Xmanifest[is.na(Xmanifest[,j]),j] = mean(Xmanifest[,j],na.rm=TRUE)
  }
  for(kv in 1:nkv){
    out[[kv]] <- switch(model,
                        "LMbasiccont" = (lmbasic.cont(Y = Y, yv = freq, k = k[kv], start = start,modBasic = modBasic,
                                                      tol = tol, maxit = maxit, out_se = out_se,
                                                           piv = parInit$piv,Pi = parInit$Pi,Mu = parInit$Mu,Si = parInit$Si, output=output, fort = fort,
                                                           ntry=ntry)),
                        "LMlatentcont" = (lmcovlatent.cont(Y = Y,X1 = Xinitial,X2 = Xtrans, yv=freq, k = k[kv],
                                                           start = start,tol = tol,maxit = maxit,paramLatent = paramLatent,
                                                           Mu = parInit$Mu, Si = parInit$Si,Be = parInit$Be,Ga = parInit$Ga,output = output, out_se = out_se,
                                                           fort = fort,ntry=ntry)),
                        "LMmanifestcont" = (lmcovmanifest.cont(Y = Y,X=Xmanifest, yv=freq, k = k[kv],start = start,modBasic = modBasic,tol = tol,maxit = maxit,
                                                               out_se = out_se,piv = parInit$piv,Pi = parInit$Pi,Mu = parInit$Mu,Si = parInit$Si, output=output,
                                                               fort = fort,ntry=ntry)))
    if(model=="LMlatentcont") out[[kv]]$responsesFormula = responsesFormula
    npv[kv] = out[[kv]]$np
    lkv[kv] = out[[kv]]$lk
    aicv[kv] = out[[kv]]$aic
    bicv[kv] = out[[kv]]$bic
  }

  if(modSel == "BIC"){
    best <- out[[which.min(bicv)]]
  }else if(modSel == "AIC"){
    best <- out[[which.min(aicv)]]
  }

  Np <- npv
  Lk <- lkv
  Bic <- bicv
  Aic <- aicv

  names(Np) = names(Lk) = names(Aic) = names(Bic) = paste("k",k,sep = "=")
  # if(nkv > 1)
  # {
  #   Bic = bicv
  #   Aic = aicv
  # }
  out <- do.call(c, list(best,list(Np=Np,Lk=Lk,Bic = Bic, Aic = Aic, call = match.call(),data = data)))
  if(miss & miss.imp) {out$Yimp = Yimp}
  attributes(out)$responsesFormula = responsesFormula
  attributes(out)$latentFormula = latentFormula
  attributes(out)$whichid = id.which
  attributes(out)$whichtv = tv.which
  attributes(out)$id = id
  attributes(out)$time = tv

  class(out) <- class(best)
  return(out)
}
