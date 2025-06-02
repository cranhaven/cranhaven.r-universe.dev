lmest <- function(responsesFormula = NULL, latentFormula = NULL,
                  data, index, k = 1:4, start = 0,
                  modSel = c("BIC", "AIC"), modBasic = 0,
                  modManifest = c("LM", "FM"),
                  paramLatent = c("multilogit", "difflogit"),
                  weights = NULL, tol = 10^-8, maxit = 1000,
                  out_se = FALSE, q = NULL, output = FALSE,
                  parInit = list(piv = NULL, Pi = NULL, Psi = NULL,
                                 Be = NULL, Ga = NULL, mu = NULL,
                                 al = NULL, be = NULL, si = NULL,
                                 rho = NULL, la = NULL, PI = NULL,
                                 fixPsi = FALSE),
                  fort = TRUE, seed = NULL, ntry = 0){

  if(inherits(data, "lmestData")){
    data <- data$data
  }else if(!is.data.frame(data)){
    data <- as.data.frame(data)
    stop("A data.frame must be provided")
  }

  if(start == 2){
    if(is.null(parInit)){
      stop("With start = 2, initial parameters must be provided")
    }else if(!is.null(parInit$Psi)){
      k <- dim(parInit$Psi)[2]
    }else if(!is.null(parInit$la)){
      k <- length(parInit$la)
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

  modSel <- match.arg(modSel, choices = eval(formals(lmest)$modSel))
  modManifest <- match.arg(modManifest, choices = eval(formals(lmest)$modManifest))
  paramLatent <- match.arg(paramLatent, choices = eval(formals(lmest)$paramLatent))

  id <- data[,id.which]
  tv <- data[,tv.which]

  if(is.character(id) | is.factor(id)){
    #warning("id column must be numeric. Coerced in numeric.", call. = FALSE)
    id <- as.numeric(factor(id))
  }

  if(is.character(tv) | is.factor(tv)){
    #warning("time column must be numeric. Coerced in numeric.", call. = FALSE)
    tv <- as.numeric(factor(tv))
  }
  data.new <- data[,-c(id.which,tv.which), drop = FALSE]
  ## of frequencies of the available configurations
  if(is.null(responsesFormula)){
    Y <- data.new
    Xmanifest <- NULL
    Xinitial <- NULL
    Xtrans <- NULL
    Y_names <- colnames(Y)
    Xmanifest_names <- colnames(Xmanifest)
  }else{
    temp <- getResponses(data = data.new,formula = responsesFormula)
    Y <- temp$Y
    Xmanifest <- temp$X
    Xinitial <- NULL
    Xtrans <- NULL
    Y_names <- colnames(Y)
    Xmanifest_names <- colnames(Xmanifest)
  }
  if(!is.null(latentFormula)){
    temp <- getLatent(data = data.new,latent = latentFormula, responses = responsesFormula)
    Xinitial <- temp$Xinitial
    Xtrans <- temp$Xtrans
    Xinitial_names <- colnames(Xinitial)
    Xtrans_names <- colnames(Xtrans)
  }
  Xinitial0 = Xinitial; Xtrans0 = Xtrans
  if(is.null(weights)) weights = rep(1,length(unique(id)))
  tmp <- long2matrices.internal(Y = Y, id = id, time = tv, yv = weights,
                                Xinitial = Xinitial, Xmanifest = Xmanifest, Xtrans = Xtrans)
  model <- tmp$model
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
  out = vector("list",nkv)

  miss = any(is.na(Y))
  if(miss){
    if(model == "LMmanifest") stop("Missing data in the dataset")
    cat("Missing data in the dataset, treated as missing at random\n")
    R = 1 * (!is.na(Y))
    Y[is.na(Y)] = 0
  }else{
    R = NULL
  }

  if(min(Y,na.rm=T)>0){
    cat("|------------------- WARNING -------------------|\n")
    cat("|The first response category must be coded as 0 |\n")
    cat("|-----------------------------------------------|\n")
    # df <- dim(Y)
    # Y <- apply(Y,2,function(x) x - min(x, na.rm = TRUE))
    # dim(Y) <- df
    for(i in 1:dim(Y)[3]){
      Y[,,i] <- Y[,,i]-min(Y[,,i],na.rm = TRUE)
    }
  }

  dimnames(Y)[[3]] <- Y_names
  if(any(is.na(Xinitial) & !any(is.na(Xinitial0))))
    for(j in 1:ncol(Xinitial)) if(any(is.na(Xinitial[,j]))) Xinitial[is.na(Xinitial[,j]),j] = mean(Xinitial[,j],na.rm=TRUE)
  if(any(is.na(Xtrans) & !any(is.na(Xtrans0)))) 
    for(h in 1:dim(Xtrans)[3]) for(j in 1:ncol(Xtrans)) if(any(is.na(Xtrans[,j,h]))) Xtrans[is.na(Xtrans[,j,h]),j,h] = mean(Xtrans[,j,h],na.rm=TRUE)
  if(!is.null(Xmanifest))
    if(any(is.na(Xmanifest))) stop("missing data in the covariates affecting the measurement model are not allowed")
  if(!is.null(Xinitial)){
    dimnames(Xinitial)[[2]] <- Xinitial_names
    if(any(is.na(Xinitial)))
      stop("missing data in the covariates affecting the initial probabilities are not allowed")
  }
  if(!is.null(Xtrans)){
    dimnames(Xtrans)[[3]] <- Xtrans_names
    if(any(is.na(Xtrans)))
      stop("missing data in the covariates affecting the transition probabilities are not allowed")
  }
  aicv = rep(NA,nkv)
  bicv = rep(NA,nkv)
  lkv = rep(NA,nkv)
  for(kv in 1:nkv){
    out[[kv]] <- switch(model,
                       "LMbasic" = lmbasic(S = Y,yv = freq,k = k[kv],start = start,
                                           modBasic = modBasic,tol = tol,maxit = maxit,
                                           out_se = out_se,piv = parInit$piv,Pi = parInit$Pi,
                                           Psi = parInit$Psi,miss = miss, R = R,output=output,ntry = ntry,
                                           fixPsi = parInit$fixPsi),
                       "LMlatent" = lmcovlatent(S = Y,X1 = Xinitial,X2 = Xtrans,yv = freq,
                                                start = start,k = k[kv],tol = tol,maxit = maxit,
                                                paramLatent = paramLatent,output = output,
                                                Psi = parInit$Psi,Be = parInit$Be,Ga = parInit$Ga,
                                                fort = fort,out_se = out_se,
                                                fixPsi = ifelse(is.null(parInit$fixPsi),FALSE,parInit$fixPsi), 
                                                miss = miss, R = R, ntry = ntry),
                       "LMmanifest" = lmcovmanifest(S = as.matrix(Y[,,1]),X = Xmanifest, yv = freq,
                                                    modManifest = modManifest,k = k[kv],tol = tol,
                                                    maxit = maxit, q = q,start = start,
                                                    out_se = out_se,mu = parInit$mu,al = parInit$al,
                                                    be = parInit$be,si = parInit$si,
                                                    rho = parInit$rho,la = parInit$la,PI = parInit$PI,
                                                    output = output, ntry=ntry))
    aicv[kv] = out[[kv]]$aic
    bicv[kv] = out[[kv]]$bic
    lkv[kv] = out[[kv]]$lk
  }

  if(modSel == "BIC") {
    best <- out[[which.min(bicv)]]
  }else if(modSel == "AIC"){
    best <- out[[which.min(aicv)]]
  }
  Bic <- bicv
  Aic <- aicv
  Lk <- lkv

  names(Bic) <- paste("k",k,sep = "=")
  names(Aic) <- paste("k",k,sep = "=")
  names(Lk) <- paste("k",k,sep = "=")
  # if(nkv > 1)
  # {
  #   Bic = bicv
  #   Aic = aicv
  # }
  out <- do.call(c, list(best,
                          list(Lk = lkv, Bic = Bic, Aic = Aic, call = match.call(),data = data)))
  #out <- append(best, list( Bic = Bic, Aic = Aic, call = match.call(),data = data))
  attributes(out)$responsesFormula = responsesFormula
  attributes(out)$latentFormula = latentFormula
  attributes(out)$whichid = id.which
  attributes(out)$whichtv = tv.which
  attributes(out)$id = id
  attributes(out)$time = tv

  class(out) <- class(best)
  return(out)
}