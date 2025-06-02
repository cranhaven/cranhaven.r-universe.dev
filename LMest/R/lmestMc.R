lmestMc <- function(responsesFormula = NULL,
                    data, index, start = 0,
                    modBasic = 0, weights = NULL,
                    tol = 10^-8, maxit = 1000,
                    out_se = FALSE, output = FALSE, fort = TRUE, seed = NULL){

  if(inherits(data, "lmestData")){
    data <- data$data
  }else if(!is.data.frame(data)){
    data <- as.data.frame(data)
    stop("A data.frame must be provided")
  }
  if(!is.null(seed)) set.seed(seed)
  if(length(index) !=2) stop("id and time must be provided")
  id.which <- which(names(data) == index[1])
  tv.which <- which(names(data) == index[2])

  if(length(id.which) == 0) stop("the id column does not exist")
  if(length(tv.which) == 0) stop("the time column does not exist")

  id <- data[,id.which]
  tv <- data[,tv.which]

  if(is.character(id) | is.factor(id)){
    warning("id column must be numeric. Coerced in numeric.", call. = FALSE)
    id <- as.numeric(id)
  }

  if(is.character(tv) | is.factor(tv)){
    warning("time column must be numeric. Coerced in numeric.", call. = FALSE)
    tv <- as.numeric(tv)
  }
  
  if(!is.null(responsesFormula)) if(length(responsesFormula)==3){
    if(is.null(responsesFormula[[3]]))
      responsesFormula[[3]]=1
    else{
      if(length(responsesFormula[[3]])>1){
        for(j in 1:length(responsesFormula[[3]])) 
          if(is.null(responsesFormula[[3]][[j]])) responsesFormula[[3]][[j]] = 1
      }
    }
  }

  data.new <- data[,-c(id.which,tv.which), drop = FALSE]

  if(is.null(responsesFormula)){
    Y <- data.new
    Xmanifest <- NULL
    Xinitial <- NULL
    Xtrans <- NULL
  }else{
    temp <- getResponses(data = data.new,formula = as.formula(paste(responsesFormula[[2]], "NULL", sep = "~" )))
    Y <- temp$Y
    Xmanifest <- temp$X
    Xinitial <- NULL
    Xtrans <- NULL
  }

  if(!is.null(responsesFormula) & !is.null(responsesFormula[[3]])){
    temp <- getLatent(data = data.new,latent = responsesFormula,responses = responsesFormula)
    Xinitial <- temp$Xinitial
    Xtrans <- temp$Xtrans
  }

  if(dim(Y)[2] > 1){
    warning("multivariate data are not allowed; only the first response variable is considered", call. = FALSE)
    Y <- Y[,1]
  }
  tmp <- long2matrices.internal(Y = Y, id = id, time = tv,yv = weights,
                          Xinitial = Xinitial, Xmanifest = NULL, Xtrans = Xtrans)
  model <- tmp$model
  Xinitial <- tmp$Xinitial
  Xtrans <- tmp$Xtrans
  Y <- tmp$Y

  if(is.null(weights)){
    freq = tmp$freq
  }else{
    freq = weights
    if(nrow(Y)!=length(weights)) stop("dimensions mismatch between data and weights")
  }

  if(min(Y,na.rm=TRUE)>0){
    cat("|------------------- WARNING -------------------|\n")
    cat("|The first response category must be coded as 0 |\n")
    cat("|-----------------------------------------------|\n")
    for(i in 1:dim(Y)[3]) Y[,,i] <- Y[,,i]-min(Y[,,i],na.rm = TRUE)
  }

  if(any(is.na(Y))) stop("Missing data in the dataset")
  if(!is.null(Xinitial))
    if(any(is.na(Xinitial)))
      stop("missing data in the covariates affecting the initial probabilities are not allowed")
  if(!is.null(Xtrans))
    if(any(is.na(Xtrans)))
      stop("missing data in the covariates affecting the transition probabilities are not allowed")

  out <- switch(model,
                "LMbasic" = mcbasic(S = Y[,,1], yv = freq, modBasic = modBasic, tol = tol, maxit = maxit, out_se = out_se),
                "LMlatent" = mccov(S = Y[,,1],X1 = Xinitial, X2 = Xtrans, start = start,yv = freq,
                                   tol = tol, maxit = maxit, out_se = out_se, output = output, fort = fort))

  class <- class(out)
  out <- do.call(c, list(out,
                         list(call = match.call(),data = data)))
  #out <- append(out, list(call = match.call(),data = data))
  attributes(out)$responsesFormula = responsesFormula
  attributes(out)$whichid = id.which
  attributes(out)$whichtv = tv.which
  attributes(out)$id = id
  attributes(out)$time = tv
  class(out) <- class
  return(out)

}

