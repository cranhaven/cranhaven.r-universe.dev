long2matrices.internal <- function(Y, id, time, yv = NULL, Xinitial = NULL, Xmanifest = NULL,
                                   Xtrans = NULL, cont = FALSE){

# ---- Preliminaries ----
  idu = unique(id)
  n = length(idu)
  TT = max(time)
  XXinitial = NULL
  XXmanifest = NULL
  XXtrans = NULL
  init = FALSE
  manifest = FALSE
  trans = FALSE
  if(!is.null(Xinitial)){
    if(dim(Xinitial)[2] == 0){
      Xinitial <- NULL
    }else{
      Xinitial = as.matrix(Xinitial)
      nxInitial = ncol(Xinitial)
      XXinitial <- matrix(NA,n,nxInitial)
    }
    init = TRUE
  }
  if(!is.null(Xtrans)){
    if(dim(Xtrans)[2] == 0){
      Xtrans <- NULL
    }else{
      Xtrans = as.matrix(Xtrans)
      nxTrans = ncol(Xtrans)
      XXtrans <- array(NA, c(n,TT,nxTrans))
    }
    trans = TRUE
  }
  if(!is.null(Xmanifest)){
    Xmanifest = as.matrix(Xmanifest)
    nxMan = ncol(Xmanifest)
    XXmanifest = array(NA,c(n,TT,nxMan))
    manifest = TRUE
  }
  if(isTRUE(init) | isTRUE(trans)){
    if(isTRUE(manifest)){
      model <- "LMlatentManifest"
      stop("covariates on both Latent and Manifest are not allowed",call. = FALSE)
    }else{
      model <- "LMlatent"
      if(cont) model = "LMlatentcont"
    }
  }else if (isTRUE(manifest)){
    model <- "LMmanifest"
    if(cont) model <- "LMmanifestcont"
  }else{
    model <- "LMbasic"
    if(isTRUE(cont)) model = "LMbasiccont"
  }
  Y = as.matrix(Y)
  ny = ncol(Y)

# ---- create matrices ----
  freq <- NULL
  if(model == "LMbasic" | model == "LMbasiccont"){
    if(is.null(yv) && !cont){
      temp <- aggr_data_long(data = Y, id = id, time = time, NAs = 999)
      freq = temp$freq
      id <- temp$Y[,1]
      time <- temp$Y[,2]
      Y = as.matrix(temp$Y[,-c(1,2)])
    }else{
      freq = yv
      id <- id
      time <- time
    }
    idu = unique(id)
    n = length(idu)
    YY = array(NA,c(n,TT,ny))
    for(i in 1:n){
      ind = which(id==idu[i])
      tmp = 0
      for(t in time[ind]){
        tmp=tmp+1
        YY[i,t,] = Y[ind[tmp],]
      }
    }
    dimnames(YY)[[3]] = colnames(Y)
  }else if(model == "LMlatent" | model == "LMlatentcont"){
    YY = array(NA,c(n,TT,ny))
    for(i in 1:n){
      ind = which(id==idu[i])
      timeid <- time[ind]
      if(!is.null(Xinitial)){
        timeid1 <- ind[timeid==1]
        if(!length(timeid1)==0) XXinitial[i,] = Xinitial[timeid1,]
      }
      colnames(XXinitial) = colnames(Xinitial)
      tmp = 0
      for(t in timeid){
        tmp = tmp+1
        indTemp <- ind[tmp]
        if(!length(indTemp)==0){
          if(!is.null(Xtrans)) XXtrans[i,t,] = Xtrans[indTemp,]
          YY[i,t,] = Y[indTemp,]
        }
      }
    }
    dimnames(XXtrans)[[3]] = colnames(Xtrans)
    dimnames(YY)[[3]] = colnames(Y)
    XXtrans <- XXtrans[,-1,, drop = FALSE]
    freq = rep(1,nrow(YY))
  }else if(model == "LMmanifest" | model == "LMmanifestcont"){
    YY = array(NA,c(n,TT,ny))
    for(i in 1:n){
      ind = which(id==idu[i])
      tmp = 0
      for(t in time[ind]){
        tmp=tmp+1
        if(!is.null(Xmanifest)) XXmanifest[i,t,] = Xmanifest[ind[tmp],]
        YY[i,t,] = Y[ind[tmp],]
      }
    }
    freq = rep(1,nrow(YY))
  }

# ---- output ----
  out = list(Y = YY,Xinitial = XXinitial,Xmanifest = XXmanifest,
             Xtrans = XXtrans,model = model,freq = freq)
  return(out)
}

# -

getResponses <- function(data, formula){

  if(is.null(formula))
  {
    Y <- data
    X <- NULL
  }else{
    formula <- Formula(formula)
    ll <- length(formula)
    Y <- model.part(formula, data = model.frame(formula, data = data,na.action = NULL), lhs = 1)
    Y <- data.matrix(Y)

    X <- NULL
    if(ll[2] != 0)
    {
      X <- model.matrix(formula, model.frame(formula = formula,data,na.action = NULL))
      X <- data.matrix(X)
    }
  }
  out <- list(Y = Y,
              X = X)
  return(out)
}

# -

getLatent <- function(data, latent, responses){

  formula <- update(Formula(responses),Formula(latent))
  formula <- Formula(formula)
  ll <- length(formula)
  Xinitial <- NULL
  Xtrans <- NULL
  if(length(Formula(latent))[2] == 1 && all(as.character(latent[[2]]) != "|")){
    if(!is.null(latent[[2]])){
     # Xinitial <- model.part(formula, data = model.frame(formula, data = data,na.action = NULL), rhs = 1)
      # Xinitial <- model.matrix(formula, data = model.frame(formula, data = data,na.action = NULL), rhs = 1)[,-1,drop=FALSE]
      Xinitial <- model.matrix(formula, data = model.frame(formula, data = data,na.action = NULL), rhs = 1)
      # Xtrans <- model.part(formula, data = model.frame(formula, data = data,na.action = NULL), rhs = 1)
      # Xtrans <- model.matrix(formula, data = model.frame(formula, data = data,na.action = NULL), rhs = 1)[,-1,drop=FALSE]
      Xtrans <- model.matrix(formula, data = model.frame(formula, data = data,na.action = NULL), rhs = 1)
      Xinitial <- data.matrix(Xinitial)
      Xtrans <- data.matrix(Xtrans)
      attributes(Xinitial)$onlyintercept = FALSE
      attributes(Xtrans)$onlyintercept = FALSE
    }
  }else{
  # if(length(Formula(latent))[2] == 1 && !is.null(latent[[2]]))
  # {
  #   Xinitial <- model.part(formula, data = model.frame(formula, data = data), rhs = 1)
  #   Xtrans <- model.part(formula, data = model.frame(formula, data = data), rhs = 1)
  # }

    if(length(Formula(latent))[2] == 2){
      # Xinitial <- model.part(formula, data = model.frame(formula, data = data,na.action = NULL), rhs = 1)
      # Xtrans <- model.part(formula, data = model.frame(formula, data = data,na.action = NULL), rhs = 2)
      Xinitial <- model.matrix(formula, data = model.frame(formula, data = data,na.action = NULL), rhs = 1)
      Xtrans <- model.matrix(formula, data = model.frame(formula, data = data,na.action = NULL), rhs = 2)
      Xinitial <- data.matrix(Xinitial)
      Xtrans <- data.matrix(Xtrans)
      attributes(Xinitial)$onlyintercept = FALSE
      attributes(Xtrans)$onlyintercept = FALSE
    }else{
      if(!is.null(latent[[2]][[2]])){
        #Xinitial <- model.matrix(formula(formula, rhs = 1),data)
#        Xinitial <- model.part(formula, data = model.frame(formula, data = data,na.action = NULL), rhs = 1)
        # Xinitial <- model.matrix(formula, data = model.frame(formula, data = data,na.action = NULL), rhs = 1)[,-1,drop=FALSE]
        Xinitial <- model.matrix(formula, data = model.frame(formula, data = data,na.action = NULL), rhs = 1)
        Xinitial <- data.matrix(Xinitial)
        attributes(Xinitial)$onlyintercept = FALSE
        #Xtrans <- NULL
      }
      if(!is.null(latent[[2]][[3]])){
        #Xinitial <- NULL
        #Xtrans <- model.matrix(formula(formula, rhs = 1),data)
        # Xtrans <- model.part(formula, data = model.frame(formula, data = data,na.action = NULL), rhs = 1)
        # Xtrans <- model.matrix(formula, data = model.frame(formula, data = data,na.action = NULL), rhs = 1)[,-1,drop=FALSE]
        Xtrans <- model.matrix(formula, data = model.frame(formula, data = data,na.action = NULL), rhs = 1)
        Xtrans <- data.matrix(Xtrans)
        attributes(Xtrans)$onlyintercept = FALSE
      }
    }
  }

  if(is.null(Xtrans)) Xtrans = model.part(Formula(~ 1),
                                          data = model.frame(Formula(~ 1), data = data,na.action = NULL), rhs = 1)
  if(is.null(Xinitial)) Xinitial =  model.part(Formula(~ 1),
                                               data = model.frame(Formula(~ 1), data = data,na.action = NULL), rhs = 1)
 # if(ll[2] == 2)
 #    {
 #      # Xinitial <- model.matrix(formula(formula, rhs = 1),data)
 #      # Xtrans <- model.matrix(formula(formula, rhs = 2),data)
 #      Xinitial <- model.part(formula, data = model.frame(formula, data = data), rhs = 1)
 #      Xtrans <- model.part(formula, data = model.frame(formula, data = data), rhs = 2)
 #      #Xtrans <- model.part(formula, data = model.frame(formula, data = data), rhs = 2)
 #
 #      #  if(dim(Xinitial)[2] == 0 & dim(Xinitial)[2] == 0)
 #      # {
 #      #   if(formula[[3]][[2]] == -1 & formula[[3]][[3]] == -1)
 #      #   {
 #      #     Xinitial <- NULL
 #      #     Xtrans <- NULL
 #      #   }
 #      # }
 #
 #    }else{
 #      if(length(latent) == 1)
 #      {
 #
 #      }else if(length(latent) == 2)
 #        {
 #          if(!is.null(latent[[2]][[2]]))
 #          {
 #            #Xinitial <- model.matrix(formula(formula, rhs = 1),data)
 #            Xinitial <- model.part(formula, data = model.frame(formula, data = data), rhs = 1)
 #            Xinitial <- data.matrix(Xinitial)
 #            Xtrans <- NULL
 #          }
 #
 #          if(!is.null(latent[[2]][[3]]))
 #          {
 #            Xinitial <- NULL
 #            #Xtrans <- model.matrix(formula(formula, rhs = 1),data)
 #            Xtrans <- model.part(formula, data = model.frame(formula, data = data), rhs = 1)
 #            Xtrans <- data.matrix(Xtrans)
 #
 #          }
 #
 #        }
 #      }

  out <- list(Xinitial = Xinitial,Xtrans = Xtrans)
  return(out)

}

matrices2long <- function(Y,X1 = NULL, X2 = NULL)
{
  maxT <- max(c(ncol(Y),ncol(X1),ncol(X2)))
  dmy <- dim(Y)

  if(is.na(dmy[3]))
  {
    nvar = 1
    Y <- array(Y, c(dmy[1],dmy[2],1))
  }else
  {
    nvar <- dmy[3]
  }
  n <- dmy[1]

  long <- matrix(NA, nrow = n*maxT,ncol = nvar)

  colnames(long) <- paste0("Y", seq(nvar))

  temp <- array(NA,c(n,maxT,nvar))
  temp[,1:dmy[2],] <- Y

  for(i in 1:nvar)
  {
    long[,i] <- as.vector(t(Y[,,i]))
  }
  id <- rep(1:n, each = maxT)
  time <- rep(1:maxT, n)

  longX1 <- NULL
  if(!is.null(X1))
  {
    dm <- dim(X1)
    if(dm[1] != dmy[1])
    {
      stop("Y anx X1 must have the same number of observations.")

    }
    if(is.na(dm[3]))
    {
      nvar = 1
      X1 <- array(X1, c(dm[1],dm[2],1))
    }else
    {
      nvar <- dm[3]
    }

    longX1 <- matrix(NA, nrow = n*maxT,ncol = nvar)

    temp <- array(NA,c(n,maxT,nvar))
    temp[,1:dm[2],] <- X1

    for(i in 1:nvar)
    {
      longX1[,i] <- as.vector(t(temp[,,i]))
    }

    colnames(longX1) <- paste0("X", seq(nvar))

  }

  longX2 <- NULL
  if(!is.null(X2))
  {
    dm <- dim(X2)

    if(dm[1] != dmy[1])
    {
      stop("Y anx X2 must have the same number of observations.")
    }

    if(is.na(dm[3]))
    {
      nvar = 1
      X1 <- array(X1, c(dm[1],dm[2],1))
    }else
    {
      nvar <- dm[3]
    }

    longX2 <- matrix(NA, nrow = n*maxT,ncol = nvar)

    temp <- array(NA,c(n,maxT,nvar))
    temp[,1:dm[2],] <- X2
    for(i in 1:nvar)
    {
      longX2[,i] <- as.vector(t(temp[,,i]))
    }

    colnames(longX2) <- paste0("Z", seq(nvar))

  }

  long <- cbind(id,time,long, longX1, longX2)

  return(as.data.frame(long))

}

aggr_data_long <- function(data, id, time, NAs = 999)
{
  temp <- data.frame(id = id, time = time, data,check.names = FALSE)
  temp.wide <- reshape(temp,idvar = "id",timevar = "time", direction = "wide")

  temp.wide[is.na(temp.wide)] <- NAs
  aggr <- aggr_data(temp.wide[,-1],fort = TRUE)


  temp <- data.frame(1:nrow(aggr$data_dis),aggr$data_dis)

  colnames(temp) <- c("id",attributes(temp.wide)$reshapeWide$varying)
  freq <- aggr$freq

  data <- reshape(temp,direction = "long",idvar = "id",varying = 2:ncol(temp),sep = ".")
  data[,-c(1,2)][data[,-c(1,2)] == NAs] <- NA

  return(list(freq = freq,
              Y = data))

}


