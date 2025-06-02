draw <- function(est,n,TT,...) {
  UseMethod("draw")
}

draw.LMbasic <- function(est, n = NULL, TT = NULL, format = c("long","matrices"), 
                         seed = NULL, ...){

# Draw a sample of size n from a Basic Latent Markov model with parameter piv, Pi and Psi
  format <- match.arg(format, choices = eval(formals(draw.LMbasic)$format))
  if(!is.null(seed)) set.seed(seed)
  piv <- est$piv
  Pi <- est$Pi
  Psi <- est$Psi

# Preliminaries
  k = length(piv)
  dd = dim(Psi)
  c = dim(Psi)[1]

  if(is.null(n)) n = ifelse(is.null(est$ns), est$n, est$ns)
  if(is.null(TT)) TT = est$TT #TT = dim(Pi)[3]
  if(length(dd)>2) r = dd[3]
  else r = 1

# For each subject
  Y = matrix(0,n,TT*r)
  cat("------------|\n")
  cat(" sample unit|\n")
  cat("------------|\n")
  for(i in 1:n){
    if(i/1000==floor(i/1000)) cat(sprintf("%11g",i),"\n",sep=" | ")
    u = k+1-sum(runif(1)<cumsum(piv))
    ind = 0
    for(j in 1:r){
      ind = ind+1
      Y[i,ind] = c-sum(runif(1)<cumsum(Psi[,u,j]))
    }
    for(t in 2:TT){
      u = k+1-sum(runif(1)<cumsum(Pi[u,,t]))
      for(j in 1:r){
        ind = ind+1
        Y[i,ind] = c-sum(runif(1)<cumsum(Psi[,u,j]))
      }
    }
  }
  if(i/1000>floor(i/1000)) cat(sprintf("%11g",i),"\n",sep=" | ")
  cat("------------|\n")
  if(format == "matrices"){
    # out = aggr_data(Y)
    # S = out$data_dis
    # yv = out$freq
    S = Y
    yv = rep(1,n)
    S = array(t(S), c(r, TT, length(yv)))
    S = aperm(S)
    if (r == 1){S = S[, , 1]}
  }
  if(format == "long"){
    S = array(t(Y), c(r,TT,n))
    S = aperm(S)
    if (r == 1){S = S[, , 1]}
    id <- 1:n
    #Y <- reshape(data = as.data.frame(Y), varying = list(1:TT),ids = id, direction = "long")
    Y <- matrices2long(Y = S)
    # out <-  aggr_data_long(data = Y[,-c(1,2)], id = Y$id,time = Y$time)
    # S = out$Y
    S = Y
    Y = as.data.frame(Y)
    # S = as.data.frame(S)
    # yv = out$freq
    yv = rep(1,n)
  }

  # S = array(t(S),c(r,TT,length(yv)))
  # S = aperm(S)
  #if(r==1) S = S[,,1]
  out = list(Y = Y, S = S, yv = yv, piv = piv, Pi = Pi,Psi = Psi,
             n = n, TT=TT, est = est)
}

draw.LMlatent <- function(est, n=NULL, TT=NULL, data, index, format = c("long", "matrices"),
                         fort = TRUE, seed = NULL, ...){

# Preliminaries
  format <- match.arg(format, choices = eval(formals(draw.LMlatentcont)$format))
  if(!is.null(seed)) set.seed(seed)
  Psi = est$Psi
  Be = est$Be
  Ga = est$Ga
  latentFormula = attributes(est)$latentFormula
  id.el <- names(data) == index[1]
  tv.el <- names(data) == index[2]
  id.which <- which(id.el == TRUE)
  tv.which <- which(tv.el == TRUE)
  if(!any(id.el)) stop("the id column does not exist")
  if(!any(tv.el)) stop("the time column does not exist")
  id <- data[,id.which]
  tv <- data[,tv.which]

  param <- est$paramLatent
  temp <-  getLatent(data = data, latent = latentFormula,
                     responses = as.formula(paste(names(data)[1],"NULL", sep = "~")))
  Xinitial <- temp$Xinitial
  Xtrans <- temp$Xtrans
  tmp <-  long2matrices.internal(Y = Xtrans, id = id, time = tv,
                                 yv = NULL, Xinitial = Xinitial, Xtrans = Xtrans)
  X1 <- tmp$Xinitial
  X2 <- tmp$Xtrans
  if(param=="difflogit"){
    cat("\n* With difflogit is not possible to avoid the intercept for the transition probabilities*\n\n")
    X2 = X2[,,-1,drop=FALSE]
  }

  if(!is.null(X1)){
    if(any(is.na(X1))) stop("missing data in the covariates affecting the initial probabilities are not allowed")
  }
  if(!is.null(X2)){
    if(any(is.na(X2))) stop("missing data in the covariates affecting the transition probabilities are not allowed")
  }

# Preliminaries
  # if(is.null(n)) n = est$n #n = nrow(X2)
  # if(is.null(TT)) TT = est$TT #TT = dim(X2)[2]+1
  if(is.null(n)) n = length(unique(data[,which(names(data)==index[1])])) #n = nrow(X2)
  if(is.null(TT)) TT = max(data[,which(names(data)==index[2])]) #TT = dim(X2)[2]+1
  
  dPsi = dim(Psi)
  if(length(dPsi)==2) r = 1
  else r = dPsi[3]
  if(length(dPsi)==1) k = 1
  else k = dPsi[2]
  if(length(dPsi)==2) Psi = array(Psi,c(dPsi,1))
  if(r==1){
    b=dim(Psi)[1]-1
  }else{
    b = rep(0,r)
    for(j in 1:r) b[j] = sum(!is.na(Psi[,1,j]))-1
  }

# Covariate structure and related matrices: initial probabilities
  if(is.null(X1)){
    nc1 = 0
    Xlab = rep(1,n)
  }else{
    if(is.vector(X1)) X1 = matrix(X1,n,1)
    nc1 = dim(X1)[2] # number of covariates on the initial probabilities
    out = MultiLCIRT::aggr_data(X1)
    Xdis = out$data_dis
    if(nc1==1) Xdis = matrix(Xdis,length(Xdis),1)
    Xlab = out$label
  }
  
  if(k == 2) GBe = as.matrix(c(0,1)) else{
    GBe = diag(k); GBe = GBe[,-1]
  }
  
  Xndis = max(Xlab)
  XXdis = array(0,c(k,(k-1)*nc1,Xndis))
  for(i in 1:Xndis){
    if(nc1==0) xdis = 1 else xdis = Xdis[i,]
    XXdis[,,i] = GBe%*%(diag(k-1)%x%t(xdis))
  }

# for the transition probabilities
  if(is.null(X2)){
    nc2 = 0
    Zlab = rep(1,n*(TT-1))
    Zndis = max(Zlab)
  }else{
    if(is.matrix(X2)) X2 = array(X2,c(n,TT-1,1))
    nc2 = dim(X2)[3] # number of covariates on the transition probabilities
    Z = NULL
    for(t in 1:(TT-1)) Z = rbind(Z,X2[,t,])
    if(nc2==1) Z = as.vector(X2)
    out = aggr_data(Z); Zdis = out$data_dis; Zlab = out$label; Zndis = max(Zlab)
    if(nc2==1) Zdis=matrix(Zdis,length(Zdis),1)
  }

  if(param=="multilogit"){
    ZZdis = array(0,c(k,(k-1)*nc2,Zndis,k))
    for(h in 1:k){
      if(k==2){
        if(h == 1) GGa = as.matrix(c(0,1)) else GGa = as.matrix(c(1,0))
      }else{
        GGa = diag(k); GGa = GGa[,-h]
      }
      for(i in 1:Zndis){
        if(nc2==0) zdis = 1 else zdis = Zdis[i,]
        ZZdis[,,i,h] = GGa%*%(diag(k-1)%x%t(zdis))
      }
    }
  }else if(param=="difflogit"){
    Zlab = (((Zlab-1)*k)%x%rep(1,k))+rep(1,n*(TT-1))%x%(1:k)
    ZZdis = array(0,c(k,k*(k-1)+(k-1)*nc2,Zndis*k))
    j = 0
    for(i in 1:Zndis){
      for(h in 1:k){
        j = j+1
        if(k==2){
          if(h == 1) GGa = as.matrix(c(0,1)) else GGa = as.matrix(c(1,0))
        }else{
          GGa = diag(k); GGa = GGa[,-h]
        }
        u = matrix(0,1,k); u[1,h] = 1
        U = diag(k); U[,h] = U[,h]-1
        U = U[,-1]
        ZZdis[,,j] = cbind(u%x%GGa,U%x%t(Zdis[i,]))
      }
    }
  }

# When there is just 1 latent class
  Y = array(0,c(n,TT,r))
  if(k == 1){
    U = array(1,n,TT)
    for(i in 1:n) for(t in 1:TT){
      if(r==1){
        Y[i,t] = which(rmultinom(1,1,Psi)==1)-1
      }else{
        for (j in 1:r) Y[i,t,j] = which(rmultinom(1,1,Psi[1:(b[j]+1),j])==1)-1
      }
    }
  }else{
# parameters on initial probabilities
    U = matrix(0,n,TT)
    be = as.vector(Be)
    out =  prob_multilogit(XXdis,be,Xlab,fort)
    Piv = out$P
    for(i in 1:n) U[i,1] = which(rmultinom(1,1,Piv[i,])==1)
# parameters on transition probabilities
    if(param=="multilogit"){
      if(is.list(Ga)) stop("invalid mode (list) for Ga")
      Ga = matrix(Ga,max(nc2,1)*(k-1),k)
      PIdis = array(0,c(Zndis,k,k)); PI = array(0,c(k,k,n,TT))
      for(h in 1:k){
        tmp = ZZdis[,,,h]
        if(nc2==1) tmp = array(tmp,c(k,(k-1),Zndis))
        out = prob_multilogit(tmp,Ga[,h],Zlab,fort)
        PI[h,,,2:TT] = array(as.vector(t(out$P)),c(1,k,n,TT-1))
      }
    }else if(param=="difflogit"){
      if(is.list(Ga)) Ga = c(as.vector(t(Ga[[1]])),as.vector(Ga[[2]]))
      if(length(Ga)!=k*(k-1)+(k-1)*nc2) stop("invalid dimensions for Ga")
      PI = array(0,c(k,k,n,TT))
      out =  prob_multilogit(ZZdis,Ga,Zlab,fort)
      Tmp = array(out$P,c(k,n,TT-1,k))
      PI[,,,2:TT] = aperm(Tmp,c(1,4,2,3))
    }
    for(i in 1:n) for(t in 2:TT){
      U[i,t] = which(rmultinom(1,1,PI[U[i,t-1],,i,t])==1)
    }
    for(i in 1:n) for(t in 1:TT) for(j in 1:r){
      Y[i,t,j] = which(rmultinom(1,1,Psi[1:(b[j]+1),U[i,t],j])==1)-1
    }
  }

# output
  if(r==1) Y = matrix(Y,n,TT)
  if(format == "long"){
    #id <- 1:n
    #Y <- reshape(data = as.data.frame(Y), varying = list(1:TT),ids = id, direction = "long")
    #out <-  aggr_data_long(data = Y[,-c(1,3)], id = Y$id,time = Y$time)
    Y <- matrices2long(Y = Y)
  }
  yv = rep(1,n)
  out = list(U = U, Y = Y, Psi = Psi, Be = Be, Ga = Ga, latentFormula = latentFormula,
             data = data, n=n, TT=TT, est = est, yv=yv)

}

draw.LMlatentcont <- function(est, n=NULL, TT=NULL, data, index, format = c("long", "matrices"),
                              fort = TRUE, seed = NULL, ...){

  # Draw a sample from LM model with covariates
  # param = type of parametrization for the transition probabilities:
  #         multilogit = standard multinomial logit for every row of the transition matrix
  #         difflogit  = multinomial logit based on the difference between two sets of parameters
  # fort  = fortran use (FALSE for not use fortran)
  # X1    = design matrix for the initial probabilities (n by n.cov.)
  # X2    = design matrix for the initial probabilities (n by TT-1 by n.cov.)

# Preliminaries
  format <- match.arg(format, choices = eval(formals(draw.LMlatentcont)$format))
  if(!is.null(seed)) set.seed(seed)
  Mu <- est$Mu
  Si <- est$Si
  Be <- est$Be
  Ga <- est$Ga
  latentFormula = attributes(est)$latentFormula
  id.el <- names(data) == index[1]
  tv.el <- names(data) == index[2]
  id.which <- which(id.el == TRUE)
  tv.which <- which(tv.el == TRUE)
  if(!any(id.el)) stop("the id column does not exist")
  if(!any(tv.el)) stop("the time column does not exist")
  id <- data[,id.which]
  tv <- data[,tv.which]
  param <- est$paramLatent
  temp <-  getLatent(data = data,latent = latentFormula,
                     responses = as.formula(paste(names(data)[1],"NULL", sep = "~")))
  Xinitial <- temp$Xinitial
  Xtrans <- temp$Xtrans
  Xinitial0 = Xinitial; Xtrans0 = Xtrans
  tmp <-  long2matrices.internal(Y = Xtrans, id = id, time = tv,
                                 yv = NULL, Xinitial = Xinitial, Xtrans = Xtrans)
  
  Xinitial <- tmp$Xinitial
  Xtrans <- tmp$Xtrans
  if(any(is.na(Xinitial)) & !any(is.na(Xinitial0)))
    for(j in 1:ncol(Xinitial)) if(any(is.na(Xinitial[,j]))) Xinitial[is.na(Xinitial[,j]),j] = mean(Xinitial[,j],na.rm=TRUE)
  if(any(is.na(Xtrans)) & !any(is.na(Xtrans0)))
    for(h in 1:dim(Xtrans)[3]) for(j in 1:ncol(Xtrans)) if(any(is.na(Xtrans[,j,h]))) Xtrans[is.na(Xtrans[,j,h]),j,h] = mean(Xtrans[,j,h],na.rm=TRUE)

  if(!is.null(est$responsesFormula)){
    data.new <- data[,-c(id.which,tv.which), drop = FALSE]
    temp1 <- getResponses(data = data.new,formula = est$responsesFormula)
    Y <- temp1$Y
    tmp1 <- long2matrices.internal(Y = Y, id = id, time = tv, cont = TRUE)
    Y <- tmp1$Y
    if(any(is.na(Y))) R = !is.na(Y)
  }

  X1 <- Xinitial
  X2 <- Xtrans
  if(param=="difflogit"){
    cat("\n* With difflogit is not possible to avoid the intercept for the transition probabilities*\n\n")
    X2 = X2[,,-1,drop=FALSE]
  }

  if(!is.null(X1)){
    if(any(is.na(X1))) stop("missing data in the covariates affecting the initial probabilities are not allowed")
  }

  if(!is.null(X2)){
    if(any(is.na(X2))) stop("missing data in the covariates affecting the transition probabilities are not allowed")
  }
  # if(is.null(n)){
  #   if(is.null(est$ns)) n = est$n else n = est$ns #SP #n = nrow(X2)
  # }
  # if(is.null(TT)) TT = est$TT #SP #TT = dim(X2)[2]+1

  if(is.null(n)) n = length(unique(data[,which(names(data)==index[1])])) #n = nrow(X2)
  if(is.null(TT)) TT = max(data[,which(names(data)==index[2])]) #TT = dim(X2)[2]+1
  
  if(is.vector(Mu)){
    r = 1
    k = length(Mu)
    Mu = matrix(Mu,r,k)
    Si = matrix(Si,r,r)
  }else{
    r = nrow(Mu)
    k = ncol(Mu)
  }

# Covariate structure and related matrices: initial probabilities
  if(is.null(X1)){
    nc1=0
    Xlab = rep(1,n)
  }else{  
    if(is.vector(X1)) X1 = matrix(X1,n,1)
    nc1 = dim(X1)[2] # number of covariates on the initial probabilities
    out =  aggr_data(X1)
    Xdis = out$data_dis
    if(nc1==1) Xdis = matrix(Xdis,length(Xdis),1)
    Xlab = out$label
  }
  
  if(k == 2){
    GBe = as.matrix(c(0,1))
  }else{
    GBe = diag(k); GBe = GBe[,-1]
  }
  
  Xndis = max(Xlab)
  XXdis = array(0,c(k,(k-1)*nc1,Xndis))
  for(i in 1:Xndis){
    if(nc1==0) xdis = 1 else xdis = Xdis[i,]
    XXdis[,,i] = GBe%*%(diag(k-1)%x%t(xdis))
  }

  # for the transition probabilities
  if(is.null(X2)){
    nc2 = 0
    Zlab = rep(1,n*(TT-1))
    Zndis = max(Zlab)
  }else{
    if(is.matrix(X2)) X2 = array(X2,c(n,TT-1,1))
    nc2 = dim(X2)[3] # number of covariates on the transition probabilities
    Z = NULL
    for(t in 1:(TT-1)) Z = rbind(Z,X2[,t,])
    if(nc2==1) Z = as.vector(X2)   
    out =  aggr_data(Z); Zdis = out$data_dis; Zlab = out$label; Zndis = max(Zlab)
    if(nc2==1) Zdis=matrix(Zdis,length(Zdis),1)
  }  

  if(param=="multilogit"){
    ZZdis = array(0,c(k,(k-1)*nc2,Zndis,k))
    for(h in 1:k){
      if(k==2){
        if(h == 1) GGa = as.matrix(c(0,1)) else GGa = as.matrix(c(1,0))
      }else{
        GGa = diag(k); GGa = GGa[,-h]
      }
      for(i in 1:Zndis){
        if(nc2==0) zdis = 1 else zdis = Zdis[i,]
        ZZdis[,,i,h] = GGa%*%(diag(k-1)%x%t(zdis))
      }
    }
  }else if(param=="difflogit"){
    Zlab = (((Zlab-1)*k)%x%rep(1,k))+rep(1,n*(TT-1))%x%(1:k)
    ZZdis = array(0,c(k,k*(k-1)+(k-1)*nc2,Zndis*k))
    j = 0
    for(i in 1:Zndis){
      for(h in 1:k){
        j = j+1
        if(k==2){
          if(h == 1) GGa = as.matrix(c(0,1)) else GGa = as.matrix(c(1,0))
        }else{
          GGa = diag(k); GGa = GGa[,-h]
        }
        u = matrix(0,1,k); u[1,h] = 1
        U = diag(k); U[,h] = U[,h]-1
        U = U[,-1]
        ZZdis[,,j] = cbind(u%x%GGa,U%x%t(Zdis[i,]))
      }
    }
  }

# Draw data
  Y = array(0,c(n,TT,r))
  U = matrix(0,n,TT)

# first time occasion
  be = as.vector(Be)
  out =  prob_multilogit(XXdis,be,Xlab,fort)
  Piv = out$P
  for(i in 1:n) U[i,1] = which(rmultinom(1,1,Piv[i,])==1)

# following time occasions
  if(param=="multilogit"){
    if(is.list(Ga)) stop("invalid mode (list) for Ga")
    Ga = matrix(Ga,max(nc2,1)*(k-1),k)
    PIdis = array(0,c(Zndis,k,k)); PI = array(0,c(k,k,n,TT))
    for(h in 1:k){
      tmp = ZZdis[,,,h]
      if(nc2==1) tmp = array(tmp,c(k,(k-1),Zndis))
      out = prob_multilogit(tmp,Ga[,h],Zlab,fort)
      PI[h,,,2:TT] = array(as.vector(t(out$P)),c(1,k,n,TT-1))
    }
  }else if(param=="difflogit"){
    if(is.list(Ga)) Ga = c(as.vector(t(Ga[[1]])),as.vector(Ga[[2]]))
    if(length(Ga)!=k*(k-1)+(k-1)*nc2) stop("invalid dimensions for Ga")
    PI = array(0,c(k,k,n,TT))
    out =  prob_multilogit(ZZdis,Ga,Zlab,fort)
    Tmp = array(out$P,c(k,n,TT-1,k))
    PI[,,,2:TT] = aperm(Tmp,c(1,4,2,3))
  }
  for(i in 1:n) for(t in 2:TT){
    U[i,t] = which(rmultinom(1,1,PI[U[i,t-1],,i,t])==1)
  }

  # draw response variables
  for(i in 1:n) for(t in 1:TT) Y[i,t,] = rmvnorm(1,Mu[,U[i,t]],Si)
  if(exists("R")) Y[!R] = NA

  # output
  if(r==1 & !exists("R")) Y = matrix(Y,n,TT)
  if(format == "long")
  {
    #id <- 1:n
    #Y <- reshape(data = as.data.frame(Y), varying = list(1:TT),ids = id, direction = "long")
    #out <-  aggr_data_long(data = Y[,-c(1,3)], id = Y$id,time = Y$time)
    Y <- matrices2long(Y = Y)
  }

  # S = array(t(S),c(r,TT,length(yv)))
  # S = aperm(S)
  #if(r==1) S = S[,,1]

  yv = rep(1,n)
  out = list(Y=Y, U=U, Mu=Mu, Si=Si, Be=Be, Ga=Ga, latentFormula=latentFormula, data=data, 
             n=n, TT=TT, est=est, yv=yv)

}

draw.LMbasiccont <- function(est,n=NULL,TT=NULL,  format = c("long","matrices"), seed = NULL, ...){

  #        [Y,yv] = draw_lm_basic(piv,Pi,Mu,Si,n)
  #
  # Draw a sample of size n from a Basic Latent Markov model for continuous data with parameter piv, Pi, Mu and Si

  # Preliminaries
  format <- match.arg(format, choices = eval(formals(draw.LMbasiccont)$format))
  if(!is.null(seed)) set.seed(seed)
  piv <- est$piv
  Pi <- est$Pi
  Mu <- est$Mu
  Si <- est$Si

  if(is.vector(Mu)){
    r = 1
    k = length(Mu)
    Mu = matrix(Mu,r,k)
  }else{
    r = nrow(Mu)
    k = ncol(Mu)
  }
  if(is.null(n)) n = ifelse(is.null(est$ns), est$n, est$ns)
  
  if(is.null(TT)) TT = est$TT #TT = dim(Pi)[3]
  if(r==1) Si = matrix(Si,r,r)
  
# For each subject
  Y = array(0,c(n,TT,r))
  cat("------------|\n")
  cat(" sample unit|\n")
  cat("------------|\n")
  for(i in 1:n){
    if(i/1000==floor(i/1000)) cat(sprintf("%11g",i),"\n",sep=" | ")
    if(k==1){
      u = 1
      Y[i,1,] = rmvnorm(1,Mu[,u],Si)
    }else{
      u = k+1-sum(runif(1)<cumsum(piv))
      Y[i,1,] = rmvnorm(1,Mu[,u],Si)
    }
    for(t in 2:TT){
      if(k==1){
        u = 1
        Y[i,t,] = rmvnorm(1,Mu[,u],Si)
      }else{
        u = k+1-sum(runif(1)<cumsum(Pi[u,,t]))
        Y[i,t,] = rmvnorm(1,Mu[,u],Si)
      }
    }
  }
  if(i/1000>floor(i/1000)) cat(sprintf("%11g",i),"\n",sep=" | ")
  if(format == "long"){
    # id <- 1:n
    # Y <- reshape(data = as.data.frame(Y), varying = list(1:TT),ids = id, direction = "long")
    # Y <- as.data.frame(Y)
    Y <- matrices2long(Y = Y)
  }
  yv = rep(1,n)
  cat("------------|\n")
  out = list(Y = Y, piv = piv, Pi = Pi, Mu = Mu, Si = Si, n = n, TT = TT,
             est = est, yv = yv)
  return(out)

}

draw.LMmixed <- function(est,n=NULL,TT=NULL,format = c("long", "matrices"), seed = NULL, ...){

  #        [Y,S,yv] = draw_lm_mixed(la,Piv,Pi,Psi,n,TT)
  #
  # Draw a sample of size n from a mixed Latent Markov model with specific parameters
  format <- match.arg(format, choices = eval(formals(draw.LMbasic)$format))

  # Preliminaries
  if(!is.null(seed)) set.seed(seed)
  Piv <- est$Piv
  Pi <- est$Pi
  Psi <- est$Psi
  la <- est$la

  k1 = length(la)
  k2 = nrow(Piv)
  dd = dim(Psi)
  l = dim(Psi)[1]
  if(length(dd)>2) r = dd[3] else r = 1
  Psi = array(Psi,c(l,k2,r))
  if(is.null(n)) n=est$n
  if(is.null(TT)) TT=est$TT
  # # For each subject
  Y = matrix(0,n,TT*r)
  cat("------------|\n")
  cat(" sample unit|\n")
  cat("------------|\n")
  for(i in 1:n){
    if(i/100==floor(i/100)) cat(sprintf("%11g",i),"\n",sep=" | ")
    u = k1+1-sum(runif(1)<cumsum(la))
    v = k2+1-sum(runif(1)<cumsum(Piv[,u]))
    ind = 0
    for(j in 1:r){
      ind = ind+1
      Y[i,ind] = l-sum(runif(1)<cumsum(Psi[,v,j]))
    }
    for(t in 2:TT){
      v = k2+1-sum(runif(1)<cumsum(Pi[v,,u])) #check se ok k2
      for(j in 1:r){
        ind = ind+1
        Y[i,ind] = l-sum(runif(1)<cumsum(Psi[,v,j]))
      }
    }
  }
  if(i/100>floor(i/100)) cat(sprintf("%11g",i),"\n",sep=" | ")
  cat("------------|\n")
  if(format == "matrices"){
    out = aggr_data(Y)
    S = out$data_dis
    yv = out$freq
    S = array(t(S), c(r, TT, length(yv)))
    S = aperm(S)
    if (r == 1){S = S[, , 1]}
  }
  if(format == "long"){
    S = array(t(Y), c(r, TT, n))
    S = aperm(S)
    if (r == 1){S = S[, , 1]}
    id <- 1:n
    #Y <- reshape(data = as.data.frame(Y), varying = list(1:TT),ids = id, direction = "long")
    Y <- matrices2long(Y = S)
    out <-  aggr_data_long(data = Y[,-c(1,2)], id = Y$id,time = Y$time)
    S = out$Y
    Y = as.data.frame(Y)
    S = as.data.frame(S)
    yv = out$freq
  }

  # S = array(t(S),c(r,TT,length(yv)))
  # S = aperm(S)
  #if(r==1) S = S[,,1]

  out = list(Y = Y, S = S, yv = yv, la = la, Piv = Piv, Pi = Pi, Psi = Psi, n = n, TT = TT,
             est = est)

}
