#' Dynamic Factor Model by Principal Components
#'
#' The function estimates the Dynamic Factor Model by Principal Components and by the estimator of Lam et al. (2011).
#'
#' @param x T by k data matrix: T data points in rows with each row being data at a given time point,
#' and k time series in columns.
#' @param stand Data standardization. The default is stand = 0 and x is not transformed,
#' if stand = 1 each column of x has zero mean an if stand=2 also unit variance.
#' @param mth Method to estimate the number of factors and the common component (factors and loadings):
#' \itemize{
#' \item mth = 0 - the number of factors must be given by the user and
#' the model is estimated by Principal Components.
#' \item mth = 1 - the number of factors must be given by the user and
#' the model is estimated using Lam et al. (2011) methodology.
#' \item mth = 2 - the number of factors is estimated using Bai and Ng (2002) ICP1 criterion and
#' the model is estimated by Principal Components.
#' \item mth = 3 - the number of factors is estimated using Bai and Ng (2002) ICP1 criterion and
#' the model is estimated using Lam et al. (2011) methodology.
#' \item mth = 4 - the number of factors is estimated by applying once the Lam and Yao (2012) criterion and
#' the model is estimated using Lam et al. (2011) methodology (default method).
#' \item mth = 5 - the number of factors is estimated using Ahn and Horenstein (2013) test and
#' the model is estimated by Principal Components.
#' \item mth = 6 - the number of factors is estimated using  Caro and Peña (2020) test and
#' the model is estimated using Lam et al. (2011) methodology with the combined correlation matrix.
#' }
#' @param r Number of factors, default value is estimated by Lam and Yao (2012) criterion.
#' @param lagk Maximum number of lags considered in the combined matrix.
#' The default is lagk = 3.
#'
#' @return A list with the following items:
#' \itemize{
#' \item r - Estimated number of common factors, if mth=0, r is given by the user.
#' \item F -  Estimated common factor matrix (T x r).
#' \item L - Estimated loading  matrix (k x r).
#' \item E - Estimated noise matrix  (T x k).
#' \item VarF - Proportion of variability explained by the factor and the accumulated sum.
#' \item MarmaF - Matrix giving the number of AR, MA, seasonal AR and seasonal MA coefficients
#' for the Factors, plus the seasonal period and the number of non-seasonal and seasonal differences.
#' \item MarmaE - Matrix giving the number of AR, MA, seasonal AR and seasonal MA coefficients for the noises,
#' plus the seasonal period and the number of non-seasonal and seasonal differences.
#' }
#'
#' @importFrom MASS mvrnorm
#'
#' @examples
#' data(TaiwanAirBox032017)
#' dfm1 <- dfmpc(as.matrix(TaiwanAirBox032017[1:100,1:30]), mth=4)
#'
#' @references Ahn, S. C. and Horenstein, A. R. (2013). Eigenvalue ratio test for the number
#' of factors. \emph{Econometrica}, 81(3):1203–1227.
#'
#' Bai, J. and Ng, S. (2002). Determining the number of factors in approximate factor
#' models. \emph{Econometrica}, 70(1):191–221.
#'
#' Caro, A. and Peña, D. (2020). A test for the number of factors in dynamic factor models.
#' UC3M Working papers. Statistics and Econometrics.
#'
#' Lam, C. and Yao, Q. (2012). Factor modeling for high-dimensional time series:
#' inference for the number of factors. \emph{The Annals of Statistics}, 40(2):694–726.
#'
#' Lam, C., Yao, Q., and Bathia, N. (2011). Estimation of latent factors for
#' high-dimensional time series. \emph{Biometrika}, 98(4):901–918.
#'
#' @export
dfmpc <- function(x, stand = 0, mth = 4, r, lagk = 0){

  if(stand == 0)
    x <- x

  if(stand == 1)
    x <- scale(x, center = TRUE, scale = FALSE)

  if(stand == 2)
    x <- scale(x, center = TRUE, scale = TRUE)

  N <- ncol(x)
  T <- nrow(x)
  rmax <- round(N/2,digits = 0)

  if (lagk == 0){
    k0 <- 3
  }else{
    k0 <- lagk
  }

  if (mth == 0){

    if(missing(r)) {
      warning("the number of factors, r, must be given by the user.")
      stop()
    }

    if(r==0) {
      warning("the number of factors must be larger than 0")
      stop()
    }

    output <- PCest(x,r)

  }

  if (mth == 1){

    if(missing(r)) {
      warning("the number of factors, r, must be given by the user.")
      stop()
    }

    if(r==0) {
      warning("the number of factors must be larger than 0")
      stop()
    }

    output <- Mkest(x,r,k0)

  }

  if (mth == 2){

    r <- BaiNgcrit(x, kmax = rmax, mtx = "PC")

    output <- PCest(x,r)

  }

  if (mth == 3){

    r <- BaiNgcrit(x, kmax = rmax, mtx = "Mk")

    output <- Mkest(x,r,k0)

  }

  if (mth == 4){

    r <- LamYaotest(x, k0, rmax)$r1

    output <- Mkest(x,r,k0)

  }

  if (mth == 5){

    r <- AhnHortest(x, rmax)$r1

    output <- PCest(x,r)

  }

  if (mth == 6) {

    r <- CaroPenatest(x, k0, rmax)$r1

    output <- Mkest_CP(x,r,k0)

  }

  F <- output$F
  L <- output$L
  E <- output$E
  varF <- output$varF[1:r,]

  armamdlF <- matrix(0,ncol = 7,nrow = ncol(F))
  colnames(armamdlF) <- c("AR","MA","sAR","sMA","period","d","sd")
  for (i in 1:ncol(F)){
    f <- F[,i]
    outputF <- auto.arima(f)
    armamdlF[i,] <- outputF$arma
  }

  armamdlE <- matrix(0,ncol = 7,nrow = ncol(E))
  colnames(armamdlE) <- c("AR","MA","sAR","sMA","period","d","sd")
  for (i in 1:ncol(E)){
    e <- E[,i]
    outputE <- auto.arima(e)
    armamdlE[i,] <- outputE$arma
  }

  varE <- diag(round(var(E),digits = 2))
  armamdlE <- cbind(armamdlE,varE)

  MarmaF <- armamdlF
  MarmaE <- armamdlE

  result <- list( r, F, L, E, varF, MarmaF,MarmaE)
  names(result) <- c( "r", "F", "L", "E","varF","MarmaF","MarmaE")
  class(result) <- "dfm"
  return(result)
}

BaiNgcrit <- function(x, kmax, mtx = "PC"){
  N <- dim(x)[1]
  P <- dim(x)[2]

  if(missing(kmax)) kmax <- round(P/2,digits = 0)
  if(missing(mtx)) mtx <- "PC"

  ICPk <- matrix(0, ncol = 2, nrow = (kmax))

  if (mtx == "PC"){
    for (r in 1:kmax){

      out <- PCest(x, r)
      ehat <- out$E

      Vfk <- mean(colMeans(ehat^2))
      CNT2 <- min(c(P,N))
      ICPk[(r),1] <- log(Vfk) + r*((P+N)/(P*N))*log((P*N)/(P+N))
      ICPk[(r),2] <- log(Vfk) + r*((P+N)/(P*N))*log(CNT2)
    }
  }

  if (mtx == "Mk"){
    for (r in 1:kmax){

      out <- Mkest(x, r, k0=3)
      ehat <- out$E

      Vfk <- mean(colMeans(ehat^2))
      CNT2 <- min(c(P,N))
      ICPk[(r),1] <- log(Vfk) + r*((P+N)/(P*N))*log((P*N)/(P+N))
      ICPk[(r),2] <- log(Vfk) + r*((P+N)/(P*N))*log(CNT2)
    }
  }

  rhat <- cbind(which(ICPk[,1] == min(ICPk[,1])),which(ICPk[,2] == min(ICPk[,2])))

  return(rhat[1])
}

LamYaotest <- function (X, k0, kmax){
  N <- dim(X)[1]
  P <- dim(X)[2]

  if(missing(kmax)) kmax <- round(P/2,digits = 0)

  if (k0==1){
    dW=cov(X[2:N,], X[1:(N-1),])
    Wy=dW%*%t(dW)
    eA=eigen(Wy, symmetric=T)
    ev=sort(eA$values, decreasing = TRUE)
  } else {
    dW=cov(X[2:N,], X[1:(N-1),])
    Wy=dW%*%t(dW)
    for(k in 2:k0) { dW=cov(X[(k+1):N,], X[1:(N-k),])
    Wy=Wy + dW%*%t(dW)
    }
    eA=eigen(Wy, symmetric=T)
    ev=sort(eA$values, decreasing = TRUE)
  }
  ratio <- rep(0,kmax)
  for (j in 1:kmax){
    ratio[j] <- ev[j]/ev[(j+1)]
  }
  r1 = which.max(ratio)

  results <- list(r1=r1, ratio=ratio, Wy=Wy)
  return(results)
}

AhnHortest <- function (X, kmax){
  N <- dim(X)[1]
  P <- dim(X)[2]

  if(missing(kmax)) kmax <- round(P/2,digits = 0)

  Wy = var(X)
  eA = eigen(Wy, symmetric=T)
  ev = sort(eA$values, decreasing = TRUE)

  ratio <- rep(0,kmax)
  for (j in 1:kmax){
    ratio[j] <- ev[j]/ev[(j+1)]
  }
  r1 = which.max(ratio)
  results <- list(r1=r1, ratio=ratio, Wy=Wy)
  return(results)
}

CaroPenatest <- function (X, k0, kmax){
  N <- dim(X)[1]
  P <- dim(X)[2]

  if(missing(kmax)) kmax <- round(P/2,digits = 0)

  wk0 = N/((k0+1)*(N-(k0/2)))
  Wy = wk0 * cor(X) %*%t (cor(X))
  dW = acf(X, lag.max = k0, type = "correlation", plot = FALSE)

  for(k in 1:k0) {
    dWk = dW$acf[(k+1),,]
    wk = (N-k)/((k0+1)*(N-(k0/2)))
    Wy = Wy + wk * dWk %*% t(dWk)
  }
  eA=eigen(Wy, symmetric=T)
  ev=sort(eA$values, decreasing = TRUE)

  ratio <- rep(0,kmax)
  for (j in 1:kmax){
    ratio[j] <- ev[j]/ev[(j+1)]
  }
  r1 = which.max(ratio)
  results <- list(r1=r1, ratio=ratio, Wy=Wy)
  return(results)
}

PCest <- function (X, r){

  T <- nrow(X)
  N <- ncol(X)

  if(T <= N){

    XXt <- X%*%t(X)
    ED <- eigen(XXt)
    index <- order(ED$values[1:r], decreasing = TRUE)

    L <- ED$vectors[,index]

    fhat <- L*sqrt(T)

    lambdahat <- t(X)%*%fhat/T

    norm <- round(t(fhat)%*%fhat/T)
  }else{
    XtX <- t(X)%*%X
    ED <- eigen(XtX)
    index <- order(ED$values[1:r], decreasing = TRUE)

    L <- ED$vectors[,index]

    lambdahat <- L*sqrt(N)

    fhat <- X%*%lambdahat/N

    norm <- round(t(lambdahat)%*%lambdahat/N)
  }

  fhat <- as.matrix(fhat)
  colnames(fhat) <- paste("f",1:r, sep = "")

  chat <- fhat%*%t(lambdahat)
  ehat <- X-chat

  F <- fhat
  L <- lambdahat
  E <- ehat
  eigvalues <- ED$values

  pvar <- eigvalues*100/sum(eigvalues)
  cumvar <- cumsum(pvar)
  varF <- data.frame(var=round(pvar,digits = 3), cumvar=round(cumvar,digits = 3))

  result <- list( r, F, L, E, varF)
  names(result) <- c( "r", "F", "L", "E","varF")
  class(result) <- "dfm"
  return(result)
}

Mkest <- function(X, r, k0){

  T <- nrow(X)
  N <- ncol(X)

  dW=cov(X[2:T,], X[1:(T-1),])
  Wy=dW%*%t(dW)
  for(k in 2:k0) { dW=cov(X[(k+1):T,], X[1:(T-k),])
  Wy=Wy + dW%*%t(dW)}

  Mk=Wy

  EDMk <- eigen(Mk)
  indexMk <- order(EDMk$values[1:r], decreasing = TRUE)

  LMk <- EDMk$vectors[,indexMk]

  lambdahatMk <- LMk

  fhatMk <- X%*%lambdahatMk

  normMk <- round(t(lambdahatMk)%*%lambdahatMk)

  fhatMk <- as.matrix(fhatMk)
  colnames(fhatMk) <- paste("f",1:r, sep = "")

  chatMk <- fhatMk%*%t(lambdahatMk)
  ehatMk <- X-chatMk
  eigvaluesMk <- EDMk$values

  FMk <- fhatMk
  LMk <- lambdahatMk
  EMk <- ehatMk


  pvarMk <- eigvaluesMk*100/sum(eigvaluesMk)
  cumvarMk <- cumsum(pvarMk)
  varFMk <- data.frame(varMk=round(pvarMk,digits = 3), cumvarMk=round(cumvarMk,digits = 3))



  resultMk <- list(FMk, LMk, EMk, varFMk)
  names(resultMk) <- c( "F", "L", "E","varF")
  class(resultMk) <- "dfm"
  return(resultMk)
}

Mkest_CP <- function(X, r, k0){

  N <- dim(X)[1]
  P <- dim(X)[2]

  wk0 = N/((k0+1)*(N-(k0/2)))
  Wy = wk0 * cor(X) %*% t(cor(X))
  dW = acf(X, lag.max = k0, type = "correlation", plot = FALSE)

  for(k in 1:k0) {
    dWk = dW$acf[(k+1),,]
    wk = (N-k)/((k0+1)*(N-(k0/2)))
    Wy = Wy + wk * dWk %*% t(dWk)
  }

  Mk=Wy

  EDMk <- eigen(Mk)
  indexMk <- order(EDMk$values[1:r], decreasing = TRUE)

  lambdahatMk <- EDMk$vectors[,indexMk]

  fhatMk <- X%*%lambdahatMk

  normMk <- round(t(lambdahatMk)%*%lambdahatMk)

  fhatMk <- as.matrix(fhatMk)
  colnames(fhatMk) <- paste("f",1:r, sep = "")

  chatMk <- fhatMk%*%t(lambdahatMk)
  ehatMk <- X-chatMk
  eigvaluesMk <- EDMk$values


  FMk <- fhatMk
  LMk <- lambdahatMk
  EMk <- ehatMk

  pvarMk <- eigvaluesMk*100/sum(eigvaluesMk)
  cumvarMk <- cumsum(pvarMk)
  varFMk <- data.frame(varMk=round(pvarMk,digits = 3), cumvarMk=round(cumvarMk,digits = 3))

  resultMk <- list(FMk, LMk, EMk, varFMk)
  names(resultMk) <- c( "F", "L", "E","varF")
  class(resultMk) <- "dfm"
  return(resultMk)
}
