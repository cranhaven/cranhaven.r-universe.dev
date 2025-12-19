
#' Minimum distance estimation in linear regression model.
#'
#' Estimates the regression coefficients in the model \eqn{Y=X\beta + \epsilon}.
#'@param Y - Vector of response variables in linear regression model.
#'@param X - Design matrix of explanatory variables in linear regression model.
#'@param D - Weight Matrix. Dimension of D should match that of X. Default value is XA where A=(X'X)^(-1/2).
#'@param b0 - Initial value for beta.
#'@param IntMeasure  - Symmetric and \eqn{\sigma}-finite measure: Lebesgue, Degenerate, and Robust
#'@param TuningConst - Used only for Robust measure.
#'@return betahat    - Minimum distance estimator of \eqn{\beta}.
#'@return residual   - Residuals after minimum distance estimation.
#'@return ObjVal     - Value of the objective function at minimum distance estimator.
#'@examples
#'####################
#'n <- 10
#'p <- 3
#'X <- matrix(runif(n*p, 0,50), nrow=n, ncol=p)  #### Generate n-by-p design matrix X
#'beta <- c(-2, 0.3, 1.5)                        #### Generate true beta = (-2, 0.3, 1.5)'
#'eps <- rnorm(n, 0,1)                           #### Generate errors from N(0,1)
#'Y <- X%*%beta + eps
#'
#'D <- "default"                                 #### Use the default weight matrix
#'b0 <- solve(t(X)%*%X)%*%(t(X)%*%Y)             #### Set initial value for beta
#'IntMeasure <- "Lebesgue"                       ##### Define Lebesgue measure
#'
#'
#'MDEResult <- KoulLrMde(Y,X,D, b0, IntMeasure, TuningConst=1.345)
#'
#'betahat <- MDEResult$betahat                   ##### Obtain minimum distance estimator
#'resid <- MDEResult$residual                    ##### Obtain residual
#'objVal <- MDEResult$ObjVal                     ##### Obtain the value of the objective function
#'
#'
#'IntMeasure <- "Degenerate"                     ##### Define degenerate measure at 0
#'
#'MDEResult <- KoulLrMde(Y,X,D, b0, IntMeasure, TuningConst=1.345)
#'betahat <- MDEResult$betahat                   ##### Obtain minimum distance estimator
#'resid <- MDEResult$residual                    ##### Obtain residual
#'objVal <- MDEResult$ObjVal                     ##### Obtain the value of the objective function
#'
#'
#'
#'IntMeasure <- "Robust"                        ##### Define "Robust" measure
#'TuningConst <- 3                              ##### Define the tuning constant
#'MDEResult <- KoulLrMde(Y,X,D, b0, IntMeasure, TuningConst)
#'
#'
#'betahat <- MDEResult$betahat                   ##### Obtain minimum distance estimator
#'resid <- MDEResult$residual                    ##### Obtain residual
#'objVal <- MDEResult$ObjVal                     ##### Obtain the value of the objective function


#'@references
#'[1] Kim, J. (2018). A fast algorithm for the coordinate-wise minimum distance estimation. J. Stat. Comput. Simul., 3: 482 - 497
#'@references
#'[2] Kim, J. (2020). Minimum distance estimation in linear regression model with strong mixing errors. Commun. Stat. - Theory Methods., 49(6): 1475 - 1494
#'@references
#'[3] Koul, H. L (1985). Minimum distance estimation in linear regression with unknown error distributions. Statist. Probab. Lett., 3: 1-8.
#'@references
#'[4] Koul, H. L (1986). Minimum distance estimation and goodness-of-fit tests in first-order autoregression. Ann. Statist., 14 1194-1213.
#'@references
#'[5] Koul, H. L (2002). Weighted empirical process in nonlinear dynamic models. Springer, Berlin, Vol. 166
#'@export
#'@seealso KoulArMde() and Koul2StageMde()
#'@importFrom Rcpp evalCpp
#'@importFrom expm sqrtm
#'@useDynLib KoulMde


######################################################################


KoulLrMde <- function(Y, X, D, b0, IntMeasure, TuningConst=1.345){

  if( (nargs() != 5) && (nargs() != 6) ){
    message("Number of arguments should be five or six.")
    stop()
  }


  if(IntMeasure == "Robust"){

    if(is.numeric(TuningConst) == FALSE ){
      message("Tuning constant should be numeric. Default value will be tried.")
      TuningConst = 1.345
    }else{

      if(TuningConst <= 0){
        message("Tuning constant should be positive. Default value will be tried.")
        TuningConst = 1.345
      }
    }


  }



  if (is.vector(X) == TRUE ){

    nXRow <- length(X)
    nXCol <- 1
    LengY <- length(Y)

    XMat <- matrix(X, nXRow, nXCol)

    if (nXRow != LengY){
      message("Dimension of X does not match dimension of Y.")
      stop()
    }

    if(is.vector(D) == TRUE){
      nDRow <-  length(D)
      nDCol <- 1

      DMat <- matrix(D, nDRow, nDCol)


    }else{
      message("When X is a vector, D should be a vector too.")
      stop()
    }

    if(nDRow != nXRow){
      str= paste("D should be ", nXRow, "-by-1 vector.")
      message(str)
      stop()
    }

  }else if(is.matrix(X) == TRUE){
    DimMat <- dim(X)

    LengY <- length(Y)

    nXRow <- DimMat[1]
    nXCol <- DimMat[2]

    XMat <- X


    if(is.matrix(D) == TRUE){
      DDimMat <- dim(D)
    }else if(D == "default"){
      tempA <- (t(X)%*%X)
      A <- sqrtm(solve(tempA))
      #A <- sqrtmat(tempA, -0.5)
      D <- X%*%A
    }else{
      message("D should be a matrix.")
      stop()
    }

    DMat <- D

    DDimMat <- dim(D)
    nDRow <- DDimMat[1]
    nDCol <- DDimMat[2]

    if (nXRow != LengY){
      message("Dimension of X does not match dimension of Y.")
      stop()
    }

    if (nXCol != length(b0) ){
      message("b0 is not conformable to X.")
      stop()
    }

    if( (nXRow != nDRow) || ((nXCol != nDCol)) ) {
      message("Dimesion of D should match dimension of X.")
      stop()
    }

  }else{
    message("X is not a valid design matrix.")
    stop()

  }

  iter <- 3000
  critVal <- 0.001

  type = 0

  if(IntMeasure == "Lebesgue"){
    type = 1
  }else if(IntMeasure == "Robust"){
    type = 2
  }else if(IntMeasure == "Degenerate"){
    type = 3
  }else{
    message("Integrating measure should be Lebesgue, Degenerate or Robust.")
    stop()
  }

  YMat = matrix(Y, LengY, 1)
  b0Mat = matrix(b0, nXCol, 1)


  bhat_ObjVal <- EstimateBetaMDESimple(YMat, XMat, DMat, b0Mat, iter, critVal, type, TuningConst)

  bhat <- bhat_ObjVal[1:nXCol]
  ObjVal <- bhat_ObjVal[(nXCol+1)]


  if (is.vector(X) == TRUE ){
    res <- YMat - XMat%*% bhat
  }else{
    res <- YMat - XMat %*% bhat
  }

  lst = list(betahat=bhat, residual = res, ObjVal = ObjVal)
  return(lst)

}



####################################

#' Minimum distance estimation in the autoregression model of the known order.
#'
#' Estimates the autoressive coefficients in the \eqn{X_t = \rho' Z_t + \xi_t } where \eqn{Z_t} is the vector of \eqn{q} observations at times \eqn{t-1,...,t-q}.
#'@param X - Vector of \code{n} observed values.
#'@param AR_Order    - Order of the autoregression model.
#'@param IntMeasure  - Symmetric and \eqn{\sigma}-finite measure: Lebesgue, Degenerate, and Robust
#'@param TuningConst - Used only for Robust measure.
#'@return rhohat     - Minimum distance estimator of \eqn{\rho}.
#'@return residual   - Residuals after minimum distance estimation.
#'@return ObjVal     - Value of the objective function at minimum distance estimator.
#'@examples
#'##### Generate stationary AR(2) process with 10 observations
#'n <- 10
#'q <- 2
#'rho <- c(-0.2, 0.8)    ##### Generate true parameters rho = (-0.2, 0.8)'
#'eps <- rnorm(n, 0,1)   ##### Generate innovations from N(0,1)
#'X <- rep(0, times=n)
#'for (i in 1:n){
#'  tempCol <- rep(0, times=q)
#'  for (j in 1:q){
#'    if(i-j<=0){
#'      tempCol[j] <- 0
#'    }else{
#'      tempCol[j] <- X[i-j]
#'    }
#'  }
#'X[i] <- t(tempCol)%*% rho + eps[i]
#'}
#'
#'IntMeasure <- "Lebesgue"                       ##### Define Lebesgue measure
#'
#'MDEResult <- KoulArMde(X, q, IntMeasure, TuningConst=1.345)
#'rhohat <- MDEResult$rhohat                     ##### Obtain minimum distance estimator
#'resid  <- MDEResult$residual                   ##### Obtain residual
#'objVal <- MDEResult$ObjVal                     ##### Obtain the value of the objective function
#'
#'
#'IntMeasure <- "Degenerate"                     ##### Define degenerate measure at 0
#'MDEResult <- KoulArMde(X, q, IntMeasure, TuningConst=1.345)
#'rhohat <- MDEResult$rhohat                     ##### Obtain minimum distance estimator
#'resid <- MDEResult$residual                    ##### Obtain residual
#'objVal <- MDEResult$ObjVal                     ##### Obtain the value of the objective function
#'
#'
#'IntMeasure <- "Robust"                         ##### Define "Robust" measure at 0
#'TuningConst <- 3                               ##### Define the tuning constant
#'MDEResult <- KoulArMde(X, q, IntMeasure, TuningConst)
#'
#'resid <- MDEResult$residual                    ##### Obtain residual
#'objVal <- MDEResult$ObjVal                     ##### Obtain the value of the objective function
#'
#'@references
#'[1] Kim, J. (2018). A fast algorithm for the coordinate-wise minimum distance estimation. J. Stat. Comput. Simul., 3: 482 - 497
#'@references
#'[2] Kim, J. (2020). Minimum distance estimation in linear regression model with strong mixing errors. Commun. Stat. - Theory Methods., 49(6): 1475 - 1494
#'@references
#'[3] Koul, H. L (1985). Minimum distance estimation in linear regression with unknown error distributions. Statist. Probab. Lett., 3: 1-8.
#'@references
#'[4] Koul, H. L (1986). Minimum distance estimation and goodness-of-fit tests in first-order autoregression. Ann. Statist., 14 1194-1213.
#'@references
#'[5] Koul, H. L (2002). Weighted empirical process in nonlinear dynamic models. Springer, Berlin, Vol. 166
#'@export
#'@seealso KoulLrMde() and Koul2StageMde()





KoulArMde <- function(X, AR_Order, IntMeasure, TuningConst=1.345){

  Hx = IntMeasure

  if(IntMeasure == "Robust"){

    if(is.numeric(TuningConst) == FALSE ){
      message("Tuning constant should be numeric. Default value will be tried.")
      TuningConst = 1.345
    }else{

      if(TuningConst <= 0){
        message("Tuning constant should be positive. Default value will be tried.")
        TuningConst = 1.345
      }
    }


  }



  if ( (Hx != "Lebesgue") && (Hx != "Degenerate") && (Hx != "Robust")){
    message("Integrating measure should be Lebesgue, Degenerate or Robust.")
    stop()
  }

  nLength <- length(X)

  if(nLength <= AR_Order){
    message("Length of vector X should be greater than AR_Order.")
    stop()
  }

  Xres <- rep(0, times=(nLength-AR_Order))
  tempvec <- rep(0, times= AR_Order*(nLength-AR_Order) )
  Xexp <- matrix( tempvec, nrow = (nLength-AR_Order), ncol = AR_Order)

  Dmat <- matrix( tempvec, nrow = (nLength-AR_Order), ncol = AR_Order)

  for (i in 1:(nLength-AR_Order) ) {
    Xres[i] <- X[nLength - (i-1)]
    for (j in 1:AR_Order){
      Xexp[i,j] <- X[nLength-(i+j-1) ]
      Dmat[i,j] <- X[nLength-(i+j-1) ] / sqrt(nLength-AR_Order)
    }
  }

  XresMat <- matrix(Xres, (nLength-AR_Order), 1)

  tempdet <- det(  t(Xexp) %*% Xexp )
  if (  tempdet < 0.01 ){
    rho0 <- 0.5*rep(1, times = AR_Order)
  }else{
    rho0 <- solve(t(Xexp)%*%Xexp)%*% (t(Xexp)%*%Xres)
  }

  rho0Mat <- matrix(rho0, AR_Order, 1)

  iter=1000
  critVal=0.001

  nXRow = nLength-AR_Order
  nXCol = AR_Order


  type = 0

  if(IntMeasure == "Lebesgue"){
    type = 1
  }else if(IntMeasure == "Robust"){
    type = 2
  }else if(IntMeasure == "Degenerate"){
    type = 3
  }else{
    message("Integrating measure should be Lebesgue, Degenerate or Robust.")
    stop()
  }

  rhohat_ObjVal <- EstimateBetaMDESimple(XresMat, Xexp, Dmat, rho0Mat, iter, critVal, type, TuningConst)

  rho_hat <- rhohat_ObjVal[1:AR_Order]
  ObjVal <- rhohat_ObjVal[(AR_Order+1)]


  resid <- XresMat - Xexp%*% rho_hat

  lst <- list(rhohat=rho_hat, residual=resid, ObjVal=ObjVal)

  return(lst)
}


#'Two-stage minimum distance estimation in linear regression model with autoregressive error.
#'
#'Estimates both regression and autoregressive coefficients in the model \eqn{Y=X\beta + \epsilon} where \eqn{\epsilon} is autoregressive process of known order \code{q}
#'@param Y - Vector of response variables in linear regression model.
#'@param X - Design matrix of explanatory variables in linear regression model.
#'@param D - Weight Matrix. Dimension of D should match that of X. Default value is XA where A=(X'X)^(-1/2).
#'@param b0 - Initial value for beta.
#'@param RegIntMeasure - Symmetric and \eqn{\sigma}-finite measure used for estimating \eqn{\beta}: Lebesgue, Degenerate or Robust.
#'@param AR_Order - Order of the autoregressive error.
#'@param ArIntMeasure - Symmetric and \eqn{\sigma}-finite measure used for estimating autoregressive coefficients of the error: Lebesgue, Degenerate or Robust.
#'@param TuningConst - Used only for Robust measure.
#'@return MDE1stage - The list of the first stage minimum distance estimation result. It contains betahat1stage, residual1stage, and rho1stage.
#'\itemize{
#'  \item betahat1stage - The first stage minimum distance estimators of regression coefficients.
#'  \item residual1stage - Residuals after the first stage minimum distance estimation.
#'  \item rho1stage - The first stage minimum distance estimators of autoregressive coefficients of the error.
#'}
#'@return MDE2stage - The list of the second stage minimum distance estimation result. It contains betahat2stage, residual2stage, and rho2stage.
#'\itemize{
#'  \item betahat2stage - The second stage minimum distance estimators of regression coefficients.
#'  \item residual2stage - Residuals after the second stage minimum distance estimation.
#'  \item rho2stage - The second stage minimum distance estimators of autoregressive coefficients of the error.
#'}
#'@examples
#'####################
#'n <- 10
#'p <- 3
#'X <- matrix(runif(n*p, 0,50), nrow=n, ncol=p)  #### Generate n-by-p design matrix X
#'beta <- c(-2, 0.3, 1.5)                        #### Generate true beta = (-2, 0.3, 1.5)'
#'rho  <- 0.4                                    #### True rho = 0.4
#'eps <- vector(length=n)
#'xi <- rnorm(n, 0,1)                            #### Generate innovation from N(0,1)
#'                                               #### Generate autoregressive process of order 1
#'for(i in 1:n){
#'  if(i==1){eps[i] <- xi[i]}
#'  else{eps[i] <- rho*eps[i-1] + xi[i]}
#'}
#'Y <- X%*%beta + eps
#'#####################
#'D <- "default"                                  #### Use the default weight matrix
#'b0 <- solve(t(X)%*%X)%*%(t(X)%*%Y)              #### Set initial value for beta
#'
#'IntMeasure <- "Lebesgue"                                ##### Define Lebesgue measure
#'MDEResult <- Koul2StageMde(Y,X, "default", b0, IntMeasure, 1, IntMeasure, TuningConst = 1.345)
#'MDE1stageResult <- MDEResult[[1]]
#'MDE2stageResult <- MDEResult[[2]]
#'
#'beta1 <- MDE1stageResult$betahat1stage
#'residual1 <- MDE1stageResult$residual1stage
#'rho1 <- MDE1stageResult$rhohat1stage
#'
#'beta2 <- MDE2stageResult$betahat2stage
#'residual2 <- MDE1stageResult$residual2stage
#'rho2 <- MDE2stageResult$rhohat2stage



#'@references
#'[1] Kim, J. (2018). A fast algorithm for the coordinate-wise minimum distance estimation. J. Stat. Comput. Simul., 3: 482 - 497
#'@references
#'[2] Kim, J. (2020). Minimum distance estimation in linear regression model with strong mixing errors. Commun. Stat. - Theory Methods., 49(6): 1475 - 1494
#'@references
#'[3] Koul, H. L (1985). Minimum distance estimation in linear regression with unknown error distributions. Statist. Probab. Lett., 3: 1-8.
#'@references
#'[4] Koul, H. L (1986). Minimum distance estimation and goodness-of-fit tests in first-order autoregression. Ann. Statist., 14 1194-1213.
#'@references
#'[5] Koul, H. L (2002). Weighted empirical process in nonlinear dynamic models. Springer, Berlin, Vol. 166
#'@seealso KoulArMde() and KoulLrMde()
#'@export

Koul2StageMde <- function(Y,X,D, b0, RegIntMeasure, AR_Order, ArIntMeasure, TuningConst=1.345){


  DimMat <- dim(X)
  n <- DimMat[1]
  p <- DimMat[2]

  if( (RegIntMeasure == "Robust") || (ArIntMeasure == "Robust") ){

    if(is.numeric(TuningConst) == FALSE ){
      message("Tuning constant should be numeric. Default value will be tried.")
      TuningConst = 1.345
    }else{

      if(TuningConst <= 0){
        message("Tuning constant should be positive. Default value will be tried.")
        TuningConst = 1.345
      }
    }


  }


  MDE1Result <- KoulLrMde(Y,X, D, b0, RegIntMeasure, TuningConst)
  beta1 <- MDE1Result$betahat
  resid1 <- MDE1Result$residual
  objval1 <- MDE1Result$ObjVal


  ArMDE1Result <- KoulArMde(resid1, AR_Order, ArIntMeasure, TuningConst)
  rho1 <- ArMDE1Result$rhohat
  MDE1 <- list(betahat1stage=beta1, residual1stage=resid1, rhohat1stage=rho1, ObjVal1 = objval1)

  ###########################   2 stage MDE
  Ytilde <- vector(length=(n-AR_Order))
  Xtilde <- matrix(rep(0,times=(n-AR_Order)*p), nrow=(n-AR_Order), ncol=p )

  for(j in 1:(n-AR_Order)){

    tempX <- rep(0, times=p)
    tempY <- 0
    for (k in 1: AR_Order){
      tempX <- tempX + rho1[k]*X[AR_Order+j-k, ]
      tempY <- tempY + rho1[k]*Y[AR_Order+j-k]
    }
    Xtilde[j, ] <- X[(j+AR_Order), ] - tempX
    Ytilde[j] <- Y[j+AR_Order] - tempY
  }


  MDE2Result <- KoulLrMde(Ytilde, Xtilde, D, beta1, RegIntMeasure, TuningConst)
  beta2 <- MDE2Result$betahat
  resid2 <- Y-X%*%beta2
  objval2 <- MDE2Result$ObjVal


  ArMDE2Result <- KoulArMde(resid2, AR_Order, ArIntMeasure, TuningConst)
  rho2 <- ArMDE2Result$rhohat

  MDE2 <- list(betahat2stage=beta2, residual2stage=resid2, rhohat2stage=rho2, ObjVal2 = objval2)

  ResultVal <- list(MDE1stage=MDE1, MDE2stage=MDE2)
  return(ResultVal)

}



#' Detecting Non-numeric Values.
#'
#' Check whether or not an input matrix includes any non-numeric values (NA, NULL, "", character, etc) before being used for training. If any non-numeric values exist, then TrainBuddle() or FetchBuddle() will return non-numeric results.
#'@param X an n-by-p matrix.
#'
#'@return A list of (n+1) values where n is the number of non-numeric values. The first element of the list is n, and all other elements are entries of X where non-numeric values occur. For example, when the (1,1)th and the (2,3)th entries of a 5-by-5 matrix X are non-numeric, then the list returned by CheckNonNumeric() will contain 2, (1,1), and (2,3).
#'
#'@examples
#'
#'n = 5;
#'p = 5;
#'X = matrix(0, n, p)       #### Generate a 5-by-5 matrix which includes two NA's.
#'X[1,1] = NA
#'X[2,3] = NA
#'
#'lst = CheckNonNumeric(X)
#'
#'lst
#'
#'@export


CheckNonNumeric = function(X){

  dimm = dim(X)
  n = dimm[1]
  p = dimm[2]


  nInc = 0
  lst = list()
  nIndex=2

  for(i in 1:n){

    for(j in 1:p){

      val = X[i, j]
      if((is.na(val)==TRUE) || is.null(val)==TRUE || is.numeric(val)==FALSE){
        nInc = nInc+1
        lst[[nIndex]] = c(i,j)
        nIndex=nIndex+1
      }
    }
  }

  lst[[1]] = nInc

  return(lst)

}


