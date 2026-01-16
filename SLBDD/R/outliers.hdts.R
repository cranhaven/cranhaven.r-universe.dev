#' Multivariate Outlier Detection
#'
#' Outlier detection in high dimensional time series by using projections as in Galeano, Peña and Tsay (2006).
#'
#' @param x T by k data matrix: T data points in rows with each row being data at a given time point,
#' and k time series in columns.
#' @param r.max The maximum number of factors including stationary and non-stationary.
#' @param type The type of series, i.e., 1 if stationary or 2 if nonstationary.
#'
#' @return A list containing:
#' \itemize{
#'   \item x.clean -  The time series cleaned at the end of the procedure (n x m).
#'   \item P.clean - The estimate of the loading matrix if the number of factors is positive.
#'   \item Ft.clean - The estimated dynamic factors if the number of factors is positive.
#'   \item Nt.clean - The idiosyncratic residuals if the number of factors is positive.
#'   \item times.idi.out - The times of the idiosyncratic outliers.
#'   \item comps.idi.out - The components of the noise affected by the idiosyncratic outliers.
#'   \item sizes.idi.out - The sizes of the idiosyncratic outliers.
#'   \item stats.idi.out - The statistics of the idiosyncratic outliers.
#'   \item times.fac.out - The times of the factor outliers.
#'   \item comps.fac.out - The dynamic factors affected by the factor outliers.
#'   \item sizes.fac.out - The sizes of the factor outliers.
#'   \item stats.fac.out - The statistics of the factor outliers.
#'   \item x.kurt - The time series cleaned in the kurtosis sub-step (n x m).
#'   \item times.kurt - The outliers detected in the kurtosis sub-step.
#'   \item pro.kurt - The projection number of the detected outliers in the kurtosis sub-step.
#'   \item n.pro.kurt - The number of projections leading to outliers in the kurtosis sub-step.
#'   \item x.rand - The time series cleaned in the random projections sub-step (n x m).
#'   \item times.rand - The outliers detected in the random projections sub-step.
#'   \item x.uni - The time series cleaned after the univariate substep (n x m).
#'   \item times.uni - The vector of outliers detected with the univariate substep.
#'   \item comps.uni - The components affected by the outliers detected with the univariate substep.
#'   \item r.rob - The number of factors estimated (1 x 1).
#'   \item P.rob - The estimate of the loading matrix (m x r.rob).
#'   \item V.rob - The estimate of the orthonormal complement to P (m x (m - r.rob)).
#'   \item I.cov.rob - The matrix (V'GnV)^{-1} used to compute the statistics to detect the idiosyncratic outliers.
#'   \item IC.1 - The values of the information criterion of Bai and Ng.
#' }
#'
#' @importFrom corpcor cov.shrink
#' @import forecast
#' @importFrom gsarima arrep
#' @importFrom imputeTS na_ma
#' @import methods
#' @importFrom matrixcalc is.singular.matrix
#' @importFrom Matrix rowSums
#' @importClassesFrom Matrix CsparseMatrix
#'
#' @export
#'
#' @references Galeano, P., Peña, D., and Tsay, R. S. (2006). Outlier detection in
#' multivariate time series by projection pursuit. \emph{Journal of the American Statistical Association}, 101(474), 654-669.
#'
#' @examples
#' data(TaiwanAirBox032017)
#' output <- outliers.hdts(as.matrix(TaiwanAirBox032017[1:100,1:3]), r.max = 1, type =2)
outliers.hdts <- function(x, r.max, type){
  Yt <- x
  # Check that type is well specified

  if (!type%in%c(1,2)){stop("type can be only 1 or 2")}

  ##########################################################################################################
  # Obtain sample size and dimension of the observed time series

  n <- nrow(Yt)
  m <- ncol(Yt)

  ##########################################################################################################
  # First step: Initial cleaning
  ##########################################################################################################

  ##########################################################################################################
  # First sub-step: Projections of maximum kurtosis

  out.pro.kurt <- pro.kurt(Yt,n,m,type)
  Yt.kurt <- out.pro.kurt$Yt.kurt
  times.kurt <- out.pro.kurt$times.kurt
  pro.kurt <- out.pro.kurt$pro.kurt
  n.pro.kurt <- out.pro.kurt$n.pro.kurt

  ##########################################################################################################
  # Second sub-step: Random projections

  out.pro.rand <- pro.rand(Yt.kurt,n.pro.kurt,n,m,type)
  Yt.rand <- out.pro.rand$Yt.rand
  times.rand <- out.pro.rand$times.rand

  ########################################################################################################
  # Third sub-step: Univariate search

  out.uni.search <- uni.search(Yt.rand,n,m,type)
  Yt.uni <- out.uni.search$Yt.uni
  times.uni <- out.uni.search$times.uni
  comps.uni <- out.uni.search$comps.uni

  ##########################################################################################################
  # Second step: Robust estimation
  ##########################################################################################################

  out.rob.est <- rob.est(Yt.uni,r.max,n,m,type)
  r.rob <- out.rob.est$r.rob
  P.rob <- out.rob.est$P.rob
  V.rob <- out.rob.est$V.rob
  I.cov.rob <- out.rob.est$I.cov.rob
  IC.1  <- out.rob.est$IC.1

  ##########################################################################################################
  # Third, fourth and fifth steps: Outlier detection in a loop
  ##########################################################################################################

  out.loop.det <- loop.det(Yt,Yt.uni,n,m,r.rob,P.rob,V.rob,I.cov.rob,type)
  Yt.clean <- out.loop.det$Yt.clean
  P.clean <- out.loop.det$P.clean
  Ft.clean <- out.loop.det$Ft.clean
  Nt.clean <- out.loop.det$Nt.clean
  times.idi.out <- out.loop.det$times.idi.out
  comps.idi.out <- out.loop.det$comps.idi.out
  sizes.idi.out <- out.loop.det$sizes.idi.out
  stats.idi.out <- out.loop.det$stats.idi.out
  times.fac.out <- out.loop.det$times.fac.out
  comps.fac.out <- out.loop.det$comps.fac.out
  sizes.fac.out <- out.loop.det$sizes.fac.out
  stats.fac.out <- out.loop.det$stats.fac.out

  ##########################################################################################################
  # Return results

  return(list("x.kurt"=Yt.kurt,"times.kurt"=times.kurt,"pro.kurt"=pro.kurt,"n.pro.kurt"=n.pro.kurt,
              "x.rand"=Yt.rand,"times.rand"=times.rand,
              "x.uni"=Yt.uni,"times.uni"=times.uni,"comps.uni"=comps.uni,
              "r.rob"=r.rob,"P.rob"=P.rob,"V.rob"=V.rob,"I.cov.rob"=I.cov.rob,"IC.1"=IC.1,
              "x.clean"=Yt.clean,"P.clean"=P.clean,"Ft.clean"=Ft.clean,"Nt.clean"=Nt.clean,
              "times.idi.out"=times.idi.out,"comps.idi.out"=comps.idi.out,"sizes.idi.out"=sizes.idi.out,"stats.idi.out"=stats.idi.out,
              "times.fac.out"=times.fac.out,"comps.fac.out"=comps.fac.out,"sizes.fac.out"=sizes.fac.out,"stats.fac.out"=stats.fac.out))

}

pro.kurt <- function(Yt,n,m,type){

  ##########################################################################################################
  # Print that the procedure is running

  message("First step - Running the first substep: projections of maximum kurtosis")

  ##########################################################################################################
  # Initilize objects

  Yt.kurt <- Yt
  do.iter <- "Yes" # Does the procedure continue iterating?
  many.pro <- "No" # Too many projections?
  many.out <- "No" # Too many outliers?
  times.kurt <- vector(mode="numeric") # The outliers detected
  pro.kurt <- vector(mode="numeric") # The projections in which the outliers have been detected
  n.pro.kurt <- 0 # Number of projections leading to outliers

  ##########################################################################################################
  # Run the first substep of step 1: projections of maximum kurtosis

  while (do.iter=="Yes"){

    ########################################################################################################
    # Project and find outliers

    n.pro.kurt <- n.pro.kurt + 1 # A new projections is going to be generated
    c.val <- sqrt(qchisq(.95^(1/(n*n.pro.kurt)),df=1)) # Critical value to use in this iteration
    if (type == 1){ # Obtain the projected time series
      Yt.max <- ts.max.kurt(Yt.kurt,n,m)$Yt.max
    } else if (type == 2){
      Yt.max <- ts.max.kurt(diff(Yt.kurt),n-1,m)$Yt.max
      Yt.max <- ts(c(0,apply(Yt.max,2,cumsum)))
    }
    out.Yt.max <- ao.uts(Yt.max,n,c.val,type) # Look for outliers in the projected series
    out.times <- sort(out.Yt.max$out.time)
    n.out.times <- length(out.times)

    message("Maximum kurtosis: ", "Direction number = ", n.pro.kurt, "Number of outliers = ", n.out.times, "\n")

    ########################################################################################################
    # Clean the series if outliers are found

    if (n.out.times==0){
      do.iter <- "No"
    } else {
      Yt.kurt[out.times,] <- NA
      for (j in 1:m){Yt.kurt[,j] <- imputeTS::na_ma(Yt.kurt[,j],k=4,weighting="exponential")}
      times.kurt <- c(times.kurt,out.times)
      pro.kurt <- c(pro.kurt,rep(n.pro.kurt,n.out.times))
    }

    ########################################################################################################
    # If the total number of outliers is large, stop the process

    n.times.kurt <- length(times.kurt)
    if ((n.times.kurt > floor(0.2*n)||(n.pro.kurt>(0.2*m)))){
      do.iter <- "No"
      many.out <- "Yes"
      many.pro <- "Yes"
    }

  }

  ##########################################################################################################
  # Consider only the number of directions with outliers

  if (many.out=="No"){n.pro.kurt <- n.pro.kurt - 1}

  ##########################################################################################################
  # Return results

  return(list("Yt.kurt"=Yt.kurt,"times.kurt"=times.kurt,"pro.kurt"=pro.kurt,"n.pro.kurt"=n.pro.kurt))

}

ts.max.kurt <- function(Yt,n,m){

  ########################################################################################################
  # When n <= m, consider the PCs

  if (n > m){
    S <- corpcor::cov.shrink(Yt,lambda=0,lambda.var=0,verbose=FALSE) # Estimate the covariance matrix
    Yt.st <- t(solve(t(chol(S)),t(scale(Yt,scale=FALSE)))) # Standardize the time series
    V.max <- MaxKur(Yt.st)$v # Obtain the direction of maximum kurtosis
    Yt.max <- ts(Yt %*% V.max) # Obtain the projected time series
  } else {
    q.1 <- sum(n/(2*(1:m))>1)
    q.2 <- sum(floor(n/(2*(1:m)))>(1:m))
    n.pcs <- min(q.1,q.2)
    Zt <- prcomp(Yt)$x[,1:n.pcs] # Obtain the PCs
    S <- corpcor::cov.shrink(Zt,lambda=0,lambda.var=0,verbose=FALSE) # Estimate the covariance matrix
    Zt.st <- t(solve(t(chol(S)),t(scale(Zt,scale=FALSE)))) # Standardize the PCs
    V.max <- MaxKur(Zt.st)$v # Obtain the direction of maximum kurtosis
    Yt.max <- ts(Zt %*% V.max) # Obtain the projected time series
  }

  ########################################################################################################
  # Return results

  return(list("Yt.max"=Yt.max,"V.max"=V.max))

}

MaxKur <- function(x,maxit = 30) {

  #
  # Compute direction maximizing the kurtosis of the projections
  # Observations passed to the routine should be standardized
  # Uses Newton's method and an augmented Lagrangian merit function
  # To be used as a subroutine of KurNwm
  #
  # Inputs:  x, observations (by rows)
  #          maxit, a limit to the number of Newton iterations
  # Outputs: v, max. kurtosis direction (local optimizer)
  #          f, max. kurtosis value
  #

  # Initialization

  ## Tolerances

  maxitini = 1
  mxitbl = 20
  tol = 1.0e-4
  tol1 = 1.0e-7
  tol2 = 1.0e-2
  tol3 = 1.0e-12
  beta = 1.0e-4
  rho0 = 0.1

  dx = dim(x)
  n = dx[1]
  p = dx[2]

  ep = matrix(1,p,1)

  ## Initial estimate of the direction

  uv = matrix(apply(x*x,1,sum),n,1)
  uw = 1/(tol3 + sqrt(uv))
  uu = x*(uw %*% t(ep))
  Su = cov(uu)
  Sue = eigen(Su)
  V = Sue$vectors
  D = Sue$values

  for (i in 1:p) {
    if (i == 1) r = ValKur(x,V[,i]) else r = cbind(r,ValKur(x,V[,i]))
  }
  ik = which.max(r)
  v = r[ik]
  a = V[,ik[1]]

  itini = 1
  difa = 1
  while ((itini <= maxitini)&&(difa > tol2)) {
    z = x %*% a
    zaux = z^2
    xaux = t(x)*(ep %*% t(zaux))
    H = 12*xaux %*% x
    He = eigen(H)
    V = He$vectors
    E = He$values
    iv = which.max(E)
    vv = E[iv]
    aa = V[,iv[1]]
    difa = norm(as.matrix(a - aa),type = "F")
    a = aa
    itini = itini + 1
  }

  ## Values at iteration 0 for the optimization algorithm

  z = x %*% a
  sk = sum(z^4)
  lam = 2*sk
  f = sk
  g = t(4*t(z^3) %*% x)
  zaux = z^2
  xaux = t(x)*(ep %*% t(zaux))
  H = 12*xaux %*% x

  al = 0
  it = 0
  diff = 1
  rho = rho0
  clkr = 0

  cc = 0

  # Newton method starting from the initial direction

  while (1) {

    ## Check termination conditions

    gl = g - 2*c(lam)*a

    A = 2*t(a)
    aqr = qr(a)
    Q = qr.Q(aqr, complete = T)
    Z = Q[,(2:p)]

    crit = norm(gl,type = "F") + abs(cc)
    if ((crit <= tol)||(it >= maxit)) break

    ## Compute search direction

    Hl = H - 2*lam[1]*diag(p)
    Hr = t(Z) %*% Hl %*% Z
    Hre = eigen(Hr)
    V = Hre$vectors
    E = Hre$values
    if (length(E) > 1) {
      Es1 = pmin(-abs(E),-tol)
      Hs = V %*% diag(Es1) %*% t(V)
    } else {
      Es1 = min(-abs(E),-tol)
      Hs = Es1 * V %*% t(V)
    }
    py = - cc/(A %*% t(A))
    rhs = t(Z) %*% (g + H %*% t(A) %*% py)
    pz = -solve(Hs,rhs)

    pp = Z %*% pz + t(A) %*% py

    dlam = (t(a) %*% (gl + H %*% pp))/(2*t(a) %*% a)

    ## Adjust penalty parameter

    f0d = t(gl) %*% pp - 2*rho*cc*t(a) %*% pp - dlam*cc
    crit1 = beta*norm(pp,type = "F")^2

    if (f0d < crit1) {
      rho1 = 2*(crit1 - f0d)/(tol3 + cc^2)
      rho = max(c(2*rho1,1.5*rho,rho0))
      f = sk - lam*cc - 0.5*rho*cc^2
      f0d = t(gl) %*% pp - 2*rho*cc*t(a) %*% pp - dlam*cc
      clkr = 0
    } else if ((f0d > 1000*crit1)&&(rho > rho0)) {
      rho1 = 2*(crit1 - t(gl) %*% pp + dlam*cc)/(tol3 + cc^2)
      if ((clkr == 4)&&(rho > 2*rho1)) {
        rho = 0.5*rho
        f = sk - lam*cc - 0.5*rho*cc^2
        f0d = t(gl) %*% pp - 2*rho*cc*t(a) %*% pp - dlam*cc
        clkr = 0
      } else clkr = clkr + 1
    }

    if (abs(f0d/(norm(g-2*rho*a*c(cc),type = "F")+abs(cc))) < tol1) break

    ## Line search

    al = 1
    itbl = 0
    while (itbl < mxitbl) {
      aa = a + al*pp
      lama = lam + al*dlam
      zz = x %*% aa
      cc = t(aa) %*% aa - 1
      sk = sum(zz^4)
      ff = sk - lama*cc - 0.5*rho*cc^2
      if (ff > f + 0.0001*al*f0d) break
      al = al/2
      itbl = itbl + 1
    }

    if (itbl >= mxitbl) break

    ## Update values for the next iteration

    a = aa
    lam = lama
    z = zz

    nmd2 = t(a) %*% a
    cc = nmd2 - 1
    f = sk - lam*cc - 0.5*rho*cc^2
    g = t(4*t(z^3) %*% x)
    zaux = z^2
    xaux = t(x)*(ep %*% t(zaux))
    H = 12*xaux %*% x

    it = it + 1
  }

  # Values to be returned

  a = matrix(c(a),p,1)
  v = a/norm(a,type = "F")
  xa = x %*% v
  f = sum(xa^4)/n

  rval = list(v = v, f = f)
  return(rval)
}

ValKur <- function(x,d) {

  #
  # Evaluate the moment coefficient of order k
  # for the univariate projection of multivariate data
  #
  #     ValKur(x,d)
  #

  # Daniel Pena/Francisco J Prieto 11/25/15

  km = 4
  dimx = dim(x)
  n = dimx[1]
  x
  t = x %*% d
  tm = mean(t)
  tt = abs(t - tm)
  vr = sum(tt^2)/(n-1)
  kr = sum(tt^km)/n
  mcv = kr/(vr^(km/2))

  ## Return values

  return(mcv)
}

ao.uts <- function(yt,n,c.val,type){

  ##########################################################################################################
  # Convert yt object into a single vector

  yt <- as.vector(yt)

  ##########################################################################################################
  # First step: Fit an initial model, obtain residuals and estimate the mad of the residuals
  ##########################################################################################################

  if (type == 1){
    fit.arima.0 <- auto.arima(yt,d=0,D=0,max.p=3,max.q=3,
                              stationary=TRUE,seasonal=FALSE,ic="bic",allowdrift=FALSE)
  } else if (type == 2){
    fit.arima.0 <- auto.arima(yt,d=1,D=0,max.p=3,max.q=3,
                              stationary=FALSE,seasonal=FALSE,ic="bic",allowdrift=FALSE)
  } else if (type == 3){
    fit.arima.0 <- auto.arima(yt,max.d=1,D=0,max.p=3,max.q=3,
                              seasonal=FALSE,ic="bic",allowdrift=FALSE)
  }
  res.0 <- fit.arima.0$residuals # Residuals of the initial fit
  mad.0 <- mad(res.0) # Estimate the mad of residuals

  ##########################################################################################################
  # Second step: Label suspicious outliers and estimate jointly model parameters and outlier effects
  ##########################################################################################################

  ##########################################################################################################
  # Label suspicious outliers

  out.0 <- which(abs(res.0/mad.0) > c.val) # Suspicious outliers

  ##########################################################################################################
  # Estimate model parameters and suspicious outliers jointly, if any

  n.out.0 <- length(out.0) # Number of suspicious outliers
  if (n.out.0 > 0){
    out.xreg <- matrix(0,nrow=n,ncol=n.out.0)
    for (i in 1 : n.out.0){out.xreg[out.0[i],i] <- 1}
    if (type == 1){
      fit.arima.r <- auto.arima(yt,d=0,D=0,max.p=3,max.q=3,
                                stationary=TRUE,seasonal=FALSE,ic="bic",xreg=out.xreg,allowdrift=FALSE)
    } else if (type == 2){
      fit.arima.r <- auto.arima(yt,d=1,D=0,max.p=3,max.q=3,
                                stationary=FALSE,seasonal=FALSE,ic="bic",xreg=out.xreg,allowdrift=FALSE)
    } else if (type == 3){
      fit.arima.r <- auto.arima(yt,max.d=1,D=0,max.p=3,max.q=3,
                                seasonal=FALSE,ic="bic",xreg=out.xreg,allowdrift=FALSE)
    }
    p.r <- fit.arima.r$arma[1] # Order p after robust estimation
    q.r <- fit.arima.r$arma[2] # Order q after robust estimation
    d.r <- fit.arima.r$arma[6] # Order d after robust estimation

    ##########################################################################################################
    # Obtain the filtered residuals of the original series with the estimated ARIMA parameters and estimate
    # the mad of such residuals

    fit.arima.aux <- fit.arima.r # Define auxiliarity model
    n.aux <- length(coef(fit.arima.aux)) - n.out.0 # Number of estimated parameters
    fit.arima.aux$coef <- coef(fit.arima.aux)[1:n.aux] # Keep only the estimated parameters
    fit.arima.aux$var.coef <- as.matrix(fit.arima.aux$var.coef[1:n.aux,1:n.aux]) # Covariance matrix of estimated parameters
    fit.arima.aux$mask <- fit.arima.aux$mask[1:n.aux] # I do not know exactly what is this. Just copy
    fit.arima.aux$call <- arima(x=yt,order=c(p.r,d.r,q.r)) # Call
    fit.arima.aux$xreg <- fit.arima.0$xreg # Skip the outlier effects
    if ((p.r+q.r)==0){ # Filtered residuals with robust parameters
      if (d.r==0){
        res.r <- yt - mean(yt)
      } else if (d.r==1){
        res.r <- ts(c(0,diff(yt)-mean(diff(yt))))
      }
    } else {
      res.r <- residuals(Arima(yt,model=fit.arima.aux))
    }
    mad.r <- mad(res.r) # Estimate the mad of residuals
  }else{
    fit.arima.r <- fit.arima.0
    res.r <- res.0
    mad.r <- mad.0
    d.r <- fit.arima.0$arma[6]
  }

  ##########################################################################################################
  # Third step: Search for outliers with robust estimates
  ##########################################################################################################

  ##########################################################################################################
  # Obtain the autoregressive representation of the model parameters and the associated vector

  pi.r <- gsarima::arrep(notation="arima",phi=fit.arima.r$model$phi,d=d.r,theta=fit.arima.r$model$theta,Phi=0,D=0,Theta=0,frequency=1)
  n.pi.r <- length(pi.r) # Length of autoregressive representation
  if (n.pi.r < (n - 1)){ # Fill with zeros
    out.pi.r <- c(-1,pi.r,rep(0,n-1-n.pi.r))
  }else{
    out.pi.r <- c(-1,pi.r[1:(n-1)])
  }

  ##########################################################################################################
  # Search for outliers

  out.look <- "Yes" # Look for outliers
  out.time <- out.size <- out.stat <- vector(mode="numeric",length=0) # Initialize vectors
  out.E <- as.numeric(res.r) # Initialize residuals vector
  mat.pi.r <- toeplitz(out.pi.r) # Define the Toeplitz matrix
  mat.pi.r[lower.tri(mat.pi.r)] <- 0 # Put 0s in the lower diagonal
  mat.pi.r <- as(mat.pi.r,"CsparseMatrix") # Define the matrix as sparse
  mat.pi.r.2 <- Matrix::rowSums(mat.pi.r^2)
  while (out.look=="Yes"){
    out.w <- - (mat.pi.r %*% out.E / mat.pi.r.2)
    out.w <- as(out.w,"matrix")
    out.w.se <- mad.r / sqrt(mat.pi.r.2)
    out.lrt <- out.w / out.w.se
    out.can <- which.max(abs(out.lrt)) # Candidate to be an outlier
    if (abs(out.lrt[out.can]) > c.val){
      out.time <- c(out.time,out.can)
      out.E[out.can:n] <- out.E[out.can:n] + out.pi.r[1:(n-out.can+1)] * out.w[out.can]
    }else{
      out.look <- "No"
    }
  }
  out.time <- unique(out.time)

  ##########################################################################################################
  # Fourth step: Joint estimation of parameters and outlier effects
  ##########################################################################################################

  n.out.time <- length(out.time) # Number of detected outliers
  if (n.out.time > 0){
    out.est <- "Yes"
    while (out.est=="Yes"){
      out.xreg <- matrix(0,nrow=n,ncol=n.out.time)
      for (i in 1 : n.out.time){out.xreg[out.time[i],i] <- 1}
      if (type == 1){
        fit.arima.end <- auto.arima(yt,d=0,D=0,max.p=3,max.q=3,
                                    stationary=TRUE,seasonal=FALSE,ic="bic",xreg=out.xreg,allowdrift=FALSE)
      } else if (type == 2){
        fit.arima.end <- auto.arima(yt,d=1,D=0,max.p=3,max.q=3,
                                    stationary=FALSE,seasonal=FALSE,ic="bic",xreg=out.xreg,allowdrift=FALSE)
      } else if (type == 3){
        fit.arima.end <- auto.arima(yt,max.d=1,D=0,max.p=3,max.q=3,
                                    seasonal=FALSE,ic="bic",xreg=out.xreg,allowdrift=FALSE)
      }
      n.end <- length(coef(fit.arima.end)) - n.out.time # Number of estimated parameters
      out.w.est <- as.numeric(coef(fit.arima.end)[(n.end+1):(n.end+n.out.time)]) # Size estimates
      out.se.w.est <- sqrt(as.numeric(diag(vcov(fit.arima.end)))[(n.end+1):(n.end+n.out.time)]) # Standard deviations
      out.t <- out.w.est / out.se.w.est # t-statistics
      out.del <- which.min(abs(out.t)) # Find the minimum t-statistic in absolute value
      if (abs(out.t[out.del]) > c.val){
        out.est <- "No"
        out.size <- out.w.est
        out.stat <- out.t
      } else {
        out.time <- out.time[-out.del]
        n.out.time <- length(out.time)
        if (n.out.time==0){
          out.est <- "No"
          out.time <- out.size <- out.stat <- vector(mode="numeric",length=0)
        }
      }
    }
  } else {
    out.time <- out.size <- out.stat <- vector(mode="numeric",length=0)
  }

  ##########################################################################################################
  # Obtain the cleaned series

  yt.clean <- yt
  yt.clean[out.time] <- yt.clean[out.time] - out.size

  ##########################################################################################################
  # Return results

  return(list("yt.clean"=yt.clean,"out.time"=out.time,"out.size"=out.size,"out.stat"=out.stat))

}

uni.search <- function(Yt.rand,n,m,type){

  ##########################################################################################################
  # Print that the procedure is running

  message("First step - Running the third substep: univariate cleaning")

  ##########################################################################################################
  # Initilize objects

  Yt.uni <- Yt.rand
  times.uni <- vector(mode="numeric") # The outliers detected
  comps.uni <- vector(mode="numeric") # The components affected by the outliers detected

  ##########################################################################################################
  # Find outliers in each time series component

  c.val <- sqrt(qchisq(.95^(1/(m*n)),df=1)) # Critical value to use
  for (i in 1:m){
    out.Yt.univ <- ao.uts(Yt.uni[,i],n,c.val,type) # Look for outliers in the projected series
    out.times <- sort(out.Yt.univ$out.time)
    n.out.times <- length(out.times)
    if (n.out.times > 0){
      times.uni <- c(times.uni,out.times)
      comps.uni <- c(comps.uni,rep(i,n.out.times))
    }
    Yt.uni[,i] <- out.Yt.univ$yt.clean

    message("Univariate cleaning: ", "Number = ", i, "Number of outliers = ", n.out.times, "\n")

  }

  ##########################################################################################################
  # Return results

  return(list("Yt.uni"=Yt.uni,"times.uni"=times.uni,"comps.uni"=comps.uni))

}

rob.est <- function(Yt.uni,r.max,n,m,type){

  ##########################################################################################################
  # Print that the procedure is running

  message("Running the second step: Robust estimation")

  ##########################################################################################################
  # Compute covariance matrix of the time series and their eigenvectors

  if (n > m){
    S.Yt <- corpcor::cov.shrink(Yt.uni,lambda=0,lambda.var=0,verbose=FALSE)
  } else {
    S.Yt <- corpcor::cov.shrink(Yt.uni,verbose=FALSE)
  }
  vec.S.Yt <- eigen(S.Yt)$vectors

  ##########################################################################################################
  # If the time series is stationary, we center the data first

  if (type == 1){
    Yt.uni.cen <- scale(Yt.uni,center=TRUE,scale=FALSE)
  } else {
    Yt.uni.cen <- Yt.uni
  }

  ##########################################################################################################
  # Compute the values of the IC1 criteria and select the number of factors

  IC.1 <- numeric(length=r.max+1)
  for (r in 0 : r.max){
    if (r == 0){
      V.r <- 1 / (n*m) * sum(Yt.uni.cen^2)
    } else {
      P.est <- vec.S.Yt[,1:r]
      Ft.est <- Yt.uni.cen %*% P.est
      Nt.est <- Yt.uni.cen - Ft.est %*% t(P.est)
      V.r <- 1 / (n*m) * sum(Nt.est^2)
    }
    IC.1[r+1] <- log(V.r) + r * ((n+m)/(n*m)) * log((n*m)/(n+m))
  }
  r.rob <- which.min(IC.1) - 1

  message("The number of factors is: ", r.rob, "\n")

  ##########################################################################################################
  # Obtain the robust estimates of P, V and (V'GnV)^{-1} if there are factors
  # Otherwise, we do not need such estimates

  if (r.rob > 0){
    P.rob <- vec.S.Yt[,1:r.rob]
    V.rob <- vec.S.Yt[,(r.rob+1):m]
    Ft.rob <- Yt.uni.cen %*% P.rob
    Nt.rob <- Yt.uni.cen - Ft.rob %*% t(P.rob)
    if (n > m){
      S.Nt.rob <- corpcor::cov.shrink(Nt.rob,lambda=0,lambda.var=0,verbose=FALSE)
    } else {
      S.Nt.rob <- corpcor::cov.shrink(Nt.rob,verbose=FALSE)
    }
    I.cov.rob <- chol2inv(chol(t(V.rob) %*% S.Nt.rob %*% V.rob))
  } else {
    P.rob <- V.rob <- I.cov.rob <- numeric(length=0)
  }

  ##########################################################################################################
  # Return results

  return(list("r.rob"=r.rob,"P.rob"=P.rob,"V.rob"=V.rob,"I.cov.rob"=I.cov.rob,"IC.1"=IC.1))

}

loop.det <- function(Yt,Yt.uni,n,m,r.rob,P.rob,V.rob,I.cov.rob,type){

  ##########################################################################################################
  # Print that the procedure is running

  message("Running the third, fourth and fifth steps: Outlier detection in a loop")

  ##########################################################################################################
  # Initialize information for factor and idiosyncratic outliers: times, components, sizes and statistics

  times.fac.out <- vector(mode="numeric",length=0)
  comps.fac.out <- vector(mode="numeric",length=0)
  sizes.fac.out <- vector(mode="numeric",length=0)
  stats.fac.out <- vector(mode="numeric",length=0)

  times.idi.out <- vector(mode="numeric",length=0)
  comps.idi.out <- vector(mode="numeric",length=0)
  sizes.idi.out <- vector(mode="numeric",length=0)
  stats.idi.out <- vector(mode="numeric",length=0)

  ##########################################################################################################
  # Distinguish whether the number of factors is 0 or larger than 0

  if (r.rob > 0){

    ########################################################################################################
    # Initialize the time series to be cleaned and the number of iteration

    Yt.out <- Yt
    iter.num <- 0
    iter.num.idi <- vector(mode="numeric",length=0)
    iter.num.fac <- vector(mode="numeric",length=0)
    iterate <- "Yes"

    ########################################################################################################
    # Initialize the estimates of the matrices P, V and the I.cov that are re-estimated in each iteration

    P.est <- P.rob
    V.est <- V.rob
    I.cov.est <- I.cov.rob

    ########################################################################################################
    # Run the loop: detect idiosyncratic outliers, then factor outliers and repeat

    while ((iterate == "Yes") && (iter.num <= 10)){

      ######################################################################################################
      # A new iteration

      iter.num <- iter.num + 1

      ######################################################################################################
      # Third step: Idiosyncratic outliers
      ######################################################################################################

      out.idi.det <- idi.det(Yt.out,n,m,V.est,I.cov.est,type)
      n.times.idi.out <- length(out.idi.det$times.idi.out)
      if (n.times.idi.out > 0){
        times.idi.out <- c(times.idi.out,out.idi.det$times.idi.out)
        comps.idi.out <- c(comps.idi.out,out.idi.det$comps.idi.out)
        sizes.idi.out <- c(sizes.idi.out,out.idi.det$sizes.idi.out)
        stats.idi.out <- c(stats.idi.out,out.idi.det$stats.idi.out)
        Yt.out <- out.idi.det$Yt.clean
        iter.num.idi <- c(iter.num.idi,rep(iter.num,n.times.idi.out))
      }

      ######################################################################################################
      # Fourth step: Factor outliers
      ######################################################################################################

      out.fac.det <- fac.det(Yt.out,n,m,r.rob,P.est,type)
      n.times.fac.out <- length(out.fac.det$times.fac.out)
      if (n.times.fac.out > 0){
        times.fac.out <- c(times.fac.out,out.fac.det$times.fac.out)
        comps.fac.out <- c(comps.fac.out,out.fac.det$comps.fac.out)
        sizes.fac.out <- c(sizes.fac.out,out.fac.det$sizes.fac.out)
        stats.fac.out <- c(stats.fac.out,out.fac.det$stats.fac.out)
        Yt.out <- out.fac.det$Yt.clean
        iter.num.fac <- c(iter.num.fac,rep(iter.num,n.times.fac.out))
      }

      ##########################################################################################################
      # Print the results of the search

      message("Third and fourth steps - Iteration number: ", iter.num,"Number of idiosyncratic outliers: ", n.times.idi.out,
          "Number of factor outliers: ", n.times.fac.out, "\n")

      ##########################################################################################################
      # If some outliers have been found, re-estimate P, V, and I.cov
      # Otherwise, re-estimate the factors and leave the loop

      if ((n.times.idi.out > 0) || (n.times.fac.out > 0)){
        if (n > m){
          S.Yt.out <- corpcor::cov.shrink(Yt.out,lambda=0,lambda.var=0,verbose=FALSE)
        } else {
          S.Yt.out <- corpcor::cov.shrink(Yt.out,verbose=FALSE)
        }
        vec.S.Yt.out <- eigen(S.Yt.out)$vectors
        P.est <- vec.S.Yt.out[,1:r.rob]
        V.est <- vec.S.Yt.out[,(r.rob+1):ncol(vec.S.Yt.out)]
        if (type == 1){
          Yt.out.cen <- scale(Yt.out,center=TRUE,scale=FALSE)
          Ft.est <- Yt.out.cen %*% P.est
          Nt.est <- Yt.out.cen - Ft.est %*% t(P.est)
        } else {
          Ft.est <- Yt.out %*% P.est
          Nt.est <- Yt.out - Ft.est %*% t(P.est)
        }
        if (n > m){
          S.Nt.est <- corpcor::cov.shrink(Nt.est,lambda=0,lambda.var=0,verbose=FALSE)
        } else {
          S.Nt.est <- corpcor::cov.shrink(Nt.est,verbose=FALSE)
        }
        I.cov.est <- chol2inv(chol(t(V.est) %*% S.Nt.est %*% V.est))
      } else {
        Yt.clean <- Yt.out
        P.clean <- P.est
        if (type == 1){
          Yt.clean.cen <- scale(Yt.clean,center=TRUE,scale=FALSE)
          Ft.clean <- Yt.clean.cen %*% P.clean
          Nt.clean <- Yt.clean.cen - Ft.clean %*% t(P.clean)
        } else {
          Ft.clean <- Yt.clean %*% P.clean
          Nt.clean <- Yt.clean - Ft.clean %*% t(P.clean)
        }
        iterate <- "No"
      }

      ##########################################################################################################
      # If the number of iterations is very large, the procedure also stops

      if (iter.num == 10){
        Yt.clean <- Yt.out
        P.clean <- P.est
        if (type == 1){
          Yt.clean.cen <- scale(Yt.clean,center=TRUE,scale=FALSE)
          Ft.clean <- Yt.clean.cen %*% P.clean
          Nt.clean <- Yt.clean.cen - Ft.clean %*% t(P.clean)
        } else {
          Ft.clean <- Yt.clean %*% P.clean
          Nt.clean <- Yt.clean - Ft.clean %*% t(P.clean)
        }
      }

    }

  } else {

    ##########################################################################################################
    # Print that there are no factors and then these steps are not needed

    message("There are no factors, thus the procedure ends")
    Yt.clean <- Yt.uni
    P.clean <- Ft.clean <- Nt.clean <- numeric(length=0)

  }

  ##########################################################################################################
  # Return results

  return(list("Yt.clean"=Yt.clean,"P.clean"=P.clean,"Ft.clean"=Ft.clean,"Nt.clean"=Nt.clean,
              "times.idi.out"=times.idi.out,"comps.idi.out"=comps.idi.out,"sizes.idi.out"=sizes.idi.out,"stats.idi.out"=stats.idi.out,
              "times.fac.out"=times.fac.out,"comps.fac.out"=comps.fac.out,"sizes.fac.out"=sizes.fac.out,"stats.fac.out"=stats.fac.out))

}

idi.det <- function(Yt.out,n,m,V.est,I.cov.est,type){

  ##########################################################################################################
  # If the time series is stationary, obtain the mean vector and the centered time series

  if (type == 1){
    Yt.out.mean <- colMeans(Yt.out)
    Yt.out <- scale(Yt.out,center=TRUE,scale=FALSE)
  }

  ##########################################################################################################
  # Obtain the transformed time series

  Yt.tran <- Yt.out %*% V.est

  ##########################################################################################################
  # Initial computation of size estimates, their standard errors and the resulting statistics

  w.est <- se.w.est <- matrix(NA,nrow=n,ncol=m)
  for (j in 1 : m){
    se.w.est[,j] <- as.numeric(V.est[j,] %*% I.cov.est %*% V.est[j,])^(-1/2)
    w.est[,j] <- se.w.est[1,j]^2 * V.est[j,] %*% I.cov.est %*% t(Yt.tran)
  }
  lamb.est <- abs(w.est / se.w.est)

  ##########################################################################################################
  # Initialize the search

  Yt.clean <- Yt.out
  times.idi.out <- vector(mode="numeric",length=0)
  comps.idi.out <- vector(mode="numeric",length=0)
  sizes.idi.out <- vector(mode="numeric",length=0)
  stats.idi.out <- vector(mode="numeric",length=0)
  iterate <- "Yes"
  while (iterate == "Yes"){
    ind.max.lamb <- which(lamb.est==max(lamb.est),arr.ind=TRUE)
    lamb.est.max <- lamb.est[ind.max.lamb[1],ind.max.lamb[2]]
    if (lamb.est.max > sqrt(qchisq(.95^(1/(m*n)),df=1))){
      times.idi.out <- c(times.idi.out,ind.max.lamb[1])
      comps.idi.out <- c(comps.idi.out,ind.max.lamb[2])
      sizes.idi.out <- c(sizes.idi.out,w.est[ind.max.lamb[1],ind.max.lamb[2]])
      stats.idi.out <- c(stats.idi.out,lamb.est[ind.max.lamb[1],ind.max.lamb[2]])
      Yt.clean[ind.max.lamb[1],ind.max.lamb[2]] <- Yt.clean[ind.max.lamb[1],ind.max.lamb[2]] - w.est[ind.max.lamb[1],ind.max.lamb[2]]
      Yt.clean.tran <- Yt.clean %*% V.est
      for (j in 1 : m){
        w.est[ind.max.lamb[1],j] <- as.numeric(se.w.est[1,j]^2 * V.est[j,] %*% I.cov.est %*% Yt.clean.tran[ind.max.lamb[1],])
        lamb.est[ind.max.lamb[1],j] <- abs(w.est[ind.max.lamb[1],j] / se.w.est[ind.max.lamb[1],j])
      }
    }else{
      iterate <- "No"
    }
  }

  ##########################################################################################################
  # If we find outliers, then refine the size estimation for those at the same time

  n.times.idi.out <- length(times.idi.out)
  if (n.times.idi.out > 1){
    uniq.times.idi.out <- sort(unique(times.idi.out))
    n.uniq.times.idi.out <- length(uniq.times.idi.out)
    if (n.times.idi.out > n.uniq.times.idi.out){
      Yt.clean.new <- Yt.out
      Yt.clean.new.tran <- Yt.out %*% V.est
      times.idi.out.new <- vector(mode="numeric",length=0)
      comps.idi.out.new <- vector(mode="numeric",length=0)
      sizes.idi.out.new <- vector(mode="numeric",length=0)
      stats.idi.out.new <- vector(mode="numeric",length=0)
      for (j in 1 : n.uniq.times.idi.out){
        out.locs.num <- which(times.idi.out==uniq.times.idi.out[j]) # Location of the affected components
        n.out.locs.num <- length(out.locs.num) # Number of affected components
        if (n.out.locs.num == 1){
          times.idi.out.new <- c(times.idi.out.new,times.idi.out[out.locs.num]) # Add time
          comps.idi.out.new <- c(comps.idi.out.new,comps.idi.out[out.locs.num]) # Add component
          sizes.idi.out.new <- c(sizes.idi.out.new,sizes.idi.out[out.locs.num]) # Add size
          stats.idi.out.new <- c(stats.idi.out.new,stats.idi.out[out.locs.num]) # Add statistic
          Yt.clean.new[times.idi.out[out.locs.num],comps.idi.out[out.locs.num]] <-
            Yt.clean.new[times.idi.out[out.locs.num],comps.idi.out[out.locs.num]] - sizes.idi.out[out.locs.num]
          Yt.clean.new.tran <- Yt.clean.new %*% V.est
        }else{
          times.idi <- unique(times.idi.out[out.locs.num])
          comps.idi <- comps.idi.out[out.locs.num]
          sizes.idi <- sizes.idi.out[out.locs.num]
          stats.idi <- stats.idi.out[out.locs.num]
          n.comps.idi <- length(comps.idi)
          iterate <- "Yes"
          while (iterate == "Yes"){
            if (n.comps.idi == 1){
              times.idi.out.new <- c(times.idi.out.new,times.idi) # Add time
              comps.idi.out.new <- c(comps.idi.out.new,comps.idi) # Add component
              sizes.idi.out.new <- c(sizes.idi.out.new,sizes.idi) # Add size
              stats.idi.out.new <- c(stats.idi.out.new,stats.idi) # Add statistic
              Yt.clean.new[times.idi,comps.idi] <- Yt.clean.new[times.idi,comps.idi] - sizes.idi
              Yt.clean.new.tran <- Yt.clean.new %*% V.est
              iterate <- "No"
            }else{
              V.I.cov.est.V <- V.est[comps.idi,] %*% I.cov.est %*% t(V.est[comps.idi,])
              V.I.cov.est.V.singular <- matrixcalc::is.singular.matrix(V.I.cov.est.V)
              if (V.I.cov.est.V.singular == TRUE){
                cov.sizes.idi <- chol2inv(chol(V.I.cov.est.V + 1e-10 * diag(n.comps.idi)))
              }else{
                cov.sizes.idi <- chol2inv(chol(V.I.cov.est.V))
              }
              sizes.idi <- as.vector(cov.sizes.idi %*% V.est[comps.idi,] %*% I.cov.est %*% Yt.clean.new.tran[times.idi,])
              stats.idi <- abs(sizes.idi / diag(cov.sizes.idi)^{1/2})
              min.stats.idi <- min(stats.idi)
              if (min.stats.idi > sqrt(qchisq(.95^(1/(m*n)),df=1))){
                times.idi.out.new <- c(times.idi.out.new,rep(times.idi,n.comps.idi)) # Add times
                comps.idi.out.new <- c(comps.idi.out.new,comps.idi) # Add components
                sizes.idi.out.new <- c(sizes.idi.out.new,sizes.idi) # Add sizes
                stats.idi.out.new <- c(stats.idi.out.new,stats.idi) # Add statistics
                Yt.clean.new[times.idi,comps.idi] <- Yt.clean.new[times.idi,comps.idi] - sizes.idi
                Yt.clean.new.tran <- Yt.clean.new %*% V.est
                iterate <- "No"
              }else{
                comps.idi <- comps.idi[-which.min(stats.idi)]
                sizes.idi <- sizes.idi[-which.min(stats.idi)]
                stats.idi <- stats.idi[-which.min(stats.idi)]
                n.comps.idi <- length(comps.idi)
              }
            }
          }
        }
      }
      times.idi.out <- times.idi.out.new
      comps.idi.out <- comps.idi.out.new
      sizes.idi.out <- sizes.idi.out.new
      stats.idi.out <- stats.idi.out.new
      Yt.clean <- Yt.clean.new
    }
  }

  ##########################################################################################################
  # If the time series is stationary, add the mean vector that was substracted at the beginning

  if (type == 1){Yt.clean <- Yt.clean + as.matrix(rep(1,n),nrow=3,ncol=1) %*% Yt.out.mean}

  ##########################################################################################################
  # Return results

  return(list("Yt.clean"=Yt.clean,
              "times.idi.out"=times.idi.out,"comps.idi.out"=comps.idi.out,"sizes.idi.out"=sizes.idi.out,"stats.idi.out"=stats.idi.out))

}

fac.det <- function(Yt.out,n,m,r.rob,P.est,type){

  ##########################################################################################################
  # Estimate the factors

  Ft.est <- Yt.out %*% P.est

  ##########################################################################################################
  # Initialize vectors of times, components, locations, sizes and statistics

  times.fac.out <- vector(mode="numeric",length=0)
  comps.fac.out <- vector(mode="numeric",length=0)
  sizes.fac.out <- vector(mode="numeric",length=0)
  stats.fac.out <- vector(mode="numeric",length=0)

  ##########################################################################################################
  # Search for factor outliers

  for (j in 1 : r.rob){

    ########################################################################################################
    # Look for outliers in each factor

    ft.est <- as.ts(Ft.est[,j])
    c.val <- sqrt(qchisq(.95^(1/(r.rob*n)),df=1))
    if (type == 1){
      out.ao.uts <- ao.uts(ft.est,n,c.val,1)
    } else {
      out.ao.uts <- ao.uts(ft.est,n,c.val,3)
    }

    ########################################################################################################
    # Save the new outliers detected, if any

    n.out.time <- length(out.ao.uts$out.time)
    if (n.out.time > 0){
      times.fac.out <- c(times.fac.out,out.ao.uts$out.time)
      comps.fac.out <- c(comps.fac.out,rep(j,n.out.time))
      sizes.fac.out <- c(sizes.fac.out,out.ao.uts$out.size)
      stats.fac.out <- c(stats.fac.out,out.ao.uts$out.stat)
    }

  }

  ########################################################################################################
  # Remove the effect of factor outliers, if any

  Yt.clean <- Yt.out
  n.times.fac.out <- length(times.fac.out)
  if (n.times.fac.out > 0){
    for (i in 1 : n.times.fac.out){
      if (r.rob == 1){Yt.clean[times.fac.out[i],] <- Yt.clean[times.fac.out[i],] - P.est * sizes.fac.out[i]}
      if (r.rob > 1){Yt.clean[times.fac.out[i],] <- Yt.clean[times.fac.out[i],] - P.est[,comps.fac.out[i]] * sizes.fac.out[i]}
    }
  }

  ##########################################################################################################
  # Return results

  return(list("Yt.clean"=Yt.clean,
              "times.fac.out"=times.fac.out,"comps.fac.out"=comps.fac.out,"sizes.fac.out"=sizes.fac.out,"stats.fac.out"=stats.fac.out))

}

ts.rand <- function(Yt,n,m){

  ########################################################################################################
  # Compute the number of PCs and then the PCs to obtain the random directions

  q.1 <- sum(n/(2*(1:m))>1)
  q.2 <- sum(floor(n/(2*(1:m)))>(1:m))
  n.pcs <- min(q.1,q.2)
  Zt <- prcomp(Yt)$x[,1:n.pcs]

  ########################################################################################################
  # When n <= n.pcs, use the shrinkage estimator of the covariance matrix

  if (n > n.pcs){
    S <- corpcor::cov.shrink(Zt,lambda=0,lambda.var=0,verbose=FALSE) # Estimate the covariance matrix
  } else {
    S <- corpcor::cov.shrink(Zt,verbose=FALSE)
  }
  Zt.st <- t(solve(t(chol(S)),t(scale(Zt,scale=FALSE)))) # Standardize the PCs
  V.rand <- SdwDir(Zt.st,1) # Obtain the random direction
  Yt.rand <- ts(Zt %*% V.rand) # Obtain the projected time series

  ########################################################################################################
  # Return results

  return(list("Yt.rand"=Yt.rand,"V.rand"=V.rand))

}

pro.rand <- function(Yt.kurt,n.pro.kurt,n,m,type){

  ##########################################################################################################
  # Print that the procedure is running

  message("First step - Running the second substep: random projections")

  ##########################################################################################################
  # Initilize objects

  Yt.rand <- Yt.kurt
  times.rand <- vector(mode="numeric") # The outliers detected

  ##########################################################################################################
  # Run the procedure only when the number of directions generated in the kurtosis substeps is positive

  c.val <- sqrt(qchisq(.95^(1/(n.pro.kurt*n)),df=1)) # Critical value to use
  if (n.pro.kurt > 0){
    for (i in 1 : n.pro.kurt){

      ######################################################################################################
      # Project and find outliers

      if (type == 1){ # Obtain the projected time series
        Yt.pro <- ts.rand(Yt.kurt,n,m)$Yt.rand
      } else if (type == 2){
        Yt.pro <- ts.rand(diff(Yt.kurt),n-1,m)$Yt.rand
        Yt.pro <- ts(c(0,apply(Yt.pro,2,cumsum)))
      }
      out.Yt.pro <- ao.uts(Yt.pro,n,c.val,type) # Look for outliers in the projected series
      out.times <- sort(out.Yt.pro$out.time)
      n.out.times <- length(out.times)

      message("Random projections: ", "Number = ", i, "Number of outliers = ", n.out.times, "\n")

      ########################################################################################################
      # Clean the series if outliers are found

      if (n.out.times > 0){
        Yt.rand[out.times,] <- NA
        for (j in 1:m){Yt.rand[,j] <- imputeTS::na_ma(Yt.rand[,j],k=4,weighting="exponential")}
      }
      times.rand <- c(times.rand,out.times)

    }
  }

  ##########################################################################################################
  # Return results

  return(list("Yt.rand"=Yt.rand,"times.rand"=times.rand))

}


SdwDir <- function(x,nd) {

  #
  # Directions computed in a manner similar to Stahel-Donoho subsampling
  # Pairs of observations are chosen, and weights are assigned based on
  # the projections, by grouping the projected observations
  #
  #  Inputs:    x, observations by rows
  #             nd, number of directions to generate
  #
  #  Outputs:   V, directions generated by the procedure
  #

  use_rnd = 1

  # if (use_rnd == 0) {
  #    k_rnd = 1
  #    load("lstunif.RData")    # Load data defined as a variable (list) lstunif
  #    n_rnd = 100
  # }

  sx = dim(x)
  n = sx[1]
  p = sx[2]

  ep = matrix(1,p,1)

  ep5 = 1.0e-07
  mdi = floor((n+1)/2)
  k1 = p-1+floor((n+1)/2)
  k2 = p-1+floor((n+2)/2)
  fct = qnorm((k1/n + 1)/2)

  ## Number of groups to consider in the projections for any weight set

  s_grp = 2*p
  n_grp = floor(n/s_grp)

  # Generating the directions

  ndir = 0
  V = matrix(0,p,0)

  while (ndir < nd) {

    ## Sampling to obtain initial projections and weights

    # if (use_rnd == 1) {
    nn1 = min(1 + floor(runif(1)*n),n)
    # } else {
    #    nn1 = min(1 + floor(lstunif[k_rnd]*n),n)
    #    k_rnd = k_rnd + 1
    #    if (k_rnd > n_rnd) k_rnd = 1
    # }

    nn2 = nn1

    while ((nn2 == nn1)||(norm(as.matrix(x[nn1,1:p] - x[nn2,1:p]),type = "F") < ep5)) {
      # if (use_rnd == 1) {
      nn2 = min(1 + floor(runif(1)*n),n)
      # } else {
      #    nn2 = min(1 + floor(lstunif[k_rnd]*n),n)
      #    k_rnd = k_rnd + 1
      #    if (k_rnd > n_rnd) k_rnd = 1
      # }
    }
    dd = as.matrix(x[nn1,1:p] - x[nn2,1:p])
    dd = dd/norm(dd,type = "F")
    pr = x %*% dd
    prs = sort.int(pr, index.return = T)
    ww = prs$x
    lbl = prs$ix

    ## Stahel-Donoho directions

    j = 1
    while ((j <= n_grp)&&(ndir < nd)) {
      # if (use_rnd == 1) {
      auxs = sort.int(runif(s_grp), index.return = T)
      # } else {
      #    auxs = sort.int(lstunif[k_rnd:(k_rnd+s_grp-1)], index.return = T)
      #    k_rnd = k_rnd + s_grp
      #    if (k_rnd > n_rnd) k_rnd = 1
      # }
      bb = auxs$ix
      nn = bb[1:p]
      idr = lbl[(1+(j-1)*s_grp):(j*s_grp)]
      y1 = x[idr[nn],]
      qry1 = qr(y1)
      if (qry1$rank >= p) {
        dd = solve(qry1,ep)
        if (norm(dd,type = "F") > ep5) dd = dd/norm(dd,type = "F")
      }
      if (dim(V)[2] > 0) V = cbind(V,dd) else V = dd
      ndir = ndir + 1
      j = j + 1
    }
  }

  ## Return values

  return(V)
}



