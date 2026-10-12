#' Transformation of regressors' matrix X. Data preparation stage for simulation
#' @description
#' Additional function to be used for simulation purposes (academical or research).
#' Transforming the matrix of regressors according to user preferences. Random disturbances (according to a chosen distribution) are entered in the initial values of the matrix X.
#' The expectation of the resulting matrix coincides with the initial matrix X.
#' @details
#' Random perturbations are added to the initial values of matrix X elements ($X_{i,j}$ + rnd), which are distributed according to a chosen distribution (possible alternatives: uniform, exponential and gamma distribution).
#' @param X
#' Matrix (n x k), n - number of observations, k - number of columns (k - 1 - number of regressors). Note, that 1st column contains only ones (1) (intercept)
#' @param p
#' Scalar (from 1 to +inf), random number for simulation. The default value is 1
#' @param k0
#' Scalar. Number from 1 to 3 (distribution selection).
#'         k0 = 1 - uniform distribution (RV Z ~ U (K1, k2)).
#'         k0 = 2 - exponential distribution (RV Z ~ exp(lambda)).
#'         k0 = 3 - Gamma distribution (RV Z ~ gamma(shape, rate)).
#' The default value is k0 = 1.
#' @param k1
#' Scalar. 1) If k0 = 1, then k1 is a left boundary of uniform distribution (RV Z ~ U(k1, k2)).
#'         2) If k0 = 2, then k1 is a parameter lambda of exponential distribution (RV Z ~ exp(lambda)).
#'         3) If k0 = 3, then k1 is a shape parameter of Gamma distribution (RV Z ~ gamma(shape, rate)).
#' The default is k1 = 0.5.
#' @param k2
#' Scalar. 1) If k0 = 1, then k2 is a right boundary of uniform distribution (RV Z ~ U(k1, k2).
#'         2) If k0 = 3, then k2 is a rate parameter of Gamma distribution (RV Z ~ gamma(shape, rate)).
#' The default is k2 = 1.
#' @param k3
#' Scalar. The number of digits after the comma when rounded.
#' The default value is 1.
#' @return
#' New transformed matrix of regressors (n x k), according to user preferences.
#' @export

#' @importFrom stats rexp rgamma rnorm runif

#' @examples
#' Xtest <- cbind(rep_len(1,10),c(2,5,7,3,1,1,2,2,3,6), c(5,9,1,2,3,2,3,5,2,2))
#' randomizeX(Xtest,1,1,1,2,2)

randomizeX <- function(X,p=1,k0=1,k1=0.5,k2=1,k3=1){
  Xf <- as.matrix(X)
  k <- ncol(Xf)
  n <- nrow(Xf)
  Xf <- as.data.frame(Xf)

  if (k0 == 1) {
    for (i in 2:k) {
      Xf[i] <- round(Xf[i]+runif(n,k1,k2),k3) }
  }
  if (k0 == 2) {
    for (i in 2:k) {
      Xf[i] <- round(Xf[i]+rexp(n,k1),k3) }
  }
  if (k0 == 3) {
    for (i in 2:k) {
      Xf[i] <- round(Xf[i]+rgamma(n,shape=k1,rate = k2),k3) }
  }
  Xf <- as.matrix(Xf)
  Xf
}

#' Transformation of the observed time vector tau. Data preparation stage for simulation
#' @description
#' Additional function to be used for simulation purposes (academical or research).
#' Transforming of the observed time vector tau according to user preferences. Random disturbances are entered in the initial values of the vector tau.
#' The expectation of new observed times coincides with initial values of vector tau.
#' @details
#' Initial values of observation times are multiplied by a random value ($tau_{i}$ x k x rnd(0, 1)). All times are independent and time of ith observation has uniform distribution on (0, k$tau_{i}$).
#' @param tau
#' Vector (n x 1), n - number of observations
#' @param p
#' Scalar (from 1 to +inf), random number for simulation. The default value is 1
#' @param k0
#' Scalar (from 1 to +inf). Multiplicative parameter for transforming the initial value
#' The default is k0 = 2.
#' @param k1
#' Scalar. The number of digits after the comma when rounded.
#' The default is 1.
#' @return
#' Vector with new observation times, according to user preferences.
#' @export

#' @importFrom stats runif rgamma

#' @examples
#' tGiven <- matrix (c(6,4.8,1,2.6,6.4,1.7,2.9,4.4,1.5,3.4), nrow = 10, ncol = 1)
#' randomizeTau(tGiven,1,2,2)

randomizeTau <- function(tau,p,k0=2,k1=1) {
  n <- nrow(as.matrix(tau))
  t <- round(tau*k0*runif(n,0,1),k1)
  t }

#' Transformation of vector with initial states I for various observations. Data preparation stage for simulation.
#' @description
#' Additional function to be used for simulation purposes (academical or research).
#' Transforming of vector with initial states I for various observations with respect to stationary distribution of the states for the random environment.
#' @details
#' The initial states (m - number of states, m = 2,3,...) for various observations are independent and are chosen with respect to stationary distribution of the states for the random environment.
#' The vector with stationary probabilities is user-defined vector.
#' @param StatPr
#' Vector (m x 1), m - number of states, m = 2,3,.. .The vector with stationary probabilities, user-defined vector.
#' @param X
#' Matrix (n x k), n - number of observations, k - number of columns (k - 1 - number of regressors).
#' The matrix is needed to get the number of observations.
#' @param p
#' Scalar (from 1 to +inf), random number for simulation. The default value is 1.
#' @return
#' Vector with new initial states, according to stationary distribution of the states for the random environment.
#' @export

#' @importFrom stats runif rgamma

#' @examples
#' Xtest <- cbind(rep_len(1,10),c(2,5,7,3,1,1,2,2,3,6), c(5,9,1,2,3,2,3,5,2,2))
#' StatPr <- matrix (c(0.364,0.242,0.394), nrow = 3, ncol = 1)
#' randomizeInitState(StatPr,Xtest,1)

randomizeInitState <- function(StatPr,X,p=1) {
  m <- nrow(as.matrix(StatPr))
  n <- nrow(X)
  if (m==1) {
    print("Too few stages!")
    #  break
  }
  R <- matrix(0,nrow(X))
  for (i in 1:n) {
    Z <- runif(1)
    S <- 0
    for (j in 1:m) {
      S <- S+StatPr[j]
      if (Z <= S) {
        R[i] <- j
        break }
    }
  }
  R }

ImTrans2 <- function(ii,la) {
  m <- nrow(la)
  if (m==1) {
    print("Too few stages!")
    #  break
  }

  if (m == 2) {
    if (runif(1) < la[ii,1]/(la[ii,1]+la[ii,2])) {J <- 1}
    else {J <- 2}
  }
  else {

    if (ii==1) {
      for (j in 1:(m-2)) {
        if (runif(1) < la[ii,j+1]/(la[ii,j+1]+la[ii,j+2])) {J <- j+1}
        else {J <- j+2}
        #break
      }
    }
    if (ii==m) {
      for (j in 1:(m-2)) {
        if (runif(1) < la[ii,j]/(la[ii,j]+la[ii,j+1])) {J <- j}
        else {J<-j+1}
        #break
      }
    }
    else {
      for (j in 1:(m-1)) {
        EL <-j+1
        if (runif(1) < la[ii,j]/(la[ii,j]+la[ii,EL])) {if (j!=ii) { J <- j } }
        else { if (EL != ii) {J<-EL} }
        #break
      }
    }
  }
  J }


ImitYr2 <- function(tr,ii,x,la,si,be) {
  t <- 0
  I <- ii
  Y <- 0
  ld <- as.matrix(rowSums(la))
  while (t<tr) {
    T <- rexp(1,ld[I,])
    tt <- t+T
    Z <- si * rnorm(1,0,1)
    if (tt < tr) {
      delta <- T
      t <- tt
      I <- ImTrans2(I,la) } else {
        delta <- tr-t
        t <- tt }
    Y <- Y+t(x)%*%be[,I]*delta+sqrt(delta)*Z
  }
  round(Y,3)
}

#' Simulation of the vector of responses Y. Data preparation stage for simulation.
#' @description
#' Additional function to be used for simulation purposes (academical or research).
#' Simulating the vector of responses Y according to the formula (see details).
#' @details
#' The i-th response $Y_{i}$ is defined by the following formula: $Y_{i}(t)=x_{i}\eqn{\beta} + Z_{i} sqrt{t}, i=1,...,n.$
#' The vector with stationary probabilities is user-defined vector.
#' @param t
#' Vector of the observed time (n x 1), n – number of observations
#' @param I
#' Vector of the initial states (n x 1), n – number of observations
#' @param X
#' Matrix of predictors (n x k), n - number of observations, k - number of columns (k - 1 - number of regressors).
#' @param lambda
#' Matrix with the known transition rates  \eqn{\lambda_{i,j}}, (m x m), m – number of states
#' @param sigma
#' Scalar, the standard deviation of the disturbance term
#' @param beta
#' Matrix (k x m), k - number of columns (k - 1 - number of regressors), m - number of states, m = 2,3,.. .
#' @return
#' Vector with new response values of vector Y (n x 1)
#' @export

#' @importFrom stats rexp rnorm runif rgamma

#' @examples
#' Xtest <- cbind(rep_len(1,10),c(2,5,7,3,1,1,2,2,3,6), c(5,4,1,2,3,2,3,5,2,2))
#' tGiven <- matrix (c(0.9,1.18,1,1.6,1.4,1.7,1.9,1.45,1.5,2.14), nrow = 10, ncol = 1)
#' initState <- matrix (c(2,1,1,2,2,2,1,1,2,1),nrow = 10, ncol = 1)
#' lambda <- matrix(c(0, 0.33, 0.45, 0), nrow = 2, ncol = 2, byrow = TRUE)
#' beta <- matrix(c(1, 2, 3, 4, 6, 8), nrow = 3, ncol = 2, byrow = TRUE)
#' Ysimulation(tGiven,initState,Xtest,lambda,1,beta)


Ysimulation <- function(t,I,X,lambda,sigma=1,beta) {
  n <- nrow(X)
  Y <- matrix(0,nrow=n,ncol=1)
  for (r in 1:n) {
    xx <- X[r,]
    tr <- t[r]
    ii <- I[r]
    Y[r] <- ImitYr2(tr,ii,xx,lambda,sigma,beta) }
  Y
}


#' Calculating the average sojourn time in each state

#' @description Calculating expectation of sojourn times in states for the observed time and for given initial state, using eigenvalues and eigenvectors.
#' @details
#' Calculating expectation of sojourn times in states for the observed time (tau_observed) and if initial state is given (ii).
#' Matrix Q is so-called Generator matrix:  \eqn{Q=\lambda-\Lambda, where \lambda} is matrix with known transition rates from state $s_{i}$ to state $s_{j}$,
#' and \eqn{\Lambda} is diagonal matrix with a vector  \eqn{(\Lambda_{1},...,\Lambda_{m}} on the main diagonal, where m is a number of states of external environment.
#' Eigenvalues and eigenvectors are used in calculations.
#' @param ii
#' number (scalar)
#' @param tau_observed
#' number (scalar), observed time
#' @param Q
#' Matrix (m x m), m - number of states
#' @return
#' Vector of average sojourn times in each state.
#' Vector components in total should give observation time (tau_observed).
#' @export

#' @examples
#' lambda <- matrix(c(0, 0.33, 0.45, 0), nrow = 2, ncol = 2, byrow = TRUE)
#' m <- nrow(lambda)
#' ld <- as.matrix(rowSums(lambda))
#' Lambda <- diag(as.vector(ld))
#' Generator <- t(lambda) - Lambda
#' Aver_soj_time(1,10,Generator)

Aver_soj_time <- function(ii,tau_observed,Q) {
  m <- nrow(Q)
  eigen_vectors <- eigen(Q)$vector
  eigenvals <- as.matrix(eigen(Q)$values)
  b <- as.matrix(solve(eigen_vectors)[,ii])
  R <- array(0,dim = c(m,1))
  for (i in 1:m) {
    if (abs(eigenvals[i,])>1e-10) {
      R <- R + b[i,] * ( (exp(tau_observed*eigenvals[i,])-1) / eigenvals[i,]) * eigen_vectors[,i] }
    else
    { R <- R + b[i,] * eigen_vectors[,i] * tau_observed }
  }
  R
}

#  ---------------Preparing data-------------------------

#' Preparing data for parameter estimation procedure
#' @description
#' Regressors matrix formation taking into account observation times and initial states.
#' Kronecker product is used.
#' @details
#' Function calculates the following expression
#' ![](matrix.png "Fig.1"),
#' where vector of average sojourn times in each state is calculated using function Aver_soj_time.
#' @param tGiven
#' Vector of the observed times (n x 1), n – number of observations
#' @param initState
#' Vector of the initial states (n x 1), n – number of observations
#' @param X
#' Matrix of predictors (n x k), n - number of observations, k - number of columns (k - 1 - number of regressors).
#' @param lambda
#' Matrix with the known transition rates \eqn{\lambda_{i,j}}, (m x m), m – number of states
#' @return
#' Matrix (n x 2k)
#' @export

#' @examples
#' Xtest <- cbind(rep_len(1,10),c(2,5,7,3,1,1,2,2,3,6), c(5,9,1,2,3,2,3,5,2,2))
#' tGiven <- matrix (c(6,4.8,1,2.6,6.4,1.7,2.9,4.4,1.5,3.4), nrow = 10, ncol = 1)
#' initState <- matrix (c(2,1,1,2,2,2,1,1,2,1),nrow = 10, ncol = 1)
#' lambda <- matrix(c(0, 0.33, 0.45, 0), nrow = 2, ncol = 2, byrow = TRUE)
#' Xreg(tGiven, initState, Xtest, lambda)

Xreg <- function(tGiven,initState,X,lambda) {
  n <- nrow(X)
  m <- nrow(lambda)
  k <- ncol(X)
  ld <- as.matrix(rowSums(lambda))
  Lambda <- diag(as.vector(ld))
  Q <- t(lambda) - Lambda
  R <- array(0, dim=c(n, m*k))
  for (r in 1:n) {
    ii <- initState[r,]
    taur <- tGiven[r,]
    A <- t(Aver_soj_time(ii,taur,Q))
    B <- t(t(X)[,r])
    R[r,] <- t(kronecker(A, B))
  }
  R }

#-------------Estimantion of unknown model parameters--------------------


#' Estimantion of unknown Markov-modulated linear regression model parameters using GLSM
#' @description
#' This function is used to fit Markov-modulated linear regression models with two states of external environment.
#' This function estimates Markov-modulated linear regression model parameters, using GLSM.
#' Function uses the algorithm based on eigenvalues and eigenvectors decompositions.
#' @details
#' Function calculates the following expression:
#' ![](vecB.png "Fig.2"),
#' where vector of average sojourn times in each state $t_{i}$ is calculated using function Aver_soj_time, $t_{i}$ is an element of tGiven, $x_{i}$ is a vector of matrix X.

#' @param tGiven
#' Vector of the observed times (n x 1), n – number of observations
#' @param initState
#' Vector of the initial states (n x 1), n – number of observations
#' @param X
#' Matrix of predictors (n x k), n - number of observations, k - number of columns (k - 1 - number of regressors).
#' @param Y
#' Vector of the responses Y, n – number of observations
#' @param lambda
#' Matrix with the known transition rates \eqn{\lambda_{i,j}}, (m x m), m – number of states
#' @param W
#' an optional logical variable indicating should vector of weights be used in the fitting process or not.
#' If TRUE, matrix with weights is used (that is, inverse values to tGiven – observed times).
#' @return
#' Vector of estimated parameters \eqn{\beta}
#' @export

#' @examples
#'
#' lambda <- matrix(c(0, 0.33, 0.45, 0), nrow = 2, ncol = 2, byrow = TRUE)
#' Xtest <- cbind(rep_len(1,10),c(2,5,7,3,1,1,2,2,3,6), c(5,9,1,2,3,2,3,5,2,2))
#' tGiven <- matrix (c(6,4.8,1,2.6,6.4,1.7,2.9,4.4,1.5,3.4), nrow = 10, ncol = 1)
#' Y <- matrix(c (5.7, 9.9, 7.8, 14.5, 8.2, 14.5, 32.2, 3.8, 16.5, 7.7),nrow = 10, ncol = 1)
#' initState <- matrix (c(2,1,1,2,2,2,1,1,2,1),nrow = 10, ncol = 1)
#' B_est(tGiven,initState,Xtest,Y,lambda,W = 1)


B_est <- function(tGiven,initState,X,Y,lambda,W = TRUE) {

  k <- ncol(X)		# number of vars
  n <- nrow(X)		# number of obs
  m <- nrow(lambda)

  if (W == TRUE)
    WF <- diag(as.vector(1/tGiven))
  else if (W == FALSE) WF <- diag(as.vector(array(1,dim=c(n,1))))
  else {		stop()
    geterrmessage() }

  b <- solve(t(Xreg(tGiven, initState, X, lambda)) %*% WF %*% Xreg(tGiven, initState, X, lambda)) %*% (t(Xreg(tGiven, initState, X, lambda)) %*% WF %*% Y)

  return(b)}

Dhi <- function(t, Q) {
  Dm <- diag(as.vector(exp(t*as.vector(eigen(Q)$values))))
  Dm }

Pr <- function(t, Q) {
  vectors <- eigen(Q)$vectors
  pp <- vectors %*% Dhi(t=t, Q=Q) %*% solve(vectors)
  t(pp)
}

Et <- function(ii,taur,Q,mm) {
  M <- eigen(Q)$vector
  hi <- as.matrix(eigen(Q)$values)
  b <- as.matrix(solve(M)[,ii])
  R <- array(0,dim=c(mm,1))
  for (i in 1:mm) {
    if (abs(hi[i,])>1e-10) {
      R <- R + b[i,] * ( (exp(taur*hi[i,])-1) / hi[i,]) * M[,i] }
    else
    { R <- R + b[i,] * M[,i] * taur }
  }
  R
}

outer <- function(i,j,L,t,Q,mm) {
  inner <- function (x) {
    inner <- Pr(x,Q)[i,j] * Et(j,t-x,Q,mm)[L,] + Pr(x,Q)[i,L] * Et(L,t-x,Q,mm)[j,]
  }
  res<-integral(Vectorize(inner), 0, t, method="Simpson")
  res
}


CovMju2 <- function(i,t,Q,mm) {
  tt <- Et(i,t,Q,mm)
  C <- array(dim=c(mm,mm))
  for (j in 1:mm)	{
    for (L in 1:mm) {
      if (j == L) { C[j,L] <- outer(i=i,j=j,L=j,t=t,Q,mm) - tt[j]*tt[j] }
      else { C[j,L] <- outer(i=i,j=j,L=L,t=t,Q,mm) - tt[j]*tt[L] }
    }
  }
  C }


#' Estimantion of the variance of the response Y
#' @description
#' This function is used for calculation of the variance of the respone Y (Var(Y))
#' @details
#' Function calculates the following expression:
#' ![](varY.png "Fig.3"),
#' where vector of average sojourn times in each state $t_{i}$ is calculated using function Aver_soj_time
#' @param bb
#' Matrix (k x m), k - number of columns (k - 1 - number of regressors), m - number of states, m = 2,3,.. .
#' @param sigma
#' Scalar, the standard deviation of the disturbance term
#' @param i
#' number (scalar), initial state
#' @param x
#' Row-vector of the matrix of predictors X (1 x k), k - number of columns.
#' @param tau
#' number (scalar), observed time
#' @param la
#' Matrix with the known transition rates \eqn{\lambda_{i,j}}, (m x m), m – number of states
#' @return
#' Estimantion of the variance of the response Y, scalar
#' @export
#' @importFrom pracma integral

#' @importFrom stats rexp rnorm runif rgamma

#' @examples
#' Xtest <- cbind(rep_len(1,10),c(2,5,7,3,1,1,2,2,3,6), c(5,4,1,2,3,2,3,5,2,2))
#' tGiven <- matrix (c(0.9,1.18,1,1.6,1.4,1.7,1.9,1.45,1.5,2.14), nrow = 10, ncol = 1)
#' initState <- matrix (c(2,1,1,2,2,2,1,1,2,1),nrow = 10, ncol = 1)
#' lambda <- matrix(c(0, 0.33, 0.45, 0), nrow = 2, ncol = 2, byrow = TRUE)
#' beta <- matrix(c(1, 2, 3, 4, 6, 8), nrow = 3, ncol = 2, byrow = TRUE)
#' VarY(beta,1,2,Xtest[3,],10,lambda)


VarY <- function(bb,sigma,i,x,tau,la) {

  m <- ncol(la)
  ld <- as.matrix(rowSums(la))
  Lambda <- diag(as.vector(ld))
  AA <- t(la) - Lambda
  Res <- sigma^2*tau + t(as.matrix(as.vector(bb))) %*% kronecker(CovMju2(i,tau,AA,m), t(t(x))%*%t(x) ) %*% as.matrix(as.vector(bb))
  Res }


