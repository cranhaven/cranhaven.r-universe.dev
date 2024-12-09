#' Extended Stereotype Model
#'
#' The function esm performs extended stereotype model analysis for multivariate logistic analysis
#' i.e. a double constrained reduced rank multinomial logistic model
#'
#' @param Y An N times R binary matrix  .
#' @param X An N by P matrix with predictor variables
#' @param S Positive number indicating the dimensionality of teh solution
#' @param Z design matrix for response
#' @param W design matrix for intercepts
#' @param ord.z if Z = NULL, the function creates Z having order ord.z
#' @param ord.m if W = NULL, the function creates W having order ord.m
#' @param scale.x whether X should be scaled to zero mean and standard deviation one
#' @param trace whether progress information should be printed on the screen
#' @param maxiter maximum number of iterations
#' @param dcrit convergence criterion
#'
#' @return This function returns an object of the class \code{esm} with components:
#' \item{call}{function call}
#' \item{Xoriginal}{Matrix X from input}
#' \item{X}{Scaled X matrix}
#' \item{mx}{Mean values of X}
#' \item{sdx}{Standard deviations of X}
#' \item{Y}{Matrix Y from input}
#' \item{pnames}{Variable names of profiles}
#' \item{xnames}{Variable names of predictors}
#' \item{znames}{Variable names of responses}
#' \item{Z}{Design matrix Z}
#' \item{W}{Design matrix W}
#' \item{G}{Profile indicator matrix G}
#' \item{m}{main effects}
#' \item{bm}{regression weights for main effects}
#' \item{Bx}{regression weights for X}
#' \item{Bz}{regression weights for Z}
#' \item{A}{regression weights (Bx Bz')}
#' \item{U}{matrix with coordinates for row-objects}
#' \item{V}{matrix with coordinates for column-objects}
#' \item{Ghat}{Estimated values of G}
#' \item{deviance}{value of the deviance at convergence}
#' \item{df}{number of paramters}
#' \item{AIC}{Akaike's informatoin criterion}
#' \item{iter}{number of main iterations from the MM algorithm}
#' \item{svd}{Singular value decomposition in last iteration}
#'
#' @examples
#' \dontrun{
#' data(dataExample_lpca)
#' Y = as.matrix(dataExample_lpca[ , 1:5])
#' X = as.matrix(dataExample_lpca[ , 9:13])
#' #unsupervised
#' output = esm(X, Y, S = 2, ord.z = 2)
#' }
#'
#' @importFrom stats plogis model.matrix formula
#' @importFrom nnet class.ind
#'
#'
#' @export
esm = function(X, Y, S = 2, Z = NULL, W = NULL, ord.z = 1, ord.m = R, scale.x = FALSE, trace = FALSE, maxiter = 65536, dcrit = 1e-6){
  # preparation
  cal = match.call()
  n = nrow(X)
  ones.n = matrix(1,n,1)
  P = ncol(X)
  R = ncol(Y)

  out = make.profile(Y = Y, ord = ord.z)
  if(is.null(Z)){ Z = out$Z }
  G = out$G
  A = out$A
  Q = ncol(Z)

  if(is.null(W)){
    if(ord.m ==1){
      qrz = qr(model.matrix(~., data = A)[, -1])
    }
    else{
      qrz = qr(model.matrix(formula(paste("~.^", ord.m, sep = "")), data = A)[ , -1])
    }
  }

  # scaling of predictor matrices
  Xoriginal = X
  outx = procx(X)
  X = outx$X
  mx = outx$mx
  sdx = outx$sdx
  # Xoriginal = X
  # if(scale.x){
  #   X = scale(X)
  #   mx = attr(X, "scaled:center")
  #   sdx = attr(X, "scaled:scale")
  # }
  # else{
  #   X = X
  #   mx = rep(0, P)
  #   sdx = rep(1, P)
  # }

  if(P == 1){
    iRx = 1/sqrt(t(X) %*% X)
  }
  else{
    eig.out = eigen(t(X) %*% X)
    iRx = eig.out$vectors %*% diag(1/sqrt(eig.out$values)) %*% t(eig.out$vectors)
  }

  if(ncol(Z) == 1){
    iRz = 1/sqrt(t(Z) %*% Z)
  }
  else{
    eig.out = eigen(t(Z) %*% Z)
    iRz = eig.out$vectors %*% diag(1/sqrt(eig.out$values)) %*% t(eig.out$vectors)
  }

  # initialization
  m = colMeans(G)
  bm = solve.qr(qrz, m)
  m = qr.fitted(qrz, m)
  #
  udv = svd(1/sqrt(n) * iRx %*% t(X) %*% G %*% Z %*% iRz)
  Bx = iRx %*% matrix(udv$u[, 1:S], P, S) * sqrt(n)
  Bz = iRz %*% matrix(udv$v[, 1:S], Q, S) %*% diag(udv$d[1:S], nrow = S, ncol = S) / sqrt(n)
  theta = ones.n %*% t(m) + X %*% Bx %*% t(Z %*% Bz)
  Ghat = exp(theta) / rowSums(exp(theta))
  dev.old = -2 * sum(log(Ghat[G == 1]))

  # iteration
  iter = 0; dif = 1
  while(dif > dcrit){
    iter = iter + 1
    # update m
    ZZ = theta + 4 * (G - Ghat)
    m = colMeans(ZZ - X %*% Bx %*% t(Z %*% Bz))
    bm = solve.qr(qrz, m)
    m = qr.fitted(qrz, m)

    # update Bx (U) and Bz (V)
    theta = ones.n %*% t(m) + X %*% Bx %*% t(Z %*% Bz)
    Ghat = exp(theta) / rowSums(exp(theta))
    ZZ = theta + 4 * (G - Ghat) - ones.n %*% t(m)
    udv = svd(iRx %*% t(X) %*% ZZ %*% Z %*% iRz)
    Bx = iRx %*% matrix(udv$u[, 1:S], P, S) * sqrt(n)
    Bz = iRz %*% matrix(udv$v[, 1:S], Q, S) %*% diag(udv$d[1:S], nrow = S, ncol = S) / sqrt(n)

    # deviance
    theta = ones.n %*% t(m) + X %*% Bx %*% t(Z %*% Bz)
    Ghat = exp(theta) / rowSums(exp(theta))
    dev.new = -2 * sum(log(Ghat[G == 1]))

    # convergence
    dif = 2 * (dev.old - dev.new)/ ((dev.old + dev.new))
    if ( trace ) cat(iter, dev.old, dev.new, dif, "\n")
    if ( dif < dcrit ) break
    if ( iter == maxiter ) warning("Maximum number of iterations reached - not converged (yet)")
    dev.old = dev.new
  } # end iteration

  BB = Bx %*% t(Bz)

  # name giving
  rownames(Bz) = colnames(Z)
  rownames(Bx) = colnames(X)
  rownames(BB) = colnames(X)
  colnames(BB) = colnames(Z)
  rownames(bm) = colnames(W)

  npar = length(bm) + (P + Q - S) * S
  # create output object
  results = list(
    call = cal,
    Xoriginal = Xoriginal,
    X = X,
    mx = mx,
    sdx = sdx,
    Y = Y,
    pnames = out$Aprofile,
    xnames = colnames(X),
    znames = colnames(Z),
    Z = Z,
    W = W,
    G = G,
    m = m,
    bm = bm,
    Bx = Bx,
    Bz = Bz,
    A = BB,
    U = X %*% Bx,
    V = Z %*% Bz,
    Ghat = Ghat,
    deviance = dev.new,
    df = npar,
    AIC = dev.new + 2 * npar,
    iter = iter,
    svd = udv
  )
  class(results) <- "esm"
  return(results)
}

make.profile = function(Y, ord){
  n = nrow(Y)
  R = ncol(Y)
  profile = matrix(NA, n, 1)
  for(i in 1:n){profile[i, 1] = paste(Y[i,], collapse = "")}
  G = class.ind(profile)
  C = ncol(G)

  A = unique(Y)
  Aprofile = matrix(NA, nrow(A),1)
  AA = ifelse(A == 0, "-", "+")
  for(i in 1:nrow(AA)){Aprofile[i, 1] = paste(AA[i,], sep="", collapse = "")}
  Ai = t(class.ind(Aprofile))
  aidx = rep(NA, nrow(A)); for(j in 1:nrow(A)){aidx[j] = which(Ai[j,] == 1, arr.ind = TRUE)}
  A = A[aidx,]
  A = (2 * A - 1)/2
  A = as.data.frame(A)
  rownames(A) = colnames(G)
  colnames(A) = colnames(Y)
  if (ord == 1){
    Z = model.matrix(~ ., data = A)[, -1]
  }
  else if (ord == 2){
    Z = model.matrix(~ .^2, data = A)[, -1]
  }
  else if (ord == 3){
    Z = model.matrix(~ .^3, data = A)[, -1]
  }
  else if(ord == R){
    Z = diag(nrow(A))
  }
  #
  output = list(
    Z = Z,
    G = G,
    A = A,
    Aprofile = Aprofile)
  return(output)
}
