#' Cumulative Logistic (Restrcited) PCA
#'
#' @param Y An N times R ordinal matrix  .
#' @param X An N by P matrix with predictor variables
#' @param S Positive number indicating the dimensionality of the solution
#' @param lambda if TRUE does lambda scaling (see Understanding Biplots, p24)
#' @param trace tracing information during iterations
#' @param maxiter maximum number of iterations
#' @param dcrit convergence criterion
#' @return Y Matrix Y from input
#' @return Xoriginal Matrix X from input
#' @return X Scaled X matrix
#' @return mx Mean values of X
#' @return sdx Standard deviations of X
#' @return ynames Variable names of responses
#' @return xnames Variable names of predictors
#' @return probabilities Estimated values of Y
#' @return m main effects
#' @return U matrix with coordinates for row-objects
#' @return B matrix with regression weight (U = XB)
#' @return V matrix with vectors for items/responses
#' @return iter number of main iterations from the MM algorithm
#' @return deviance value of the deviance at convergence
#'   
#' @examples
#' \dontrun{
#' data(dataExample_clpca)
#' Y<-as.matrix(dataExample_clpca[,5:8])
#' X<-as.matrix(dataExample_clpca[,1:4])
#' out = clpca(Y)
#' out = clpca(Y, X)
#' }
#'
#' @importFrom MASS polr
#' @export

clpca <- function(Y, X = NULL, S = 2, lambda = FALSE, trace = FALSE, maxiter = 65536, dcrit = 1e-6){

  cal = match.call()

  # checks
  if ( is.null( Y ) ) stop( "missing response variable matrix Y")
  if( ! is.null(X)) if ( nrow(Y) != nrow(X) ) stop( "number of rows in X and Y should match")
  if( S < 1) stop("dimensionality (S) should be larger than 1")
  if( maxiter < 1) stop("maximum number of iterations should be a positive number")
  if( dcrit < 0) stop("converence criterion should be a positive number")

  ynames = colnames(Y)

  # Data Coding
  N = nrow(Y)
  R = ncol(Y)

  ###############################################################
  # unsupervised analysis
  ###############################################################
  if( is.null(X) ){
    mx = NULL
    sdx = NULL
    B = NULL
    Xoriginal = NULL
    xnames = NULL

    # starting values - PCA on data
    udv = svd(scale(Y, center = TRUE, scale = FALSE))
    U = matrix(udv$u[, 1:S], N, S)
    V = matrix(udv$v[, 1:S], R, S)
    D = diag(udv$d[1:S], nrow = S, ncol = S)
    theta = U %*% D %*% t(V)

    m = list()
    dev = rep(NA, R)
    for(r in 1:R){
      polr.out = polr(as.factor(Y[ , r]) ~ 1 + offset(theta[, r]))
      m[[r]] = polr.out$zeta
      dev[r] = polr.out$deviance
    }
    dev.old = sum(dev)

    for (iter in 1:maxiter) {
      # get expectation of p_{ir}
      xi = matrix(NA, N, R)
      for(r in 1:R){
        Ep = expected.p(Y[ , r], theta[ , r], m[[r]])
        xi[ , r] = 1 - 2 * Ep
      }

      # compute working response
      Z = as.matrix(theta - 4 * xi)

      # update U and V
      udv = svd(scale(Z, center = TRUE, scale = FALSE))
      U = matrix(udv$u[, 1:S], N, S) * sqrt(N)
      VV = matrix(udv$v[, 1:S], R, S)
      D = diag(udv$d[1:S], nrow = S, ncol = S)
      V = VV %*% D/sqrt(N)
      theta = U %*% t(V)

      # update threshold
      for(r in 1:R){
        polr.out = polr(as.factor(Y[ , r]) ~ 1 + offset(theta[, r]), start = m[[r]])
        m[[r]] = polr.out$zeta
        dev[r] = polr.out$deviance
      }

      # compute deviance
      dev.new <- sum(dev)
      if (trace) {cat(iter, dev.new, (2*(dev.old - dev.new)/(dev.old + dev.new)), "\n")}
      if ( (2*(dev.old - dev.new)/(dev.old + dev.new)) < dcrit ) break
      if (iter == maxiter) warning("Maximum number of iterations reached - not converged (yet)")
      dev.old = dev.new
    }

    if(lambda){
      # lambda-scaling - see Gower/Le Roux & Lubbe, page 24
      ssqu = sum(U^2)
      ssqv = sum(V^2)
      lambda = (N/R) * ssqv/ssqu
      U = lambda*U
      V = V/lambda
    }
  }
  ###############################################################
  # supervised analysis - TO DO
  ###############################################################
  if( !is.null(X) ){

    xnames = colnames(X)
    P = ncol(X)

    # center and scale X
    Xoriginal = X
    outx = procx(X)
    X = outx$X
    mx = outx$mx
    sdx = outx$sdx

    #
    eig.out = eigen(t(X) %*% X)
    Rx = eig.out$vectors %*% diag(sqrt(eig.out$values)) %*% t(eig.out$vectors)
    iRx = solve(Rx)
    iRxX = iRx %*% t(X)

    # starting values
    udv = svd(iRxX %*% scale(Y, center = TRUE, scale = FALSE))
    B = iRx %*% matrix(udv$u[, 1:S], P, S) * sqrt(N)
    V = matrix(udv$v[, 1:S], R, S) %*% diag(udv$d[1:S], nrow = S, ncol = S) / sqrt(N)

    theta = X %*% B %*% t(V)

    m = list()
    dev = rep(NA, R)
    for(r in 1:R){
      polr.out = polr(as.factor(Y[ , r]) ~ 1 + offset(theta[, r]))
      m[[r]] = polr.out$zeta
      dev[r] = polr.out$deviance
    }

    # compute deviance
    dev.old = sum(dev)

    xi = matrix(NA, N, R)

    for (iter in 1:maxiter) {
      # get expectation of p_{ir}
      for(r in 1:R){
        Ep = expected.p(Y[ , r], theta[ , r], m[[r]])
        xi[ , r] = 1 - 2 * Ep
      }

      # compute working response
      Z = as.matrix(theta - 4 * xi)

      # update B and V
      udv = svd(iRxX %*% Z)
      B = iRx %*% matrix(udv$u[, 1:S], P, S) * sqrt(N)
      V = matrix(udv$v[, 1:S], R, S) %*% diag(udv$d[1:S], nrow = S, ncol = S) / sqrt(N)

      # compute new structural part
      theta = X %*% B %*% t(V)

      # update thresholds
      for(r in 1:R){
        polr.out = polr(as.factor(Y[ , r]) ~ 1 + offset(theta[, r]))
        m[[r]] = polr.out$zeta
        dev[r] = polr.out$deviance
      }

      # compute deviance
      dev.new <- sum(dev)

      if (trace) {cat(iter, dev.new, (2*(dev.old - dev.new)/(dev.old + dev.new)), "\n")}
      if ((2*(dev.old - dev.new)/(dev.old + dev.new)) < dcrit) break
      if (iter == maxiter) warning("Maximum number of iterations reached - not converged (yet)")
      dev.old = dev.new
    }
    U = X %*% B
  }

  if(is.null(X)){
    npar = sum(sapply(m, length)) + (N + R - S) *S
  }
  else{
    npar = sum(sapply(m, length)) + (P + R - S) *S
  }

  # create output object of class "l.pca"
  output = list(
    call = cal,
    Y = Y,
    Xoriginal = Xoriginal,
    X = X,
    mx = mx,
    sdx = sdx,
    ynames = ynames,
    xnames = xnames,
    svd = udv,
    m = m,
    U = U,
    B = B,
    V = V,
    npar = npar,
    AIC = dev.new + 2 * npar,
    BIC = dev.new + log(N) * npar,
    iter = iter,
    deviance = dev.new,
    item.dev = dev
  )
  #
  class(output) = "clpca"
  return(output)
}


expected.p = function(y, theta, m){
  # computes expected value of p (E(p))
  # y: ordinal response variable
  # theta: current ``(bi)-linear predictor''
  # m: current thresholds
  tau = c(-Inf, m)
  # y.adjusted: drop missing levels and reorder accordingly
  y.adj <- as.numeric(factor(rank(y)))
  ymax = max(y.adj)
  y = y.adj + 1
  ymin1 = y.adj
  # compute expected value of p
  p = ifelse(y.adj == 1, exp(2*tau[y] - 2*theta) / (2 * ((exp(tau[y]- theta) + 1)^2))/plogis(tau[y] - theta),
             ifelse(y.adj == ymax,(2 * exp(tau[ymin1] - theta) + 1)/(2 * ((exp(tau[ymin1]- theta) + 1)^2))/(1 -  plogis(tau[ymin1] - theta)),
                    ((2 * exp(tau[ymin1] - theta) + 1)/(2 * ((exp(tau[ymin1] - theta) + 1)^2)) - (2 * exp(tau[y] - theta) + 1)/
                       (2 * ((exp(tau[y] - theta) + 1)^2)))/(plogis(tau[y] - theta) - plogis(tau[ymin1] - theta))
             ))
  return(p)
}

