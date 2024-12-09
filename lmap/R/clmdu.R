#' Cumulative Logistic (Restricted) MDU																		
#'
#' @param Y An N times R ordinal matrix coded with integers 1,2,.. .
#' @param X An N by P matrix with predictor variables
#' @param S Positive number indicating the dimensionality of the solution
#' @param trace boolean to indicate whether the user wants to see the progress of the function (default=TRUE)
#' @param start either starting values (list with (U,V) or (B,V)) or way to compute them (svd, random, ca)
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
#' @examples
#' \dontrun{
#' data(dataExample_clmdu)
#' Y<-dataExample_clmdu
#' X<-dataExample_clmdu
#' output1 = clmdu(Y)
#' plot(output1)
#' plot(output1, circles = NULL)
#' summary(output1)
#'
#' output2 = clmdu(Y = Y, X = X)
#' plot(output2, circles = c(1,2))
#' summary(output2)
#' }
#'
#' @importFrom MASS polr
#' @import fmdu
#' @importFrom dplyr group_by_all
#' @importFrom dplyr summarise
#' @export

clmdu = function(Y, X = NULL, S = 2, trace = FALSE, start = "svd", maxiter = 65536, dcrit = 1e-6){

  cal = match.call()

  cat("Note this is a slow procedure, if you like to see progress, set trace = TRUE", "\n")

  #
  Yoriginal = Y
  ynames = colnames(Y)
  R = ncol(Y)

  ###############################################################
  # unsupervised analysis
  ###############################################################
  if( is.null(X) ){
    Xoriginal = NULL
    mx = NULL
    sdx = NULL
    B = NULL
    xnames = NULL

    # remove all totally disagree
    # assumes coding starts with 1
    if(length(which(rowSums(Y) == R))>0) { Y = Y[-which(rowSums(Y) == R), ] }
    cmeans <- attr(scale(Y, center = TRUE, scale = FALSE), "scaled:center")

    Ydf = as.data.frame(Y)
    Yr = Ydf %>% group_by_all() %>% summarise(count = n(), .groups = "drop")
    f = Yr$count
    Y = as.matrix(Yr[, 1:R])
    N = nrow(Y)

    # starting values U, V
    if(is.list(start)){
      U = start$U
      V = start$V
    }
    else if(start == "random"){
      U = matrix(rnorm(N*S), N, S)
      V = matrix(rnorm(R*S), R, S)
    }
    else if(start == "svd"){
      udv = svd(sqrt(diag(c(f))) %*% scale(Y, center = cmeans, scale =F))
      U = diag(sqrt(1/c(f))) %*% matrix(udv$u[, 1:S], N, S) %*% diag(udv$d[1:S], nrow = S, ncol = S)
      V = matrix(udv$v[, 1:S], R, S)
    }
    else if(start == "ca"){
      Z = f*apply(Y, 2, function(x) x - min(x)) # added: f*
      r = rowSums(Z)
      cc = colSums(Z)
      udv = svd(diag(1/sqrt(r)) %*% (Z - outer(r,cc)) %*% diag(1/sqrt(cc)))
      U = matrix(diag(1/sqrt(f)) %*% udv$u[, 1:S], N, S) %*% diag(sqrt(udv$d[1:S]), nrow = S, ncol = S) # added 1/sqrt(f)
      V = matrix(udv$v[, 1:S], R, S) %*% diag(sqrt(udv$d[1:S]), nrow = S, ncol = S)
    }

    theta = - twomodedistance(U, V)

    # get radii
    m = list()
    dev = rep(NA, R)
    for(r in 1:R){
      polr.out = polr(as.factor(Y[ , r]) ~ 1 + offset(theta[, r]), weights = f) # added weights
      m[[r]] = polr.out$zeta
      dev[r] = polr.out$deviance
    }
    dev.old = sum(dev)

    # start iterating
    for (iter in 1:maxiter) {
      # compute working response
      xi = matrix(NA, N, R)
      for(r in 1:R){
        Ep = expected.p(Y[ , r], theta[ , r], m[[r]])
        xi[ , r] = 1 - 2 * Ep
      }

      # compute working response
      Z = as.matrix(theta - 4 * xi)

      # update U and V
      mdu.out = fastmdu(-Z, w = f, p = S, x = U, rx = NULL, y = V, ry = NULL, MAXITER = 10) # added weights
      U = mdu.out$row.coordinates
      V = mdu.out$col.coordinates
      theta = - twomodedistance(U, V)

      # update threshold
      for(r in 1:R){
        polr.out = polr(as.factor(Y[ , r]) ~ 1 + offset(theta[, r]), start = m[[r]], weights = f)
        m[[r]] = polr.out$zeta
        dev[r] = polr.out$deviance
      }

      # compute deviance
      dev.new <- sum(dev)

      # some iterative output or warnings
      if (trace) {cat(iter, mdu.out$lastdif, dev.new, dev.old, 2*(dev.old - dev.new)/(dev.old + dev.new), "\n")}
      if (2*(dev.old - dev.new)/(dev.old + dev.new) < dcrit){
        # rotation to principal components
        A = t(f*U)%*%U #added frequency weights
        E = eigen(A)
        RR = E$vectors
        U = U %*% RR
        V = V %*% RR
        break
      }
      if (iter == maxiter) {warning("Maximum number of iterations reached - not converged (yet)")}
      dev.old = dev.new
    }
  }

  ###############################################################
  # supervised analysis
  ###############################################################
  if( !is.null(X) ){
    f = NULL
    N = nrow(Y) #no weights so N = number of rows in Y

    Xoriginal = X
    outx = procx(X)
    X = outx$X
    mx = outx$mx
    sdx = outx$sdx

    xnames = colnames(X)
    P = ncol(X)

    if(is.list(start)){
      B = start$B
      U = X %*%B
      V = start$V
      theta = - twomodedistance(U, V)
    }
    else if (start == "random"){
      B = matrix(rnorm(P*S), P, S)
      U = X %*% B
      V = matrix(rnorm(R*S), R, S)
      theta = - twomodedistance(U, V)
    } # starting values through GSVD
    else if(start == "svd") {
#      if(P > 1) {
        eig.out = eigen(t(X) %*% X)
        iRx = eig.out$vectors %*% diag(1/sqrt(eig.out$values)) %*% t(eig.out$vectors)
        iRxX = iRx %*% t(X)
        udv = svd(iRxX %*% scale(Y))
        B = iRx %*% matrix(udv$u[, 1:S], P, S) * sqrt(N)
        ##### CHECK
      # } else {
      #   udv = svd(scale(Y))
      #   B = matrix(udv$u[, 1:S], P, S) * sqrt(N)
      # }
      #####
      U = X %*% B
      V = matrix(udv$v[, 1:S], R, S) %*% diag(udv$d[1:S], nrow = S, ncol = S) / sqrt(N)
      theta = - twomodedistance(U, V)
    }

    #thresholds
    m = list()
    dev = rep(NA, R)
    for(r in 1:R){
      polr.out = polr(as.factor(Y[ , r]) ~ 1 + offset(theta[, r]))
      m[[r]] = polr.out$zeta
      dev[r] = polr.out$deviance
    }
    dev.old = sum(dev)

    # start iterating
    for (iter in 1:maxiter) {
      # compute working response
      xi = matrix(NA, N, R)
      for(r in 1:R){
        Ep = expected.p(Y[ , r], theta[ , r], m[[r]])
        xi[ , r] = 1 - 2 * Ep
      }

      # compute working response
      Z = as.matrix(theta - 4 * xi)

      # update B/U and V
      mdu.out = fastmdu(-Z, w = NULL, p = S, x = X, rx = B, y = V, ry = NULL, MAXITER = 10)
      B = mdu.out$row.coefficients
      U = mdu.out$row.coordinates # X %*% B
      V = mdu.out$col.coordinates
      theta = - twomodedistance(U, V)

      # update threshold
      for(r in 1:R){
        # polr.out = polr(as.factor(Y[ , r]) ~ 1 + offset(theta[, r]))
        polr.out = polr(as.factor(Y[ , r]) ~ 1 + offset(theta[, r]), start = m[[r]])
        m[[r]] = polr.out$zeta
        dev[r] = polr.out$deviance
      }

      # compute deviance
      dev.new <- sum(dev)

      # some iterative output or warnings
      if (trace) {cat(iter, mdu.out$lastdif, dev.new, dev.old, 2*(dev.old - dev.new)/(dev.old + dev.new), "\n")}
      if (2*(dev.old - dev.new)/(dev.old + dev.new) < dcrit) {
        # rotation to principal components
        A = t(U)%*%U
        E = eigen(A)
        RR = E$vectors
        B = B %*% RR
        U = X %*% B
        V = V %*% RR
        break
      }
      if (iter == maxiter) {warning("Maximum number of iterations reached - not converged (yet)")}
      dev.old = dev.new
    }
  }

  if(is.null(X)){
    npar = sum(sapply(m, length)) + (N + R) * S - S*(S-1)/2
  }
  else{
    npar = sum(sapply(m, length)) + (P + R) * S - S*(S-1)/2
  }

  # output object
  output = list(call = cal,
                mdu = mdu.out,
                Yoriginal = Yoriginal,
                Xoriginal = Xoriginal,
                ynames = ynames,
                xnames = xnames,
                Y = Y,
                X = X,
                f = f,
                mx = mx,
                sdx = sdx,
                m = m,
                U = U,
                B = B,
                V = V,
                npar = npar,
                AIC = dev.new + 2 * npar,
                BIC = dev.new + log(N) * npar,
                iters = iter,
                deviance = dev.new,
                item.deviance = dev)
  class(output) = "clmdu"
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

