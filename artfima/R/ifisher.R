ifisher <- function(d = numeric(0), lambda = numeric(0),  phi = numeric(0), 
                    theta = numeric(0), sigma2 = 1, n = 1, obj=NULL, 
                    alg=c("Fisher", "Whittle", "approx")) {
  if (!is.null(obj)) {
    if ("artfima" == class(obj)) {
      d <- obj$dHat
      lambda <- obj$lambdaHat
      phi <- obj$phiHat
      theta <- obj$thetaHat
      sigma2 <- obj$sigmaSq
    }
  }
  alg <- match.arg(alg)
  if(length(phi)>0 || length(theta)> 0) {
    stop("only TFD and FD implemented at this time - ARMA extension soon!")
  }
  if ((lambda==0 || length(lambda)==0)) { #FD
    f <- dilog(1)
    se <- sqrt(diag(solve(f*n)))
    } else { #TFD 
    v11 <- switch(alg,
      Fisher = dilog(exp(-lambda)),
      Whittle = getWv11(lambda),
      approx = exp(-2*lambda)*(1+ 0.5*exp(-2*lambda)))
    v22 <- (d^2)*exp(-2*lambda)/(1-exp(-2*lambda))
    v12 <- d*log(1-exp(-2*lambda))
    f <- matrix(c(v11,v12,v12,v22), ncol=2, 
                dimnames=list(c("d", "lambda"),c("d", "lambda")))
    se <- sqrt(diag(solve(f*n)))
    }
  list(se = se, f = f)
}

getWv11 <- function(lambda){
  v11d <- function(lam, w) {
    -log(1+exp(-2*lam)-2*cos(w)*exp(-lam))
  }
  f <- function(x) v11d(0.01, x)^2
  (integrate(f, lower=-pi, upper=pi)$value)/(4*pi)
}

"iARMA" <-
  function(phi = numeric(0), theta = numeric(0), period = 0)
  {
    ########information matrix of arma#####################
    if(!(InvertibleQ(phi) & InvertibleQ(theta))) {
      cat("Model is non-causal or non-invertible\n")
      return(NULL)
    }
    p <- length(phi)
    q <- length(theta)
    unames <- character(0)
    vnames <- character(0)
    if(p > 0) {
      if(p > 1) {
        vvmatrix <- (tccfAR(phi, phi)[ - (1:(p - 1))])[ - (p + 
                                                             1)]
      }
      else if(p == 1) {
        vvmatrix <- tccfAR(phi, phi)[ - (p + 1)]
      }
      vvmatrix <- toeplitz(vvmatrix)
      imatrix <- vvmatrix
      if(!period) vnames <- paste("phi(", 1:p, ")", sep = "")
      else vnames <- paste("phi.",period,"(", 1:p, ")", sep = "")
    }
    if(q > 0) {
      if(q > 1) {
        uumatrix <- (tccfAR(theta, theta)[ - (1:(q - 1))])[ - (
          q + 1)]
      }
      else if(q == 1) {
        uumatrix <- tccfAR(theta, theta)[ - (q + 1)]
      }
      uumatrix <- toeplitz(uumatrix)
      imatrix <- uumatrix
      if(!period) unames <- paste("theta(", 1:q, ")", sep = "")
      else unames <- paste("theta.",period,"(", 1:q, ")", sep = "")
    }
    if(p > 0 && q > 0) {
      uvmatrix <- matrix(numeric(1), nrow = p, ncol = q)
      tuv <-  - tccfAR(phi, theta)
      for(i in 1:p) {
        for(j in 1:q) {
          uvmatrix[i, j] <- tuv[q + i - j]
        }
      }
      imatrix <- cbind(rbind(vvmatrix, t(uvmatrix)), rbind(uvmatrix, 
                                                           uumatrix))
    }
    
    inames <- c(vnames, unames)
    dimnames(imatrix) <- list(inames, inames)
    return(imatrix)
  }
"tccfAR" <-
  function(phi, theta)
  {
    #auxilary function used with iarma#########
    #computes the theoretical cross-covariance function of two autoregressions
    # z[t]-phi[1] z_[t-1] --- phi[p] z[t-p]     = a[t]
    # z[t]-theta[1] z_[t-1] --- theta[q] z[t-q] = a[t]
    # where p, q are length(phi), length(theta)
    p <- length(phi)
    q <- length(theta)
    if(p == 0 || q == 0)
      return(numeric(0))
    k <- p + q
    rhs <- c(-1, rep(0, k - 1))
    A <- matrix(numeric(k^2), nrow = k, ncol = k)
    for(i in 1:k) {
      for(j in 1:k) {
        imj <- i - j
        ijq <- i + j - q - 1
        if(i > q) {
          if(i > j && imj <= q)
            A[i, j] <- theta[imj]
          else if(i > q && imj == 0)
            A[i, j] <- -1
        }
        else {
          if(ijq > 0 && ijq <= p)
            A[i, j] <- phi[ijq]
          else if(ijq == 0)
            A[i, j] <- -1
        }
      }
    }
    return(solve(A, rhs))
  }

"psiwtsAR" <-
  function(phi, maxlag)
  {
    p <- length(phi)
    if(p == 0) return(0)
    x <- numeric(maxlag + 1)
    x <- 1
    for(i in 1:p) {
      x[i + 1] <- crossprod(phi[1:i], (rev(x))[1:i])
    }
    if(maxlag > p) {
      for(i in (p + 1):maxlag) {
        x[i + 1] <- crossprod(phi, (rev(x))[1:p])
      }
    }
    return(x)
  }

InvertibleQ <- 
  function(phi){
    if(length(phi) == 0) return(T)
    return(all(abs(ARToPacf(phi))<1))
  }

"ARToPacf"   <-  
  function(phi){
    phik <- phi
    L <- length(phi)
    if(L == 0) return(numeric(0))
    pi <- numeric(L)
    for (k in 1:L){
      LL <- L+1-k
      pi[L+1-k] <- a <- phik[LL]
      phikp1 <- phik[-LL]
      if(abs(a)==1) break
      phik <- (phikp1+a*rev(phikp1))/(1-a^2)
    }
    pi
  }

"PacfToAR" <-
  function(pi){
    L <- length(pi)
    if (L == 0) return(numeric(0))
    if (L == 1) return(pi)
    phik <- pi[1]
    for (k in 2:L){
      phikm1 <- phik
      phik <- c(phikm1-pi[k]*rev(phikm1),pi[k])
    }
    phik
  }


