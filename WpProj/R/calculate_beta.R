calc.beta <- function(xtx = NULL, xty = NULL, active.idx = NULL, 
                       method = c("projection","selection.variable","scale"), 
                       OToptions = list(
                         same = FALSE,
                         transport.method = "exact",
                         epsilon = 0.05,
                         niter = 100
                       ),
                       x = NULL,
                       theta = NULL,Y = NULL, niter=5e2) {
  tol <- 1e-07
  method <- match.arg(method)

  p <- length(active.idx)
  
  stopifnot(length(active.idx) > 0)
  
  if (method == "projection") {
    
    if(is.null(xtx)){
      if(is.null(x)) stop("must specify xtx or x")
      xtx <- tcrossprod(x)
    }
    # p_star <- nrow(xtx)
    
    if (is.null(xty)){
      if ( !is.null(Y) ) {
        if ( nrow(theta) != ncol(xtx) ) theta <- t(theta)
        mu <- Y
        # if ( nrow(mu) != n ) mu <- t(mu)
      } else {
        if(is.null(x) | is.null(theta)) {
          stop("Must specify x and theta or Y")
        }
        mu <-  crossprod(x , theta)
      }
      xty <- x %*% mu
    }
    
    # checks 
    stopifnot(nrow(xty) == ncol(xtx))
    # stopifnot(ncol(xty) == ncol(mu))
    # stopifnot(nrow(xty) == nrow(x))
    # stopifnot(ncol(xtx) == nrow(x))
    
    #set up data for loop
    # wt <- n/(n+pseudo.obs)
    # if(length(active.idx) <= 0) browser()
    U <- chol(xtx[active.idx,active.idx,drop=FALSE])
    # x_temp <- x[active.idx, ,drop=FALSE]
    # theta_temp <- theta[active.idx, , drop=FALSE]
    # mu_temp <- crossprod(x_temp, theta_temp)
    xty_temp <- xty[active.idx,,drop=FALSE]
    
    # theta_perp_old <- matrix(Inf, nrow=length(active.idx),ncol=length(active.idx))
    theta_perp <- matrix(0, nrow=p,ncol=p)
    # beta <- NULL
    
    theta_perp <- backsolve(U, backsolve(U, xty_temp, transpose=TRUE))
      
      # beta <- c(beta,  theta_perp[1,1])
    beta <- theta_perp
  } 
  else if(method == "selection.variable") {
    beta <- rep(1, p)
  } 
  else if (method=="scale") {
    if(is.null(OToptions)){
      OToptions <- list(
        same = FALSE,
        method = method,
        transport.method = "exact",
        epsilon = 0.05,
        niter = 100
      )
    } else {
      if(is.null(OToptions$same)) OToptions$same <- FALSE
      if(is.null(OToptions$transport.method)) OToptions$transport.method <- "exact"
      if(is.null(OToptions$epsilon)) OToptions$epsilon <- 0.05
      if(is.null(OToptions$niter)) OToptions$niter <- 100
      OToptions$method = method
    }
    # checks
    
    if ( !is.null(Y) ) {
      if (nrow(theta) != nrow(x)) theta <- t(theta)
      stopifnot(nrow(theta) == nrow(x))
      mu <- Y
      # if ( nrow(mu) != n ) mu <- t(mu)
    } else {
      mu <- crossprod(x, theta)
    }
    # n <- ncol(x)
   
    # wt <- n/(n+pseudo.obs)
    
    # if ( is.null(theta_norm) ) theta_norm <- colMeans(theta^2)
    if ( is.null(xtx) | is.null(xty) ) {
      # sufficientStatistics(const NumericMatrix & X_, const NumericMatrix & Y_, 
      #                      const NumericMatrix & theta_,
      #                      const Rcpp::List & options)
      suffStat <- sufficientStatistics(t(x), mu, t(theta), OToptions)
      xtx <- suffStat$XtX #* wt + diag(theta_norm) * (1-wt)
      xty <- suffStat$XtY #* wt + theta_norm * (1-wt)
    }
    # p_star <- nrow(xty)
    # initialize selection vector
    beta <- beta_old <- rep(Inf, p)
    
    #set up data for loop
    U <- chol(xtx[active.idx,active.idx, drop = FALSE])
    x_temp <- x[active.idx, , drop=FALSE]
    theta_temp <- theta[active.idx,, drop=FALSE]
    xty_temp <- xty[active.idx]
    txtemp <- t(x_temp)
    ttheta <- t(theta_temp)
    # theta_norm_temp <- theta_norm[active.idx]
    for (i in 1:niter) {
      beta <- backsolve(U, backsolve(U, xty_temp, transpose=TRUE))
      # mu <- selVarMeanGen(x_temp, beta, theta_temp)
      xty_temp <- xtyUpdate(txtemp, mu, ttheta, beta, OToptions)
      if (not.converged(beta, beta_old, tol)) {
        beta_old <- beta
      } else {
        break
      }
    }
    if( i == niter) warning("Maximum iterations hit in optimizing coefficients")
    beta <- c(beta)
  } 
  if(is.null(xtx)) {
    if(!is.null(x)) {
      p_star <- nrow(x)
    } else
      p_star <- nrow(theta)
  } else {
    p_star <- ncol(xtx)
  }
  # print(p_star)
  if(method == "projection") {
    beta_out <- matrix(0, nrow=p_star, ncol=ncol(xty))
    beta_out[active.idx, ] <- beta
  } else {
    beta_out <- rep(0, p_star)
    beta_out[active.idx] <- c(beta)
  }
  
  return(beta_out)
}
