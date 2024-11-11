extractCoef <- function(fit, drop=FALSE) {

  lambda <- unlist(fit$lambda)
  nlambda <- length(lambda)

  if(any(class(fit) == "glmnet")){
    nvar <- fit$df+1
    extractCoefVar <- tapply(lambda, nvar, min)
    coefs <- stats::coef(fit, s = extractCoefVar)
    idx <- which(lambda %in% extractCoefVar)
  } else if(any(class(fit)=="oem")){
      nvar <- colSums(fit$beta[[1]] != 0)
      extractCoefVar <- tapply(lambda, nvar, min)
      idx <- which(lambda %in% extractCoefVar)
      coefs <- fit$beta[[1]][,idx,drop=FALSE]
  } else if(any(inherits(fit, "sparse-posterior") | inherits(fit, "WpProj"))) {
      nvar <- colSums(fit$beta != 0)
      if(length(nvar) < length(lambda)) lambda <- lambda[1:length(nvar)]
      extractCoefVar <- tapply(lambda, nvar, min)
      iters <- fit$niter
      maxit <- fit$maxit
      if(length(dim(iters)) == 2) {
        lastHitMax <- (iters[nrow(iters),nlambda]>=maxit)
      } else {
        lastHitMax <- FALSE
      }
      if(lastHitMax == TRUE & sum(nvar==max(nvar))>1) {
        last.idx <- which(nvar==max(nvar))
        hitMax <- ifelse(iters[nrow(iters),last.idx]>=maxit,1,0)
        cummulative <- cumsum(hitMax)
        diffs <- diff(c(0,cummulative))
        rep.idx <- max(last.idx[diffs == 0])
        extractCoefVar[length(extractCoefVar)] <- lambda[rep.idx]
      }
      # hitInf <- is.infinite(colSums(fit$beta))
      # if(lastHitMax == TRUE & sum(nvar==max(nvar))>1) {
      #   last.idx <- which(nvar==max(nvar))
      #   hitMax <- ifelse(iters[nrow(iters),last.idx]>=maxit,1,0)
      #   cummulative <- cumsum(hitMax)
      #   diffs <- diff(c(0,cummulative))
      #   rep.idx <- max(last.idx[diffs == 0])
      #   extractCoefVar[length(extractCoefVar)] <- lambda[rep.idx]
      # }
      idx <- which(lambda %in% extractCoefVar)

      coefs <- fit$beta[,idx,drop=FALSE]

      orders <- order(nvar[idx])
      coefs <- coefs[,orders,drop=FALSE]
      idx <- idx[orders]
    }
  selLamb <- lambda[idx]
  selVar <- nvar[idx]
  if(drop!=FALSE) coefs <- coefs[-drop,,drop=FALSE]
  return(list(coefs = coefs, lambda = selLamb, nzero = selVar))
}

L0Coef <- function(fit) {
  nzero <- colSums(fit$beta != 0)
  return(list(coefs = fit$beta, nzero = nzero))
}

annealCoef <- function(fit, theta) {
  if(inherits(fit[[1]], "sparse-posterior") | inherits(fit[[1]], "WpProj")){
    idx <- lapply(fit, function(f) f$optimal$index)
    numActive <- sapply(idx, length)
    beta <- lapply(fit, function(f) f$optimal$beta)
    p <- length(numActive)
    force <- fit[[1]]$force
    
    coefs <- matrix(0, nrow=length(beta[[1]]), ncol=p)
    coefs[force,1:length(force)] <- 1
    coefs[,p] <- 1
    
    for(j in seq_along(numActive)){
      jj <- numActive[j]
      coefs[, jj] <- c(beta[[j]])
    }
    numActive <- c(numActive)
  } else if (inherits(fit, "WpProj") && is.list(fit$history)) {
    idx <- lapply(fit$optimal, function(f) f$index)
    numActive <- sapply(idx, length)
    beta <- lapply(fit$optimal, function(f) f$beta)
    keep <- numActive >0
    idx <- idx[keep]
    numActive <- numActive[keep]
    beta <- beta[keep]
    p <- length(numActive)
    force <- fit$force
    if(missing(theta)) theta <- NULL
    if(length(beta) == 0 & !is.null(theta)) {
      d <- nrow(theta)
    } else if (length(beta) == 0 & is.null(theta)) {
      d <- 0
    } else {
      d <- length(beta[[1]])
    }
    
    coefs <- matrix(0, nrow=d, ncol=p)
    # coefs[force,1:length(force)] <- 1
    # coefs[,p] <- 1
    
    if(d > 0) {
      for(j in seq_along(numActive)){
      jj <- numActive[j]
      coefs[, j] <- c(beta[[j]])
      }
      numActive <- c(numActive)
    }
  } else if(inherits(fit, "WpProj")) {
  if(is.matrix(fit$optimal$beta)){
    nr <- nrow(fit$optimal$beta)
    nc <- ncol(fit$optimal$beta)
  } else {
    nr <- length(fit$optimal$beta)
    nc <- 1
  }
  coefs <- matrix(c(fit$optimal$beta), nrow = nr, ncol = nc)
  numActive <- length(fit$optimal$index)
  } else {
    stop("Not of class 'WpProj'")
  }
  

  return(list(coefs = coefs, nzero = numActive))
}

stepCoef <- function(fit, theta) {
  # idx <- fit$beta
  # 
  # coefs <- sapply(idx, function(i) {
  #   ones <- rep(0, p)
  #   ones[i] <- 1
  #   return(ones)
  # })
  return(list(coefs = fit$beta, nzero = fit$nzero))
}

IPCoef <- function(fit) {
  return(list(coefs = fit$beta, nzero = fit$nonzero_beta))
}

extractTheta <- function(fit, theta) {
  if(any(class(fit)=="optimization")) {
    coefficient_trajectory <- extractCoef(fit)
    method <- fit$method
  } else if ( any(class(fit)=="stepwise" ) ) {
    coefficient_trajectory <- stepCoef(fit, theta)
    method <- fit$method
  } else if ( any(class(fit)=="annealing" )) {
    coefficient_trajectory <- annealCoef(fit, theta)
    method <- fit$method
  } else if ( inherits(fit, "IP") ) {
    coefficient_trajectory <- IPCoef(fit)
    method <- "IP"
  } else if (inherits(fit, "L0")) {
    coefficient_trajectory <- L0Coef(fit)
    method <- fit$method
  }

  nzero <- coefficient_trajectory$nzero
  n.coefsets <- length(nzero)
  if (all(nzero == 0) || all(is.na(nzero))) return(list(theta = list(matrix(0, nrow = nrow(theta), ncol = ncol(theta))), 
                                  nzero = 0))
  p <- nrow(theta)
  s <- ncol(theta)
  
  theta_list <- vector("list", n.coefsets)

  if ( method == "projection" ) {
    for ( i in seq_along(theta_list) ) {
      theta_list[[i]] <- matrix(coefficient_trajectory$coef[,i],p,s)
    }
    nzero <- ceiling(nzero/s)
  } else {
    is_loc_scale <- (method == "location.scale")

    if ( is_loc_scale ) {
      mean_theta <- matrix(rowMeans(theta),p, s)
      c_theta    <- theta - mean_theta
      theta <- rbind(c_theta, mean_theta)
      nzero <- ceiling(nzero/2)
    }

    for ( i in seq_along(theta_list) ) {
        theta_list[[i]] <- diag(coefficient_trajectory$coef[,i] ) %*% theta
      if ( is_loc_scale ) {
        theta_list[[i]] <- theta_list[[i]][1:p,] + theta_list[[i]][( p + 1):(2 * p),]
      }
    }
  }
  if(any(is.na(nzero))) {
    idx <- which(is.na(nzero))
    theta_list[idx] <- NULL
    nzero <- nzero[-idx]
  }

  return(list(theta=theta_list, nzero=nzero))

}
