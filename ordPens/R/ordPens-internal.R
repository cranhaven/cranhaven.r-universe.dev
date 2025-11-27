coding <- function(x, constant=TRUE, splitcod=TRUE)
{
  # Converts ordinal/categorical data into split-coding, or dummy-coding.
  
  # Input:
  # x: data matrix with ordinal/categorical data
  # constant: should a constant added in the regression
  
  # Output:
  # Split/dummy-coded data-matrix
  # Reference-Category = 1
  n <- nrow(x)
  kx <- apply(x,2,max) - 1
  xds <- matrix(0,n,sum(kx))
  
  # Loop to run through the columns of x
  for (j in 1:ncol(x))
  {
    j1col <- ifelse(j>1,sum(kx[1:(j-1)]),0)
    # Loop to run through the rows of x
    for (i in 1:n)
    {
      if (x[i,j] > 1)
      {
        if (splitcod)
          xds[i,j1col + 1:(x[i,j]-1)] <- 1
        else
          xds[i,j1col + (x[i,j]-1)] <- 1
      }
    }
  }
  ## Output
  if (constant)
    return(cbind(1,xds))
  else
    return(xds)
}



genRidge <- function(x, y, offset, omega, lambda, model, delta=1e-6, maxit=25)
{
  coefs <- matrix(0,ncol(x),length(lambda))
  fits <- matrix(NA,nrow(x),length(lambda))
  l <- 1
  for (lam in lambda)
  {
    if (model == "linear")
    {
      yw <- y - offset
      chollam <- chol(t(x)%*%x + lam*omega)
      coefs[,l] <- backsolve(chollam,
                             backsolve(chollam, t(x)%*%yw, transpose=TRUE))
      fits[,l] <- x%*%coefs[,l] + offset
    }
    else
    {
      ## penalized logistic/poisson regression
      conv <- FALSE
      
      # start values
      if (l > 1)
      {
        b.start <- coefs[,l-1]
      }
      else
      {
        if (model == "logit")
        {
          gy <- rep(log(mean(y)/(1-mean(y))),length(y)) - offset
        }
        else
        {
          gy <- rep(log(mean(y)),length(y)) - offset
        }
        chollam <- chol(t(x)%*%x + lam*omega)
        b.start <- backsolve(chollam,
                             backsolve(chollam, t(x)%*%gy, transpose=TRUE))
      }
      
      # fisher scoring
      b.old <- b.start
      i <- 1
      while(!conv)
      {
        eta <- x%*%b.old + offset
        if (model == "logit")
        {
          mu <- plogis(eta)
          sigma <- as.numeric(mu*(1-mu))
        }
        else
        {
          mu <- exp(eta)
          sigma <- as.numeric(mu)
        }
        score <- t(x)%*%(y-mu)
        fisher <- t(x)%*%(x*sigma)
        choli <- chol(fisher + lam*omega)
        b.new <- b.old + backsolve(choli,
                                   backsolve(choli, (score - lam*omega%*%b.old), transpose=TRUE))
        
        if(sqrt(sum((b.new-b.old)^2)/sum(b.old^2)) < delta | i>=maxit)
        {
          # check the stop criterion
          conv <- TRUE
        }
        b.old <- b.new
        i <- i+1
      }  # end while
      coefs[,l] <- b.old   
      fits[,l] <- mu       
    }
    l <- l+1
  }
  rownames(fits) <- NULL
  colnames(fits) <- lambda
  rownames(coefs) <- NULL
  colnames(coefs) <- lambda
  
  return(list(fitted = fits, coefficients = coefs))   
}



cd <- function(x)
{
  n <- length(x)
  
  X <- matrix(1,n,1)
  
  Z <- matrix(rep(x,max(x)-1),n,max(x)-1)
  Z <- Z - matrix(rep(1:(max(x)-1),n),dim(Z)[1],dim(Z)[2],byrow=T)
  Z[Z<0] <- 0
  Z[Z>1] <- 1
  
  res <- list(X = X, Z = Z)
  return(res)
}



ordAOV1 <- function(x, y, type = "RLRT", nsim = 10000, null.sample = NULL ,...){
  
  x <- as.numeric(x)
  
  ## check x
  if(min(x)!=1 | length(unique(x)) != max(x))
    stop("x has to contain levels 1,...,max")
  
  if(length(x) != length(y))
    stop("x and y have to be of the same length")
  
  k <- length(unique(x))
  cdx <- cd(x)
  X <- cdx$X
  Z <- cdx$Z
  
  # RLRT
  if (type == "RLRT")
  {
    # model under the alternative
    m <- gam(y ~ Z, paraPen=list(Z=list(diag(k-1))), method="REML")
    
    # null model
    m0 <- gam(y ~ 1, method="REML")
    
    # log-likelihood
    rlogLik.m <- -summary(m)$sp.criterion
    rlogLik.m0 <- -summary(m0)$sp.criterion
    rlrt.obs <- max(0, 2*(rlogLik.m - rlogLik.m0))
    
    # null distribution
    if (rlrt.obs != 0) {
      if (length(null.sample)==0)
        RLRTsample <- RLRTSim(X, Z, qr(cdx$X), chol(diag(k-1)), nsim = nsim, ...)
      else
        RLRTsample <- null.sample
      
      p <- mean(rlrt.obs < RLRTsample)
    }
    else
    {
      if (length(null.sample)==0)
        RLRTsample <- NULL
      else
        RLRTsample <- null.sample
      
      p <- 1
    }
    
    # return
    RVAL <- list(statistic = c(RLRT = rlrt.obs), p.value = p,
                 method = paste("simulated finite sample distribution of RLRT.\n (p-value based on",
                                length(RLRTsample), "simulated values)"), sample = RLRTsample)
  }
  
  # LRT
  else
  {
    # model under the alternative
    m <- gam(y ~ Z, paraPen=list(Z=list(diag(k-1))), method="ML")
    
    # null model
    m0 <- gam(y ~ 1, method="ML")
    
    # log-likelihood
    logLik.m <- -summary(m)$sp.criterion
    logLik.m0 <- -summary(m0)$sp.criterion
    lrt.obs <- max(0, 2*(logLik.m - logLik.m0))
    
    # null distribution
    if (lrt.obs != 0) {
      if (length(null.sample)==0)
        LRTsample <- LRTSim(X, Z, q=0, chol(diag(k-1)), nsim = nsim, ...)
      else
        LRTsample <- null.sample
      
      p <- mean(lrt.obs < LRTsample)
    }
    else
    {
      if (length(null.sample)==0)
        LRTsample <- NULL
      else
        LRTsample <- null.sample
      
      p <- 1
    }
    
    # return
    RVAL <- list(statistic = c(LRT = lrt.obs), p.value = p,
                 method = paste("simulated finite sample distribution of LRT.\n (p-value based on",
                                length(LRTsample), "simulated values)"), sample = LRTsample)
  }
  
  class(RVAL) <- "htest"
  return(RVAL)
}



ordAOV2 <- function(x, y, type = "RLRT", nsim = 10000, null.sample = NULL, ...){
  
  n <- length(y)
  p <- ncol(x)
  k <- apply(x, 2, max)
  
  ## check x
  if(min(x[,1])!=1 | length(unique(x[,1])) != max(x[,1]))
    stop("x has to contain levels 1,...,max")
  
  if(nrow(x) != length(y))
    stop("nrow(x) and length(y) do not match")
  
  cdx <- cd(x[,1])
  X <- cdx$X
  ZZ <- vector("list", p)
  ZZ[[1]] <- cdx$Z
  Z <- ZZ[[1]]
  DD <- vector("list", p)
  DD[[1]] <- diag(c(rep(1,k[1]-1),rep(0,sum(k[2:p]-1))))
  for (j in 2:p)
  {
    
    ## check x
    if(min(x[,j])!=1 | length(unique(x[,j])) != max(x[,j]))
      stop("x has to contain levels 1,...,max")
    
    ZZ[[j]] <- cd(x[,j])$Z
    Z <- cbind(Z,ZZ[[j]])
    if (j < p)
      DD[[j]] <- diag(c(rep(0,sum(k[1:(j-1)]-1)),rep(1,k[j]-1),rep(0,sum(k[(j+1):p]-1))))
    else
      DD[[j]] <- diag(c(rep(0,sum(k[1:(j-1)]-1)),rep(1,k[j]-1)))
  }
  RRVAL <- vector("list", p)
  
  # RLRT
  if (type == "RLRT")
  {
    # model under the alternative
    mA <- gam(y ~ Z, paraPen=list(Z=DD), method="REML")
    
    for (j in 1:p)
    {
      # null model
      Z0 <- matrix(unlist(ZZ[-j]),n,sum(k[-j]-1))
      D0 <- DD[-j]
      if(!is.list(D0))
        D0 <- list(D0)
      
      if (j == 1)
        out <- 1:(k[1]-1)
      else
        out <- sum(k[1:(j-1)]-1)+(1:(k[j]-1))
      
      for(j0 in 1:length(D0))
      {
        D0[[j0]] <- D0[[j0]][-out,-out]
      }
      m0 <- gam(y ~ Z0, paraPen=list(Z0=D0), method="REML")
      
      # log-likelihood
      rlogLik.mA <- -summary(mA)$sp.criterion
      rlogLik.m0 <- -summary(m0)$sp.criterion
      rlrt.obs <- max(0, 2*(rlogLik.mA - rlogLik.m0))
      
      # model that only contains the variance
      # ...
      
      # null distribution
      Z1 <- ZZ[[j]]
      if (rlrt.obs != 0) {
        if (length(null.sample) == 0)
          RLRTsample <- RLRTSim(X, Z1, qr(X), chol(diag(k[j]-1)), nsim = nsim, ...)
        else
        {
          if (length(null.sample) == p)
            RLRTsample <- null.sample[[j]]
          else
            stop("wrong number of null.sample elements")
        }
        
        p <- mean(rlrt.obs < RLRTsample)
      }
      else
      {
        if (length(null.sample) == 0)
          RLRTsample <- NULL
        else
        {
          if (length(null.sample) == p)
            RLRTsample <- null.sample[[j]]
          else
            stop("wrong number of null.sample elements")
        }
        
        p <- 1
      }
      
      # return
      RVAL <- list(statistic = c(RLRT = rlrt.obs), p.value = p,
                   method = paste("simulated finite sample distribution of RLRT.\n (p-value based on",
                                  length(RLRTsample), "simulated values)"), sample = RLRTsample)
      
      class(RVAL) <- "htest"
      RRVAL[[j]] <- RVAL
    }
  }
  
  # LRT
  else
  {
    # model under the alternative
    mA <- gam(y ~ Z, paraPen=list(Z=DD), method="ML")
    
    for (j in 1:p)
    {
      # null model
      Z0 <- matrix(unlist(ZZ[-j]),n,sum(k[-j]-1))
      D0 <- DD[-j]
      if(!is.list(D0))
        D0 <- list(D0)
      
      if (j == 1)
        out <- 1:(k[1]-1)
      else
        out <- sum(k[1:(j-1)]-1)+(1:(k[j]-1))
      
      for(j0 in 1:length(D0))
      {
        D0[[j0]] <- D0[[j0]][-out,-out]
      }
      m0 <- gam(y ~ Z0, paraPen=list(Z0=D0), method="ML")
      
      # log-likelihood
      logLik.mA <- -summary(mA)$sp.criterion
      logLik.m0 <- -summary(m0)$sp.criterion
      lrt.obs <- max(0, 2*(logLik.mA - logLik.m0))
      
      # model that only contains the variance
      # ...
      
      # null distribution
      Z1 <- ZZ[[j]]
      if (lrt.obs != 0) {
        if (length(null.sample) == 0)
          LRTsample <- LRTSim(X, Z1, q=0, chol(diag(k[j]-1)), nsim = nsim, ...)
        else
        {
          if (length(null.sample) == p)
            LRTsample <- null.sample[[j]]
          else
            stop("wrong number of null.sample elements")
        }
        
        p <- mean(lrt.obs < LRTsample)
      }
      else
      {
        if (length(null.sample) == 0)
          LRTsample <- NULL
        else
        {
          if (length(null.sample) == p)
            LRTsample <- null.sample[[j]]
          else
            stop("wrong number of null.sample elements")
        }
        
        p <- 1
      }
      
      # return
      RVAL <- list(statistic = c(LRT = lrt.obs), p.value = p,
                   method = paste("simulated finite sample distribution of LRT.\n (p-value based on",
                                  length(LRTsample), "simulated values)"), sample = LRTsample)
      
      class(RVAL) <- "htest"
      RRVAL[[j]] <- RVAL
    }
  }
  
  names(RRVAL) <- colnames(x)
  return(RRVAL)
}



## Defines a new type of "spline basis" for ordered factors
## with difference penalty:

#' smooth constructor:
smooth.construct.ordinal.smooth.spec <- 
  function(object, data, knots){
    x <- data[[object$term]]
    ## stop if co is not an ordered factor:
    stopifnot(is.ordered(x))
    
    nlvls <- nlevels(x)
    
    #default to 1st order differences (penalizations of deviations from constant):
    if(is.na(object$p.order)) object$p.order <- 1
    
    # construct difference penalty
    Diffmat <- diag(nlvls)
    if(object$p.order > 0){
      for(d in 1:object$p.order) Diffmat <- diff(Diffmat)
    } 
    object$S[[1]] <- crossprod(Diffmat)
    
    # construct design matrix
    object$X <- model.matrix(~ x - 1)
    object$rank <- c(nlvls - object$p.order)
    object$null.space.dim <- object$p.order
    object$knots <- levels(x) 
    object$df <- nlvls
    
    class(object) <- "ordinal.smooth"
    object
  }
#' for predictions:
Predict.matrix.ordinal.smooth <- 
  function(object,data){
    x <- ordered(data[[object$term]], levels = object$knots)
    X <- model.matrix(~ x - 1)
  }

#' for plots:
plot.ordinal.smooth <- 
  function(x,P=NULL,data=NULL,label="",se1.mult=1,se2.mult=2,
           partial.resids=FALSE,rug=TRUE,se=TRUE,scale=-1,n=100,n2=40,n3=3,
           pers=FALSE,theta=30,phi=30,jit=FALSE,xlab=NULL,ylab=NULL,main=NULL,
           ylim=NULL,xlim=NULL,too.far=0.1,shade=FALSE,shade.col="gray80",
           shift=0,trans=I,by.resids=FALSE,scheme=0,...) {
    if (is.null(P)) { ## get plotting info
      xx <- unique(data[x$term])  
      dat <- data.frame(xx)
      names(dat) <- x$term
      X <- PredictMat(x, dat)
      
      if (is.null(xlab)) xlabel <- x$term else xlabel <- xlab
      if (is.null(ylab)) ylabel <- label else ylabel <- ylab
      return(list(X=X,scale=FALSE,se=TRUE, xlab=xlabel,ylab=ylabel,
                  raw = data[x$term], 
                  main="",x=xx,n=n, se.mult=se1.mult))
    } else { ## produce the plot
      n <- length(P$fit)
      if (se) { ## produce CI's
        if (scheme == 1) shade <- TRUE
        ul <- P$fit + P$se ## upper CL
        ll <- P$fit - P$se ## lower CL
        if (scale==0 && is.null(ylim)) { ## get scale 
          ylimit <- c(min(ll), max(ul))
          if (partial.resids) { 
            max.r <- max(P$p.resid,na.rm=TRUE)
            if (max.r> ylimit[2]) ylimit[2] <- max.r
            min.r <-  min(P$p.resid,na.rm=TRUE)
            if (min.r < ylimit[1]) ylimit[1] <- min.r
          }
        }
        if (!is.null(ylim)) ylimit <- ylim
        if (length(ylimit)==0) ylimit <- range(ul,ll)
        ## plot the smooth...
        plot(P$x, trans(P$fit+shift),  
             xlab=P$xlab, ylim=trans(ylimit+shift),
             xlim=P$xlim, ylab=P$ylab, main=P$main, ...)
        if (shade) { 
          rect(xleft=as.numeric(P$x[[1]])-.4, 
               xright=as.numeric(P$x[[1]])+.4, 
               ybottom=trans(ll+shift), 
               ytop=trans(ul+shift), col = shade.col,border = NA)
          segments(x0=as.numeric(P$x[[1]])-.4, 
                   x1=as.numeric(P$x[[1]])+.4, 
                   y0=trans(P$fit+shift), 
                   y1=trans(P$fit+shift), lwd=3)
        } else { ## ordinary plot 
          if (is.null(list(...)[["lty"]])) { 
            segments(x0=as.numeric(P$x[[1]]), 
                     x1=as.numeric(P$x[[1]]), 
                     y0=trans(ul+shift), 
                     y1=trans(ll+shift), lty=2,...)
          } else { 
            segments(x0=as.numeric(P$x[[1]]), 
                     x1=as.numeric(P$x[[1]]), 
                     y0=trans(ul+shift), 
                     y1=trans(ll+shift),...)
            segments(x0=as.numeric(P$x[[1]]), 
                     x1=as.numeric(P$x[[1]]), 
                     y0=trans(ul+shift), 
                     y1=trans(ll+shift),...)
          }
        }
      }  else {
        if (!is.null(ylim)) ylimit <- ylim
        if (is.null(ylimit)) ylimit <- range(P$fit) 
        ## plot the smooth... 
        plot(P$x, trans(P$fit+shift),  
             xlab=P$xlab, ylim=trans(ylimit+shift),
             xlim=P$xlim, ylab=P$ylab, main=P$main, ...)
      }    ## ... smooth plotted
      if (partial.resids&&(by.resids||x$by=="NA")) { ## add any partial residuals
        if (length(P$raw)==length(P$p.resid)) {
          if (is.null(list(...)[["pch"]]))
            points(P$raw,trans(P$p.resid+shift),pch=".",...) else
              points(P$raw,trans(P$p.resid+shift),...) 
        } else {
          warning("Partial residuals not working.")
        }
      } ## partial residuals finished 
      
    }
  } 


crO <- function(k, d=2){
  
  Dd <- cbind(diag(-1,k-1),0) + cbind(0,diag(1,k-1))
  if (d > 1)
  {
    Dj <- Dd
    for (j in 2:d) {
      Dj <- Dj[-1,-1]
      Dd <- Dj%*%Dd
    }
  }
  
  Om <- t(Dd)%*%Dd
  return(Om)
}

# penalized nonlinear PCA algorithm
penALS <- function(H, p, lambda, qstart, crit, maxit, Ks, constr){
  
  Q <- as.matrix(H) 
  n <- nrow(Q)     
  m <- ncol(Q)      
  
  qs <- list()
  Z <- list()
  ZZO <- list()
  iZZO <- list()
  
  tracing <- c()
  
  for (j in 1:m) 
  { 
    qj <- c(Q[,j],1:Ks[j])                          
    Z[[j]] <- model.matrix(~ factor(qj) - 1)[1:n,] 
    Om <- (Ks[j]-1)*crO(Ks[j])                      
    ZZO[[j]] <- (t(Z[[j]])%*%Z[[j]] + lambda*Om)   
    iZZO[[j]] <- chol2inv(chol(ZZO[[j]]))          
    qs[[j]] <- ((1:Ks[j]) - mean(Q[,j]))/sd(Q[,j]) 
  }
  
  if(length(qstart) > 0){
    Qstart <- mapply("%*%", Z, qstart, SIMPLIFY = TRUE)
    Q <- scale(Qstart)
    qs <- qstart
  }else{
    Q <- scale(Q) 
  }
  
  iter <- 0
  conv <- FALSE
  QQ <- Q
  while(!conv & iter < maxit)
  {
    
    pca <- prcomp(Q, scale = FALSE)  
    X <- pca$x[,1:p, drop = FALSE]        
    A <- pca$rotation[,1:p, drop = FALSE]  
    
    for (j in 1:m){
      
      if (constr[j]){ 
        bvec <- c(n-1,numeric(Ks[j]-1)) 
        dvec <- t(Z[[j]])%*%(X%*%A[j,]) 
        Amat2 <- qs[[j]] %*% t(Z[[j]]) %*% Z[[j]]
        Amat3 <- cbind(0,diag(Ks[j]-1)) - cbind(diag(Ks[j]-1),0)
        Amat <- rbind( Amat2, Amat3)  
        Amat <- t(Amat) 
      }else{
        bvec <- c(n-1)   
        dvec <- t(Z[[j]])%*%(X%*%A[j,]) 
        Amat2 <- qs[[j]] %*% t(Z[[j]]) %*% Z[[j]]
        Amat <- rbind( Amat2)  
        Amat <- t(Amat)
      }
      
      qj <- solve.QP(Dmat=ZZO[[j]], dvec=dvec, Amat=Amat, bvec=bvec, meq=1)$solution
      
      qj <- as.numeric(qj)
      Zqj <- Z[[j]]%*%qj
      Q[,j] <- Zqj/sd(Zqj)
      qs[[j]] <- qj/sd(Zqj)   
      
    }
    
    tracing <- c(tracing, sum(pca$sdev[1:p]^2) / sum(pca$sdev^2))
    
    # convergence?
    if (sum((QQ - Q)^2)/(n*m) < crit)
      conv <- TRUE
    
    QQ <- Q
    iter <- iter + 1
  }
  
  # final pca
  pca <- prcomp(Q, scale = FALSE)  
  X <- pca$x[,1:p, drop = FALSE]  
  A <- pca$rotation[,1:p, drop = FALSE]  
  
  tracing[length(tracing)] <- sum(pca$sdev[1:p]^2) / sum(pca$sdev^2)
  
  return(list("Q" = Q, "qs" = qs, "iter" = iter, "trace" = tracing))
}


# cumulative logistic group lasso
invlink   <- function(eta) plogis(eta) #1 / (1 + exp(-eta))

ordglasso_control <- function(control = list()){
  ## update.hess: should the hessian be updated in each
  ##              iteration ("always")? update.hess = "lambda" will update
  ##              the Hessian once for each component of the penalty
  ##              parameter "lambda" based on the parameter estimates
  ##              corresponding to the previous value of the penalty
  ##              parameter.
  ## inner.loops: how many loops should be done (at maximum) when solving
  ##              only the active set (without considering the remaining
  ##              predictors)
  ## tol: convergence tolerance; the smaller the more precise.
  ## lower: lower bound for the diagonal approximation of the
  ##        corresponding block submatrix of the Hessian of the negative
  ##        log-likelihood function.
  ## upper: upper bound for the diagonal approximation of the
  ##        corresponding block submatrix of the Hessian of the negative
  ##        log-likelihood function.
  ## delta: scaling factor beta < 1 of the Armijo line search.
  ## sigma: 0 < \sigma < 1 used in the Armijo line search.
  default_control <- list(
    update.hess  = "lambda", 
    update.every = 3, 
    inner.loops = 10,
    max.iter     = 500,
    tol          = 5 * 10^-8,
    lower        = 10^-2,
    upper        = 10^9, 
    delta         = 0.5,  
    sigma        = 0.1
  )
  control <- modifyList(default_control, control)
  return(control)
}

ord.glasso <- function(x, y, lambda, weights = rep(1, length(y)), 
                       penscale = sqrt, standardize = TRUE, restriction = c("refcat", "effect"), 
                       nonpenx = NULL, control=ordglasso_control()){ # , ...
  ## x: design matrix (matrix of ordinal predictors)
  ## y: response vector 
  ## lambda: vector of penalty (starts with first component) 

  restriction <- match.arg(restriction)
  ## Error checking 
  ## Check the x matrix
  if(!(is.matrix(x) | is.numeric(x) | is.data.frame(x)))
    stop("x has to be a matrix, numeric vector or data.frame")
  
  if(any(is.na(x)))
    stop("Missing values in x are not allowed")
  
  ## Check the response
  tol <- .Machine$double.eps^0.5 
  if(any(abs(y[!is.na(y)] - round(y[!is.na(y)])) > tol) | any(y[!is.na(y)] < 0))
    stop("y has to contain nonnegative integers")
  
  ## Check the other arguments
  if(is.unsorted(rev(lambda)))
    warning("lambda values should be sorted in decreasing order")
  
  lambda <- sort(lambda, decreasing = TRUE)
  
  ## ordinal predictors
  x <- as.matrix(x)
  if(nrow(x) != length(y))
    stop("x and y do not have correct dimensions")
  if(any(!apply(x,2,is.numeric)))
    stop("Entries of x have to be of type 'numeric'")
  if(any(abs(x - round(x)) > tol) | any(x < 1))
    stop("x has to contain positive integers")
  
  update.hess  <- control$update.hess
  update.every <- control$update.every
  inner.loops  <- control$inner.loops
  max.iter     <- control$max.iter
  tol          <- control$tol
  lower        <- control$lower
  upper        <- control$upper
  delta        <- control$delta
  sigma        <- control$sigma
  
  
  px <- ncol(x)  
  kx <- apply(x, 2, max)
  xnames <- colnames(x)
  grp <- rep(1:px, kx-1)
  Xcod <- coding(x, constant = FALSE) 
  Xcod <- Xcod[!is.na(y),]
  y <- y[!is.na(y)]
  
  lev <- levels(factor(y))  
  n <- nrow(Xcod)
  p <- ncol(Xcod)
  q <- length(lev) - 1
  Y <- matrix(0, n, q)
  Yr <- col(Y) == y  
  Yr_1 <- col(Y) == y - 1 
  ind_pvars <- seq_len(ncol(Xcod)) 
  ind_q <- seq_len(q)
  
  ## group index for glasso
  xind <- c() 
  tab <- apply(x, 2, table)
  xind <- rep(1:ncol(x), kx-1)
  index <- c(xind, rep(NA, q))
  if(!is.null(nonpenx)){
    for(j in nonpenx)  index[xind == j] <- NA
  } 
  
  
  ## Index of beta0
  any.notpen <- any(is.na(index))
  inotpen.which <- which(is.na(index))
  nnotpen <- length(inotpen.which) 
  
  intercept.which <- (1:q) + p
  
  ## Index of beta1,..,G
  if(any.notpen){
    ipen <- index[-inotpen.which]  
    ipen.which <- split((1:ncol(Xcod))[-inotpen.which], ipen) 
  }else{
    ipen <- index
    ipen.which <- split((1:ncol(Xcod)), ipen) 
  }
  
  npen <- length(ipen.which)
  ipen.tab <- table(ipen)[as.character(sort(unique(ipen)))] # table of df
  
  ### centering ### 
  mu.x <- apply(Xcod, 2, mean) 
  
  ### standardize ###
  if(standardize){ 
    xSD <- sqrt(rowSums(weights*(t(Xcod)-mu.x)^2) / sum(weights))
    xSD[xSD==0] <- 1
    Xcod <- xStd <- t((t(Xcod) - mu.x) / xSD)
  }else{
    Xcod <- sweep(Xcod, 2, mu.x)
  }
  
  ## starting values
  coef.init <-  rep(0, p+q)
  coef <- coef.init
  coef.pen <- coef.init
  
  ## Penalty term
  if(any.notpen) coef.pen <- coef[-inotpen.which]
  norms.pen <- c(sqrt(rowsum(coef.pen^2, group = ipen)))
  
  ## Empty matrices to save the results
  nloglik.v <- numeric(length(lambda))
  coef.m <- matrix(0, nrow = (p+q), ncol = length(lambda),  
                   dimnames = list(NULL, lambda)) 
  fitted <- lin.pred <- matrix(0, nrow = nrow(Xcod), ncol = length(lambda),
                               dimnames = list(NULL, lambda)) 
  
  
  if(any.notpen) nH.notpen <- numeric(nnotpen)
  nH.pen <- numeric(npen)
  
  ## init lin. predictor 
  eta <- c(Xcod %*% coef[ind_pvars])
  alpha <- coef[p + ind_q]
  theta <- c(-Inf, cumsum(c(alpha[1], exp(alpha[-1]))), Inf)
  mu <- invlink(pmin(100, theta[y+1] - eta)) - invlink(pmax(-100, theta[y] - eta))
  
  ## Now loop through (descreasing) lambdas 
  for(pos in 1:length(lambda)){
    l <- lambda[pos]
    
    ## Initial (or updated) Hessian Matrix
    if(update.hess == "lambda" & pos %% update.every == 0 | pos == 1){ 
      
      nh <- nhessian(coef, y, Xcod, weights)
      ## Non-penalized groups
      if(any.notpen){ 
        for(j in 1:nnotpen){ 
          ind <- inotpen.which[[j]] 
          nH.notpen[j] <- min(max(nh[ind,ind], lower), upper)  
        }
      }
      ## Penalized groups
      for(j in 1:npen){
        ind <- ipen.which[[j]]
        diagH <- numeric(length(ind))
        for(i in 1:length(ind)){
          diagH[i] <- nh[ind[i],ind[i]]
        }
        nH.pen[j] <- min(max(diagH, lower), upper)
      }
    } # end if update.hess
    
    ##### Start optimization #####
    fn.val <- nloglik(coef, y, Xcod, weights) + l * sum(penscale(ipen.tab) * norms.pen)
    
    d.par <- d.fn <- 1
    m <- 0  ## Count the loops through *all* groups
    
    while(d.fn > tol | d.par > sqrt(tol)){
      
      if(m >= max.iter){
        warning(paste("Maximal number of iterations reached for lambda[",pos,"]", sep=""))
        break
      }
      
      ## Save the parameter vector and the function value of the previous step
      fn.val.old <- fn.val
      coef.old <- coef    
      
      m <- m + 1  
      
      ## Initialize line search
      start.notpen <- rep(1, nnotpen)
      start.pen <- rep(1, npen)
      
      ## Optimize intercept 
      if(any.notpen){ 
        for(j in 1:nnotpen){
          
          ind <- inotpen.which[j] 
          
          ## Gradient of the negative log-likelihood function
          nscores <- c(nscore(coef, y, Xcod, weights)[ind])  
          
          nH <- nH.notpen[j]
          
          ## Calculate d (search direction)
          d <- -(1/nH) * nscores
          ## Set to 0 if value is very small compared to current coef estimate
          d <- zapsmall(c(coef[ind], d), digits = 16)[2]
          
          ## Line search if d!=0
          if(d != 0){ 
            scale <- min(start.notpen[j] / delta, 1)  
            coef.test      <- coef
            coef.test[ind] <- coef[ind] + scale * d
             
            
            qh <- sum(nscores * d)
            fn.val0     <- nloglik(coef, y, Xcod, weights)
            fn.val.test <- nloglik(coef.test, y, Xcod, weights) 
            
            qh <- zapsmall(c(qh, fn.val0), digits = 16)[1]
            
            ## Armijo rule
            while(fn.val.test > fn.val0 + sigma * scale * qh & scale > 10^(-30)){ 
              scale <- scale * delta 
              coef.test[ind] <- coef[ind] + scale * d
              fn.val.test <- nloglik(coef.test, y, Xcod, weights)  
              
            } # end while armijo 
            coef <- coef.test
            alpha <- coef[p + ind_q]
            theta <- c(-Inf, cumsum(c(alpha[1], exp(alpha[-1]))), Inf)
            mu <- invlink(pmin(100, theta[y+1] - eta)) - invlink(pmax(-100, theta[y] - eta))
            start.notpen[j] <- scale 
          } # end if d !=0 
        } # end for j in notpen 
      } # end optimize notpen
      
      ## Optimize the *penalized* parameter groups
      for(j in 1:npen){  
        
        ind <- ipen.which[[j]]
        npar <- ipen.tab[j]
        
        coef.ind <- coef[ind]
        cross.coef.ind <- crossprod(coef.ind)
        

        # Negative score function (-> neg. gradient)
        nscores <- c(nscore(coef, y, Xcod, weights)[ind])

        nH <- nH.pen[j]
        
        cond <- -nscores + nH * coef.ind 
        cond.norm2 <- sqrt(c(crossprod(cond)))
        
        ## Check whether minimum is at the non-differentiable position (i.e. -coef.ind)
        if(cond.norm2 > penscale(npar) * l){
          d <- (1/nH) * (-nscores - l * penscale(npar) * (cond/cond.norm2)) 
        }else{
          d <- -coef.ind
        }
        
        ## Line search
        if(!all(d == 0)){
          
          ## Init 
          scale <- min(start.pen[j] / delta, 1) # as found in Tseng, Yun (2009), sec. 7
          
          coef.test <- coef
          coef.test[ind] <- coef.ind + scale * d

          qh <- sum(nscores * d) + l * penscale(npar) * sqrt(crossprod(coef.ind + d)) - 
            l * penscale(npar) * sqrt(cross.coef.ind)  
          
          fn.val.test <- nloglik(coef.test, y, Xcod, weights)
          fn.val0 <- nloglik(coef, y, Xcod, weights)
          
          left <- fn.val.test + l * penscale(npar) * sqrt(crossprod(coef.test[ind]))
          
          right <- fn.val0 + l * penscale(npar) * sqrt(cross.coef.ind) + 
            scale * sigma * qh 
          
          while(left > right & scale > 10^(-30)){ 
            scale <- scale * delta 
            coef.test[ind] <- coef.ind + scale * d
            fn.val.test <- nloglik(coef.test, y, Xcod, weights)
            
            left <- fn.val.test + l * penscale(npar) * sqrt(crossprod(coef.test[ind]))
            
            right <- fn.val0 + l * penscale(npar) * sqrt(cross.coef.ind) + 
              scale * sigma * qh   
          } # end while armijo rule 
          
          coef <- coef.test 
          alpha <- coef[p + ind_q]
          theta <- c(-Inf, cumsum(c(alpha[1], exp(alpha[-1]))), Inf)
          mu <- invlink(pmin(100, theta[y+1] - eta)) - invlink(pmax(-100, theta[y] - eta))
          start.pen[j] <- scale # save step size for next iteration 
          
        } # end if !all(d==0)
        
        norms.pen[j] <- sqrt(crossprod(coef[ind])) # update penalty term with new beta
        
      } # end for j in guessed.active
      
      fn.val <- nloglik(coef, y, Xcod, weights) + l * sum(penscale(ipen.tab) * norms.pen)
      # -> we're adding the jth contribution in every loop step
      
      ## Rel. difference w.r.t param vector
      d.par <- max(abs(coef - coef.old) / (1 + abs(coef)))
      
      ## Rel. difference w.r.t function value (penalized loglik)
      d.fn <- abs(fn.val.old - fn.val) / (1 + abs(fn.val))
      
    } # end while (d.fn, d.par)
    
    ## Save (final) results for current lambda value
    coef.m[,pos] <- c(beta=coef[ind_pvars], theta= theta[-c(1,length(theta))]) 
    nloglik.v[pos] <- nloglik(coef, y, Xcod, weights)
  } # end for pos in lambda 
  
  
  
  ## Backtransform coefs 
  if(standardize){   
    # if(any.notpen) 
    coef.m[intercept.which,] <- coef.m[intercept.which,] + t((t(coef.m[unlist(-intercept.which),]) %*% (mu.x / xSD))[ , rep(1, q), drop=FALSE])
    coef.m[-intercept.which,]  <- coef.m[-intercept.which,] / xSD
  }else{
    # if(any.notpen) 
    coef.m[intercept.which,] <- coef.m[intercept.which,] + t((t(coef.m[unlist(ipen.which),]) %*% mu.x)[ , rep(1, q), drop=FALSE])
  }
  
  for(j in  1:length(ipen.which)){ 
    if(length(ipen.which[[j]]) > 1){  
      coef.m[ipen.which[[j]],] <- apply(cbind(coef.m[ipen.which[[j]],]),2,cumsum)
    }else{
      coef.m[ipen.which[[j]],] <- apply(rbind(coef.m[ipen.which[[j]],]),2,cumsum)
    }
  }
  
  coefs <- matrix(0, sum(kx), ncol(coef.m))
  coefs[sequence(kx)>1,] <- coef.m[ind_pvars,]
  constant <- constref <- coef.m[ind_q+p,,drop=F]
  xcrefcat <- coefs # save for predict() with restriction="effect"
  
  ## Effect coding / sum to zero restriction
  if(restriction == "effect"){
    modelbjk <-  coef.m[1:p,, drop=F]  
    transcoef <- matrix(NA, p, ncol(coef.m)) 
    transb1 <- matrix(NA, px, ncol(coef.m)) 
    for(j in 1:px){ 
      means <- drop(apply(modelbjk[grp==j,,drop=F], 2, function(x) mean(c(0,x))) )
      for(i in 1:ncol(coef.m)){
        transcoef[grp==j,i] <- beffcjk <- modelbjk[grp==j,i] - means[i]
        transb1[j,i] <- - sum(beffcjk)
      }   
    }
    coefs[sequence(kx)>1,] <- transcoef 
    coefs[sequence(kx)==1,] <- transb1 
    constant <- sweep(constant, 2, apply(transb1,2,sum), "+")
}
  
  coefs <- rbind(coefs, constant)
  
  if (length(xnames)==0)
    xnames <- paste("x",1:px,sep="")
  xnames <- rep(xnames,kx)
  xnames <- paste(xnames,":",sequence(kx),sep="") 
  thetanames <- paste("intercept:",ind_q, sep="")
  
  rownames(coefs) <- c(xnames, thetanames)
  
  flmodel <- structure(list(), class = "model")
  flmodel$lambda <- lambda; flmodel$model <- "cumulative"; flmodel$xlevels <- kx; flmodel$zcovars <- 0; class(flmodel) <- "ordPen"; flmodel$restriction <- "refcat"
  flmodel$coefficients <- rbind(xcrefcat, constref)  
  fits <- predict(flmodel, newx = x, type = "response") 
  
  ## output
  out <- list(fitted = fits,
              coefficients = coefs,
              nloglik = nloglik.v, 
              model = "cumulative",
              restriction = restriction,
              lambda = lambda,
              xlevels = kx,
              ulevels = NULL,
              zcovars = 0,
              m = m) 
  structure(out, class = "ordPen")
}  



nloglik <- function(beta, y, x, weights = rep(1, nrow(x))){ 
  
  lev <- levels(factor(y)) 
  p <- ncol(x)
  q <- length(lev) - 1 
  ind_pvars <- seq_len(ncol(x)) 
  ind_q <- seq_len(q)
  
  alpha <- beta[p + ind_q]
  theta <- c(-Inf, cumsum(c(alpha[1], exp(alpha[-1]))), Inf)
  eta <- drop(x %*% beta[ind_pvars])
  
  Pr <- plogis(pmin(100, theta[y + 1] - eta)) - plogis(pmax(-100, theta[y] - eta))  
  return(- sum(weights * log(Pr))) 
} 

nscore <- function(beta, y, x, weights = rep(1, nrow(x))){ 
  
  lev <- levels(factor(y))
  n <- nrow(x) 
  q <- length(lev) - 1
  Y <- matrix(0, n, q)
  Yr <- col(Y) == y  
  Yr_1 <- col(Y) == y - 1 
  ind_pvars <- seq_len(ncol(x)) 
  ind_q <- seq_len(q)
  
  alpha <- beta[ncol(x) + ind_q]
  theta <- c(-Inf, cumsum(c(alpha[1], exp(alpha[-1]))), Inf)
  eta <- drop(x %*% beta[ind_pvars])
  
  Pr <- plogis(pmin(100, theta[y + 1] - eta)) - plogis(pmax(-100, theta[y] - eta))
  dr <- dlogis(pmin(100, theta[y + 1] - eta)) 
  dr_1 <- dlogis(pmax(-100, theta[y] - eta))
  
  jacobian <- matrix (0, q, q)
  jacobian[,1] <- 1
  for(i in 2:ncol(jacobian)) jacobian[i:q, i] <- exp(alpha)[i]
  
  s1 <-  - t((dr - dr_1) * (weights/Pr)) %*% x  
  
  s2 <- t(t(Yr * dr - Yr_1 * dr_1) %*% (weights/Pr)) %*% jacobian 
  
  return(- c(s1,s2)) 
}

nhessian <- function(beta, y, x, weights = rep(1, nrow(x))){  
  
  lev <- levels(factor(y))
  n <- nrow(x) 
  q <- length(lev) - 1
  Y <- matrix(0, n, q)
  Yr <- col(Y) == y  
  Yr_1 <- col(Y) == y - 1 
  ind_pvars <- seq_len(ncol(x)) 
  ind_q <- seq_len(q)
  
  alpha <- beta[ncol(x) + ind_q]
  theta <- c(-Inf, cumsum(c(alpha[1], exp(alpha[-1]))), Inf)
  eta <- drop(x %*% beta[ind_pvars])
  
  Pr <- plogis(pmin(100, theta[y + 1] - eta)) - plogis(pmax(-100, theta[y] - eta))
  dr <- dlogis(pmin(100, theta[y + 1] - eta)) 
  dr_1 <- dlogis(pmax(-100, theta[y] - eta))
  
  # define (scalar) gradient of density function 
  glogis <- function(x){
    glogist <- exp(x)*(exp(x)-1) / (1+exp(x))^3
    return(glogist)
  }
  gr <- glogis(pmin(100, theta[y + 1] - eta))
  gr_1 <- glogis(pmax(-100, theta[y] - eta))
  
  # define jacobian (d theta/d alpha)
  jacobian <- matrix (0, q, q)
  jacobian[,1] <- 1
  for(i in 2:ncol(jacobian)) jacobian[i:q, i] <- exp(alpha)[i]
  
  # dscore / dtheta dtheta'
  dtheta2 <- - crossprod((Yr * dr - Yr_1 * dr_1) %*% jacobian, ((Yr * dr - Yr_1 * dr_1) * (weights/Pr/Pr)) %*% jacobian ) +
    ( crossprod((Yr * gr * (weights/Pr)) %*% jacobian, Yr %*% jacobian) -
        crossprod((Yr_1 * gr_1 * (weights/Pr)) %*% jacobian, (Yr_1) %*% jacobian) ) 
  
  # dscore / dbeta dbeta'
  dbeta2 <- - crossprod(x * ((dr - dr_1) * (weights/Pr)), x * (dr - dr_1) / Pr) + 
    crossprod(x * ((gr - gr_1) * (weights/Pr)), x) 
  
  # dscore / dbeta dtheta
  dbt.th <- crossprod( ((Yr * dr - Yr_1 * dr_1) * (weights/Pr)) %*% jacobian , x * ((dr - dr_1) / Pr) ) -
    t(crossprod( crossprod((Yr * gr * (weights/Pr)) , x) - crossprod(Yr_1 * gr_1 * (weights/Pr), x), jacobian  ))
  
  H <- rbind(cbind(dbeta2, t(dbt.th)),
             cbind(dbt.th, dtheta2)) 
  
  return(-(H)) 
}

