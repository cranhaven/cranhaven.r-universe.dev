simestgcv <- function(x, y, w = NULL, beta.init = NULL, nmulti = NULL,
                    lambda = NULL, maxit = 100, bin.tol = 1e-06, beta.tol = 1e-05, 
                    agcv.iter = 100, progress = TRUE) UseMethod("simestgcv")

simestgcv.default <- function(x, y, w = NULL, beta.init = NULL, nmulti = NULL,
                            lambda = NULL, maxit = 100, bin.tol = 1e-06, beta.tol = 1e-05, 
                            agcv.iter = 100, progress = TRUE){
  x <- as.matrix(x)
  y <- as.vector(y)
  n <- length(y)
  if(n <= 2)
    stop("Number of samples must be greater than 2.")
  if (!all(is.finite(cbind(x, y))))
    stop("missing or infinite values in inputs are not allowed")
  if(nrow(x) != length(y))
    stop("Number of rows of 'x' must match the length of 'y'")
  if(is.null(lambda))
    stop("Required input 'lambda' missing.")
  if(length(lambda) == 2)
    lambda <- c(min(lambda), max(lambda))
  beta.path <- function(x, G, tt, xG, GG, tmp){
    denom <- 1 - 0.25*tt*tt*tmp
    return(as.vector({{1 + 0.25*tt*tt*tmp + tt*xG}*x - tt*G}/denom))
  }
  foo <- function(t, z, q, lam){
    return(smooth.pen.reg(t, z, lambda = lam, w = q))
  }
  d <- ncol(x)
  if(is.null(w)){
    w <- rep_len(1,n)
  } else{
    if(length(w) != n)
      stop("'w' and 'y' should be of same length!")
    if(any(w <= 0))
      stop("'w' should contain positive elements!")
    w <- w
  }
  if(is.null(beta.init)){
    if(is.null(nmulti)){
      nmulti <- round(d*log(d)) + 1
      warning(paste("'nmulti' not given. \nTaking nmulti = ",nmulti,sep = ""))
    }  
    beta.init <- matrix(rnorm(nmulti*d),ncol = d)
  } else{
    beta.init <- matrix(beta.init,ncol = d)
    nmulti <- nrow(beta.init)
  }
  beta.init <- beta.init*sign(beta.init[,1])/sqrt(rowSums(beta.init*beta.init))
  GcvOpt <- function(lam, BetaInit){
    ObjValPath <- rep_len(1,nmulti)
    for(k in 1:nmulti){
      bfit <- 1e05
      flag <- 0
      iter <- 0
      while(iter <= maxit && flag == 0){
        iter <- iter + 1
        A <- cbind(x%*%BetaInit[k,], y, x)
        tmmp <- fastmerge(A, w = w, tol = bin.tol)
        A <- tmmp$DataMat
        sw <- tmmp$w
        TT <- cbind(sw, A)
        TT <- TT[order(TT[,2]),]
        A <- TT[,-1]; sw <- TT[,1]
        if(min(diff(A[,1])) < 0.9*bin.tol){
          cat("MinDiff and bin.tol are:","\n")
          print(c(min(diff(A[,1])), bin.tol),20)
          print(sort(A[,1]),20)
          stop("Fastmerge Error!! Datapoints too close!")
        }
        fit <- foo(A[,1], A[,2],sw,lam)
        G <- -colSums(fit$residuals*fit$deriv*A[,-c(1,2)])
        xG <- sum(BetaInit[k,]*G)
        GG <- sum(G*G)
        tmp <- xG*xG - GG
        if(tmp > 0){
          cat("xG*xG - GG = ", tmp,"\n")
          stop("something went wrong!")
        }
        bp <- xG - G[1]/BetaInit[k,1]
        r1 <- {bp - sqrt(bp*bp - tmp)}/{-0.5*tmp}
        r2 <- {bp + sqrt(bp*bp - tmp)}/{-0.5*tmp}
        if(is.na(r1) || is.na(r2)) cat("(r1, r2) = ", c(r1, r2),"\n")        
        if(bfit - fit$minvalue < beta.tol || iter == maxit){
          if(iter == maxit){
            warning(paste("'maxit' reached for Start no. !!", k, sep = ""))
          }
          ObjValPath[k] <- fit$minvalue
          flag <- 1
        } else{
          bfit <- fit$minvalue
        }
        if(r2 - r1 < 2e-05){
          warning("First coordinate stuck at zero.")
          ObjValPath[k] <- fit$minvalue
          flag <- 1
        } else{
          ToOpt <- function(tt){
            A <- cbind(x%*%beta.path(BetaInit[k,], G, tt, xG, GG, tmp), y)
            tmmp <- fastmerge(A,w = w,tol=bin.tol)
            A <- tmmp$DataMat
            sw <- tmmp$w
            TT <- cbind(sw, A)
            TT <- TT[order(TT[,2]),]
            A <- TT[,-1]; sw <- TT[,1]
            if(min(diff(A[,1])) < 0.9*bin.tol){
              cat("MinDiff and bin.tol are:","\n")
              print(c(min(diff(A[,1])), bin.tol),20)
              print(sort(A[,1]),20)
              stop("Fastmerge Error!! Datapoints too close!")
            }
            ffit <- foo(A[,1],A[,2],sw,lam)
            ffit$minvalue
          }
          # opt <- optimize(ToOpt, lower = r1+1e-05, upper = r2-1e-05)$minimum
          opt <- optim(0, ToOpt, method = "L-BFGS-B", lower = r1 + 1e-05, upper = r2 - 1e-05)$par
          BetaInit[k,] <- beta.path(BetaInit[k,], G, opt, xG, GG, tmp)
        }
      }
    }
    K <- which.min(ObjValPath)
    A <- cbind(x%*%BetaInit[K,], y, x)
    tmmp <- fastmerge(A, w = w, tol = bin.tol)
    A <- tmmp$DataMat
    sw <- tmmp$w
    TT <- cbind(sw, A)
    TT <- TT[order(TT[,2]),]
    A <- TT[,-1]; sw <- TT[,1]
    if(min(diff(A[,1])) < 0.9*bin.tol){
            cat("MinDiff and bin.tol are:","\n")
            print(c(min(diff(A[,1])), bin.tol),20)
            print(sort(A[,1]),20)
            stop("Fastmerge Error!! Datapoints too close!")
    }
    tt <- smooth.pen.reg(A[,1], A[,2],w=sw,lambda=lam,agcv = TRUE, agcv.iter = agcv.iter)
    return(tt$agcv.score)
  }
  # BestGcv <- optim(0.01*n^{1/5}, GcvOpt, method = "L-BFGS-B", lower = lambda[1], 
  		# upper = lambda[2], BetaInit = beta.init)
  BestGcv <- optimize(GcvOpt, lower = lambda[1], upper = lambda[2], BetaInit = beta.init, maximum  = FALSE)
  lam <- BestGcv$minimum
  ret <- sim.est(x = x,y = y, method = "smooth.pen", lambda = lam, beta.init = beta.init, progress = progress)
  ret$method <- "smooth.pen.gcv"
  ret$GCVoptim <- BestGcv
  ret$call <- match.call()
  return(ret)
}