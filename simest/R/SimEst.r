sim.est <- function(x, y, w = NULL, beta.init = NULL, nmulti = NULL, L = NULL,
                    lambda = NULL, maxit = 100, bin.tol = 1e-05, beta.tol = 1e-05,
                    method = c("cvx.pen","cvx.lip","cvx.lse","smooth.pen"), 
                    progress = TRUE, force = FALSE) UseMethod("sim.est")

sim.est.default <- function(x, y, w = NULL, beta.init = NULL, nmulti = NULL, L = NULL,
                            lambda = NULL, maxit = 100, bin.tol = 1e-05, beta.tol = 1e-05,
                            method = c("cvx.pen", "cvx.lip", "cvx.lse", "smooth.pen"), 
                            progress = TRUE, force = FALSE){
  x <- as.matrix(x)
  y <- as.vector(y)
  n <- length(y)
  if(n <= 2)
    stop("Number of samples must be greater than 2.")
  if (!all(is.finite(cbind(x, y))))
    stop("missing or infinite values in inputs are not allowed")
  if(nrow(x) != length(y))
    stop("Number of rows of 'x' must match the length of 'y'")
  if (length(method) > 1L)
    stop("'method' should be a vector of length 1!")
  if (!(method %in% c("cvx.pen", "cvx.lip", "cvx.lse", "smooth.pen","cvx.lse.con")))
    stop("The input for argument 'method' is unrecognized!")
  if (length(lambda) >= 1L && !any(method == c("cvx.pen", "smooth.pen"))) {
    warning("Tuning parameter 'lambda' can only be \nused with 'cvx.pen' or 'smooth.pen'!")
    # method <- "cvx.pen"
  }
  if(length(L) >= 1L && method != "cvx.lip"){
    warning("Tuning parameter 'L' can only used with 'cvx.lip'!")
    # method <- "cvx.lip"
  }
  if (method == "cvx.lip" && is.null(L)) {
    stop("The input method 'cvx.lip' requires a \ntuning parameter specification.")
  }
  if(is.null(lambda) && method %in% c("cvx.pen","smooth.pen")){
    lambda <- 0.01*n^{1/5}
    warning(paste("Required tuning parameter not given. \nSetting lambda = ",lambda,sep = ""))
  }
  beta.path <- function(x, G, tt, xG, GG, tmp){
    denom <- 1 - 0.25*tt*tt*tmp
    return(as.vector({{1 + 0.25*tt*tt*tmp + tt*xG}*x - tt*G}/denom))
  }
  if (method == "cvx.pen"){
    foo <- function(t, z, q){
        ter <- cvx.pen.reg(t, z, lambda = lambda, w = q)
        if(min(diff(diff(ter$fit.values)/diff(ter$x.values))) < -1e-01){
          stop("Function Estimate is not Convex from cvx.pen.reg!!")
        }
        return(ter)
    }
  }
  if (method == "cvx.lip"){
    foo <- function(t, z, q){
      ter <- cvx.lip.reg(t, z, w = q, L = L)
      if(min(diff(diff(ter$fit.values)/diff(ter$x.values))) < -1e-01){
        stop("Function Estimate is not Convex from cvx.lip.reg!!")
      }
      return(ter)
    }
  }
  if (method == "cvx.lse" && !force){
    # print("using cvx.lse.con")
    foo <- function(t, z, q){
      ter <- cvx.lse.con.reg(t, z, w = q)
      if(min(diff(diff(ter$fit.values)/diff(ter$x.values))) < -1e-01){
        stop("Function Estimate is not Convex from cvx.lse.con.reg!!")
      }
      return(ter)
    }
  }
  if (method == "cvx.lse" && force){
    foo <- function(t, z, q){
      ter <- cvx.lse.reg(t, z, w = q)
      if(min(diff(diff(ter$fit.values)/diff(ter$x.values))) < -1e-01){
        stop("Function Estimate is not Convex from cvx.lse.reg!!")
      }
      return(ter)
    }
  }
  if (method == "smooth.pen"){
    foo <- function(t, z, q){
      return(smooth.pen.reg(t, z, lambda = lambda, w = q))
    }
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
  BetaInit <- beta.init
  BetaPath <- matrix(0,nrow = nmulti, ncol = d)
  ObjValPath <- rep_len(1,nmulti)
  FitPath <- vector("list",nmulti)
  xMatPath <- vector("list",nmulti)
  TotIter <- 0
  ConvCheck <- rep_len(0,nmulti)
  itervec <- rep_len(1,nmulti)
  for(k in 1:nmulti){
    bfit <- 1e05
    if(progress && k > 1) cat("\rmultistart ", k-1, " of ", nmulti, " done!")
    flag <- 0
    iter <- 0
    while(iter <= maxit && flag == 0){
      iter <- iter + 1
      TotIter <- TotIter + 1
      A <- cbind(x%*%beta.init[k,], y, x)
      tmmp <- fastmerge(A, w = w, tol = bin.tol)
      A <- tmmp$DataMat
      sw <- tmmp$w
      TT <- cbind(sw, A)
      TT <- TT[order(TT[,2]),]
      A <- TT[,-1]; sw <- TT[,1]
      if(min(diff(A[,1])) < 0.9*bin.tol){
        cat("\nMinDiff = ", min(diff(A[,1]))," and bin.tol = ",bin.tol,"\n")
        print(A[,1],20)
        stop("Fastmerge Error!! Datapoints too close!")
      }
      fit <- foo(A[,1], A[,2],sw)
      G <- -colSums(fit$residuals*fit$deriv*A[,-c(1,2)])
      xG <- sum(beta.init[k,]*G)
      GG <- sum(G*G)
      tmp <- xG*xG - GG
      if(tmp > 0){
        cat("xG*xG - GG = ", tmp,"\n")
        stop("Problem with the Cauchy-Schwarz inequality!")
      }
      bp <- xG - G[1]/beta.init[k,1]
      r1 <- {bp - sqrt(bp*bp - tmp)}/{-0.5*tmp}
      r2 <- {bp + sqrt(bp*bp - tmp)}/{-0.5*tmp}
      if(is.na(r1) || is.na(r2)) cat("(r1, r2) = ", c(r1, r2),"\n")
      if(bfit - fit$minvalue < beta.tol || iter == maxit){
        if(iter == maxit){
        	warning(paste("'maxit' reached for Start no. !!", k, sep = ""))
        	ConvCheck[k] <- ConvCheck[k] + 1
        }
        itervec[k] <- iter
        BetaPath[k,] <- beta.init[k,]
        FitPath[[k]] <- fit
        xMatPath[[k]] <- A[,-c(1,2)]
        ObjValPath[k] <- fit$minvalue
        flag <- 1
      } else{
        bfit <- fit$minvalue
      }
      if(r2 - r1 < 2e-05){
        warning("First coordinate stuck at zero.")
        ConvCheck[k] <- ConvCheck[k] + 1
      	itervec[k] <- iter
        BetaPath[k,] <- beta.init[k,]
        FitPath[[k]] <- fit
        xMatPath[[k]] <- A[,-c(1,2)]
        ObjValPath[k] <- fit$minvalue
        flag <- 1
      } else{
        ToOpt <- function(tt){
          A <- cbind(x%*%beta.path(beta.init[k,], G, tt, xG, GG, tmp), y)
          tmmp <- fastmerge(A,w = w,tol=bin.tol)
          A <- tmmp$DataMat
          sw <- tmmp$w
          TT <- cbind(sw, A)
          TT <- TT[order(TT[,2]),]
          A <- TT[,-1]; sw <- TT[,1]
          if(min(diff(sort(A[,1]))) < 0.9*bin.tol){
            cat("MinDiff = ", min(diff(sort(A[,1])))," and bin.tol = ",bin.tol,"\n")
            print(sort(A[,1]),20)
            stop("Fastmerge Error!! Datapoints too close!")
          }
          ffit <- foo(A[,1],A[,2],sw)
          ffit$minvalue
        }
        # opt <- optimize(ToOpt, lower = r1+1e-05, upper = r2-1e-05)$minimum
        opt <- optim(0, ToOpt, method = "L-BFGS-B", lower = r1 + 1e-05, upper = r2 - 1e-05)$par
        beta.init[k,] <- beta.path(beta.init[k,], G, opt, xG, GG, tmp)
      }
    }
  }
  if(progress){
  	cat("\rmultistart ", k, " of ", nmulti, " done!")
  	cat("\n")
  }
  K <- which.min(ObjValPath)
  fit <- FitPath[[K]]
  ret <- list(beta = BetaPath[K,], x.mat = xMatPath[[K]], lambda = lambda, L = L, minvalue = ObjValPath[K],
              nmulti = nmulti, K = K, BetaPath = BetaPath, BetaInit = BetaInit,
              ObjValPath = ObjValPath, regress = fit, iter = TotIter, itervec = itervec,
              x.values = fit$x.values, y.values = fit$y.values, fit.values = fit$fit.values,
              deriv = fit$deriv, residuals = fit$residuals)
  ret$call <- match.call()
  ret$method <- method
  ret$convergence <- length(which(ConvCheck == 0))
  class(ret) <- "sim.est"
  return(ret)
}

print.sim.est <- function(x,...){
	cat("Call:\n")
	print(x$call)
	cat("Estimate of beta is:\n")
	print(x$beta)
	cat("Number of Starting Vectors:\n")
	print(x$nmulti)
	cat("Initial vector leading to the minimum:\n")
	print(x$BetaInit[x$K,])
	cat("Minimum Criterion Value Obtained:\n")
	print(x$minvalue)
	cat("Total Number of Iterations:\n")
	print(x$iter)
	cat("Convergence Status:\n")
	print(paste(x$convergence," out of ",x$nmulti," converged",sep = ""))
}

plot.sim.est <- function(x,...){
	xx <- x$x.values
    yx <- x$y.values
    fitx <- x$fit.values
    resx <- x$residuals
    if(x$method == "cvx.pen")
    	tt <- "Convex Link Function Estimate\n using Penalized LS Objective"
    if(x$method == "cvx.lse.con")
      tt <- "Convex Conreg Link Function Estimate\n using Least Squares Objective"
    if(x$method == "cvx.lse")
    	tt <- "Convex Link Function Estimate\n using Least Squares Objective"
    if(x$method == "cvx.lip")
    	tt <- "Convex Link Function Estimate\n using Lipschitz LS Objective"
    if(x$method == "smooth.pen")
    	tt <- "Unconstrained Link Function Estimate\n using Penalized LS Objective"
    if(x$method == "smooth.pen.gcv")
      tt <- "Unconstrained Link Function Estimate\n using Penalized LS Objective (GCV)"
	plot.window(c(0,7), c(0,7))
	par(mfrow = c(2,2), mar = c(3,3,3,1), mgp = c(1.8,0.5,0))
	plot(xx, yx, xlab = expression(x^T*hat(beta)), ylab = expression(paste('y and ',hat(y),' values')),
		type = 'p', pch = 20, cex = 1, main = tt)
	lines(xx, fitx, lwd = 2,col = "red")
	plot(fitx,resx,xlab = 'Fitted Values',ylab = "Residuals",pch = 20, type = 'p', main = "Fitted vs Residuals")
  abline(h = 0.0, lty = 4, col = "red")
  # plot(1:x$nmulti, x$ObjValPath,xlab="Start No.",ylab="Objective Value", main = "Minimum Obtained from each Start")
  hist(x$ObjValPath, breaks = x$nmulti, main = "Minimum Obtained from each Start", xlab = "Objective Values")
  qqnorm(resx,main="Normal Q-Q Plot: Residuals",pch = 20)
  qqline(resx)
}

predict.sim.est <- function(object, newdata = NULL, deriv = 0, ...){
    if(deriv == 0 || deriv == 1) f = deriv
    else stop("deriv must either be 0 or 1!")
	req <- object$regress
	B <- object$beta
	if(!is.numeric(newdata)){
		stop("'newdata' should be numeric!")
	}
	if(!is.null(newdata)){
		newdata <- c(newdata)
		newdata <- matrix(newdata, ncol = length(B))
		t <- newdata%*%B
		if(object$method != "cvx.pen"){
			return(predict(req, t, deriv = deriv))
		}
		else{
			return(predict(req, t)[,deriv+2])
		}
	} else{
		warning("No 'newdata' found and so using input 'x' values")
		if(deriv == 0) return(object$fit.values)
        if(deriv == 1) return(object$deriv)
	}
}