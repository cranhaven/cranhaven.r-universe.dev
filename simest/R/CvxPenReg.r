cvx.pen.reg <- function(x, y, lambda, w = NULL, tol = 1e-05, maxit = 1000,...) UseMethod("cvx.pen.reg")

cvx.pen.reg.default <- function(x, y, lambda, w = NULL, tol = 1e-05, maxit = 1000,...){
	if(length(x) != length(y))
		stop("'x' and 'y' must have same length.")  
	if (!all(is.finite(c(x, y)))) 
		stop("missing or infinite values in inputs are not allowed")
	n <- length(x)
	if(n <= 2)
		stop("Number of samples must be greater than 2.")
	w <- if (is.null(w)) 
	    rep_len(1, n)
	else {
	    if (n != length(w)) 
	        stop("lengths of 'x' and 'w' must match")
	    if (any(w <= 0)) 
	        stop("all weights should be positive")
	    w
	}	
	A <- cbind(x,y,w)
	A <- A[order(A[,1]),]
	t <- A[,1]
	z <- A[,2]
	w <- A[,3]
	n <- length(w)
	Ky <- diff(diff(z)/diff(t))/diff(t,2)
	a0 <- rep(1,n-2)
	out <- .C(cpen,dim = as.integer(c(n,maxit)), t = as.double(t), z = as.double(z), w = as.double(w), 
		a0 = as.double(a0), lambda = as.double(lambda), Ky = as.double(Ky), L = double(n-1), U = double(n-1), 
		fun = double(n), res = double(n), flag = as.integer(1), tol = as.double(tol), zhat = double(n), 
		iter = as.integer(1), Deriv = double(n))
	ret <- list(x.values = t, y.values = z)
	ret$residuals <- out$res
	ret$fit.values <- out$zhat
	ret$alpha <- out$a0
	ret$deriv <- out$Deriv
	ret$minvalue <- lambda*sum(out$a0*Ky)
	ret$lower <- out$L
	ret$upper <- out$U
	ret$iter <- out$iter
	ret$convergence <- out$flag
	ret$AlphaMVal <- out$fun
	ret$call <- match.call()
	class(ret) <- "cvx.pen.reg"
	return(ret)
}

print.cvx.pen.reg <- function(x,...){
  cat("Call:\n")
  print(x$call)
  cat("Minimum Criterion Value Obtained:\n")
  print(x$minvalue)
  cat("Number of Iterations:\n")
  print(x$iter)
  cat("Convergence Status:\n")
  print(x$convergence)
}

plot.cvx.pen.reg <- function(x, ...){
	xx <- x$x.values
	yx <- x$y.values
	fitx <- x$fit.values
	resx <- x$residuals
	plot.window(c(0,7), c(0,7))
	par(mfrow=c(2,2), mar=c(3,3,3,1), mgp=c(1.3,.5,0))
	plot(xx,yx,xlab = 'x',ylab = expression(paste('y and ',hat(y),' values')), 
		type = 'p', pch = "*", cex = 1, main = "Convex Regression using\n Penalized Least Squares")
	lines(xx, fitx, lwd = 2, col = "red")
	plot(fitx,resx,xlab = 'Fitted Values',ylab = "Residuals",pch = "*", type = 'p', main = "Fitted vs Residuals")
	abline(h = 0.0, lty = 4,col = "red")
	plot(yx,fitx,xlab = "Actual Values",ylab = "Fitted Values",pch = "*", type = 'p', main = "Actual vs Fitted")
	abline(a = 0, b = 1, lty = 4, col = "red")
	qqnorm(resx)
	qqline(resx)
	invisible(list(x = xx, y = yx, fit = fitx))
}

predict.cvx.pen.reg <- function(object, newdata = NULL,...){
	if(is.null(newdata)){
		x <- object$x.values
	}
	else{
		x <- as.vector(newdata)
	}
	## P stores the values of derivatives at newdata.
	## Q stores the function values at newdata.
	## R stores the values of the second derivative at newdata.
	k <- length(x)
	t <- object$x.values
	n <- length(t)
	zhat <- object$fit.values
	deriv <- object$deriv
	L <- object$lower
	U <- object$upper
	fun <- object$AlphaMVal
	out <- .C(predcvxpen, dim = as.integer(c(k,n)), x = as.double(x), t = as.double(t), 
			zhat = as.double(zhat), deriv = as.double(deriv), L = as.double(L), U = as.double(U), 
			fun = as.double(fun), P = double(k), Q = double(k), R = double(k))
	ret <- cbind(out$x, out$Q, out$P, out$R)
	colnames(ret) <- c("newdata", "fun", "1der", "2der")
	return(ret)
}