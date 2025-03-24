smooth.pen.reg <- function(x, y, lambda, w = NULL, agcv = FALSE, agcv.iter = 100, ...) UseMethod("smooth.pen.reg")

smooth.pen.reg.default <- function (x, y, lambda, w = NULL, agcv = FALSE, agcv.iter = 100, ...) {
	if (!all(is.finite(c(x, y))))
	  stop("missing or infinite values in inputs are not allowed")
	if (length(x) != length(y))
	  stop("'x' and 'y' must have same length.")
	n <- length(x)
	w <- if (is.null(w))
	  rep_len(1, n)
	else {
	  if (n != length(w))
	    stop("lengths of 'x' and 'w' must match")
	  if (any(w <= 0))
	    stop("all weights should be positive")
	  w
	}
	if(agcv) {
	  flag <- 1L
	} else {
		flag <- 0L
	}
	# print(flag)
	A <- cbind(as.vector(unname(x)), as.vector(unname(y)), as.vector(w))
	A <- A[order(A[, 1]), ]
	x <- as.vector(A[, 1])
	y <- as.vector(A[, 2])
	w <- as.vector(A[, 3])
	if (!is.finite(lambda) || lambda < 0)
	  stop("'lambda' must be non-negative and finite")
	h <- diff(x)
	Qty <- diff(diff(y)/h)
	out <- .C(spen_egcv, n = as.integer(n), x = as.double(x), y = as.double(y),
	  w = as.double(w),  h = as.double(h), Qty = as.double(Qty),
	  lm = as.double(c(lambda,0)), yhat = double(n), iter = as.integer(agcv.iter),
	  EGCV = as.integer(flag), agcv = as.double(0))
	# print(out$EGCV)
	m <- out$yhat
	tmp <- splinefun(x, m)
	deriv <- unname(tmp(x, deriv = 1))
	ret <- list(x.values = x, y.values = y, fit.values = m, w = w,
	            deriv = as.vector(deriv), residuals = y - m, iter = 1,
	            convergence = 0)
	ret$minvalue <- out$lm[2]
	ret$agcv.score <- out$agcv
	ret$call <- match.call()
	ret$splinefun <- tmp
	class(ret) <- "smooth.pen.reg"
	return(ret)
}

print.smooth.pen.reg <- function(x, ...){
  cat("Call:\n")
  print(x$call)
  cat("Minimum Criterion Value Obtained:\n")
  print(x$minvalue)
  cat("Number of Iterations:\n")
  print(x$iter)
  cat("Convergence Status:\n")
  print(x$convergence)	
}

plot.smooth.pen.reg <- function(x, ...){
  xx <- x$x.values
  yx <- x$y.values
  fitx <- x$fit.values
  resx <- x$residuals
  diagnostics = TRUE
  if(diagnostics){
	plot.window(c(0,7), c(0,7))
	par(mfrow=c(2,2), mar=c(3,3,3,1), mgp=c(1.3,.5,0))
	plot(xx,yx,xlab = 'x',ylab = expression(paste('y and ',hat(y),' values')), 
	  type = 'p', pch = "*", cex = 1, main = "Smoothing Spline using\n Penalized Least Squares")
	lines(xx, fitx, lwd = 2,col = "red")
	plot(fitx,resx,xlab = 'Fitted Values',ylab = "Residuals",pch = "*", type = 'p', main = "Fitted vs Residuals")
	abline(h = 0.0, lty = 4, col = "red")
	plot(yx,fitx,xlab = "Actual Values",ylab = "Fitted Values",pch = "*", type = 'p', main = "Actual vs Fitted")
	abline(a = 0, b = 1, lty = 4, col = "red")
	qqnorm(resx)
	qqline(resx)
  } else{
  plot.window(c(0,7), c(0,7))
	par(mfrow=c(1,1), mar=c(3,3,3,1), mgp=c(1.3,.5,0))
	plot(xx,yx,xlab = 'x',ylab = expression(paste('y and ',hat(y),' values')), 
	  type = 'p', pch = "*", cex = 1, main = "Smoothing Spline using\n Penalized Least Squares")
	lines(xx, fitx, lwd = 2)  	
  }
  invisible(list(x = xx, y = yx, fit = fitx))
}

predict.smooth.pen.reg <- function(object, newdata = NULL, deriv = 0, ...){
  if(deriv == 0 || deriv == 1) f = deriv
  else stop("deriv must either be 0 or 1!")  
	if(!is.null(newdata))
		return(object$splinefun(as.vector(newdata), deriv = deriv))
	else{
		warning("No 'newdata' found and so using input 'x' values")
		if(deriv == 0) return(object$fit.values)
    if(deriv == 1) return(object$deriv)  
	}
}