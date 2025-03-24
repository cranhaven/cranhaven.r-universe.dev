cvx.lse.con.reg <- function(t, z, w = NULL, ...) UseMethod("cvx.lse.con.reg")

cvx.lse.con.reg.default <- function(t, z, w = NULL, ...){
	t <- as.vector(t)
	z <- as.vector(z)
	if (!all(is.finite(c(t, z))))
    stop("missing or infinite values in inputs are not allowed")
    if(length(t) != length(z))
      stop("'x' and 'y' must have same length.")
    n = length(t)
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
  A <- cbind(t, z, w)
  A <- A[order(A[,1]),]
  x <- as.vector(A[,1])
  y <- as.vector(A[,2])
  w <- as.vector(A[,3])
  out<-conreg(x,y,w, convex=TRUE)
  fit<-out$yf
	deriv <- diff(fit)/diff(x)
	deriv <- c(deriv, deriv[length(deriv)])
	ret1 <- list(x.values = x, y.values = y, w = w, fit.values = fit, deriv = deriv, iter = 1,
		residuals = y - fit, minvalue = mean({w*{y-fit}^2}), convergence = 1)
	ret1$call <- match.call()
	class(ret1) <- "cvx.lse.reg"
	return(ret1)
}