solve.pentadiag <- function(a, b,...){
	a <- as.matrix(a)
	if(nrow(a) != ncol(a))
		stop("'a' is not a square matrix!")
	b <- as.vector(b)
	if(length(b) != ncol(a))
		stop("'a' and 'b' should be of same length!")
	n <- nrow(a)
	D <- diag(a)
	C <- c(a[cbind(1:(n - 1), 2:n)], 0)
	F <- c(a[cbind(1:(n - 2), 3:n)], 0, 0)
	A <- c(a[cbind(2:n, 1:(n - 1))], 0)
	E <- c(a[cbind(3:n, 1:(n - 2))], 0, 0)
	out <- .C(penta, as.integer(n), as.double(E), as.double(A), 
	as.double(D), as.double(C), as.double(F), as.double(b), double(n))
	z <- as.vector(out[[length(out)]])
	return(z)
}