rifle <-
function(A,B,init,k,eta=0.01,convergence=1e-3,maxiter=5000){
	p <- ncol(B)
	x <- init
	x <- init/sqrt(sum(x^2))
	criteria <- 1e10
	iter <- 0
	while(criteria > convergence && iter <= maxiter){
	rho <- as.numeric(t(x)%*%A%*%x/(t(x)%*%B%*%x))
	C <- diag(1,p,p)+ eta/rho*(A-rho*B)
	xprime <- C%*%x/sqrt(sum((C%*%x)^2))
# Perform truncation
	truncate.value <- sort(abs(xprime),decreasing=TRUE)[k]
	xprime[which(abs(xprime)<truncate.value)] <- 0
	xprime <- xprime/sqrt(as.numeric(t(xprime)%*%xprime))
	criteria <- sqrt(sum((x-xprime)^2))
    #print(criteria)
	x <- xprime
	iter <- iter+1
	}
    #print(iter)
	return(xprime)
}
