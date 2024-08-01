lpml <-
function(y, X, res, L, nsim, nburn) {
	n <- length(y) 	  
	term <- array(0,c(nsim-nburn,L,n))
	for(k in 1:(nsim-nburn)) {
		for(l in 1:L) {
			term[k,l,] <- res[[1]][k+nburn,l]*dnorm(y, mean = X%*%res[[2]][k+nburn,l,], sd = sqrt(res[[3]][k+nburn,l]))  
		}
	}
	termsum <- matrix(0, nrow = nsim-nburn, ncol = n)
	# Very time consuming: see why
	# termsum <- apply(term, c(1,3), sum)
	# Need this
	for(i in 1:n) {
		for(k in 1:(nsim-nburn)) {
			termsum[k,i] <- sum(term[k,,i])
		}
	}		
	cpoinv <- apply(termsum, 2, function(x) mean(1/x))

	cpo <- 1/cpoinv
	lpml <- sum(log(cpo))
	
	res <- list()
	res$cpo <- cpo
	res$lpml <- lpml
	res 
}
