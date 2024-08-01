waicnp <-
function(y, X, res, L, nsim, nburn) {
	n <- length(y)	  
	term <- array(0,c(nsim-nburn,L,n))
	logtermsum <- matrix(0, nrow = nsim-nburn, ncol = n)
	
	for(k in 1:(nsim-nburn)) {
		for(l in 1:L) {
			term[k,l,] <- res[[1]][k+nburn,l]*dnorm(y, mean = X%*%res[[2]][k+nburn,l,], sd = sqrt(res[[3]][k+nburn,l]))  
		}
		# Need this :(
		logtermsum[k,] <- apply(term[k,,], 2, function(x) log(sum(x)))
	}
	# Very time consuming: see why
	#logtermsum <- apply(term, c(1,3), function(x) log(sum(x)))

	lpd <- sum(log(apply(exp(logtermsum),2,mean)))
	p2 <- sum(apply(logtermsum,2,var))
	waic <- -2*(lpd-p2)
	return(waic)
}
