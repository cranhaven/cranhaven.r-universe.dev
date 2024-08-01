waicp <-
function(y, X, res, nsim, nburn) {
	n <- length(y)   
	logterm <- matrix(0, nrow = nsim-nburn, ncol = n)

	for(k in 1:(nsim-nburn)){
		logterm[k,] <- dnorm(y, mean = X%*%res[[1]][k+nburn,], sd = sqrt(res[[2]][k+nburn]),log = TRUE)  
	}  
	
  
	lpd <- sum(log(apply(exp(logterm),2,mean)))
	p2 <- sum(apply(logterm,2,var))
	waic <- -2*(lpd-p2)  
	return(waic)
}
