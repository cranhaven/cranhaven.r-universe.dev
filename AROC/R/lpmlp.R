lpmlp <-
function(y, X, res, nsim, nburn) {
	n <- length(y)
	aux <- matrix(0,nrow=n,ncol=nsim-nburn)
	cpoinv <- numeric(n)

	for(k in 1:(nsim-nburn)) {
		aux[,k] <- dnorm(y, mean = X%*%res[[1]][k+nburn,], sd = sqrt(res[[2]][k+nburn]))
	}
	cpoinv <- apply(aux, 1, function(x) mean(1/x))  
	cpo <- 1/cpoinv
	lpml <- sum(log(cpo))

	res <- list()
	res$cpo <- cpo
	res$lpml <- lpml
	res 
}
