getVarExpSim <-
function(ORs,K,iter=1000){
	# calculate variance explained by simulation
	sam <- replicate(iter,(runif(ORs[,5]) < ORs[,5]) + (runif(ORs[,5]) < ORs[,5]))
	temp <- apply(sam,2,function(x) prod((1*(x == 0) + ORs[,3]*(x == 1) + ORs[,4]*(x==2))/ORs[,6]))
	post <- applyORs(temp,K)
	T <- qnorm(1 - K)
	mu <- T - qnorm(1 - post)
	return(var(mu))
}

