predictive.checks.AROC.bsp <-
function(object, statistics = c("min","max","median","skewness"), devnew = TRUE) {
	if(class(object)[2] != "AROC.bsp") {
		stop(paste0("This function can not be used for this object class: ", class(object)[2]))
	}
	y0 <- object$data_model$y$h
	n0 <- length(y0)
	nrep <- length(object$fit$sd)
	yrep <- matrix(0, nrow = n0, ncol = nrep)
	
	for(l in 1:nrep) {
		yrep[,l] <- rnorm(n = n0, mean = as.numeric(object$data_model$X$h%*%object$fit$beta[l,]), sd = object$fit$sd[l])
	}
	i = 1
	for(stat in statistics) {
		if(i != 1 & devnew) dev.new()
		yrepstat <- apply(yrep, 2, function(y, stat) {do.call(stat, list(y))}, stat = stat)
		ystat <- do.call(stat, list(y0))
		xlim <- range(c(yrepstat,ystat))
		hist(yrepstat, col = "gray60", main = stat, xlim = xlim, xlab = "Statistic")
		abline(v = ystat,col="red",lwd=3)
		i = i + 1
	}
	if(devnew) dev.new()
	ylim <- c(0, max(density(y0)$y) + 0.2)
	xlim <- c(min(density(y0)$x) - 0.2, max(density(y0)$x) - 0.2)
	plot(density(yrep[,1]), col = "lightskyblue1", ylim = ylim, xlim = xlim, main = "Density", xlab = "Diagnostic test outcome (nondiseased group)")
	# Only a sample
	s <- sample(1:nrep, ifelse(nrep < 500, nrep, 500)) 
	for(i in s){
		lines(density(yrep[,i]), col = "lightskyblue1")  
	}
	lines(density(y0),col = "black",lwd = 4)

	res <- list()
	res$yrep <- yrep
	res$y0 <- y0
	invisible(res)
}
