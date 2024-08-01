predictive.checks.AROC.bnp <-
function(object, statistics = c("min","max","kurtosis","skewness"), devnew = TRUE) {
	if(class(object)[2] != "AROC.bnp") {
		stop(paste0("This function can not be used for this object class: ", class(object)[object]))
	}

	y0 <- object$data_model$y$h
	n0 <- length(y0)
	nrep <- nrow(object$fit$probs)
	yrep <- matrix(0, nrow = n0, ncol = nrep)
	
	aux <- t(apply(object$fit$probs[1:nrep,], 1, function(x, n)  sample(1:length(x), n, replace = TRUE, prob = x), n = n0))
	for(l in 1:nrep) {
		yrep[,l] <- rnorm(n = n0, mean = colSums(t(object$data_model$X$h)*t(object$fit$beta[l,aux[l,],])), sd = object$fit$sd[l,aux[l,]])		
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
	# Density
	if(devnew) dev.new()
	ylim <- c(0, max(density(y0)$y) + 0.2)
	xlim <- c(min(density(y0)$x) - 0.2, max(density(y0)$x) - 0.2)
	plot(density(yrep[,1]),col = "lightskyblue1", ylim = ylim, xlim = xlim, main = "Density", xlab = "Diagnostic test outcome (nondiseased group)")
	s <- sample(1:nrep, ifelse(nrep < 500, nrep, 500)) 
	for(i in s){
		lines(density(yrep[,i]), col="lightskyblue1")
	}
	lines(density(y0), col = "black", lwd = 4)

	res <- list()
	res$yrep <- yrep
	res$y0 <- y0
	invisible(res)
}
