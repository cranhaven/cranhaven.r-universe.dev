AROC.kernel <-
function(marker, covariate, group, tag.healthy, data, p = seq(0,1,l = 101), B = 1000) {
	np <- length(p)
	compute.ROC <- function(marker, covariate, group, tag.healthy, data, p = seq(0,1,l = 101)) {
		data.h <- data[data[,group] == tag.healthy,]
		data.d <- data[data[,group] != tag.healthy,]

		n0 <- nrow(data.h)
		n1 <- nrow(data.d)

		np <- length(p)

		x0 <- data.h[,covariate]
		y0 <- data.h[,marker]
		x1 <- data.d[,covariate]
		y1 <- data.d[,marker]

		# Fit the location-scale model in the healthy population
		bw.mean.h.p <-  npregbw(ydat = y0, xdat = x0, regtype = "lc", bwmethod = "cv.ls")
		fit.mean.h.p <- npreg(bw.mean.h.p, exdat = x0,residuals = TRUE)
		bw.var.h.p <- npregbw(ydat = (fit.mean.h.p$resid^2), xdat = x0, regtype = "lc", bwmethod = "cv.ls")
		fit.var.h.p <- npreg(bw.var.h.p, exdat = x0, residuals = TRUE)

		res0p <- fit.mean.h.p$resid/sqrt(fit.var.h.p$mean)
		F0res <- ecdf(res0p)

		# Evaluate the model in the diseased population, and compute the AROC
		fit.mean.d.p <- npreg(bw.mean.h.p, exdat = x1,residuals = TRUE)
		fit.var.d.p <- npreg(bw.var.h.p, exdat = x1, residuals = TRUE)
		u1 <- 1 - F0res((y1-fit.mean.d.p$mean)/sqrt(fit.var.d.p$mean))
		arocp <- numeric(np)
		for(i in 1:np){
		  arocp[i] <- sum(u1<=p[i])/n1
		}
		aarocp <- simpson(arocp, p)

		res <- list()
		res$p <- p
		res$ROC <- arocp
		res$AUC <- aarocp
		res$data.h <- data.h
		res$data.d <- data.d
		res$bw.mean <- bw.mean.h.p
		res$bw.var <- bw.var.h.p
		res$fit.mean <- fit.mean.h.p
		res$fit.var <- fit.var.h.p
		res 
	}

	croc <- compute.ROC(marker = marker, covariate = covariate, group = group, tag.healthy = tag.healthy, data = data, p = p)
	arocp <- croc$ROC
	aarocp <- croc$AUC
	if(B > 0) {
		arocpb <- matrix(0, nrow = np, ncol = B)
		aarocpb <- numeric(B)
		for(l in 1:B) {
			# Another option: healthy (residuals) - diseased (original sample)
			data.boot.d <- croc$data.d[sample(nrow(croc$data.d), replace=TRUE),]
			data.boot.h <- croc$data.h
			res.h.b <- sample(croc$fit.mean$resid/sqrt(croc$fit.var$mean), replace = TRUE)
			data.boot.h[,marker] <-croc$fit.mean$mean + sqrt(croc$fit.var$mean)*res.h.b
			data.boot <- rbind(data.boot.d, data.boot.h)

			res.boot <- compute.ROC(marker = marker, covariate = covariate, group = group, tag.healthy = tag.healthy, data = data.boot, p = p)
			arocpb[,l] <- res.boot$ROC
			aarocpb[l] <- res.boot$AUC
		}
	}	
	columns <-switch(as.character(B>0),"TRUE" = 1:3,"FALSE"=1)			    		
	col.names <-c("est","ql", "qh")[columns]

	poolROC <- matrix(0, ncol = length(columns), nrow = np, dimnames = list(1:np, col.names))
	poolROC[,1] <- arocp
	AUC <- aarocp
	if(B > 0) {
		poolROC[,2] <- apply(arocpb,1,ql)
		poolROC[,3] <- apply(arocpb,1,qh)
		AUC <- c(AUC,quantile(aarocpb,c(0.025,0.975)))
	}
	names(AUC) <- col.names

	res <- list()
	res$call <- match.call()
	res$p <- p
	res$ROC <- poolROC
	res$AUC <- AUC
	res$bw.mean <- croc$bw.mean
	res$bw.var <- croc$bw.var
	res$fit.mean <- croc$fit.mean
	res$fit.var <- croc$fit.var
	class(res) <- c("AROC","AROC.kernel")
	res
}
