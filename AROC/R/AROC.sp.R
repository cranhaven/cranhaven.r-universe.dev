AROC.sp <- 
function(formula.healthy, group, tag.healthy, data, est.surv.h = c("normal", "empirical"), p = seq(0,1,l = 101), B = 1000) {
	est.surv.h <- match.arg(est.surv.h)
	np <- length(p)
	if(inherits(formula.healthy, "character")) {		  
		formula.healthy <- as.formula(formula.healthy)
	}
	marker <- all.vars(formula.healthy)[1]

	compute.ROC <- function(formula.healthy, group, tag.healthy, data, est.surv.h, p = seq(0,1,l = 101)) {
		data.h <- data[data[,group] == tag.healthy,]
		data.d <- data[data[,group] != tag.healthy,]

		n0 <- nrow(data.h)
		n1 <- nrow(data.d)

		np <- length(p)

		marker <- all.vars(formula.healthy)[1]

		# Fit the model in the healthy population
		fit0p <- lm(formula = formula.healthy, data = data.h)
		sigma0p <- summary(fit0p)$sigma
		pre.placement.values <- (data.d[,marker]-predict(fit0p, newdata = data.d))/sigma0p

		# Evaluate the model in the diseased population
		if(est.surv.h == "normal") {
			u1 <- 1-pnorm(pre.placement.values)
		} else {
			res0p <- fit0p$residuals/sigma0p
			F0res <- ecdf(res0p)
			u1 <- 1 - F0res(pre.placement.values)
		}
		# Compute the AROC
		arocp <- numeric(np)
		for(i in 1:np){
		  arocp[i] <- sum(u1<=p[i])/n1
		}
		aarocp <- simpson(arocp, p)

		res <- list()
		res$p <- p
		res$ROC <- arocp
		res$AUC <- aarocp
		res$fit <- fit0p
		res$data.h <- data.h
		res$data.d <- data.d
		res
	}
	res.fit <- compute.ROC(formula.healthy = formula.healthy, group = group, tag.healthy = tag.healthy, data = data, est.surv.h = est.surv.h, p = p)
	arocp  <- res.fit$ROC
	aarocp <- res.fit$AUC
	if(B > 0) {
		# Confidence intervals
		arocpb <- matrix(0, nrow = np, ncol = B)
		aarocpb <- numeric(B)
		for(l in 1:B) {
			# Another option: healthy (residuals) - diseased (original sample)
			data.boot.d <- res.fit$data.d[sample(nrow(res.fit$data.d), replace=TRUE),]
			data.boot.h <- res.fit$data.h
			res.h.b <- sample(res.fit$fit$residuals, replace = TRUE)
			data.boot.h[,marker] <- res.fit$fit$fitted + res.h.b
			data.boot <- rbind(data.boot.d, data.boot.h)

			res.boot <- compute.ROC(formula.healthy = formula.healthy, group = group, tag.healthy = tag.healthy, data = data.boot, est.surv.h = est.surv.h, p = p)
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
	res$fit.h <- res.fit$fit
	res$est.surv.h <- est.surv.h
	class(res) <- c("AROC","AROC.sp")
	res
}
