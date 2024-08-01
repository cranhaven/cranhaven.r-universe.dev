AROC.bnp <-
function(formula.healthy, group, tag.healthy, data, scale = TRUE, p = seq(0,1,l = 101), paauc = paauccontrol(), compute.lpml = FALSE, compute.WAIC = FALSE, m0, S0, nu, Psi, alpha = 1, a = 2, b = 0.5, L = 10, nsim = 10000, nburn = 2000) {
	if(inherits(formula.healthy, "character")) {		  
		formula.healthy <- as.formula(formula.healthy)
	}

	paauc <- do.call("paauccontrol", paauc)

	data.h <- data[data[,group] == tag.healthy,]
	data.d <- data[data[,group] != tag.healthy,]

	n0 <- nrow(data.h)
	n1 <- nrow(data.d)
	np <- length(p)

	# Construct design matrix
	MM0 <- design.matrix.bnp(formula.healthy, data.h)
	X0 <- MM0$X

	# Construct design matrix in diseased population (based on healthy)
	X1 <- predict(MM0, data.d)$X
  	k <-  ncol(X0)
	

	if(missing(m0)) {
		m0 <- rep(0,k)
	} else {
		if(length(m0) != k) {
			stop(paste0("Argument 'm0' must be a vector of length ", k))
		}
	}

	if(missing(S0)) {
		S0 <- 100*diag(k)
	} else {
		if(!is.matrix(S0) | !all(dim(S0) == c(k,k))) {
			stop(paste0("Argument 'S0' must be a matrix of dimension ", k, "x", k))
		}
	}

	if(missing(nu)) {
		nu <- k + 2
	} else {
		if(nu < k + 2) {
			stop(paste0("Argument 'nu' must be larger than ", k + 2))
		}
	}

	if(missing(Psi)) {
		Psi <- diag(k)
	} else {
		if(!is.matrix(Psi) | !all(dim(Psi) == c(k,k))) {
			stop(paste0("Argument 'Psi' must be a matrix of dimension ", k, "x", k))
		}
	}
	# Fit the model in the healthy population
	res0 <- bddp(y = data.h[,MM0$iformula$marker], X = X0, alpha = alpha, m = m0, S = S0, nu = nu, psi = Psi, a = a, b = b, nsim = nsim, L = L, scale = TRUE)

	udddp <- matrix(0, nrow = n1, ncol = (nsim-nburn))
	y1 <- data.d[,MM0$iformula$marker]
	prob <- res0[[1]]
	beta <- res0[[2]]
	sd <- sqrt(res0[[3]])
	for(l in (nburn+1):nsim) {
    	udddp[,l-nburn] <- 1 - apply(t(prob[l,]*t(pnorm(y1, mean = X1%*%t(beta[l,,]), sd = rep(sd[l,], each = length(y1))))),1, sum)
    }
	weights <- matrix(0, nrow = n1, ncol=(nsim-nburn))
	for(l in 1:(nsim-nburn)) {
  		aux1 <- rexp(n1,1)
  		weights[,l] <- aux1/sum(aux1)
	}
	arocbbddp <- matrix(0, nrow = np, ncol = (nsim-nburn))
	aucddp <- numeric(nsim-nburn)
	if(paauc$compute) {
		paucddp <- numeric(nsim-nburn)
		# Truncated pv
		tudddp <- matrix(pmin(paauc$value, udddp), nrow = n1)
  	}
  			
	for(j in 1:np) {
		arocbbddp[j,] <- colSums(weights*(udddp<=p[j])) 
	}  

	aucddp <- 1 - colSums(weights*udddp)
	if(paauc$compute) {
		paucddp <- paauc$value - colSums(weights*tudddp)
	}

	AROC <- matrix(0, ncol = 3, nrow = np, dimnames = list(1:np, c("est","ql", "qh")))
	AROC[,1] <- apply(arocbbddp,1,mean)
	AROC[,2] <- apply(arocbbddp,1,ql)
	AROC[,3] <- apply(arocbbddp,1,qh)
	AUC <- c(mean(aucddp), quantile(aucddp,c(0.025,0.975)))
	names(AUC) <- c("est","ql", "qh")

	res <- list()
	res$call <- match.call()
	res$p <- p
	res$ROC <- AROC
	res$AUC <- AUC
	if(paauc$compute) {
		res$pAUC <- c(mean(paucddp), quantile(paucddp,c(0.025,0.975)))
		names(res$pAUC) <- c("est","ql", "qh")
		attr(res$pAUC, "value") <- paauc$value
	}
	if(compute.lpml) {
		res$lpml <- lpml(y = data.h[,MM0$iformula$marker], X = X0, res = res0, L = L, nsim = nsim, nburn = nburn)
	}
	if(compute.WAIC) {
		res$WAIC<- waicnp(y = data.h[,MM0$iformula$marker], X = X0, res = res0, L = L, nsim = nsim, nburn = nburn)
	}
	# Results of the fit in the healthy population (neeeded to calculate predictive checks or other statistics)
	res$fit <- list(mm = MM0, beta = res0[[2]][(nburn+1):nsim,,], sd = sqrt(res0[[3]][(nburn+1):nsim,]), probs = res0[[1]][(nburn+1):nsim,])
	res$data_model <- list(y = list(h = data.h[,MM0$iformula$marker], d = data.d[,MM0$iformula$marker]), X = list(h = X0, d = X1))
	class(res) <- c("AROC","AROC.bnp")
	res
}
