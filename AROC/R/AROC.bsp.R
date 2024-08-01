AROC.bsp <-
function(formula.healthy, group, tag.healthy, data, scale = TRUE, p = seq(0,1,l = 101), paauc = paauccontrol(), compute.lpml = FALSE, compute.WAIC = FALSE, m0, S0, nu, Psi, a = 2, b = 0.5, nsim = 5000, nburn = 1500) {
	
	if(inherits(formula.healthy, "character")) {		  
		formula.healthy <- as.formula(formula.healthy)
	}

	paauc <- do.call("paauccontrol", paauc)

	tf <- terms.formula(formula.healthy, specials = c("f"))
	if (attr(tf, "response") > 0) {
		marker <- as.character(attr(tf, "variables")[2])
	} else {
		stop("The formula should include the response variable (left hand side)")
	}

	data.h <- data[data[,group] == tag.healthy,]
	data.d <- data[data[,group] != tag.healthy,]

	marker <- all.vars(formula.healthy)[1]

	MM0 <- design.matrix.bsp(update(formula.healthy, NULL ~ .), data.h)
	X0 <- MM0$X

	X1 <- predict(MM0, data.d)$X

	n0 <- nrow(data.h)
	n1 <- nrow(data.d)
	np <- length(p)

	k <- ncol(X0)

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


	res0 <- regnth(y = data.h[,marker], X = X0, m0 = m0, S0 = S0, nu0 = nu, psi0 = Psi, a = a, b = b, nsim = nsim, scale = TRUE)

	up <- matrix(0, nrow = n1, ncol = nsim-nburn)
	#for(k in (nburn+1):nsim) {
	#	for(i in 1:n1){  
	#		up[i,k-nburn] = 1 - pnorm(data.d[i,marker], mean = X1[i,]%*%res0[[1]][k,], sd = sqrt(res0[[2]][k]))
	#	}  
	#}

	for(k in (nburn+1):nsim) {
		up[,k-nburn] = 1 - pnorm(data.d[,marker], mean = X1%*%res0[[1]][k,], sd = sqrt(res0[[2]][k]))
	}

	weights <- matrix(0, nrow = n1, ncol=(nsim-nburn))
	for(l in 1:(nsim-nburn)) {
  		aux1 <- rexp(n1,1)
  		weights[,l] <- aux1/sum(aux1)
	}


	arocp <- matrix(0, nrow = np, ncol = (nsim-nburn))
	aarocp <- numeric(nsim-nburn)
	
	for(j in 1:np) {
		arocp[j,] <- colSums(weights*(up<=p[j])) 
	}  

	if(paauc$compute) {
		paucp <- numeric(nsim-nburn)
		# Truncated pv
		tup <- matrix(pmin(paauc$value, up), nrow = n1)
  	}
  	
	aarocp <- 1 - colSums(weights*up)
	if(paauc$compute) {
		paucp <- paauc$value - colSums(weights*tup)
	}


	AROC <- matrix(0, ncol = 3, nrow = np, dimnames = list(1:np, c("est","ql", "qh")))
	AROC[,1] <- apply(arocp,1,mean)
	AROC[,2] <- apply(arocp,1,ql)
	AROC[,3] <- apply(arocp,1,qh)
	AUC <- c(mean(aarocp), quantile(aarocp,c(0.025,0.975)))
	names(AUC) <- c("est","ql", "qh")
	
	res <- list()
	res$call <- match.call()
	res$p <- p
	res$ROC <- AROC
	res$AUC <- AUC
	if(paauc$compute) {
		res$pAUC <- c(mean(paucp), quantile(paucp,c(0.025,0.975)))
		names(res$pAUC) <- c("est","ql", "qh")
		attr(res$pAUC, "value") <- paauc$value
	}  	

	if(compute.lpml) {
		res$lpml <- lpmlp(y = data.h[,marker], X = X0, res = res0, nsim = nsim, nburn = nburn)
	}
	if(compute.WAIC) {
		res$WAIC <- waicp(y = data.h[,marker], X = X0, res = res0, nsim = nsim, nburn = nburn)
	}
	# Results of the fit in the healthy population (neeeded to calculate predictive checks or other statistics)
	res$fit <- list(mm = MM0, beta = res0[[1]][(nburn+1):nsim,], sd = sqrt(res0[[2]][(nburn+1):nsim]))
	res$data_model <- list(y = list(h = data.h[,marker], d = data.d[,marker]), X = list(h = X0, d = X1))
	class(res) <- c("AROC","AROC.bsp")
	res
}
