require(nlme)

# Transform to Z score 
Ztransform <- function(p.value, e.sign, eff.sign=TRUE, tol=1E-15) {
    p.value[p.value <= tol] <- tol
	p.value[p.value >= 1 - tol] <- 1 - tol
	if (eff.sign == TRUE) {
		e.sign[e.sign == 0] <- sample(c(-1, 1), sum(e.sign == 0), replace=T)
		z1 <- qnorm(p.value / 2)
		z2 <- qnorm(1 - p.value / 2)
		z <- ifelse(e.sign > 0, z2, z1)
	} else {
		z <- qnorm(1 - p.value)
	}
	return(z)
}

# Internal functions
# For gls with generic correlation structure
corHerit <- function(value, paras, form = ~1, fixed = TRUE) {
	# Place holder - check the validity of the parameter
	object <- value
	attr(object, "formula") <- form
	attr(object, "fixed") <- fixed
	attr(object, "paras") <- paras
	class(object) <- c("corHerit", "corStruct")
	object
}

Initialize.corHerit <- function (object, data, ...) {	
	# Place holder - check the validity of the parameter
	form <- formula(object)
	if (!is.null(getGroupsFormula(form))) {
		attr(object, "groups") <- getGroups(object, form, data = data)
		attr(object, "Dim") <- Dim(object, attr(object, "groups"))
	} else {
		attr(object, "Dim") <- Dim(object, as.factor(rep(1, nrow(data))))
	}
	attr(object, "covariate") <- getCovariate(object, data = data)
	object
}


corMatrix.corHerit <- function (object, covariate = getCovariate(object), ...) {
	
	paras <- attr(object, "paras")
	p <- paras[['p']]
	I <- diag(p)
	hyper <- as.vector(object)
	
	lambda <- 1 / (1 + exp(hyper[1]))
	V <- exp(-((paras[['D']])) * (exp(hyper[2])))
	cor.m <- (1 - lambda) * V +  lambda * I
	cor.m
}

# Extract the coefficient
coef.corHerit <- function (object,  ...) {
	
	paras <- attr(object, "paras")
	coefs <- as.vector(object)	
	coef1 <- 1 / (1 + exp(coefs[1]))
	coef1 <- coef1 / (1 - coef1)
	
	coef2 <-  exp(coefs[2])
	coefs <- c(coef1, coef2)	
	names(coefs) <- paste("Hyper", 1:length(coefs), sep="")
	coefs
}

# Estimating the hyper parameters
EstHyper <- function (y, D, init.val=c(0, 0)) {

	obj.gls <- gls(model = y ~ 1, 
			correlation = corHerit(value=init.val, paras = list(p=length(y), D = D)))
	cc <- c(coef(obj.gls$modelStruct$corStruct), obj.gls$coefficients, obj.gls$logLik)
	cc	
}	

AdjStats <- function (y, V, k, mu, fudge=0.005) {
	p <- nrow(V)
	# Add small fudge
	V.inv <- solve(V + fudge * diag(p))
	I <- diag(p)
	y.adj  <- solve(I + k * V.inv) %*% (k * mu * rowSums(V.inv) + y)
	y.adj
}


# Compute the NFP on the null.ind
PermFDR <- function (F0, Fp, null.ind) {
	
	Fp <- as.matrix(Fp)
	pct <- sum(null.ind) / length(F0)
	Fp <- Fp[null.ind, ]
	
	ord <- order(F0, decreasing = T)
	F0 <- F0[ord]
	perm.no <- ncol(Fp)
	Fp <- as.vector(Fp)
	
	pct.non.na <- mean(!is.na(Fp))
	
	Fp <- Fp[!is.na(Fp)]
	Fp <- sort(c(Fp, F0), decreasing = F)
	
	n <- length(Fp)
	m <- length(F0)
	
	FPN <- (n + 1) - match(F0, Fp) - 1:m
	# Handle identical F0 situations
	FPN <- cummax(FPN)
	
	p.adj.fdr <- FPN / pct.non.na / pct / perm.no / (1:m)
	
	p.adj.fdr <- pmin(1, rev(cummin(rev(p.adj.fdr))))[order(ord)]
}

# Compute tree-based FDR control	
TreeFDR <- function (X, Y, tree, test.func, perm.func, eff.sign = TRUE,  B = 20,  q.cutoff = 0.5, alpha = 1,
		adaptive = c('Fisher', 'Overlap'), alt.FDR = c('BH', 'Permutation'), ...) {
	
	adaptive <- match.arg(adaptive)
	alt.FDR <- match.arg(alt.FDR)
	
	# Make sure the rows of X and tree tips are labeled to avoid error
	if (is.null(rownames(X)) | is.null(tree$tip.label)) {
		warning('Both the data matrix and the tree should have labels (rownames, tip.label) to avoid potential errors!\n')
	} else {
		if (sum(!(rownames(X) %in% tree$tip.label))){
			stop('Some features in the data matrix are not in the tree! Please check!\n')
		} else {
			if (sum(!(tree$tip.label %in% rownames(X)))) {
				warning('The tree have more features than the data matrix! \n')
			} 
		}
	}
	
	# Patristic distance
	D <- (cophenetic(tree)) ^ alpha
	
	if (!is.null(rownames(X)) & !is.null(tree$tip.label)) {
		D <- D[rownames(X), rownames(X)]
	}
	
	cat('Test on original data sets  ...\n')
	test.obs <- test.func(X, Y, ...)
	
	
	if (!is.list(test.obs) | !all(c("e.sign", "p.value") %in% 
					names(test.obs))) {
		stop("test.func should return a list with names e.sign and p.valueif z.transform=TRUE! Please check!\n")
	}
	
	null.ind <- test.obs$p.value >= quantile(test.obs$p.value, 1 - q.cutoff)
	
	z.obs <- Ztransform(test.obs$p.value, test.obs$e.sign, eff.sign)	
	
	
	cat('Test on permuted data sets  ...\n')
	z.perm <- z.perm2 <- matrix(NA, nrow(X), B)	
	for (i in 1:B) {
		perm.obj <- perm.func(X, Y, ...)
		if (!is.list(perm.obj) | !all(c("X", "Y") %in% names(perm.obj))) {
			stop("perm.func should return a list with names X and Y! Please check!\n")
		}
		X.perm <- perm.obj$X
		Y.perm <- perm.obj$Y
		test.perm <- test.func(X.perm, Y.perm, ...)
		
		z.perm[, i] <- z.perm2[, i] <- Ztransform(test.perm$p.value, test.perm$e.sign, eff.sign)
		
		z.perm2[!null.ind, i] <- z.obs[!null.ind]
	}
	
	if (alt.FDR == 'Permutation') {
		cat('Perform ordinary permutation-based FDR control ...\n')
		if (eff.sign == TRUE) {
			p.adj0 <- PermFDR(abs(z.obs), abs(z.perm), rep(TRUE, length(z.obs)))
		} else {
			p.adj0 <- PermFDR(z.obs, z.perm, rep(TRUE, length(z.obs)))
		}
	}
	
	if (alt.FDR == 'BH') {
		cat('Perform ordinary BH-based FDR control ...\n')
		p.adj0 <- p.adjust(test.obs$p.value, 'fdr')
		
	}
	
	cat("Estimating hyperparameter ... \n")
	error <- try(obj <- EstHyper(y = z.obs, D = D))
	if (inherits(error, "try-error")) {
		cat('Hyperparameter estimation failed! Ordinary permutation-based FDR control will be used!\n')
		# p.adj <- p.adjust(test.obs$p.value,'fdr')
		p.adj <- p.adj0
		k <- NULL
		rho <- NULL
		z.adj <- NULL
	} else {
		cat("Structure-based adjustment ...\n")
		k <- obj[1]
		rho <- obj[2]
		mu <- obj[3]
		V <- exp(-1 * rho * D)
		z.adj <- stat.o <- AdjStats(y = z.obs, V = V, k = k, mu = mu)[, 1]
		stat.p <- AdjStats(y = z.perm2, V = V, k = k, mu = mu)
		
		if (eff.sign == TRUE) {
			stat.o <- abs(stat.o)
			stat.p <- abs(stat.p)
		}
		
		p.adj <- PermFDR(stat.o, stat.p, null.ind)			
		
		# Power loss check
		if (adaptive == 'Overlap') {
			fdr.cutoff <- 0.2
			pct.cutoff <- 0.5
			
			ind0 <- p.adj0 <= fdr.cutoff
			ind <- p.adj <= fdr.cutoff
			
			if (sum(p.adj[ind0] <= fdr.cutoff) <  pct.cutoff * sum(ind0)) {
				
				# Over-adjustment checking
				cat('Potential over-adjustment! Alternative FDR control will be used!\n')
				p.adj <- p.adj0
				k <- NULL
				rho <- NULL
				z.adj <- NULL
			}
		} 
		
		if (adaptive == 'Fisher') {
			# These cutoffs are used emprically
			fdr.cutoff <- 0.2
			
			ind0 <- p.adj0 <= fdr.cutoff
			ind <- p.adj <= fdr.cutoff
			
			n <- nrow(X)
			test.p <- fisher.test(matrix(c(sum(ind), sum(ind0), n - sum(ind), n - sum(ind0)), 2, 2), alternative = 'less')$p.value
			
			if (test.p <= 0.05) {
				# Over-adjustment checking
				cat('Potential over-adjustment! Alternative FDR control will be used!\n')
				p.adj <- p.adj0
				k <- NULL
				rho <- NULL
				z.adj <- NULL
			} 
		}
	}
	
	cat("Done!\n")
	
	return(list(p.adj = p.adj,  p.unadj = test.obs$p.value, z.adj = z.adj, z.unadj = z.obs, k = k, rho = rho))
}


# Compute tree-based FDR control	
StructFDR <- function (X, Y, D, test.func, perm.func, eff.sign = TRUE,  B = 20,  q.cutoff = 0.5, alpha = 1,
		adaptive = c('Fisher', 'Overlap'), alt.FDR = c('BH', 'Permutation'), ...) {
	
	adaptive <- match.arg(adaptive)
	alt.FDR <- match.arg(alt.FDR)
	
	# Make sure the rows of X and tree tips are labeled to avoid error
	if (is.null(rownames(X)) | is.null(colnames(D))) {
		warning('Both the data matrix and the distance matrix should have labels (rownames) to avoid potential errors!\n')
	} else {
		if (sum(!(rownames(X) %in% colnames(D)))){
			stop('Some features in the data matrix are not in the distance matrix! Please check!\n')
		} else {
			if (sum(!(colnames(D) %in% rownames(X)))) {
				warning('The distance  matrix has more features than the data matrix! \n')
			} 
		}
	}
	
	D <- D ^ alpha
	
	if (!is.null(rownames(X)) & !is.null(colnames(D))) {
		D <- D[rownames(X), rownames(X)]
	}
	
	cat('Test on original data sets  ...\n')
	test.obs <- test.func(X, Y, ...)
	
	
	if (!is.list(test.obs) | !all(c("e.sign", "p.value") %in% 
					names(test.obs))) {
		stop("test.func should return a list with names e.sign and p.valueif z.transform=TRUE! Please check!\n")
	}
	
	null.ind <- test.obs$p.value >= quantile(test.obs$p.value, 1 - q.cutoff)
	
	z.obs <- Ztransform(test.obs$p.value, test.obs$e.sign, eff.sign)	
	
	
	cat('Test on permuted data sets  ...\n')
	z.perm <- z.perm2 <- matrix(NA, nrow(X), B)	
	for (i in 1:B) {
		perm.obj <- perm.func(X, Y, ...)
		if (!is.list(perm.obj) | !all(c("X", "Y") %in% names(perm.obj))) {
			stop("perm.func should return a list with names X and Y! Please check!\n")
		}
		X.perm <- perm.obj$X
		Y.perm <- perm.obj$Y
		test.perm <- test.func(X.perm, Y.perm, ...)
		
		z.perm[, i] <- z.perm2[, i] <- Ztransform(test.perm$p.value, test.perm$e.sign, eff.sign)
		
		z.perm2[!null.ind, i] <- z.obs[!null.ind]
	}
	
	if (alt.FDR == 'Permutation') {
		cat('Perform ordinary permutation-based FDR control ...\n')
		if (eff.sign == TRUE) {
			p.adj0 <- PermFDR(abs(z.obs), abs(z.perm), rep(TRUE, length(z.obs)))
		} else {
			p.adj0 <- PermFDR(z.obs, z.perm, rep(TRUE, length(z.obs)))
		}
	}
	
	if (alt.FDR == 'BH') {
		cat('Perform ordinary BH-based FDR control ...\n')
		p.adj0 <- p.adjust(test.obs$p.value, 'fdr')
		
	}
	
	cat("Estimating hyperparameter ... \n")
	error <- try(obj <- EstHyper(y = z.obs, D = D))
	if (inherits(error, "try-error")) {
		cat('Hyperparameter estimation failed! Ordinary permutation-based FDR control will be used!\n')
		# p.adj <- p.adjust(test.obs$p.value,'fdr')
		p.adj <- p.adj0
		k <- NULL
		rho <- NULL
		z.adj <- NULL
	} else {
		cat("Structure-based adjustment ...\n")
		k <- obj[1]
		rho <- obj[2]
		mu <- obj[3]
		V <- exp(-1 * rho * D)
		z.adj <- stat.o <- AdjStats(y = z.obs, V = V, k = k, mu = mu)[, 1]
		stat.p <- AdjStats(y = z.perm2, V = V, k = k, mu = mu)
		
		if (eff.sign == TRUE) {
			stat.o <- abs(stat.o)
			stat.p <- abs(stat.p)
		}
		
		p.adj <- PermFDR(stat.o, stat.p, null.ind)			
		
		# Power loss check
		if (adaptive == 'Overlap') {
			fdr.cutoff <- 0.2
			pct.cutoff <- 0.5
			
			ind0 <- p.adj0 <= fdr.cutoff
			ind <- p.adj <= fdr.cutoff
			
			if (sum(p.adj[ind0] <= fdr.cutoff) <  pct.cutoff * sum(ind0)) {
				
				# Over-adjustment checking
				cat('Potential over-adjustment! Alternative FDR control will be used!\n')
				p.adj <- p.adj0
				k <- NULL
				rho <- NULL
				z.adj <- NULL
			}
		} 
		
		if (adaptive == 'Fisher') {
			# These cutoffs are used emprically
			fdr.cutoff <- 0.2
			
			ind0 <- p.adj0 <= fdr.cutoff
			ind <- p.adj <= fdr.cutoff
			
			n <- nrow(X)
			test.p <- fisher.test(matrix(c(sum(ind), sum(ind0), n - sum(ind), n - sum(ind0)), 2, 2), alternative = 'less')$p.value
			
			if (test.p <= 0.05) {
				# Over-adjustment checking
				cat('Potential over-adjustment! Alternative FDR control will be used!\n')
				p.adj <- p.adj0
				k <- NULL
				rho <- NULL
				z.adj <- NULL
			} 
		}
		
	}
	
	cat("Done!\n")
	
	return(list(p.adj = p.adj,  p.unadj = test.obs$p.value, z.adj = z.adj, z.unadj = z.obs, k = k, rho = rho))
}


GMPR <- function (comm, intersect.no=4, ct.min=1, verbose=FALSE) {
	# Computes the GMPR size factor
	#
	# Args:
	#   comm: a matrix of counts, row - features (OTUs, genes, etc) , column - sample
	#   intersect.no: the minimum number of shared features between sample pair, where the ratio is calculated
	#   ct.min: the minimum number of counts required to calculate ratios
	
	#
	# Returns:
	#   a list that contains:
	#      gmpr: the GMPR size factors for all samples; Samples with distinct sets of features will be output as NA.
	#      nss:   number of samples with significant sharing (> intersect.no) including itself
	
	# mask counts < ct.min
	comm[comm < ct.min] <- 0
	
	if (is.null(colnames(comm))) {
		colnames(comm) <- paste0('S', 1:ncol(comm))
	}
	
	if (verbose) {
		cat('Begin GMPR size factor calculation ...\n')
	}
	
	
	comm.no <- numeric(ncol(comm))
	gmpr <- sapply(1:ncol(comm),  function(i) {		
				if (verbose) {
					if (i %% 50 == 0) {
						cat(i, '\n')
					}
				}

				x <- comm[, i]
				# Compute the pairwise ratio
				pr <- x / comm
				# Handling of the NA, NaN, Inf
				pr[is.nan(pr) | !is.finite(pr) | pr == 0] <- NA
				# Counting the number of non-NA, NaN, Inf
				incl.no <- colSums(!is.na(pr))		
				# Calculate the median of PR
				pr.median <- colMedians(pr, na.rm=TRUE)
				# Record the number of samples used for calculating the GMPR
				comm.no[i] <<- sum(incl.no >= intersect.no)
				# Geometric mean of PR median
				if (comm.no[i] > 1) {
					return(exp(mean(log(pr.median[incl.no >= intersect.no]))))
				} else {
					return(NA)
				}
			}
	)
	
	if (sum(is.na(gmpr))) {
		warning(paste0('The following samples\n ', paste(colnames(comm)[is.na(gmpr)], collapse='\n'), 
						'\ndo not share at least ', intersect.no, ' common taxa with the rest samples! ',
						'For these samples, their size factors are set to be NA! \n', 
						'You may consider removing these samples since they are potentially outliers or negative controls!\n',
						'You may also consider decreasing the minimum number of intersecting taxa and rerun the procedure!\n'))
	}
	
	if (verbose) {
		cat('Completed!\n')
		cat('Please watch for the samples with limited sharing with other samples based on NSS! They may be outliers! \n')
	}

	attr(gmpr, 'NSS') <- comm.no
	names(gmpr) <- colnames(comm)
	return(gmpr)
}

# Data simulation 
SimulateData <- function (nCases = 50, nControls = 50, nOTU = 400, nCluster = 20, depth = 10000,
		p.est, theta, scene = c('S1', 'S2', 'S3', 'S4', 'S5', 'S6'), signal.strength = 4, 
		otu.no.min = 40, otu.no.max = 80, zero.pct = 0,  balanced = FALSE) {
	# Input
	# nCases, nControls: number of cases and controls
	# nCluster: the number of clusters. nClusters=20, 20, 100, 20, 20 for the five scenarios
	# nOTU: number of OTUs simulated.  
	# depth: average library sizes/sequencing depth
	# p.est, theta: the parameters of the Dirichlet distribution
	# scene: simulation scenarios. S1 , S2 , S3 , S4 , S5 denote five scenarios, respectively.
	# signal.strength: the strength of signal (related to the mean and sd of the effect sizes).
	#                  4, 4, 4, 2, 4 for the five scenarios.
	# zero.pct: the percentage of non-differential OTU - relevant for S1 and S2
	# Balanced: whether the fold change should be multiplied to both cases/control samples 
	
	scene <- match.arg(scene)
	
	n <- nCases + nControls        # Number of smaples
	
	###### Take  most abundant OTUs based on their proportions 
	otu.ids.o <- names(p.est)
	otu.ids <- names(sort(p.est, decreasing = T)[1 : nOTU])
	
	p.est <- p.est[otu.ids];
	p.est <- p.est / sum(p.est);
	
	gplus <- (1 - theta) / theta
	g.est <- p.est * gplus	
	
	# Initialize the effect size (log fold change)
	beta.true <- numeric(nOTU)
	
	# Generate a random coalescence tree
	tree <- rcoal(nOTU)
	D <- cophenetic(tree)
	
	obj <- pam(D, nCluster)
	clustering <- obj$clustering
	
	# Generate coefficients
	# Phylogeny-informative or Clade-consistent S1-S3
	# Phylogeny-noninformative or Clade-inconsistent S4-S5
	if (scene == 'S1') {
		# Identical coeffients, two clusters
		# Make sure the signal densites on the same level for each simulation (10-20%)
		iter <- 1
		while (iter <= 200){
			cluster.ind <- sample(1:nCluster, 2)
			cluster.size <- sum(table(clustering)[cluster.ind])
			if (cluster.size > otu.no.min & cluster.size < otu.no.max) break
			iter <- iter + 1
		}
		
		nOTU1 <- round(sum(clustering == cluster.ind[1]) * (1 - zero.pct))
		nOTU2 <- round(sum(clustering == cluster.ind[2]) * (1 - zero.pct))
		beta.true[sample(which(clustering == cluster.ind[1]), nOTU1)] <- rnorm(nOTU1, signal.strength, 0)
		beta.true[sample(which(clustering == cluster.ind[2]), nOTU2)] <- rnorm(nOTU2, -signal.strength, 0)
		
	}	
	
	if (scene == 'S2') {
		# Similar coeffiients, two clusters
		iter <- 1
		while (iter <= 200){
			cluster.ind <- sample(1:nCluster, 2)
			cluster.size <- sum(table(clustering)[cluster.ind])
			if (cluster.size > otu.no.min & cluster.size < otu.no.max) break
			iter <- iter + 1
		}

		nOTU1 <- round(sum(clustering == cluster.ind[1]) * (1 - zero.pct))
		nOTU2 <- round(sum(clustering == cluster.ind[2]) * (1 - zero.pct))
		beta.true[sample(which(clustering == cluster.ind[1]), nOTU1)] <- rnorm(nOTU1, signal.strength, 0.5 * signal.strength)
		beta.true[sample(which(clustering == cluster.ind[2]), nOTU2)] <- rnorm(nOTU2, -signal.strength, 0.5 * signal.strength)
	}		
	
	if (scene == 'S3') {
		# Identical coefficients, ten small clusters
		iter <- 1
		while (iter <= 200){
			cluster.ind <- sample(1:nCluster, 10)
			cluster.size <- sum(table(clustering)[cluster.ind])
			if (cluster.size > otu.no.min & cluster.size < otu.no.max) break
			iter <- iter + 1
		}
		
		for (i in 1:10) {
			signal.sign <- sample(c(-1, 1), 1)
			beta.true[which(clustering == cluster.ind[i])] <- rnorm(length(which(clustering == cluster.ind[i])), signal.sign * signal.strength, 0)
		}
	}		
	
	if (scene == 'S4') {
		# Coefficients of opposite signs, two clusters
		iter <- 1
		while (iter <= 200){
			cluster.ind <- sample(1:nCluster, 2)
			cluster.size <- sum(table(clustering)[cluster.ind])
			if (cluster.size > otu.no.min & cluster.size < otu.no.max) break
			iter <- iter + 1
		}
		beta.true[which(clustering == cluster.ind[1])] <- rnorm(length(which(clustering== cluster.ind[1])),  0, 1 * signal.strength)
		beta.true[which(clustering == cluster.ind[2])] <- rnorm(length(which(clustering== cluster.ind[2])),  0, 1 * signal.strength)
	}	
	
	if (scene == 'S5') {
		# Random coefficients with Random 10% OTUs
		beta.true[sample(1: nOTU, round(0.1 * nOTU))] <- rnorm(round(0.10 * nOTU), 0, 1 * signal.strength)
		
	}		
	
	# Generate the counts of cases and controls
	X1 <- matrix(0, nOTU, nCases, dimnames = list(rownames(D), paste0('Case', 1:nCases)))
	X2 <- matrix(0, nOTU, nControls, dimnames = list(rownames(D), paste0('Ctrl', 1:nControls)))
	
	# Generate the sequencing depth
	nSeq <- rnbinom(n, mu = depth, size = 25)
	
	# Generate the proportions 
	prop.case <- rdirichlet(nCases, g.est)
	prop.control <- rdirichlet(nControls, g.est)
	
	# Generate the counts
	for (i in 1: nCases) {
		X1[, i] <- rmultinom(1, nSeq[i], prob = prop.case[i, ])[, 1]
	}	
	
	for (i in 1: nControls) {
		X2[, i] <- rmultinom(1, nSeq[nCases + i], prob = prop.control[i, ])[, 1]
	}	
	
	if (balanced == FALSE) {
		# Apply fold changes to the case samples
		X1 <- X1 * exp(beta.true)
		X <- cbind(X1, X2)
	} else {
		# To create more balanced changes, consider multiplying the fold changes to both case and case samples
		beta.true1 <- beta.true2 <- abs(beta.true)
		beta.true1[beta.true < 0] <- 0
		beta.true2[beta.true > 0] <- 0
		X1 <- X1 * exp(beta.true1)
		X2 <- X2 * exp(beta.true2)
		X <- cbind(X1, X2)
	}

	# The data are normalized by the original library sizes
    # Comment: in practice, you need to estimate the library sizes such as using our recently developed GMPR method
    # Here we use the 'ideal' library sizes to control the effects on the type I error and power due to inaccurate estimation of the library sizes

    size.factor <- nSeq
    X <- t(t(X) / size.factor)
	y <- c(rep(1, nCases), rep(0, nControls))

	return(list(y = y, X = X, beta.true = beta.true, D = D, tree = tree, clustering = clustering))	
}			   				   


MicrobiomeSeqTreeFDR <- function (otu.tab, tree, meta.dat, grp.name, adj.name=NULL, raw.count=FALSE, B=100, ...) {

	if (raw.count) {
		size.factor <- GMPR(otu.tab)
		X <- otu.tab
		X <- X / size.factor
		X <- sqrt(X)
	} else {
		X <- otu.tab
	}
	row.names <- rownames(X)
	
	if (is.null(adj.name)) {
		X <- t(resid(lm(as.formula(paste('t(X) ~ 1')), meta.dat)))
	} else {
		X <- t(resid(lm(as.formula(paste('t(X) ~ ', paste(adj.name, collapse='+'))), meta.dat)))
	}
	rownames(X) <- row.names
	# Prepare model matrix
	n <- ncol(X)
	I <- diag(n)
	if (is.null(adj.name)) {
		M0 <- model.matrix(~ 1, meta.dat)
	} else {
		df0 <- meta.dat[, c(adj.name), drop=F]
		M0 <- model.matrix( ~., df0)
	}
	
	df1 <- meta.dat[, c(adj.name, grp.name), drop=F]
	M1 <- model.matrix( ~., df1)
	
	# QR decompostion
	qrX0 <- qr(M0, tol = 1e-07)
	Q0 <- qr.Q(qrX0)
	Q0 <- Q0[, 1:qrX0$rank, drop=FALSE]
	
	qrX1 <- qr(M1, tol = 1e-07)
	Q1 <- qr.Q(qrX1)
	Q1 <- Q1[, 1:qrX1$rank, drop=FALSE]
	
	test.func <- function (X, Y, Q1, Q0) {
		# Y not used
		TSS <- rowSums(X^2)
		MSS1 <- rowSums((X %*% Q1)^2)
		MSS0 <- rowSums((X %*% Q0)^2)  # Not necessary, it's zero
		F0 <- (MSS1 - MSS0) /  (TSS - MSS1) 
		return(list(p.value=pf(F0, qrX1$rank - qrX0$rank, n - qrX1$rank, lower.tail=FALSE), e.sign=NULL))
	}
	
	perm.func <- function (X, Y, Q1, Q0) {
		return(list(X=X[, sample(n)], Y=Y))
	}
	
	obj <- TreeFDR(X, NULL, tree, test.func, perm.func, eff.sign=FALSE, alt.FDR = 'Permutation', B=B, Q1=Q1, Q0=Q0, ...)

	return(obj)
}



