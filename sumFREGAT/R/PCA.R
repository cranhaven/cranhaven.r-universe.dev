# sumFREGAT (2017-2022) Gulnara R. Svishcheva & Nadezhda M. Belonogova, ICG SB RAS

sumstat.PCA <- function(obj) {

	with(obj, with(df, { # Z, w, U, n, var.fraction, p.sum
		m1  <- length(Z)         ### length of gene
		WZ  <- as.vector(w * Z) * sqrt(n)
		WUW <- as.matrix(t(U * w) * w) * n
		pPCA <- numberPCA(WZ, WUW, n, var.fraction) ### function output: c(minP, minM, prop.variance[minM])
		
		if (exists('p.sum')) {  ### large gene approx
			pPCA[1] <- ACATO(c(pPCA[1], p.sum))
		}
		return(pPCA)
	}))

}

numberPCA <- function(WZ, WUW, n, var.fraction) {
	pCA <- PC(WUW, var.fraction)                           ### n = N - 1 
#	CPV <- pCA$importance                    ### Cumulative Proportion of Variance (CPV)
#	M   <- min(which(CPV >= var.fraction))   ### components for which Explained variance fraction is about 85%
#	values <- pCA$eig.values
#	prop.var <- values / sum(values)
	#browser()
#	BBB <- pCA$scores[,1:M]                  ### the truncated matrix of eigen.vectors  as.matrix
#	GY  <- t(BBB) %*% WZ                     ### crossprod (BBB, WZ) as.vector
#	CC  <- pCA$eig.values[1:M]               ### CC <-  as.matrix(t(BBB) %*% (WUW %*% BBB))
#	m <- min(pCA$rank, M)                  ### m=qr(BBB)$rank 
#	RSS   <-  n - sum(GY^2 / CC)             ### RSS <- (n - sum(GY * as.vector(solve(CC , GY))))
	GY  <- t(pCA$scores) %*% WZ
	m <- pCA$M 
	RSS   <-  n - sum(GY^2 / pCA$eig.values)
	Fstat <- ((n - m) / m) * (n - RSS) / RSS    # F-statistic
	p <- pf(Fstat, m, n - m, lower.tail = FALSE)
	#browser()
	c(p, m, pCA$CPV)
}

PC <- function(X, var.fraction) {
	eX1 <- eigen(X, symmetric = TRUE)
	with (eX1, {
		rank <- sum(values > 1e-7)
		values <- values[1:rank]
		prop.var <- values / sum(values)
		cum.var <- cumsum(prop.var)
		M <- min(which(cum.var >= var.fraction))   ### components for which Explained variance fraction is about 85%
		M2 <- max(which(prop.var > 0.001))
		M <- min(M, M2)
	#browser()
		list(scores = as.matrix(vectors[,1:M]), CPV = cum.var[M], M = M, eig.values = values[1:M])
	})
}

'PCA' <- function (score.file, gene.file, genes = 'all', cor.path = 'cor/', approximation = TRUE,
anno.type = '', n, beta.par = c(1, 1), weights.function = NULL, user.weights = FALSE,
reference.matrix = TRUE, fun = 'LH', var.fraction = 0.85, write.file = FALSE, quiet = FALSE) {

	do.call(PCA.int, c(as.list(environment()), prob = NA, phred = NA))

}

PCA.int <- function (score.file, gene.file, genes = 'all', cor.path = 'cor/', approximation = TRUE,
anno.type = '', n, beta.par = c(1, 1), weights.function = NULL, user.weights = FALSE,
reference.matrix = TRUE, fun = 'LH', var.fraction = 0.85, write.file = FALSE, quiet = FALSE, prob = NA, phred) {

############ COMMON CHECKS

tmp <- check.input(score.file, cor.path, gene.file, genes)
for (i in 1:length(tmp)) assign(names(tmp)[i], tmp[[i]])

############ SPECIFIC CHECKS
if (var.fraction > .99) {
warning('var.fraction too large, reset to 0.99')
var.fraction <- .99
}

tmp <- check.weights(weights.function, beta.par)
for (i in 1:length(tmp)) assign(names(tmp)[i], tmp[[i]])

if (!missing(n)) n <- n - 1
if (!is.na(prob)) user.weights <- prob
check.list <- get.check.list('PCA', score.file, anno.type, user.weights, gen.var.weights, fweights, n = n)

############ ANALYSIS

genewise(score.file, gene.file, gf, anno.type, cor.path, cor.file.ext, check.list, write.file, obj0 = list(var.fraction = var.fraction),
	ncl = 5, c('ncomponents', 'explained.variance.fraction'), gen.var.weights, fweights, reference.matrix, fun, n, 
	quiet = quiet, phred = phred, approximation = approximation, test = 'PCA')

}
