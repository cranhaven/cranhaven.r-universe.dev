# sumFREGAT (2017-2022) Gulnara R. Svishcheva & Nadezhda M. Belonogova, ICG SB RAS

sumstat.SKAT <- function(obj) {
	
	with(obj, with(df, {# Z, U, w, method, acc, lim

		Q <- sum((w * Z) ^ 2) # weighting Z-score statistic
		KKK <- t(U * w) * w # kernel matrix
		m1 <- length(Z)
		
		### New
			C05 <- suppressWarnings(chol(KKK, pivot = TRUE, tol = 1e-7))  ### KKK = t(C05) %*% C05    
			ran  <- attr(C05, "rank")
			if (ran < nrow(KKK)) {
				C05[-(1:ran), -(1:ran)] <- 0
				oo   <- order(attr(C05, "pivot"))
				Z1   <- as.matrix(C05[,oo])### Right Chol-matrix
				vec0 <- rowSums(Z1^2)
				Z1   <- Z1[which(vec0 > 1e-7) , ]
				KKK <- tcrossprod(Z1)                ### Z1 %*% t(Z1)	
			}
			### end new
		
		if (method != 'hybrid') {
			ev <- eigen(KKK, symmetric = TRUE, only.values = TRUE)$values
			#ev <- eig[1:ran]  # ev <- eig[eig > 1e-6 * eig[1]]
			if (method == 'kuonen') {
				#p <- pchisqsum(Q, rep(1, length(ev)), ev, lower.tail = F, method = 'sad') 
				p <- pchisqsum(Q, rep(1, ran), ev, lower.tail = F, method = 'sad') 

			} else { #if (method == 'davies') {
				p <- davies(Q, ev, acc = acc, lim = lim)$Qq
			} 
		} else { #if (method == 'hybrid') {
			lam <- svd(KKK, nu = 0, nv = 0)$d 	#lam <- lam[lam > 1e-6 * lam[1]]
			p <- KAT.pval(Q, lam)
		}
		pSKAT <-max(min(p, 1), 0)
		##new end 

		if (exists('p.sum')) {  ### large gene approx
			pSKAT <-  ACATO(c(pSKAT, p.sum))
		}
		return(pSKAT)
	}))

}

'SKAT' <- function (score.file, gene.file, genes = 'all', cor.path = 'cor/', approximation = TRUE, anno.type = '', beta.par = c(1, 25),
weights.function = NULL, user.weights = FALSE, gen.var.weights = 'se.beta', method = 'kuonen', acc = 1e-8, lim = 1e+6, rho = FALSE,
p.threshold = 0.8, write.file = FALSE, quiet = FALSE) {

	do.call(SKAT.int, c(as.list(environment()), prob = NA, phred = NA))

}

SKAT.int <- function (score.file, gene.file, genes = 'all', cor.path = 'cor/', approximation = TRUE, anno.type = '', beta.par = c(1, 25),
weights.function = NULL, user.weights = FALSE, gen.var.weights = 'se.beta', method = 'kuonen', acc = 1e-8, lim = 1e+6, rho = FALSE,
p.threshold = 0.8, write.file = FALSE, quiet = FALSE, prob = NA, phred) {

############ COMMON CHECKS

tmp <- check.input(score.file, cor.path, gene.file, genes)
for (i in 1:length(tmp)) assign(names(tmp)[i], tmp[[i]])

############ SPECIFIC CHECKS

tmp <- check.weights(weights.function, beta.par, gen.var.weights)
for (i in 1:length(tmp)) assign(names(tmp)[i], tmp[[i]])

tmp <- check.spec.SKAT(method, rho)
for (i in 1:length(tmp)) assign(names(tmp)[i], tmp[[i]])
test <- 'SKAT'
if (rho) test <- 'SKATO'

if (!is.na(prob)) user.weights <- prob
check.list <- get.check.list('SKAT', score.file, anno.type, user.weights, gen.var.weights, fweights, rho)

############ ANALYSIS

obj0 <- sapply(c('method', 'acc', 'lim', 'rhos', 'p.threshold'),
	function(x) get(x), simplify = FALSE, USE.NAMES = TRUE)

genewise(score.file, gene.file, gf, anno.type, cor.path, cor.file.ext, check.list, write.file, obj0, ncl = 3, NULL, gen.var.weights, fweights, quiet = quiet, phred = phred, approximation = approximation, test = test)

}


