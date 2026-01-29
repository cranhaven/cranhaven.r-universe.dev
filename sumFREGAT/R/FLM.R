# sumFREGAT (2017-2022) Gulnara R. Svishcheva & Nadezhda M. Belonogova, ICG SB RAS

"%^%" <- function(U, k) {
	UUU <- eigen(U, symmetric = TRUE)  # UUU = Uvec %*% diag(Uval) %*% t(Uvec)
	Uval <- UUU$val; 	Uvec <- UUU$vec
	Uvec <- Uvec[, Uval > 1e-7]
	Uval <- Uval[Uval > 1e-7]
	Uvec %*% (t(Uvec) * (Uval ^ k))   #	Uvec %*% (diag(Uval ^ k) %*% t(Uvec))
}

sumstat.FLM <- function(obj) {

	with(obj, with(df, { # Z, POS, w, U, n, m, k, basis, model, p.sum

		B     <- eval.basis(POS, basis)       ### as matrix (m x Kb)
		WB    <- B * w
		Mat   <- crossprod(WB, (U %*% WB))    ### t(WB) %*% (U %*% WB)
		rnk <- qr(Mat, tol = 1e-3)$rank
		if (rnk < k) {
			return(c(NA, model))
		}

		BUB_1 <- Mat %^% -1

		BZstat <- crossprod(WB, Z)            ### as.vector(t(WB) %*% Z.2)
		RSS    <- n - sum(BZstat * (BUB_1 %*% BZstat))

		Fstat <- ((n - k) / k) * (n - RSS) / RSS   # F-statistic
		pFLM <- pf(Fstat, k, n - k, lower.tail = FALSE)

		if (exists('p.sum')) {  ### large gene approx
			pFLM <-  ACATO(c(pFLM, p.sum))
		}
		return(c(pFLM, model))

	}))

}


'FLM' <- function (score.file, gene.file, genes = 'all', cor.path = 'cor/', approximation = TRUE,
anno.type = '', n, beta.par = c(1, 1), weights.function = NULL, user.weights = FALSE,
basis.function = 'fourier', k = 25, order = 4, flip.genotypes = FALSE,
Fan = TRUE, reference.matrix = TRUE, fun = 'LH', write.file = FALSE, quiet = FALSE) {

	do.call(FLM.int, c(as.list(environment()), prob = NA, phred = NA))

}

FLM.int <- function (score.file, gene.file, genes = 'all', cor.path = 'cor/', approximation = TRUE,
anno.type = '', n, beta.par = c(1, 1), weights.function = NULL, user.weights = FALSE,
basis.function = 'fourier', k = 25, order = 4, flip.genotypes = FALSE,
Fan = TRUE, reference.matrix = TRUE, fun = 'LH', write.file = FALSE, quiet = FALSE, prob = NA, phred) {

############ COMMON CHECKS

tmp <- check.input(score.file, cor.path, gene.file, genes)
for (i in 1:length(tmp)) assign(names(tmp)[i], tmp[[i]])

############ SPECIFIC CHECKS

tmp <- check.weights(weights.function, beta.par)
for (i in 1:length(tmp)) assign(names(tmp)[i], tmp[[i]])

tmp <- check.spec.FLM(basis.function, k, order)
for (i in 1:length(tmp)) assign(names(tmp)[i], tmp[[i]])

if (!missing(n)) n <- n - 1
if (!is.na(prob)) user.weights <- prob
check.list <- get.check.list('FLM', score.file, anno.type, user.weights, gen.var.weights, fweights, n = n, flip.genotypes = flip.genotypes)

############ ANALYSIS

obj0 <- sapply(c('k', 'basis', 'model'),
	function(x) get(x), simplify = FALSE, USE.NAMES = TRUE)

genewise(score.file, gene.file, gf, anno.type, cor.path, cor.file.ext, check.list, write.file, obj0, ncl = 4, 'model',
	gen.var.weights, fweights, reference.matrix, fun, n, Fan, flip.genotypes, quiet = quiet, phred = phred, approximation = approximation,
	test = 'FLM')

}