# sumFREGAT (2017-2022) Gulnara R. Svishcheva & Nadezhda M. Belonogova, ICG SB RAS
sumstat.BT <- function(obj) {

	with(obj, with(df, { # Z, U, w, p.sum
		chi2 <- sum(w * Z) ^ 2
		KKK <- sum(t(U * w) * w) 
		chi2 <- chi2/KKK
		pBurden <- pchisq(chi2, 1, lower.tail = FALSE)
		betaBT <- sum(w * Z)/KKK * length(w)
		se.beta <- sqrt((betaBT ^ 2) / chi2)
		return(c(pBurden, betaBT, se.beta))
	}))

}

'BT' <- function (score.file, gene.file, genes = 'all', cor.path = 'cor/',
anno.type = '', beta.par = c(1, 25), weights.function = NULL,
user.weights = FALSE, write.file = FALSE, quiet = FALSE) {

	do.call(BT.int, c(as.list(environment()), prob = NA, phred = NA))

}

BT.int <- function (score.file, gene.file, genes = 'all', cor.path = 'cor/',
anno.type = '', beta.par = c(1, 25), weights.function = NULL,
user.weights = FALSE, write.file = FALSE, quiet = FALSE, prob = NA, phred) {

############ COMMON CHECKS

tmp <- check.input(score.file, cor.path, gene.file, genes)
for (i in 1:length(tmp)) assign(names(tmp)[i], tmp[[i]])

############ SPECIFIC CHECKS

if (!is.na(prob)) user.weights <- prob
tmp <- check.weights(weights.function, beta.par)
for (i in 1:length(tmp)) assign(names(tmp)[i], tmp[[i]])

check.list <- get.check.list('BT', score.file, anno.type, user.weights, gen.var.weights, fweights)

############ ANALYSIS

genewise(score.file, gene.file, gf, anno.type, cor.path, cor.file.ext, check.list, write.file, NULL, ncl = 5, c('beta', 'se.beta'), gen.var.weights, fweights, quiet = quiet, phred = phred, test = 'BT')

}

if (getRversion() >= "2.15.1") utils::globalVariables(c('cor.file.ext', 'fweights', 'gen.var.weights', 'gf')) 