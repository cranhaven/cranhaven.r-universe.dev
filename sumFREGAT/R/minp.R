# a wrapper to call functions of the 'GBJ' package

sumstat.minp <- function(obj) {
	#p <- minP_norestriction(test_stats = obj$df$Z, cor_mat = obj$U)$minP_pvalue
	p <- GBJ::minP(test_stats = obj$df$Z, cor_mat = obj$U)$minP_pvalue
	return(p)
}


minp <- function (score.file, gene.file, genes = 'all', cor.path = 'cor/',
anno.type = '', write.file = FALSE, quiet = FALSE) {

############ COMMON CHECKS

tmp <- check.input(score.file, cor.path, gene.file, genes)
for (i in 1:length(tmp)) assign(names(tmp)[i], tmp[[i]])

############ SPECIFIC CHECKS

check.list <- get.check.list('minP', score.file, anno.type)

############ ANALYSIS

genewise(score.file, gene.file, gf, anno.type, cor.path, cor.file.ext, check.list, write.file, NULL, ncl = 3, quiet = quiet, test = 'minp')

}

