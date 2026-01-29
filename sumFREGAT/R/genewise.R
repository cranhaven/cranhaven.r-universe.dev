# sumFREGAT (2017-2022) Gulnara R. Svishcheva & Nadezhda M. Belonogova, ICG SB RAS

genewise <- function(score.file, gene.file, gf, anno.type, cor.path, cor.file.ext, check.list, write.file, obj0 = NULL, ncl = 3, cn = NULL, gen.var.weights = FALSE, fweights = NULL, reference.matrix = FALSE, fun, n = NULL, mac.threshold = NA, staar = FALSE, Fan = FALSE, flip.genotypes = FALSE, quiet, phred, approximation = FALSE, test) {

	ngenes <- dim(gf)[1]
	gf <- cbind(gf, matrix(NA, nrow = ngenes, ncol = ncl))
	colnames(gf) <- c('gene', 'chrom', 'start', 'end', 'markers', 'filtered.markers', 'pvalue', cn)
	#if (write.file != FALSE) write.table(t(c(colnames(gf), 'gene.user.self', 'gene.sys.self', 'gene.elapsed', 'gene.user.child', 'gene.sys.child')), file = write.file, col.names = FALSE, row.names = FALSE, quote = FALSE)
	if (write.file != FALSE) write.table(t(colnames(gf)), file = write.file, col.names = FALSE, row.names = FALSE, quote = FALSE)
	nc <- dim(gf)[2]
	nc2 <- nc - 6
	sumstat.region <- as.function(get(paste('sumstat', test, sep = '.')))

	for (i in 1:ngenes) {

		#t0 <- proc.time()

		gene <- as.character(gf[i, 1])

		obj <- c(obj0, get.sumstat(score.file, gene.file, gene, anno.type, cor.path, cor.file.ext, check.list,
			reference.matrix, gen.var.weights, fweights, mac.threshold, n, staar, Fan, flip.genotypes, fun, k = obj0$k, quiet, phred, approximation, test))

		out <- check.sumstat(obj, nc2, test)
		if (out[1]) {
			out <- sumstat.region(obj)
		} else {
			out <- out[-1]
		}
		gf[i, 5:nc] <- c(obj$m0, obj$m1, out)
		if (write.file != FALSE) write.table(gf[i, ], file = write.file, append = TRUE, col.names = FALSE, row.names = FALSE, quote = FALSE)
		gc()
		#t0 <- proc.time() - t0
		#if (write.file != FALSE) write.table(t(c(gf[i, ], t0)), file = write.file, append = TRUE, col.names = FALSE, row.names = FALSE, quote = FALSE)
	}

	gf[, 3:7] <- sapply(3:7, function(x) as.numeric(as.character(gf[, x])))
	if (test %in% c('BT', 'PCA')) gf[, 8:9] <- sapply(8:9, function(x) as.numeric(as.character(gf[, x])))
	gf

}
