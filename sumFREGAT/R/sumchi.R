# sumFREGAT (2017-2018) Gulnara R. Svishcheva & Nadezhda M. Belonogova, ICG SB RAS

'sumchi' <- function (score.file, gene.file, genes = 'all', cor.path = 'cor/', approximation = TRUE,
anno.type = '', method = 'kuonen', acc = 1e-8, lim = 1e+6, write.file = FALSE, quiet = FALSE) {

	my.args <- c(as.list(environment()), weights.function = NULL,
		user.weights = FALSE, gen.var.weights = 'none', prob = NA, phred = NA)
	my.args$beta.par <- c(1, 1) 
	do.call(SKAT.int, my.args)

}