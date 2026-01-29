# sumFREGAT (2021-2022) Gulnara R. Svishcheva & Nadezhda M. Belonogova, ICG SB RAS

sumSTAAR <- function(score.file, gene.file, genes = 'all', cor.path = 'cor/', tests = c('BT', 'SKAT', 'ACAT'), beta.par.matrix = rbind(c(1, 1), c(1, 25)), prob.causal = 'all', phred = TRUE, n = NA, mac.threshold = NA, approximation = TRUE, write.file = FALSE, staar.output = TRUE, quiet = FALSE) {

if (any(c('PCA', 'FLM') %in% tests) & is.na(n)) stop('n must be set for PCA/FLM analyses') 
if (length(beta.par.matrix) < 2 & any(c('SKAT', 'SKATO', 'PCA', 'FLM', 'ACAT') %in% tests)) stop ("Please set beta.par.matrix values")
beta.par.matrix <- as.matrix(beta.par.matrix)
if (all(dim(beta.par.matrix) == c(2, 1))) beta.par.matrix <- t(beta.par.matrix)

if (length(prob.causal) == 1) {
	if (!is.na(prob.causal)) {
		if (prob.causal == FALSE) prob.causal <- NA
		if (prob.causal == 'all' | prob.causal == TRUE) {
			if (missing(score.file)) stop("'score.file' is missing, with no default")
			if (!file.exists(score.file)) {
				score.file1 <- paste0(score.file, '.vcf.gz')
				if (file.exists(score.file1)) {
					score.file <- score.file1
				} else { stop(paste(score.file, '- No such file or directory')) }
			}
			con <- file(score.file, "r")
			h <- c()
			while (TRUE) {
				line <- readLines(con, 1)
				if (grepl("##", line, fixed = TRUE)) {
					h <- c(h, line)
				} else { break }
			}
			close(con)
			h <- grep('PROB', h, value = TRUE)
			if (length(h) == 0) stop('Vector(s) of probabilities not found in the score file')
			h <- gsub("##INFO=<ID=", "", h)
			h <- gsub("'>", "", h)
			prob.causal <- as.character(as.data.frame(strsplit(h, ",Number=1,Type=Float,Description='"))[1, ])
			print(paste0("Vector(s) ", paste(prob.causal, collapse = ', '), ' will be used as probabilities'))
		}
	}
}

beta.i.0 <- dim(beta.par.matrix)[1]

if (staar.output) {
	pval.all <- c()
} else {
	pval.tests <- c()
}

for (tt in tests) {

	my.args0 <- list(score.file = score.file, gene.file = gene.file, genes = genes, quiet = quiet)
	if (tt %in% c('sumchi', 'SKAT', 'SKATO')) {
		sumstat.function <- SKAT.int
		if (tt == 'sumchi') {
			my.args0 <- c(my.args0, gen.var.weights = 'none')
			my.args0$beta.par <- c(1, 1)
		}
		if (tt == 'SKATO') my.args0 <- c(my.args0, rho = TRUE)
	} else if (tt %in% c('PCA', 'FLM', 'BT', 'ACAT')) {
		sumstat.function <- as.function(get(paste0(tt, '.int'))) 
	} else {
		sumstat.function <- as.function(get(tt))
	}
	if (tt == 'ACAT') {
		my.args0 <- c(my.args0, gen.var.weights = 'af', mac.threshold = mac.threshold, n = n, staar = TRUE)
	} else {
		my.args0 <- c(my.args0, cor.path = cor.path)
	}
	if (tt %in% c('PCA', 'FLM')) my.args0 <- c(my.args0, n = n)
	if (tt %in% c('SKAT', 'SKATO', 'PCA', 'FLM')) my.args0 <- c(my.args0, approximation = approximation)

	beta.i <- beta.i.0
	use.beta <- TRUE
	if (tt %in% c('sumchi', 'simpleM', 'minp')) {
		beta.i <- 1
		use.beta <- FALSE
	}

	if (!staar.output) pval.weights.and.prob <- c()

	for (i in 1:beta.i) {
		my.args2 <- my.args0
		a1 <- a2 <- ''
		if (use.beta) {
			a1 <- beta.par.matrix[i, 1]
			a2 <- beta.par.matrix[i, 2]
			my.args2$beta.par <- c(a1, a2)
		}
		pval <- c()
		if (tt %in% c('sumchi', 'simpleM', 'minp')) {
			ncyc <- 1
		} else {
			ncyc <- ifelse(is.na(prob.causal[1]), 1, length(prob.causal) + 1)
		}
		for (a in 1:ncyc) {
			prob <- ifelse(a == 1, NA, prob.causal[a - 1])
			wf <- ifelse(write.file != FALSE, paste(tt, a1, a2, ifelse(a == 1, 'PROB0', prob.causal[a - 1]), write.file, sep = '.'), FALSE)
			my.args <- c(my.args2, write.file = wf)
			if (ncyc > 1) my.args <- c(my.args, phred = phred, prob = prob)
			res <- do.call('sumstat.function', my.args)
			pval <- cbind(pval, res$pvalue)
		}
		if (staar.output) {
			if (ncyc == 1) {
				names.tmp <- c('PROB0')
			} else {
				pval <- cbind(pval, sapply(1:dim(pval)[1], function(x) ACATO(pval[x, ]))) # combine by annotations
				names.tmp <- c('PROB0', prob.causal, 'STAAR')
			}
			colnames(pval) <- paste(tt, a1, a2, names.tmp, sep = '.')
			colnames(pval) <- gsub('...', '.', colnames(pval), fixed = TRUE)
			pval.all <- cbind(pval.all, pval)
		} else {
			pval.weights.and.prob <- cbind(pval.weights.and.prob, pval)
		}
	}
	if (!staar.output) {
		pval.tests <- cbind(pval.tests, sapply(1:dim(pval.weights.and.prob)[1], function(x) ACATO(pval.weights.and.prob[x, ])))
	}
}

if (staar.output) {
	v <- grepl('PROB0', colnames(pval.all))
	pval.all <- cbind(pval.all, sumSTAAR.ACAT_O = sapply(1:dim(pval.all)[1], function(x) ACATO(pval.all[x, v])))
	if (!is.na(prob.causal[1])) {
		v <- grepl('STAAR', colnames(pval.all)) # check that
		pval.all <- cbind(pval.all, sumSTAAR.STAAR_O = sapply(1:dim(pval.all)[1], function(x) ACATO(pval.all[x, v])))
	}
	pval.all <- as.data.frame(pval.all)
	pval.all <- cbind(gene = res$gene, pval.all)
} else {
	pval.all <- as.data.frame(cbind(pval.tests, sapply(1:dim(pval.tests)[1], function(x) ACATO(pval.tests[x, ]))))
	pval.all <- cbind(res$gene, pval.all)
	colnames(pval.all) <- c('gene', tests, 'sumSTAAR.STAAR_O')
}

if (write.file != FALSE) write.table(pval.all, file = write.file, row.names = FALSE, quote = FALSE)

as.data.frame(pval.all)

}
