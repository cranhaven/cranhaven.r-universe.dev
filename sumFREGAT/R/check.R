# sumFREGAT (2017-2022) Gulnara R. Svishcheva & Nadezhda M. Belonogova, ICG SB RAS


check.sumstat <- function(obj, lgt, test = '') {
#browser()
	if (!'df' %in% names(obj)) {
		if ('p.sum' %in% names(obj)) {  ### approx & all pvals large
			return(c(0, obj$p.sum, rep(NA, lgt - 1)))
		}
		return(c(0, rep(NA, lgt)))
	}

	m <- dim(obj$df)[1]
#browser()
	if (m == 1) {
		obj$df$Z <- as.numeric(obj$df$Z)
		p <- pnorm(abs(obj$df$Z), lower.tail = FALSE) * 2
		if (test == 'BT') {
			obj$df$SE.Beta <- as.numeric(obj$df$SE.Beta)
			beta <- sign(obj$df$Z) * sqrt((obj$df$Z ^ 2) * (obj$df$SE.Beta ^ 2))
			return(c(0, p, beta, obj$df$SE.Beta))
		}
		if ('p.sum' %in% names(obj)) {  ###  approx & all but one large pvals
			p <-  ACATO(c(p, obj$p.sum))
		}
		return(c(0, p, rep(NA, lgt - 1)))
	}

	1

}

check.input <- function(score.file, cor.path, gene.file, genes) {
	if (missing(score.file)) stop("'score.file' is missing, with no default")
	if (!file.exists(score.file)) {
		score.file1 <- paste0(score.file, '.vcf.gz')
		if (file.exists(score.file1)) {
			score.file <- score.file1
		} else { stop(paste(score.file, '- No such file or directory')) }
	}

	if (missing(gene.file)) {
		gene.file <- system.file("testfiles/NCBI37.3.geneFile.txt.gz", package = "sumFREGAT")
	} else {
		if (gene.file %in% c('hg19', 'hg37')) gene.file <- system.file("testfiles/NCBI37.3.geneFile.txt.gz", package = "sumFREGAT")
		if (gene.file == 'hg38') gene.file <- system.file("testfiles/ensembl.hg38.txt", package = "sumFREGAT")
	}
	#if (gene.file == 'old') gene.file <- system.file("testfiles/refFlat_hg19_6col.txt.gz", package = "sumFREGAT")

	gf <- read.table(gene.file)
	genes <- as.character(genes)
	#if (length(genes) == 1) {
		if (genes[1] != 'all') {
			chr.list <- paste0('chr', c(1:22, 'X', 'Y'))
			chr.list <- chr.list[chr.list %in% tolower(genes)]
	#		if (length(chr.list) > 0) {
				gf <- rbind(gf[gf$V3 %in% chr.list, ], gf[gf$V1 %in% genes, ])
	#		} else {
	#			gf <- gf[gf$V1 %in% genes, ]
	#		}
		}
	#}
	if (dim(gf)[1] == 0) stop("No genes found in gene.file")
	gf <- gf[, c(1, 3, 5:6)]

	if (cor.path == 'do not check cor.path') {
		return(sapply(c('score.file', 'gene.file', 'gf'),
		function(x) get(x), simplify = FALSE, USE.NAMES = TRUE))
	}

	if (missing(cor.path)) {
		cor.path <- ''
	} else if (cor.path != '') {
		if (substr(cor.path, nchar(cor.path), nchar(cor.path)) != '/') cor.path <- paste(cor.path, '/', sep = '')
	}
#browser()
	gf <- gf[!duplicated(gf[, 1]), ]
	i <- 1
	max.iter <- min(dim(gf)[1], 50)
	cor.file.ext <- NULL
	while (is.null(cor.file.ext) & i <= max.iter) {
		gene <- as.character(gf[i, 1])
		cor.file.ext <- detect.cor.file.ext(cor.path, gene)
		i <- i + 1
	}
	if (is.null(cor.file.ext)) stop ('Correlation files not found')

	sapply(c('score.file', 'gene.file', 'gf', 'cor.path', 'cor.file.ext'),
		function(x) get(x), simplify = FALSE, USE.NAMES = TRUE)

}

detect.cor.file.ext <- function(cor.path, gene) {
	for (cor.file.ext in c('.RDa', '.RData', '.cor', '.dat', '.txt', '')) {
		cor.file <- paste0(cor.path, gene, cor.file.ext)
		if (file.exists(cor.file)) return(cor.file.ext)
	}
	return(NULL)
}

get.check.list <- function(test, score.file, anno.type, user.weights = FALSE, gen.var.weights = FALSE, fweights = NULL, rho = FALSE, n = NULL, flip.genotypes = FALSE, mac.threshold = NA) {

	con <- file(score.file, "r")
	h <- c()
	while (TRUE) {
		line <- readLines(con, 1)
		if (grepl("##", line, fixed = TRUE)) {
			h <- paste(h, line)
		} else { break }
	}
	close(con)
	# h <- read.table(score.file, nrows = 10, comment.char = '', sep = '\n', as.is = T)

	if (anno.type[1] != '') {
		if (!grepl('ANNO', h)) stop ('Annotations not found in input file')
	}

check.list <- c()

	if (test %in% c('MLR', 'FLM', 'PCA')) {
		if (missing(n) & !'N' %in% check.list) stop ("Sample size(s) must be set for this type of analysis")
		#if (!missing(n) & 'N' %in% check.list) warning ("Sample sizes column found in input file, 'n' argument ignored")
		if (test == 'FLM') check.list <- c(check.list, 'POS')
	}

	if (rho | gen.var.weights == 'se.beta' | test %in% c('MLR', 'FLM', 'PCA')) {
		if (!grepl('SE.Beta', h)) stop ('Effect sizes (beta) info not found in input file')
		check.list <- c(check.list, 'SE.Beta')
	}

	if (!is.null(fweights) | gen.var.weights == 'af' | (test == 'FLM' & flip.genotypes) | !is.na(mac.threshold)) {
		if (!grepl('Frequency', h, ignore.case = TRUE)) {
			if (!is.na(mac.threshold)) stop ('Allele frequencies not found in input file, mac.threshold cannot be used')
			stop ('Allele frequencies not found in input file, consider changing "beta.par" and "gen.var.weights" parameters"')
		}
		check.list <- c(check.list, 'AF')
	}

	check.list <- c(check.list, 'Z')

	if (length(user.weights) > 1 | (length(user.weights) == 1 & !(is.character(user.weights) | is.logical(user.weights)))) stop("Wrong 'user.weights' input, please provide single logical or character name corresponding to the input data used")
	if (is.logical(user.weights)) {
		if (user.weights) {
			if (!grepl('Weights', h)) stop ('User weights not found in input file')
			check.list <- c(check.list, 'W')
		}
	} else { check.list <- c(check.list, user.weights) }

	check.list
}

check.method <- function(method) {
	method <- tolower(method)
	method <- match.arg(method, c('davies', 'kuonen', 'hybrid'))
	method
}

check.weights <- function(weights, beta.par, gen.var.weights) {

	if (!missing(gen.var.weights)) {
		if (gen.var.weights == FALSE) gen.var.weights <- 'none'
		gen.var.weights <- tolower(gen.var.weights)
		gen.var.weights <- match.arg(gen.var.weights, c('se.beta', 'af', 'none'))
	} else {
		gen.var.weights <- 'se.beta'
	}
	if (is.null(weights)) {
#		fweights <- function(maf, beta.par) ifelse(maf > 0, dbeta(maf, beta.par[1], beta.par[2]), 0)
		if (all(beta.par == c (1, 1))) {
			fweights <- NULL
		} else {
			fweights <- function(maf, a = as.numeric(beta.par[1]), b = as.numeric(beta.par[2])) ifelse(maf > 0, dbeta(maf, a, b), 0)
		}
	} else {
		if (is.function(weights)) {
			fweights <- weights
		} else {
			stop("'weights.function' should be a function of MAF")
		}
	}
	sapply(c('fweights', 'gen.var.weights'),
		function(x) get(x), simplify = FALSE, USE.NAMES = TRUE)

}

check.basis <- function(value, name, base = 'none', order) {
	if (base == 'bspline') {
		if (value < order) {
			value <- order
			warning(paste("bspline basis cannot be less than order, set to", value))
		}
	}
	if (value < 1) {
		value <- 1
		warning(paste(name, "cannot be less than 1, set to", value))
	}
	if (base == 'fourier' & (value + 1) %% 2 != 0) {
		if (ceiling(value) %% 2 != 0) { value <- ceiling(value)
		} else if (floor(value) %% 2 != 0) { value <- floor(value)
		} else value <- value - 1
		warning(paste("fourier basis should be an odd integer, set to", value))
	}
	if (value %% 1 != 0) {
		value <- round(value)
		warning(paste(name, "should be an integer number, set to", value))
	}
	value
}

check.rho <- function(rho) {
	if (length(rho) == 1) {
		if (is.logical(rho) & rho) {
			#rho <- (0:10)/10
			rho <- c(0, 0.1^2, 0.2^2, 0.3^2, 0.4^2,0.5^2, 0.5, 1)
		}
	} else {
		for (i in 1:length(rho)) {
			if (rho[i] < 0 || rho[i] > 1) {
				stop("rho should be >= 0 and <= 1")
			}
		}
	}
	rho
}

check.spec.SKAT <- function(method, rho) {

	method <- check.method(method)
	rhos <- NULL
	if (length(rho) > 1 | (length(rho) == 1 & rho)) {
		rhos <- check.rho(rho)
		rho <- TRUE
	} else {
		rho <- FALSE
	}
	sapply(c('method', 'rhos', 'rho'),
		function(x) get(x), simplify = FALSE, USE.NAMES = TRUE)

}

check.spec.FLM <- function(basis.function, k, order) {

	basis.function <- match.arg(tolower(basis.function), c('fourier', 'bspline'))

	if (basis.function == 'bspline') order <- check.basis(order, 'order')
	k <- check.basis(k, 'k', basis.function, order)

	if (basis.function == 'bspline') basis = create.bspline.basis(norder = order, nbasis = k)
	else basis <- create.fourier.basis(c(0, 1), nbasis = k)

	model <- paste0(toupper(substr(basis.function, 1, 1)), k)
	# all fourier bases are odd

	sapply(c('k', 'model', 'basis'),
			function(x) get(x), simplify = FALSE, USE.NAMES = TRUE)

}
