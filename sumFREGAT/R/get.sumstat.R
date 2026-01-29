# sumFREGAT (2017-2022) Gulnara R. Svishcheva & Nadezhda M. Belonogova, ICG SB RAS

get.sumstat <- function(score.file, gene.file, gene, anno.type, cor.path, cor.file.ext, check.list, reference.matrix, gen.var.weights = FALSE,
fweights = NULL, mac.threshold = NA, n = NULL, staar = FALSE, Fan = FALSE, flip.genotypes, fun, k, quiet, phred, approximation, test) {

	uw <- check.list[length(check.list)]
	if (uw == 'Z') uw <- ''

	### read gene info
	df <- c()
	for (anno in anno.type) {
		df0 <- read.vcf.info(score.file, gene.file, gene, anno, uw, quiet)
		if (is.null(df0$Z)) next
		df0$sampleId <- NULL
		df <- rbind(df, as.data.frame(df0, stringsAsFactors = FALSE)) # POS, ID, AF, EAF, SE.Beta, Z, weights or prob
	}
	if (is.null(df)) {
		warning("No variants to analyze in gene ", gene, ", skipped")
		return(list(m0 = 0, m1 = 0))
	}
	m0 <- dim(df)[1]
	df[df == 'NA'] <- NA
	if (m0 == 1) return(list(m0 = m0, m1 = 1, df = df))

	### load/read the correlation matrix from file

	doU <- ifelse (test != 'ACAT', 1, 0)
	
	if (doU) {
		cor.file <- paste0(cor.path, gene, cor.file.ext)
		if (!file.exists(cor.file)) {
			warning("No correlation file for gene ", gene, ", skipped")
			return(list(m0 = m0, m1 = 0))
		}
		if (cor.file.ext %in% c('.RDa', '.RData')) {
			U <- get(load(cor.file))
		} else {
			if (requireNamespace("data.table", quietly = TRUE)) {
				U <- as.matrix(suppressWarnings(data.table::fread(cor.file)), rownames = 1)
			} else {
				U <- read.table(cor.file)
			}
		}
		if (length(U) == 1) {
			warning("No matching variants in correlation file for gene ", gene, ", skipped")
			return(list(m0 = m0, m1 = 0))
		}

		### subset

		v <- match(df$ID, rownames(U))
		if (sum(!is.na(v)) == 0) {
			warning("No matching variants in correlation file for gene ", gene, ", skipped")
			return(list(m0 = m0, m1 = 0))
		}
		df <- df[!is.na(v), ]
		if (sum(!is.na(v)) == 1) return(list(m0 = m0, m1 = 1, df = df))
		v <- v[!is.na(v)]
		U <- as.matrix(U[v, v])

		### remove NAs from U

		U.na <- is.na(U)
		v <- which(colSums(!U.na) <= 1)
		if (length(v) > 0) {
			U <- as.matrix(U[-v, -v])
			df <- df[-v, ]
		}
		if (length(U) > 0) {
			U.na <- which(is.na(U), arr.ind = TRUE)
			while(length(U.na) > 0) {
				v <- as.numeric(names(sort(-summary(as.factor(U.na), maxsum = dim(U)[1]))[1]))
				U <- U[-v, -v]
				df <- df[-v, ]
				U.na <- which(is.na(U), arr.ind = TRUE)
			}
		}

		if (length(U) == 0) {
			warning("No correlation values for gene ", gene, ", skipped")
			return(list(m0 = m0, m1 = 0))
		} else if (length(U) == 1) return(list(m0 = m0, m1 = 1, df = df))
gc()
	} # end U

	# compatibility, remove this later, as well as reading EAF from file
	if ('AF' %in% check.list) {
		if (!is.na(df[1, 'AF'])) {
			if (df[1, 'AF'] == '') {
				df[, 'AF'] <- df[, 'EAF']
				df$EAF <- NULL
			}
		}
	}

	# exclude NA by all items required

	for (i in check.list) { # can be: SE.Beta, AF, POS, Z, W or other character
		df[, i] <- as.numeric(df[, i])
		v <- !is.na(df[, i])
		if (sum(v) == 0) {
			warning("No ", i, " assigned for gene ", gene, ", skipped")
			return(list(m0 = m0, m1 = 0))
		}
		df <- df[v, ]
		if (doU) U <- as.matrix(U[v, v])
	}
	if (sum(v) == 1) return(list(m0 = m0, m1 = 1, df = df))

	# check AF

	if ('AF' %in% check.list) {

		### check whether AF are in range (0, 1)

		v <- sapply(1:length(df$AF), function(x) df$AF[x] >= 0 & df$AF[x] <= 1)
		if (sum(v) == 0) {
			warning("Allele frequencies are not in range 0..1 in gene ", gene, ", skipped")
			return(list(m0 = m0, m1 = 0))
		}
		if (sum(v) == 1) return(list(m0 = m0, m1 = 1, df = df))
		df <- df[v, ]
		if (doU) U <- as.matrix(U[v, v])

		### flip to minor for BT, SKAT

		if (any(df$AF > .5) & (test %in% c('BT', 'SKAT', 'SKATO') | (test == 'ACAT' & !is.na(mac.threshold)))) {#, 'MLR')) {
			v <- which(df$AF > .5)
			df$Z[v] <- -df$Z[v]
			df$AF[v] <- 1 - df$AF[v]
			if (doU) {
				for (i in v) { # reverse correlation for reverse coding
					U[i, ] <- -U[i, ]
					U[, i] <- -U[, i]
				}
			}
		}
	}

	if (test == 'ACAT') {
		if (!is.na(mac.threshold)) {
			df$is.rare <- round(df$AF * n * 2) <= mac.threshold
		} else {
			df$is.rare <- FALSE
		}
	n0 <- sum(df$is.rare)
	}

### exclude linearly dependent variants for MLR

	if (test == 'MLR' | (test == 'FLM' & Fan)) {
		v <- Indep.Variants(U)
		df <- df[v, ]
		if (length(v) == 1) return(list(m0 = m0, m1 = 1, df = df))
		U <- as.matrix(U[v, v])
	}

### manage weights

	if (test == 'SKATO') df$Pi <- rep(1, dim(df)[1]) # for SKAT-specific weights in SKATO
	# set ini weights
	if (uw == '') {
		df$w <- rep(1, dim(df)[1])
	} else {
		colnames(df)[colnames(df) == uw] <- 'w'
		if (uw != 'W') { # PROB case
			if (phred) {
				df$w <- 1 - 10^(-df$w/10)
			} else {
				if (any(df$w < 0 | df$w > 1)) warning ("Probabilities are not in range 0..1 for gene ", gene)
			}
			if (test == 'SKATO') df$Pi <- df$w
			if (test %in% c('SKAT', 'ACAT')) {
				df$w <- sqrt(df$w)
			}
			if (test == 'ACAT') {
				if (n0 > 0) {
					df$wb <- df$w # sqrt anno
				}
			}
		}
	}
	

	# with AF
	if (!is.null(fweights)) {
		df$w <- df$w * fweights(df$AF) # df$w already defined as user weights or 1
	}
	if (gen.var.weights == "se.beta") {
		df$w <- df$w / df$SE.Beta
	} else if (gen.var.weights == "af") { # add this
		df$w <- df$w * sqrt(2. * df$AF * (1. - df$AF))  #Beta-weighted genotype under HWE
	}
	if (staar) { # acat
		if (n0 > 0) { # burden weights
			if (uw == '') {
				df$wb <- df$w
			} else { # anno
				df$wb <- df$wb * df$w
			}
		}
		df$w <- df$w ^ 2
	}
######## approximation

	m1 <- dim(df)[1]

	if (approximation) {

		p.sum <- NA

		if (m1 >= 500){ #-------- for LARGE gene -------

			mean.w <- mean(df$w)
			pvalS  <- pnorm(abs((df$w/mean.w) * df$Z), lower.tail = FALSE) * 2	## convert Z to Pval
			is.small <- (pvalS < 0.8)  ### main threshold for pvalues
			n.small <- sum(is.small)
			if (n.small < m1){         ### There are 'small' and 'large' w.p.values
				p.sum <- mean(pvalS[!is.small])
				if (n.small == 0) {
					return(list(m0 = m0, m1 = m1, p.sum = p.sum))
				}
				df   <- df[is.small, ]
				U   <- U[is.small, is.small]
			}
		}
	}

	### FLM case
	
	if (test == 'FLM') {
		v <- !is.na(df$POS)
		if (sum(v) == 0) {
			warning("Positions are not assigned in gene ", gene, ", skipped")
			return(list(m0 = m0, m1 = 0))
		} else {
			v <- order(df$POS)
			df <- df[v, ]
			U <- as.matrix(U[v, v])
			if (sum(duplicated(df$POS)) > 0) {
				for (i in 1:dim(df)[1]) {
					df$POS[duplicated(df$POS)] <- df$POS[duplicated(df$POS)] + 1
					if (sum(duplicated(df$POS)) == 0) break
				}
			}
			if (max(df$POS) > min(df$POS)) {
				df$POS <- (df$POS - min(df$POS)) / (max(df$POS) - min(df$POS))
				# if (!Fan) df$POS <- (df$POS * (length(df$POS) - 1) + 1) / length(df$POS) 
			}
			if (max(df$POS) == min(df$POS)) df$POS <- .5
			if (flip.genotypes == TRUE) {
			#browser()
				flip <- sumstat.flipper(U, df$Z, df$AF)
				U <- flip$U
				df$Z <- flip$Z
			}
		}
		if (Urank(U) <= k) {
			warning("Gene ", gene, " skipped due to (near-)collinearity")
			return(list(m0 = m0, m1 = m1))
		}
	}

	if (test %in% c('FLM', 'PCA', 'MLR')) {
		if (reference.matrix & dim(U)[1] > 1) U <- reference_sample(U, df$Z, fun)
	}

	if (doU) {
		U[U == 1] <- 0.999
		U[U == -1] <- -0.999
		#diag(U) <- 1
	} else { U <- NULL }
	
	if (approximation) return(list(m0 = m0, m1 = m1, df = df, p.sum = p.sum, U = U, n = n))
	
	list(m0 = m0, m1 = m1, df = df, U = U, n = n)
	
}

read.vcf.info <- function(data, gene.file, gene, anno.type, uw, quiet) {
	if (getRversion() >= "3.2.0" & quiet == TRUE) {
		invisible(capture.output(invisible(capture.output(info <- seqminer::readVCFToListByGene(data, gene.file, gene, anno.type, c('ID', 'POS'), c('AF', 'EAF', 'Z', 'SE.Beta', uw), ''), type = 'output')),  type = 'message'))
	} else { info <- seqminer::readVCFToListByGene(data, gene.file, gene, anno.type, c('ID', 'POS'), c('AF', 'EAF', 'Z', 'SE.Beta',  uw), '') }
	info
}

Indep.Variants <- function(U) {
	q <- qr(U, tol = 1.e-7)
	q$pivot[seq(q$rank)]
}

sumstat.flipper <- function(U, Z, AF) {
	D1 <- sqrt(2. * AF*(1.-AF))
	for (j in 1:(length(Z)-1)) {
		A_B <- U[j,j+1]*D1[j]*D1[j+1] + (1.-2.*AF[j])*(1.-2.*AF[j+1])
		if (A_B < 0) {
			U[,j+1] <- -1.*U[,j+1]
			U[j+1,] <- -1.*U[j+1,]
			Z[j+1]  <- -1.*Z[j+1]
			AF[j+1] <-  1.-AF[j+1]
		}
	}
	list(U = U, Z = Z)#, AF = AF))
}

reference_sample <- function(U, Z, fun) {
	eigU <- eigen(U, symmetric = TRUE)
	vecUZ2 <- (t(eigU$vec) %*% Z)^2 # vector
	lambda <- optimize(f = as.function(get(fun)), interval = c(0,1), eigval = eigU$val, vecUZ2 = vecUZ2, maximum = FALSE)$minimum
	U * lambda + diag(length(Z)) * (1. - lambda)
}

derivLH <- function(x, eigval, vecUZ2){ # equation for derivative of LH
    ex <- eigval * x + 1. - x
	vvv <- (eigval - 1.)/ex^2
	abs(1. - sum(vvv * vecUZ2) / sum(vvv * ex))
}

LH <- function(x, eigval, vecUZ2){ # max LH
   ex <- eigval * x + 1. - x
   sum(log(ex)) + sum(vecUZ2/ex)
}

Urank <- function(U) {
	Q <- suppressWarnings(chol(U, pivot = TRUE))
    attr(Q, "rank")
}
