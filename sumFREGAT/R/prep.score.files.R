# sumFREGAT (2017-2022) Gulnara R. Svishcheva & Nadezhda M. Belonogova, ICG SB RAS

prep.score.files <- function(data, reference = 'ref1KG.MAC5.EUR_AF.RData', output.file.prefix) {
# 'CHROM', 'POS', 'ID', 'EA', 'P', 'BETA', 'EAF'
	if (length(data) == 1) {
		input.file <- data
		if (requireNamespace("data.table", quietly = TRUE)) {
			suppressWarnings(df <- data.table::fread(input.file, header = TRUE, data.table = FALSE))
		} else {
			df <- read.table(input.file, header = TRUE, as.is = TRUE)
		}
	} else if (length(data) > 1) {
		df <- data
		input.file <- 'scores'
	}

	cn <- toupper(colnames(df))
	v <- which(cn %in% c('CHR', 'CHROMOSOME', 'CHROM'))
	if (length(v) == 1) colnames(df)[v] <- 'CHROM'
	v <- which(cn %in% c('POSITION', 'POSITIONS', 'MAP', 'POS'))
	if (length(v) == 1) colnames(df)[v] <- 'POS'
	v <- which(cn %in% c('PVALUE', 'PV', 'PVAL', 'P.VALUE', 'P_VALUE', 'P'))
	if (length(v) == 1) colnames(df)[v] <- 'P'
	v <- which(cn %in% c('RSID', 'RS.ID', 'RS_ID', 'SNP.ID', 'SNP_ID', 'ID'))
	if (length(v) == 1) colnames(df)[v] <- 'ID'
	v <- which(cn == 'EA')
	if (length(v) == 1) {
		colnames(df)[v] <- 'EFFECT.ALLELE'
		df[, 'EFFECT.ALLELE'] <- toupper(df[, 'EFFECT.ALLELE'])
		}
	
	# ID and PVAL mandatory
	# others from user file or reference

	ColNames <- c('ID', 'P')
	v <- !ColNames %in% colnames(df)
	if (sum(v)) stop(paste("Mandatory column(s) missing:", paste(ColNames[v], collapse = ', ')))

	df <- df[!is.na(df$P) & !is.na(df$ID), ]
	if (dim(df)[1] == 0) stop("No values assigned for P or ID")

	ColNames <- c('CHROM', 'POS', 'EAF')
	v <- !ColNames %in% colnames(df)
	take <- ColNames[v]
	if (sum(v)) print(paste("Columns that are missing and will be looked for in reference data:", paste(take, collapse = ', ')))
	take[take == 'EAF'] <- 'AF'

	if ('BETA' %in% colnames(df)) {
		df$BETA[df$BETA == 0] <- 1e-16
		if ('EFFECT.ALLELE' %in% colnames(df)) {
			colnames(df)[which(colnames(df) == 'REF')] <- 'REF0'
			colnames(df)[which(colnames(df) == 'ALT')] <- 'ALT0'
			take <- c(take, 'REF', 'ALT')
		} else {
			print("Effect allele column not found, effect sizes cannot be linked")
		}
	} else {
		print("Effect sizes (beta) column not found")
	}
	if (length(take) > 0) {
		is.ref <- 0
		is.ref.object <- 0
		if (length(reference) == 1) {
			if (!is.na(reference)) {
				if (file.exists(reference)) {
					is.ref <- 1
				} else {
					if (reference != '') print ("Reference file not found! Please download it from https://mga.bionet.nsc.ru/sumFREGAT/ref1KG.MAC5.EUR_AF.RData to use 1000 Genome Reference correlation matrices")
				}
			}
		} else if (length(reference) > 1) is.ref <- is.ref.object <- 1
		
		if (is.ref) {
			if (is.ref.object) {
				ref <- reference
			} else {
				print('Loading reference file...')
				ref <- get(load(reference))
			}
			colnames(ref) <- toupper(colnames(ref))
			if ('CHROM' %in% take & !'CHROM' %in% colnames(ref)) stop ("No CHROM column in data and reference")
			if ('POS' %in% take & !'POS' %in% colnames(ref)) stop ("No POS column in data and reference")
			v <- match(df$ID, ref$ID)
			
			if (!sum(v, na.rm = TRUE)) {
				if (all(c('CHROM', 'POS') %in% colnames(df))) {
					df$ind <- paste(df$CHROM, df$POS, sep = ':')
					print('No IDs matching, trying to link through map data...')
					ref$ind <- paste(ref$CHROM, ref$POS, sep = ':')
					v <- match(df$ind, ref$ind)
					if (sum(!is.na(v)) < (length(v) / 2)) {
						print("Too few variants match between input file and reference data")
						v <- NA
					}
				}
			}
			if (sum(v, na.rm = TRUE)) {
				print(paste(sum(!is.na(v)), "of", length(v), "variants found in reference"))
				vv <- take %in% colnames(ref)
				if (sum(!vv)) {
					print(paste("Columns that are missing in reference data:", paste(take[!vv], collapse = ', ')))
					if ('REF' %in% take & !'REF' %in% colnames(ref)) {
						print ("Reference alleles not found, effect sizes cannot be linked")
						df$BETA <- df$EFFECT.ALLELE <- NULL
					}
					if ('AF' %in% take & !'AF' %in% colnames(ref)) print ("Allele frequencies not found, some weighted tests will be unavailable")
				}
				df <- cbind(df, ref[v, take[vv]])
			}
		} else {
			v <- NA
		}
		if (sum(v, na.rm = TRUE) == 0) { # fail to open or link reference data
			if (any(c('CHROM', 'POS') %in% take)) stop ("Cannot find map data (chromosome, position)")
			if ('BETA' %in% colnames(df)) {
				warning ("Reference unavailable, effect sizes not linked")
				df$BETA <- df$EFFECT.ALLELE <- NULL
			}
		}
	}

	if ('REF' %in% colnames(df) & 'EFFECT.ALLELE' %in% colnames(df)) {
		v <- c()
		if (all(c('REF', 'REF0', 'ALT', 'ALT0') %in% colnames(df))) {
			v <- which((df$REF0 != df$REF & df$REF0 != df$ALT) | (df$ALT0 != df$REF & df$ALT0 != df$ALT))
		}
		if ('ALT' %in% colnames(df)) {
			v <- unique(c(v, which(df$EFFECT.ALLELE != df$REF & df$EFFECT.ALLELE != df$ALT)))
		}
		if (sum(v, na.rm = T)) {
			print(paste("Effect alleles or REF/ALT alleles do not match reference data for", sum(v), "variant(s)"))
			df[v, 'BETA'] <- NA
		}
		df[is.na(df$EFFECT.ALLELE) | is.na(df$REF), 'BETA'] <- NA
		v <- which(df$EFFECT.ALLELE == df$REF)
		#here we go
		df$BETA[v] <- -df$BETA[v]
		if ('EAF' %in% colnames(df)) {
			df$EAF[v] <- 1 - df$EAF[v]
			colnames(df)[colnames(df) == 'EAF'] <- 'AF'
		}
		print(paste('Effect sizes recoded for', length(v), 'variant(s)'))
	}

	if (any(df$P == 0)) {
		print("Some P values equal zero, will be assigned to minimum value in the sample")
		df$P[df$P == 0] <- min(df$P[df$P > 0])
	}
	df$Z <- qnorm(df$P / 2, lower.tail = FALSE)
	if ('BETA' %in% colnames(df)) {
		df$Z <- df$Z * sign(df$BETA)
		df$SE.BETA <- df$BETA / df$Z
	}
	
	if (!missing(output.file.prefix)) {
		fn <- paste(output.file.prefix, 'vcf', sep = '.')
	} else {
		fn <- paste(input.file, 'vcf', sep = '.')
	}

	df <- df[order(df[, 'POS']), ]
	df <- df[order(df[, 'CHROM']), ]
	if (!'ALT' %in% colnames(df)) df$ALT <- NA
	if (!'REF' %in% colnames(df)) df$REF <- NA
	vcf <- df[, c('CHROM', 'POS', 'ID', 'REF', 'ALT')]
	colnames(vcf)[1] <- '#CHROM'
	vcf$POS <- format(vcf$POS, scientific = FALSE)
	vcf$POS <- gsub(' ', '', vcf$POS)
	vcf <- cbind(vcf, QUAL = '.', FILTER = '.')
	vcf$INFO <- paste0('Z=', df$Z)
	title <- c('##INFO=<ID=Z,Number=1,Type=Float,Description="Z statistics">')

	if ('BETA' %in% colnames(df)) {
		vcf$INFO <- paste0(vcf$INFO, ';SE.Beta=', df$SE.BETA)
		title <- c(title, '##INFO=<ID=SE.Beta,Number=1,Type=Float,Description="SE Beta">')
	}

	if ('EAF' %in% colnames(df)) colnames(df)[colnames(df) == 'EAF'] <- 'AF'
	if ('AF' %in% colnames(df)) {
		vcf$INFO <- paste0(vcf$INFO, ';AF=', df$AF)
		title <- c(title, '##INFO=<ID=AF,Number=1,Type=Float,Description="Frequency of alternative allele">')
		print(paste0('Allele frequencies found and linked'))
	}

	a <- grep('\\bW', colnames(df))
	if (length(a) == 1) {
		vcf$INFO <- paste0(vcf$INFO, ';W=', df[, a])
		title <- c(title, '##INFO=<ID=W,Number=1,Type=Float,Description="Weights">')
		print(paste0("User weights ('", colnames(df)[a], "') found and linked"))
	}

	a <- grep('\\bANNO', colnames(df), value = TRUE)
	if (length(a) == 1) {
		vcf$INFO <- paste0(vcf$INFO, ';ANNO=', df[, a])
		title <- c(title, '##INFO=<ID=ANNO,Number=1,Type=String,Description="Variants annotations">')
		print(paste0("Annotations ('", colnames(df)[a], "') found and linked"))
	}

	a <- grep('\\bPROB', colnames(df), value = TRUE)
	for (an in a) {
		vcf$INFO <- paste0(vcf$INFO, ';', an, '=', df[, as.character(an)])
		title <- c(title, paste0("##INFO=<ID=", an, ",Number=1,Type=Float,Description='", an, "'>"))
		print(paste0("Column '", an, "' linked"))
	}

	write.table(title, fn, col.names = FALSE, row.names = FALSE, quote = FALSE, sep = '\t')
	if (requireNamespace("data.table", quietly = TRUE)) {
		suppressWarnings(data.table::fwrite(vcf, fn, row.names = FALSE, quote = FALSE, append = TRUE, col.names = TRUE, sep = '\t', na = 'NA'))
	} else {
		suppressWarnings(write.table(vcf, fn, row.names = FALSE, quote = FALSE, append = TRUE, sep = '\t'))
	}

	fn.gz <- paste(fn, 'gz', sep = '.')
	if (file.exists(fn.gz)) system(paste('rm', fn.gz))
	system(paste('bgzip', fn))
	system(paste('tabix -p vcf', fn.gz))
	print(paste('File', fn.gz, 'has been created'))

}

