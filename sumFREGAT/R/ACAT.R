# ACAT() function written by Yaowu Liu

sumstat.ACAT <- function(obj){
	with(obj, with(df, {# Z, w, is.rare, U  
        
		rare <- sum(is.rare)

		if(rare == 0) { ### only ACAT
			Bp <- c()
			Bw <- c()
		} else { ### Burden
			wa.rare <- w[is.rare]
			wb.rare <- wb[is.rare]
			w <- w[!is.rare]
			Z.rare <- Z[is.rare]
			Z <- Z[!is.rare]
			#U.rare  <- U[is.rare,is.rare]

			Bp <- pchisq((sum(wb.rare * Z.rare) ^ 2)/sum(wb.rare^2), 1, lower.tail = FALSE) ### with w, but without U
			#Bp <- pchisq((sum(Z.rare) ^ 2)/rare, 1, lower.tail = FALSE)					### without weigths and U
			#Bp  <- pchisq((sum(w.rare * Z.rare) ^ 2)/sum(t(U.rare * w.rare) * w.rare) , 1, lower.tail = FALSE)  ### classic
			#Bp  <- pchisq((sum(Z.rare) ^ 2)/sum(U.rare) , 1, lower.tail = FALSE)  ###  with U, but without w

			Bw <- mean(wa.rare)
		}
		#browser()
		if (rare == length(is.rare)){ ### only burden
			pval <- Bp
		} else {
			p <- pnorm(abs(Z), lower = FALSE) * 2
			p <- c(Bp, p)
			p[p == 1] <- 1 - 1e-16
			w <- c(Bw, w)
		#browser()
			w <- w / sum(w)
			#### check if there are very small non-zero p values
			is.small <- (p < 1e-16)
			if (sum(is.small) == 0) {
				cct.stat <- sum(w * tan((0.5 - p) * pi))
			} else {
				cct.stat <- sum((w[is.small] / p[is.small]) / pi)
				cct.stat <- cct.stat + sum(w[!is.small] * tan((0.5 - p[!is.small]) * pi))
			}
			#### check if the test statistic is very large.
			if (cct.stat > 1e+15){
				pval <- (1 / cct.stat) / pi
			} else {
				pval <- 1 - pcauchy(cct.stat)
			}
		}
		return(pval)
	}))
}

'ACAT' <- function (score.file, gene.file, genes = 'all', anno.type = '',
beta.par = c(1, 1), weights.function = NULL, user.weights = FALSE, gen.var.weights = 'none', mac.threshold = NA, n = NA,
write.file = FALSE, quiet = FALSE) {

do.call(ACAT.int, c(as.list(environment()), prob = NA, phred = NA))

}

'ACAT.int' <- function (score.file, gene.file, genes = 'all', anno.type = '',
beta.par = c(1, 1), weights.function = NULL, user.weights = FALSE, gen.var.weights = 'none', mac.threshold = NA, n = NA,
write.file = FALSE, quiet = FALSE, prob = NA, phred, staar = FALSE) {

############ COMMON CHECKS

tmp <- check.input(score.file, 'do not check cor.path', gene.file, genes)
for (i in 1:length(tmp)) assign(names(tmp)[i], tmp[[i]])

############ SPECIFIC CHECKS

if (!is.na(mac.threshold)) {
	if (!is.numeric(mac.threshold)) stop('mac.threshold must be a numeric value')
	if (is.na(n)) stop('n must be set to enable mac.threshold')
}

tmp <- check.weights(weights.function, beta.par, gen.var.weights)
for (i in 1:length(tmp)) assign(names(tmp)[i], tmp[[i]])

if (!is.na(prob)) user.weights <- prob
check.list <- get.check.list('ACAT', score.file, anno.type, user.weights, gen.var.weights, fweights, n = n, mac.threshold = mac.threshold)

############ ANALYSIS

genewise(score.file, gene.file, gf, anno.type, check.list = check.list, write.file = write.file, gen.var.weights = gen.var.weights, fweights = fweights, mac.threshold = mac.threshold, n = n, staar = staar, quiet = quiet, phred = phred, test = 'ACAT')

}
