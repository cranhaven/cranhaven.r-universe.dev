# simpleM() function written by Minghui Wang, Yiyuan Liu, Shizhong Han

sumstat.simpleM <- function(obj) {

	with(obj, with(df, {# Z, U, var.fraction

		x <- pnorm(abs(Z), lower.tail = FALSE) * 2
		min_p_obs <- min(x)
		num_of_snps <- length(x)
		
		eigen_values <- eigen(U, only.values = TRUE, symmetric = TRUE)$values
		eigen_values_sorted <- sort(eigen_values, decreasing = TRUE)
		
		sum_eigen_values <- sum(eigen_values_sorted)
		
		M_eff_G <- 1
		for(k in 1:num_of_snps){
			temp <- sum(eigen_values_sorted[1:k])/sum_eigen_values
			if(temp >= var.fraction){
				M_eff_G <- k
				break
			}
		}
		1-(1-min_p_obs)^M_eff_G

	}))
}

'simpleM' <- function (score.file, gene.file, genes = 'all', cor.path = 'cor/',
anno.type = '', var.fraction = .995, write.file = FALSE, quiet = FALSE) {

############ COMMON CHECKS

tmp <- check.input(score.file, cor.path, gene.file, genes)
for (i in 1:length(tmp)) assign(names(tmp)[i], tmp[[i]])

############ SPECIFIC CHECKS

check.list <- get.check.list('simpleM', score.file, anno.type)

############ ANALYSIS

genewise(score.file, gene.file, gf, anno.type, cor.path, cor.file.ext, check.list, write.file, obj0 = list(var.fraction = var.fraction), quiet = quiet, test = 'simpleM')

}