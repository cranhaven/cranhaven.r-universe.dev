##
## Function to perform Tukey test
##
make.TukeyC.test <- function(obj,
			     MSE,
			     sig.level,
			     dfr,
			     round,
			     adjusted.pvalue)
{

	means <- obj[['means']]
	names(means) <- obj[['reps']]

	vece <- outer(X = means,
		      Y = means,  # (v)ariance (e)stimation of (c)ontrast (e)stimation
		      function(X, Y) as.vector(MSE) * (1/as.numeric(names(X)) + 1/as.numeric(names(Y))))

	qTukey <- qtukey(p = sig.level,
			 nmeans = dim(obj)[1],
			 df = dfr,
			 lower.tail = FALSE)

	msd <- qTukey * sqrt(1/2 * vece)  # minimum significative difference

	diag(msd) <- 0

	dimnames(msd) <- list(row.names(obj),
			      row.names(obj))

	names(means) <- row.names(obj) # necessary to difm object 

	difm <- abs(outer(means,
			  means,
			  "-"))  # means difference

	# The below estimates the probability of observed difference betweeen means be significative
	# Matrix of the difference of means above diagonal and respective p-values of the Tukey test
	# below diagonal 
	pval_tukey <- ptukey(q = difm[lower.tri(difm)] / sqrt(1/2 * vece[lower.tri(vece)]),
			     nmeans = dim(obj)[1],
			     df = dfr,
			     lower.tail = FALSE) 

	adjusted_pvalue <- p.adjust(pval_tukey,
				    method = adjusted.pvalue)

	new_dif <- difm
	new_dif[lower.tri(new_dif)] <- adjusted_pvalue
	new_dif1 <- t(new_dif)
	new_dif1[lower.tri(new_dif1)] <- adjusted_pvalue
	diag(new_dif1) <- 1

	new_dif2 <- ifelse(new_dif1 >= sig.level,
			   FALSE,
			   TRUE)

	out_groups  <- make.TukeyC.groups(new_dif2)

	res  <- cbind(format(round(means,
				   round),
			     nsmall = 2),
		      out_groups)

	colnames(res) <- c('Means',
			   paste('G',
				 1:(ncol(res) - 1),
				 sep=''))

	difm[lower.tri(difm)] <- adjusted_pvalue

	diag(difm) <- 0  # To be sure!

	out <- list(Result     = as.data.frame(res),
		    Sig.level  = sig.level,
		    Diff_Prob  = round(difm, 3),
		    MSD        = round(msd, 3),
		    Replicates = obj[['reps']])

	return(out)
}
