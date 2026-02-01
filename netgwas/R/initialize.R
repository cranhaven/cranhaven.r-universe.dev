#-------------------------------------------------------------------------------#
# Package: Network-Based Genome-Wide Association Studies                        #
# Author: Pariya Behrouzi                                                       #
# Emails: <pariya.Behrouzi@gmail.com>                                           #
# Date: Nov 21th 2017                                                           #
#-------------------------------------------------------------------------------#

initialize = function(y, rho = NULL, n_rho = NULL, rho_ratio = NULL, ncores=NULL )
{
	p <- ncol(y)
	n <- nrow(y)
	lower_upper = lower.upper(y)
	
	if(is.null(rho))
	{
		if(is.null(n_rho)) n_rho = 10
		if(is.null(rho_ratio)) rho_ratio = 0.3
		cr = cor(y, method="spearman") - diag(p)
		cr[is.na(cr)] <- 0
		rho_max = max( max(cr),-min(cr) )
		if(rho_max == 0) 
		{
			ty <- npn(y, npn.func= "shrinkage")
			cr = cor(ty, method="spearman") - diag(p)
			rho_max = max(max(cr),-min(cr))
		}
		if(rho_max >= .7) rho_max = .7
		rho_min = rho_ratio * rho_max
		rho = exp(seq(log(rho_max), log(rho_min), length = n_rho))
		rm(cr, rho_max, rho_min, rho_ratio)
		gc()
	}

	## Initialize S
	Z <- matrix(0, n, p)
	diag_element <- rep(0, p)
	if(ncores > 1)
	{
		cl <- makeCluster(ncores)
		tmp2 <- parLapply(cl = cl, 1:p, function(i) { 
		element_S_j(i, lower_upper ); 
		})
		stopCluster(cl)
	}else{
		tmp2 <- lapply(1:p, function(i){ element_S_j(i, lower_upper );})
	}
	Z <-  do.call(cbind, lapply(1:p, function(x) tmp2[[x]]$EX ))
	diag_element <- unlist(lapply(1:p, function(x)mean(tmp2[[x]]$EXX)))
	ES <- t(Z) %*% Z / n
	diag(ES) <- diag_element
	rm(tmp2, diag_element)	 
	gc() 

	return(list(Z=Z, ES = ES, rho = rho, lower_upper=lower_upper))
}	