#-------------------------------------------------------------------------------#
# Package: Network-Based Genome-Wide Association Studies                        #
# Author: Pariya Behrouzi                                                       #
# Emails: <pariya.Behrouzi@gmail.com>                                           #
# Date: Nov 21th 2017                                                           #
#-------------------------------------------------------------------------------#

cond_N <- function(j, Sigma, Z , Z_new, diag_element, lower_upper)
{
	p <- ncol(Sigma)
	tmp <- matrix(Sigma[j, -j], 1, p-1)
	tmp1 <- solve(Sigma[-j, -j])

	mu <- tmp %*% tmp1 %*% t(Z[, -j])				
	mu <- as.vector(mu)		 
	sigma <- Sigma[j, j] - tmp %*% tmp1 %*% t(tmp)
	sigma <- sqrt(sigma)  		
		  
	obj <- element_S( lower= lower_upper$lower[ ,j], upper= lower_upper$upper[ ,j], mu=mu, sigma=sigma)      

	Z_new <- obj$EX
	diag_element <- mean(obj$EXX)
	
	rm(tmp, tmp1, mu, sigma, obj )
	gc()
	
	return(list(Z_new=Z_new, diag_element=diag_element))
}
