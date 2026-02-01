#-------------------------------------------------------------------------------#
# Package: Network-Based Genome-Wide Association Studies                        #
# Author: Pariya Behrouzi                                                       #
# Emails: <pariya.Behrouzi@gmail.com>                                           #
# Date: Nov 21th 2017                                                           #
#-------------------------------------------------------------------------------#

calculate_EM_Gibbs = function(chain, y, rho=rho[1], Theta=NULL, lower.upper, em.tol=.001, em.iter=10, c.em.iter=1, gibbs.iter=1000, mc.iter=1000, ncores = 4)
{
	n = nrow(y)
	p = ncol(y) 
	c.em.iter = 1
	dif  =  100
	while((c.em.iter <= em.iter) && (dif >= em.tol)) 
	{   
		R   <- calculate.R.internal( y, theta = Theta, lower.upper = lower.upper, gibbs.iter = gibbs.iter, mc.iter = mc.iter, ncores = ncores)
		R.gl <- glasso(s= R, rho, maxit=1000, penalize.diagonal=FALSE)
		if(det(R.gl$w) <= 0)
		{
		  R.gl$w <- nearPD(R.gl$w,keepDiag=TRUE)$mat 
		}
		dif <- sum(abs(Theta - R.gl$wi)/p^2) 
		Theta = as(R.gl$wi, "dgTMatrix") 
		Theta = as(Theta, "sparseMatrix") 
		c.em.iter <- c.em.iter + 1
	}
	  
	results <- list()
	results$Theta	<- Matrix(Theta, sparse=TRUE)
	results$Sigma	<- Matrix(R.gl$w, sparse=TRUE)
	results$ES		<- Matrix(R, sparse = TRUE)
	results$Z		<- NULL
	results$rho		<- rho
	results$loglik	<- n/2 *(determinant(results$Theta, logarithm = TRUE)$modulus - sum(diag(results$ES %*%results$Theta)))

	return(results)
}